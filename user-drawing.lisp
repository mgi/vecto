;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; $Id: user-drawing.lisp,v 1.21 2007/10/01 14:12:55 xach Exp $

(in-package #:vecto)

(defvar *graphics-state*)
(setf (documentation '*graphics-state* 'variable)
      "The currently active graphics state. Bound for the
      duration of WITH-GRAPHICS-STATE.")

;;; Low-level path construction

(defgeneric %move-to (state x y)
  (:documentation "Move to (x,y) position in graphics state."))

(defmethod %move-to ((state png-graphics-state) x y)
  (let ((path (paths:create-path :open-polyline)))
    (push (setf (path state) path) (paths state))
    (paths:path-reset path (paths:make-point x y))))

(defmethod %move-to ((state svg-graphics-state) x y)
  (let ((path (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (push (setf (path state) path) (paths state))
    (with-accessors ((h height)) state
      (with-output-to-string (s path)
        (format s "M ~d ~d " x (- h y))))))

(defgeneric %line-to (state x y)
  (:documentation "Draw a line up to (x,y) position in graphics
  state."))

(defmethod %line-to ((state png-graphics-state) x y)
  (paths:path-extend (path state) (paths:make-straight-line)
                     (paths:make-point x y)))

(defmethod %line-to ((state svg-graphics-state) x y)
  (with-accessors ((h height)
                   (path path)) state
    (with-output-to-string (s path)
      (format s "L ~d ~d " x (- h y)))))

(defgeneric %curve-to (state cx1 cy1 cx2 cy2 x y)
  (:documentation "Draw a cubic Bezier curve from the current point
  to (x,y) through two control points."))

(defmethod %curve-to ((state png-graphics-state) cx1 cy1 cx2 cy2 x y)
  (let ((control-point-1 (paths:make-point cx1 cy1))
        (control-point-2 (paths:make-point cx2 cy2))
        (end-point (paths:make-point x y)))
    (paths:path-extend (path state)
                       (paths:make-bezier-curve (list control-point-1
                                                      control-point-2))
                       end-point)))

(defmethod %curve-to ((state svg-graphics-state) cx1 cy1 cx2 cy2 x y)
  (with-accessors ((h height)
                   (path path)) state
    (with-output-to-string (s path)
      (format s "C ~d ~d ~d ~d ~d ~d " cx1 (- h cy1) cx2 (- h cy2) x (- h y)))))

(defgeneric %quadratic-to (state cx cy x y)
  (:documentation "Draw a quadratic Bezier curve from the current
  point to (x,y) through one control point."))

(defmethod %quadratic-to ((state png-graphics-state) cx cy x y)
  (paths:path-extend (path state)
                     (paths:make-bezier-curve (list (paths:make-point cx cy)))
                     (paths:make-point x y)))

(defmethod %quadratic-to ((state svg-graphics-state) cx cy x y)
  (with-accessors ((h height)
                   (path path)) state
    (with-output-to-string (s path)
      (format s "S ~d ~d ~d ~d " cx (- h cy) x (- h y)))))

(defun draw-arc-curves (curves)
  (destructuring-bind (((startx . starty) &rest ignored-curve)
                       &rest ignored-curves)
      curves
    (declare (ignore ignored-curve ignored-curves))
    (if (path *graphics-state*)
        (line-to startx starty)
        (move-to startx starty)))
  (loop for ((x1 . y1)
             (cx1 . cy1)
             (cx2 . cy2)
             (x2 . y2)) in curves
        do (curve-to cx1 cy1 cx2 cy2 x2 y2)))

(defgeneric %close-subpath (state)
  (:documentation "Close the current path of the graphics state."))

(defmethod %close-subpath ((state png-graphics-state))
  (setf (paths::path-type (path state)) :closed-polyline))

(defmethod %close-subpath ((state svg-graphics-state))
  (with-output-to-string (s (path state))
    (format s "Z")))

;;; Clipping path

(defgeneric %end-path-no-op (state)
  (:documentation "End the current path without painting anything."))

(defmethod %end-path-no-op ((state png-graphics-state))
  (after-painting state))

(defgeneric %clip-path (state)
  (:documentation "Defines a clipping path based on the current
  path."))

(defmethod %clip-path ((state png-graphics-state))
  (call-after-painting state
                       (make-clipping-path-function state :nonzero-winding)))

(defgeneric %even-odd-clip-path (state)
  (:documentation "Like %CLIP-PATH, but uses the even/odd fill rule to
  determine the outline of the clipping path."))

(defmethod %even-odd-clip-path ((state png-graphics-state))
  (call-after-painting state
                       (make-clipping-path-function state :even-odd)))

;;; Text

(defgeneric %get-font (state file)
  (:documentation "Get a font object from file."))

(defmethod %get-font ((state png-graphics-state) file)
  (find-font-loader state file))

(defgeneric %set-font (state font size)
  (:documentation "Set the size of the font object."))

(defmethod %set-font ((state png-graphics-state) loader size)
  (let* ((scale (loader-font-scale size loader))
         (matrix (scaling-matrix scale scale)))
    (setf (font state)
          (make-instance 'font
                         :loader loader
                         :transform-matrix matrix
                         :size size))))

(defgeneric %string-paths (state x y string)
  (:documentation "Add a path made of string in current font in the
  graphics state."))

(defmethod %string-paths ((state png-graphics-state) x y string)
  (let ((font (font state)))
    (unless font
      (error "No font currently set"))
    (string-primitive-paths x y string font
                            :character-spacing (character-spacing state))))

(defgeneric %draw-string (state x y string)
  (:documentation "Draws string on the canvas with the active font."))

(defmethod %draw-string ((state png-graphics-state) x y string)
  (draw-paths/state (%string-paths state x y string)
                    state))

(defgeneric %draw-centered-string (state x y string)
  (:documentation "Like %DRAW-STRING but the horizontal center of the
  string is positioned at x and the baseline of the string is
  positioned at y."))

(defmethod %draw-centered-string ((state png-graphics-state) x y string)
  (let* ((font (font state))
         (bbox
          (string-bounding-box string
                               (size font)
                               (loader font)
                               :character-spacing (character-spacing state)))
         (xmin (xmin bbox))
         (width/2 (/ (- (xmax bbox) xmin) 2.0)))
    (%draw-string state (- x (+ width/2 xmin)) y string)))

(defun string-paths (x y string)
  (setf (paths *graphics-state*)
        (append (paths *graphics-state*)
                (%string-paths *graphics-state* x y string)))
  (values))

(defun centered-string-paths (x y string)
  (let* ((font (font *graphics-state*))
         (bbox (string-bounding-box string (size font) (loader font)))
         (width/2 (/ (- (xmax bbox) (xmin bbox)) 2.0)))
    (setf (paths *graphics-state*)
          (append (paths *graphics-state*)
                  (%string-paths *graphics-state* (- x width/2) y string)))
    (values)))

;;; Low-level transforms

(defun %translate (state tx ty)
  (apply-matrix state (translation-matrix tx ty)))

(defun %scale (state sx sy)
  (apply-matrix state (scaling-matrix sx sy)))

(defun %skew (state x y)
  (apply-matrix state (skewing-matrix x y)))

(defun %rotate (state radians)
  (apply-matrix state (rotation-matrix radians)))

;;; User-level commands

(defun move-to (x y)
  (%move-to *graphics-state* x y))

(defun line-to (x y)
  (%line-to *graphics-state* x y))

(defun curve-to (cx1 cy1 cx2 cy2 x y)
  (%curve-to *graphics-state* cx1 cy1 cx2 cy2 x y))

(defun quadratic-to (cx cy x y)
  (%quadratic-to *graphics-state* cx cy x y))

(defun arc (cx cy r theta1 theta2)
  (loop while (< theta2 theta1) do (incf theta2 (* 2 pi)))
  (let ((curves
         (approximate-elliptical-arc cx cy r r 0 theta1 theta2)))
    (draw-arc-curves curves)))

(defun arcn (cx cy r theta1 theta2)
  (loop while (< theta1 theta2) do (decf theta2 (* 2 pi)))
  (let ((curves (approximate-elliptical-arc cx cy r r 0 theta2 theta1)))
    (draw-arc-curves (nreverse (mapcar #'nreverse curves)))))

(defun ellipse-arc (cx cy rx ry theta eta1 eta2)
  (loop while (< eta2 eta1) do (incf eta2 (* 2 pi)))
  (let ((curves (approximate-elliptical-arc cx cy rx ry theta eta1 eta2)))
    (draw-arc-curves curves)))

(defun ellipse-arcn (cx cy rx ry theta eta1 eta2)
  (loop while (< eta1 eta2) do (decf eta2 (* 2 pi)))
  (let ((curves (approximate-elliptical-arc cx cy rx ry theta eta2 eta1)))
    (draw-arc-curves (nreverse (mapcar #'nreverse curves)))))



(defun close-subpath ()
  (%close-subpath *graphics-state*))

(defun end-path-no-op ()
  (%end-path-no-op *graphics-state*)
  (clear-paths *graphics-state*))

(defun clip-path ()
  (%clip-path *graphics-state*))

(defun even-odd-clip-path ()
  (%even-odd-clip-path *graphics-state*))

(defun get-font (file)
  (%get-font *graphics-state* file))

(defun set-font (font size)
  (%set-font *graphics-state* font size))

(defun set-character-spacing (spacing)
  (setf (character-spacing *graphics-state*) spacing))

(defun draw-string (x y string)
  (%draw-string *graphics-state* x y string))

(defun draw-centered-string (x y string)
  (%draw-centered-string *graphics-state* x y string))

(defun set-dash-pattern (vector phase)
  (if (zerop (length vector))
      (setf (dash-vector *graphics-state*) nil
            (dash-phase *graphics-state*) nil)
      (setf (dash-vector *graphics-state*) vector
            (dash-phase *graphics-state*) phase)))

(defun set-line-cap (style)
  (assert (member style '(:butt :square :round)))
  (setf (cap-style *graphics-state*) style))

(defun set-line-join (style)
  (assert (member style '(:bevel :miter :round)))
  (setf (join-style *graphics-state*) (if (eql style :bevel) :none style)))

(defun set-line-width (width)
  (setf (line-width *graphics-state*) width))

(defun set-rgba-color (color r g b a)
  (setf (red color) (clamp-range 0.0 r 1.0)
        (green color) (clamp-range 0.0 g 1.0)
        (blue color) (clamp-range 0.0 b 1.0)
        (alpha color) (clamp-range 0.0 a 1.0))
  color)

(defun set-rgb-color (color r g b)
  (setf (red color) (clamp-range 0.0 r 1.0)
        (green color) (clamp-range 0.0 g 1.0)
        (blue color) (clamp-range 0.0 b 1.0)
        (alpha color) 1.0)
  color)

(defun set-rgb-stroke (r g b)
  (set-rgb-color (stroke-color *graphics-state*) r g b))

(defun set-rgba-stroke (r g b a)
  (set-rgba-color (stroke-color *graphics-state*) r g b a))

(defun set-rgb-fill (r g b)
  (clear-fill-source *graphics-state*)
  (set-rgb-color (fill-color *graphics-state*) r g b))

(defun set-rgba-fill (r g b a)
  (clear-fill-source *graphics-state*)
  (set-rgba-color (fill-color *graphics-state*) r g b a))

(defun stroke ()
  (draw-stroked-paths *graphics-state*)
  (clear-paths *graphics-state*))

(defun stroke-to-paths ()
  (let ((paths (state-stroke-paths *graphics-state*)))
    (clear-paths *graphics-state*)
    (setf (paths *graphics-state*) paths)
    (%close-subpath *graphics-state*)))

(defun fill-path ()
  (draw-filled-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun even-odd-fill ()
  (draw-even-odd-filled-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun fill-and-stroke ()
  (draw-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))

(defun even-odd-fill-and-stroke ()
  (draw-even-odd-filled-paths *graphics-state*)
  (draw-stroked-paths *graphics-state*)
  (after-painting *graphics-state*)
  (clear-paths *graphics-state*))


(defun clear-canvas ()
  (let ((color (fill-color *graphics-state*)))
    (fill-image (image-data *graphics-state*)
                (red color)
                (green color)
                (blue color)
                (alpha color))))

(defun translate (x y)
  (%translate *graphics-state* x y))

(defun scale (x y)
  (%scale *graphics-state* x y))

(defun skew (x y)
  (%skew *graphics-state* x y))

(defun rotate (radians)
  (%rotate *graphics-state* radians))

(defun rotate-degrees (degrees)
  (%rotate *graphics-state* (* (/ pi 180) degrees)))

(defgeneric %save (graphics-state file-or-stream)
  (:documentation "Output the graphics state to a file or a stream."))

(defmethod %save ((gs png-graphics-state) (file pathname))
  (zpng:write-png (image gs) file))

(defmethod %save ((gs png-graphics-state) (file string))
  (zpng:write-png (image gs) file))

(defmethod %save ((gs png-graphics-state) (stream stream))
  (zpng:write-png-stream (image gs) stream))

(defmethod %save ((gs svg-graphics-state) (file string))
  (with-open-file (fd file :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
    (write-string (image gs) fd)))

(defmacro with-png-canvas ((&key width height output) &body body)
  `(let ((*graphics-state* (make-instance 'png-graphics-state)))
     (state-image *graphics-state* ,width ,height)
     (unwind-protect
          (progn
            ,@body)
       (%save *graphics-state* ,output)
       (clear-state *graphics-state*))))

(defmacro with-svg-canvas ((&key width height output) &body body)
  (let ((stream (gensym "STREAM")))
    `(let ((*graphics-state* (make-instance 'svg-graphics-state))
           (*print-pprint-dispatch* (copy-pprint-dispatch)))
       (state-image *graphics-state* ,width ,height)
       (set-pprint-dispatch 'float (lambda (s f) (format s "~,4f" f)))
       (unwind-protect
            (who:with-html-output-to-string (,stream (image *graphics-state*)
                                                     :prologue "<?xml version=\"1.0\" standalone=\"yes\"?>"
                                                     :indent t)
              (who:fmt "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")
              (:svg :width ,width :height ,height
                    ,@body))
         (%save *graphics-state* ,output)))))

(defmacro with-graphics-state (&body body)
  `(let ((*graphics-state* (copy *graphics-state*)))
     ,@body))
