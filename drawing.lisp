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
;;; $Id: drawing.lisp,v 1.17 2007/10/01 19:05:13 xach Exp $

(in-package #:vecto)

(deftype octet ()
  '(unsigned-byte 8))

(deftype vector-index ()
  `(mod ,array-dimension-limit))

(deftype octet-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(defun nonzero-winding-alpha (alpha)
  (min 255 (abs alpha)))

(defun even-odd-alpha (alpha)
  (let ((value (mod alpha 512)))
    (min 255 (if (< value 256) value (- 512 value)))))

;; ( (t) = (a) * (b) + 0x80, ( ( ( (t)>>8 ) + (t) )>>8 ) )

(defun imult (a b)
  (let ((temp (+ (* a b) #x80)))
    (logand #xFF (ash (+ (ash temp -8) temp) -8))))

(defun lerp (p q a)
  (logand #xFF (+ p (imult a (- q p)))))

(defun prelerp (p q a)
  (logand #xFF (- (+ p q) (imult a p))))

(defun png-draw-function (data width height fill-source alpha-fun)
  "From http://www.teamten.com/lawrence/graphics/premultiplication/"
  (declare (ignore height))
  (lambda (x y alpha)
    (multiple-value-bind (r.fg g.fg b.fg a.fg)
        (funcall fill-source x y)
      (setf alpha (funcall alpha-fun alpha))
      (when (plusp alpha)
        (let* ((i (* +png-channels+ (+ x (* y width))))
               (r.bg (aref data (+ i 0)))
               (g.bg (aref data (+ i 1)))
               (b.bg (aref data (+ i 2)))
               (a.bg (aref data (+ i 3)))
               (a.fg (imult alpha a.fg))
               (gamma (prelerp a.fg a.bg a.bg)))
          (flet ((blend (fg bg)
                   (let ((value (lerp (imult bg a.bg) fg a.fg)))
                     (float-octet (/ value gamma)))))
            (unless (zerop gamma)
              (setf (aref data (+ i 0)) (blend r.fg r.bg)
                    (aref data (+ i 1)) (blend g.fg g.bg)
                    (aref data (+ i 2)) (blend b.fg b.bg)))
            (setf (aref data (+ i 3)) gamma)))))))

(defun png-draw-function/clipped (data clip-data
                              width height
                              fill-source
                              alpha-fun)
  "Like DRAW-FUNCTION, but uses uses the clipping channel."
  (declare (ignore height))
  (lambda (x y alpha)
    (let* ((clip-index (+ x (* y width)))
           (clip (aref clip-data clip-index)))
      (setf alpha (imult clip (funcall alpha-fun alpha)))
      (when (plusp alpha)
        (multiple-value-bind (r.fg g.fg b.fg a.fg)
            (funcall fill-source x y)
          (let* ((i (* clip-index +png-channels+))
                 (r.bg (aref data (+ i 0)))
                 (g.bg (aref data (+ i 1)))
                 (b.bg (aref data (+ i 2)))
                 (a.bg (aref data (+ i 3)))
                 (a.fg (imult alpha a.fg))
                 (gamma (prelerp a.fg a.bg a.bg)))
            (flet ((blend (fg bg)
                     (let ((value (lerp (imult bg a.bg) fg a.fg)))
                       (float-octet (/ value gamma)))))
              (unless (zerop gamma)
                (setf (aref data (+ i 0)) (blend r.fg r.bg)
                      (aref data (+ i 1)) (blend g.fg g.bg)
                      (aref data (+ i 2)) (blend b.fg b.bg)))
              (setf (aref data (+ i 3)) gamma))))))))

(defun make-png-draw-function (data clipping-path
                               width height
                               fill-source
                               alpha-fun)
  (if (emptyp clipping-path)
      (png-draw-function data width height fill-source alpha-fun)
      (png-draw-function/clipped data (clipping-data clipping-path)
                             width height
                             fill-source
                             alpha-fun)))

(defun svg-draw-stroke-function (data stroke-color line-width)
  (lambda (path)
    (let ((r (float-octet (red stroke-color)))
          (g (float-octet (green stroke-color)))
          (b (float-octet (blue stroke-color)))
          (a (float-octet (alpha stroke-color))))
      (who:with-html-output-to-string (s data :indent t)
        (who:htm (:path :d path
                        :fill "none"
                        :stroke (format nil "rgb(~d, ~d, ~d)" r g b)
                        :stroke-width line-width))))))

(defun svg-draw-fill-function (data fill-color)
  (lambda (path)
    (let ((r (float-octet (red fill-color)))
          (g (float-octet (green fill-color)))
          (b (float-octet (blue fill-color)))
          (a (float-octet (alpha fill-color))))
      (who:with-html-output-to-string (s data :indent t)
        (who:htm (:path :d path
                        :fill (format nil "rgb(~d, ~d, ~d)" r g b)))))))

(defun make-svg-draw-stroke-function (data clipping-path stroke-color line-width)
  (if (emptyp clipping-path)
      (svg-draw-stroke-function data stroke-color line-width)
      (svg-draw-stroke-function/clipped data (clipping-data clipping-path) stroke-color line-width)))

(defun make-svg-draw-fill-function (data clipping-path fill-color)
  (if (emptyp clipping-path)
      (svg-draw-fill-function data fill-color)
      (svg-draw-fill-function/clipped data (clipping-data clipping-path) fill-color)))

(defun intersect-clipping-paths (data temp)
  (declare (type (simple-array (unsigned-byte 8) (*)) data temp))
  (map-into data #'imult temp data))

(defun draw-clipping-path-function (data width height alpha-fun)
  (declare (ignore height)
           (type (simple-array (unsigned-byte 8) (*)) data))
  (lambda (x y alpha)
    (let ((i (+ x (* width y))))
      (let ((alpha (funcall alpha-fun alpha)))
        (setf (aref data i) alpha)))))

(defun png-draw-paths (&key width height paths
                            transform-function
                            draw-function)
  "Use DRAW-FUNCTION as a callback for the cells sweep function
for the set of paths PATHS."
  (let ((state (aa:make-state))
        (paths (mapcar (lambda (path)
                         ;; FIXME: previous versions lacked
                         ;; paths:path-clone, and this broke fill &
                         ;; stroke because transform-path damages the
                         ;; paths. It would be nicer if transform-path
                         ;; wasn't destructive, since I didn't expect
                         ;; it to be.
                         (transform-path (paths:path-clone path)
                                         transform-function))
                       paths)))
    (vectors:update-state state paths)
    (aa:cells-sweep/rectangle state 0 0 width height draw-function)))

(defun svg-draw-paths (&key width height paths
                           transform-function
                           draw-function)
  (declare (ignorable width height transform-function))
  (dolist (path paths)
    (funcall draw-function path)))

;;; FIXME: this was added for drawing text paths, but the text
;;; rendering mode could be changed in the future, making it a little
;;; silly to have a fixed draw-function.

(defun draw-paths/state (paths state)
  (draw-paths :paths paths
              :width (width state)
              :height (height state)
              :transform-function (transform-function state)
              :draw-function (fill-draw-function state)))

(defun fill-image (image-data red green blue alpha)
  "Completely fill IMAGE with the given colors."
  (let ((r (float-octet red))
        (g (float-octet green))
        (b (float-octet blue))
        (a (float-octet alpha)))
    (do ((h 0 (+ h 4))
         (i 1 (+ i 4))
         (j 2 (+ j 4))
         (k 3 (+ k 4)))
        ((<= (length image-data) k))
      (setf (aref image-data h) r
            (aref image-data i) g
            (aref image-data j) b
            (aref image-data k) a))))

(defun color-source-function (color)
  (let ((red (float-octet (red color)))
        (green (float-octet (green color)))
        (blue (float-octet (blue color)))
        (alpha (float-octet (alpha color))))
    (lambda (x y)
      (declare (ignore x y))
      (values red green blue alpha))))

(defun fill-source-function (state)
  (or (fill-source state)
      (color-source-function (fill-color state))))

(defun stroke-source-function (state)
  (color-source-function (stroke-color state)))

(defun state-draw-function (state fill-source fill-style)
  "Create a draw function for the graphics state STATE."
  (make-png-draw-function (image-data state)
                          (clipping-path state)
                          (width state)
                          (height state)
                          fill-source
                          (ecase fill-style
                            (:even-odd #'even-odd-alpha)
                            (:nonzero-winding #'nonzero-winding-alpha))))

(defgeneric stroke-draw-function (state)
  (:documentation "Function for drawing a stroke in this graphics state."))

(defmethod stroke-draw-function ((state png-graphics-state))
  (state-draw-function state
                       (stroke-source-function state)
                       :nonzero-winding))

(defmethod stroke-draw-function ((state svg-graphics-state))
  (make-svg-draw-stroke-function (image-data state)
                                 (clipping-path state)
                                 (stroke-color state) (line-width state)))

(defgeneric fill-draw-function (state)
  (:documentation "Function for drawing a fill in this graphics state."))

(defmethod fill-draw-function ((state png-graphics-state))
  (state-draw-function state
                       (fill-source-function state)
                       :nonzero-winding))

(defmethod fill-draw-function ((state svg-graphics-state))
  (make-svg-draw-fill-function (image-data state)
                               (clipping-path state)
                               (fill-color state)))

(defgeneric even-odd-fill-draw-function (state)
  (:documentation "Function for even-odd drawing a fill in this graphics state."))

(defmethod even-odd-fill-draw-function ((state png-graphics-state))
  (state-draw-function state
                       (fill-source-function state)
                       :even-odd))

(defun tolerance-scale (state)
  (let ((matrix (transform-matrix state)))
    (abs (/ 1.0 (min (transform-matrix-x-scale matrix)
                     (transform-matrix-y-scale matrix))))))

(defgeneric state-stroke-paths (state)
  (:documentation "Compute the outline paths of the strokes for the
current paths of STATE."))

(defmethod state-stroke-paths ((state png-graphics-state))
  (let ((paths (dash-paths (paths state)
                           (dash-vector state)
                           (dash-phase state)))
        (paths:*bezier-distance-tolerance*
         (* paths:*bezier-distance-tolerance* (tolerance-scale state))))
    (png-stroke-paths paths
                      :line-width (line-width state)
                      :join-style (join-style state)
                      :cap-style (cap-style state))))

(defmethod state-stroke-paths ((state svg-graphics-state))
  (svg-stroke-paths (paths state)
                    :line-width (line-width state)
                    :join-style (join-style state)
                    :cap-style (cap-style state)))

(defgeneric draw-stroked-paths (state)
  (:documentation "Create a set of paths representing a stroking of
the current paths of STATE, and draw them to the image."))

(defmethod draw-stroked-paths ((state png-graphics-state))
  (png-draw-paths :paths (state-stroke-paths state)
                  :width (width state)
                  :height (height state)
                  :transform-function (transform-function state)
                  :draw-function (stroke-draw-function state)))

(defmethod draw-stroked-paths ((state svg-graphics-state))
  (svg-draw-paths :paths (paths state)
                  :width (width state)
                  :height (height state)
                  :transform-function (transform-function state)
                  :draw-function (stroke-draw-function state)))

(defun png-close-paths (paths)
  (dolist (path paths)
    (setf (paths::path-type path) :closed-polyline)))

(defgeneric draw-filled-paths (state)
  (:documentation "Fill the paths of STATE into the image."))

(defmethod draw-filled-paths ((state png-graphics-state))
  (png-close-paths (paths state))
  (png-draw-paths :paths (paths state)
                  :width (width state)
                  :height (height state)
                  :transform-function (transform-function state)
                  :draw-function (fill-draw-function state)))

(defun svg-close-path (path)
  (with-output-to-string (s path) (format s "Z")))

(defmethod draw-filled-paths ((state svg-graphics-state))
  (svg-close-path (path state))
  (svg-draw-paths :paths (paths state)
                  :width (width state)
                  :height (height state)
                  :transform-function (transform-function state)
                  :draw-function (fill-draw-function state)))

(defun draw-even-odd-filled-paths (state)
  "Fill the paths of STATE into the image."
  (close-paths (paths state))
  (draw-paths :paths (paths state)
              :width (width state)
              :height (height state)
              :transform-function (transform-function state)
              :draw-function (even-odd-fill-draw-function state)))

(defun draw-clipping-path (state alpha-fun)
  (let ((data (writable-clipping-data (clipping-path state)))
        (scratch (scratch (clipping-path state)))
        (width (width state))
        (height (height state)))
    (declare (type octet-vector data scratch))
    (fill scratch 0)
    (draw-paths :paths (paths state)
                :width (width state)
                :height (height state)
                :transform-function (transform-function state)
                :draw-function (draw-clipping-path-function scratch
                                                            width
                                                            height
                                                            alpha-fun))
    (intersect-clipping-paths data scratch)))

(defun make-clipping-path-function (state type)
  (ecase type
    (:nonzero-winding
     (lambda ()
       (draw-clipping-path state #'nonzero-winding-alpha)))
    (:even-odd
     (lambda ()
       (draw-clipping-path state #'even-odd-alpha)))))
