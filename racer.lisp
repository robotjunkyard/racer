;;;; cl-racer v0.90
;;
;; A graphical demonstration of that non-polygon-based scanline-road effect
;; exhibited in many arcade and console racing games in the early-mid '90s.
;;
;; Dependencies:
;;   Quicklisp:  get it at https://www.quicklisp.org/
;;
;; === LICENSE TEXT ======================================================================
;;
;; Copyright 2017 Nick Baker
;; 
;; Redistribution and use in source and binary forms, with or without modification,
;; are permitted provided that the following conditions are met:
;; 
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation and/or
;;    other materials provided with the distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
;; OF SUCH DAMAGE.
;;
;; === END LICENSE TEXT===================================================================


;; =======================================================================================
;; Various TODO items/ideas/challenges for near or distant future:
;;    * More sophisticated rendering that draws hills/inclines/declines over
;;      a distance more believably.
;;    * Scaling foliage, street signs, lights, etc. on sides of the road
;;    * Cars and trucks
;;    * Weather
;;    * Day & Night transitions
;; =======================================================================================


;; You must have installed Quicklisp prior to loading this file or this will fail.
;; Quicklisp's setup plops a setup.lisp file in these respective locations during install;
;; refer to Quicklisp's documentation for more details -- https://www.quicklisp.org/beta/#installation
(load
 #+linux (format nil "/home/~a/quicklisp/setup.lisp"
		 (sb-unix::posix-getenv "USERNAME"))
 #+win32 (format nil "C:/Users/~a/quicklisp/setup.lisp"
		 (sb-unix::posix-getenv "USERNAME"))
)

#-SDL-LOADED (ql:quickload "lispbuilder-sdl")
(pushnew :SDL-LOADED *features*)

(defpackage :racer
  (:use :common-lisp)
  (:use :common-lisp-user))

(in-package :racer)

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
#-SWANK  nil
#+SWANK  (continuable
          (let ((connection (or swank::*emacs-connection*
				(swank::default-connection))))
            (when connection
              (swank::handle-requests connection t)))))

(deftype uint8 ()
  '(unsigned-byte 8))
(deftype int8 ()
  '(signed-byte 8))
(deftype uint16 ()
  '(unsigned-byte 16))
(deftype int16 ()
  '(signed-byte 16))

(defparameter *colors*
  (coerce (list sdl:*black*
		sdl:*blue*
		sdl:*green*
		sdl:*red*)
	  'vector))

(defparameter *draw-frame* 0)
(defvar *res-x* 1280)
(defvar *res-y* 720)
(declaim (type fixnum *draw-frame* *res-x* *res-y*))

(defparameter *player-leftright-speed* 32)
(defparameter *gravity* 0.02)
(defparameter *px* 0)  ;; player position (0 = center of road)
(defparameter *py* 0)  ;; draw-road can not yet render height
(defparameter *pyf* 0) ;; upward force of player height

(defparameter *sky-color* (sdl:color :r 64 :g 64 :b 172))
(defparameter *sky-color-light* (sdl:color :r 122 :g 64 :b 238))
(defparameter *pavement-color* (sdl:color :r 56 :g 56 :b 48))
(defparameter *grass-light* (sdl:color :r 0 :g 128 :b 0))
(defparameter *grass-dark* (sdl:color :r 0 :g 64 :b 0))


(defun uilerp (a b f)
  (declare (type uint8 a b)
	   (type (unsigned-byte 16) f))
  (the uint8
       (ash (+ (* b f)
	       (* a (- 1024 f)))
	    -10)))

(defun lerp (a b f)
  (declare (type single-float a b f))
  (+ a (* f (- b a))))

(defun iilerp (a b f)
  (declare 
   (type int16 a b)
   (type single-float f))
  (nth-value 0 (truncate (+ a (* f (the int16 (- (the int16 b) (the int16 a)))))
			 1)))
(defun setup-window ()
  (print "-------")
  (print sdl:*default-display*)
  (print
   (sdl:window
    *res-x* *res-y*
    :title-caption "racer.lisp"
    :icon-caption "racer.lisp"
    :fullscreen nil))
  (setf (sdl:frame-rate) 60)
  (print sdl:*default-display*))

(let ((draw-rect (sdl:rectangle :fp nil))
      (pcolor (sdl:color :r 0 :g 0 :b 0))
      (stars (make-array 16 :element-type 'uint16
			 :initial-contents
			 (loop for x below 16
			    collect (random (if (evenp x) *res-x* 80))))))
  (defun draw-sky (&key
		     (horizon (truncate *res-y* 2))
		     (albedo-thickness 32))
    ;; draw horizon light stripes
    (sdl:set-rectangle-* draw-rect
			 :x 0 :y (- horizon albedo-thickness)
			 :w *res-x* :h albedo-thickness)
    (sdl:draw-box draw-rect :color *sky-color-light*)
    (sdl:set-rectangle-* draw-rect
			 :x 0 :y (- horizon (* 1.5 albedo-thickness))
			 :w *res-x* :h (truncate albedo-thickness 3))
    (sdl:draw-box draw-rect :color *sky-color-light*)
    (sdl:set-rectangle-* draw-rect
			 :x 0 :y (- horizon (* 1.75 albedo-thickness))
			 :w *res-x* :h (truncate albedo-thickness 8))
    (sdl:draw-box draw-rect :color *sky-color-light*)
    (sdl:draw-box draw-rect :color *sky-color-light*)
    (sdl:set-rectangle-* draw-rect
			 :x 0 :y (- horizon (* 1.9 albedo-thickness))
			 :w *res-x* :h (truncate albedo-thickness 16))
    (sdl:draw-box draw-rect :color *sky-color-light*)

    ;; draw stars
    ;; (todo)
    )
  
  (defun draw-road (&key
		      (cam 0)
		      (horizon (truncate *res-y* 2))
		      (hill 0.1)
		      (elevation 0.0)
		      (bend 0.0))
    (let* ((speed 0.50)
	   (vstretch 1.0)
	   (road-width 2048)
	   (shoulder-width 128)
	   (shoulder-strip-period 1.25)
	   (stripe-halfwidth 16)
	   (stripe-strip-period 1.25)
	   (center-x (truncate *res-x* 2))
	   (elevation-* (- 1.0 elevation)))
       ;; draw road
      (loop for line# from horizon below *res-y*
	 for dist% = (lerp hill (max 0.1 elevation-*) (float (- 1.0 (/ (- *res-y* line#)  (- *res-y* horizon)))))
	 for dist%width = (* dist% road-width)
	 for dist%width/2 = (truncate (* dist% road-width) 2)
	 for shoulder%width = (* dist% shoulder-width)
	 for stripe%width = (* dist% stripe-halfwidth)
	 for schoff = (iilerp 0 cam dist%)  ;; scanline horiz offset
	 for grsin = (sin (+ (* speed *draw-frame*) 
			     (/ (* shoulder-strip-period 2.0)
				(* vstretch dist%))))
	 for grcolor = (if (plusp grsin) *grass-light* *grass-dark*)
	 for shsin    ;; alternating colors for shoulders
	   = (sin (+ (* speed *draw-frame*) 
		     (/ shoulder-strip-period
			(* vstretch dist%))))
	 for shcolor = (if (plusp shsin) sdl:*red* sdl:*white*)
	 for stsin
	   = (sin (+ (* speed *draw-frame*)
		     (/ stripe-strip-period
			(* vstretch dist%))))
	 for stcolor = (if (plusp stsin) sdl:*white* *pavement-color*)
	 for cxi = (/ bend (* vstretch dist%))
	 for crx = (+ center-x cxi)
	 do
	 ;; draw grass
	   (sdl:draw-line-* 0 line# *res-x* line#
			    :color grcolor)
	 ;; draw pavement
	   (sdl:draw-line-*
	    (+ schoff (truncate (- crx dist%width)))
	    line#
	    (+ schoff (truncate (+ crx dist%width)))
	    line#
	    :color *pavement-color*
	    :clipping t)

	   ;; draw left shoulder
	   (sdl:draw-line-*
	    (+ schoff (truncate (- crx shoulder%width dist%width)))
	    line#
	    (+ schoff (truncate (- crx dist%width)))
	    line#
	    :color shcolor
	    :clipping t)

	   ;; draw right shoulder
	   (sdl:draw-line-*
	    (+ schoff (truncate (+ crx shoulder%width dist%width)))
	    line#
	    (+ schoff (truncate (+ crx dist%width)))
	    line#
	    :color shcolor
	    :clipping t)
	   
	 ;; draw center stripe
	   (sdl:draw-line-*
	    (+ schoff (truncate (+ crx (- stripe%width))))
	    line#
	    (+ schoff (truncate (+ crx stripe%width)))
	    line#
	    :color stcolor
	    :clipping t)

	 ;; draw left stripe
	   (sdl:draw-line-*
	    (+ schoff (truncate (- crx (- stripe%width) dist%width/2)))
	    line#
	    (+ schoff (truncate (- crx stripe%width dist%width/2)))
	    line#
	    :color stcolor
	    :clipping t)

	 ;; draw right stripe
	   (sdl:draw-line-*
	    (+ schoff (truncate (+ crx (- stripe%width) dist%width/2)))
	    line#
	    (+ schoff (truncate (+ crx stripe%width dist%width/2)))
	    line#
	    :color stcolor
	    :clipping t)	   
	   
	   ))))
  
(defun render ()
  (let* ((hill (* 0.035 (1+ (* 0.95 (sin (* 0.00525 *draw-frame*))))))
	 (horizon (truncate (+ (truncate *res-y* 2)
			       (* 256 hill))))
	 (shoriz (truncate (+ (truncate *res-y* 2)
			      (* 1024 hill)))))
    (draw-sky :horizon shoriz)
    (draw-road :CAM *px* ;; (truncate (* 400 (sin (* 0.015 *draw-frame*))))
	       :hill hill
	       :bend (sin (* 0.01125 *draw-frame*))
	       :horizon horizon))
  (incf *draw-frame*))

(defun main ()
  (setq *draw-frame* 0)
  (sdl:with-init (sdl:sdl-init-video)
    (setup-window)
    (sdl:with-events ()
      (:video-expose-event 
       ()
       (sdl:update-display))
      (:idle
       ()
       (update-swank)
       (cond
	 ((sdl:key-pressed-p :SDL-KEY-ESCAPE)
	  (sdl:quit-sdl)
	  ;; required or else "memory fault" fatal error
	  (return-from main))
	 ((sdl:key-held-p :SDL-KEY-LEFT)
	  (incf *px* *player-leftright-speed*))
	 ((sdl:key-held-p :SDL-KEY-RIGHT)
	  (decf *px* *player-leftright-speed*)))
       (sdl:clear-display *sky-color*)
       (render)
       (sdl:update-display))
      (:quit-event
       ()
       (format t "Okay, bye...~%")
       t))))
