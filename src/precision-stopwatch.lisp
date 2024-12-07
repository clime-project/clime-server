;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### precision-stopwatch.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "precision-stopwatch"       :depends-on ("package"))
#|EXPORT|#				;precision-stopwatch.lisp
 |#

(in-package :clime)

(defun precision-stopwatch-start ()
  #+:sbcl
  (multiple-value-bind (dummy1 sec1 nsec1) (sb-unix:unix-gettimeofday)
	(declare (ignore dummy1))
	(lambda ()
	  (multiple-value-bind (dummy2 sec2 nsec2) (sb-unix:unix-gettimeofday)
		(declare (ignore dummy2))
		(values (+ (* 1000000 (- sec2 sec1)) (- nsec2 nsec1)) 1000000))))
  #-:sbcl
  (let ((start-time (get-internal-real-time)))
	(lambda ()
	  (let ((current-time (get-internal-real-time)))
		(values (- current-time start-time) internal-time-units-per-second)))))

