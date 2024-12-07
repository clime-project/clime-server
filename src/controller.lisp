;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### controller.lisp
;;
;;　各サービススレッドを統括するコントローラを実装します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "controller"                :depends-on ("package"
#|ASD|#	                                                            "client-data"
#|ASD|#	                                                            "datetime-coder"
#|ASD|#	                                                            "dictionary-set"
#|ASD|#	                                                            "digits"
#|ASD|#	                                                            "service.logger"
#|ASD|#	                                                            "service.port-listener"
#|ASD|#	                                                            "service.transactor"))
#|EXPORT|#				;controller.lisp
|#

(in-package :clime)

(defparameter *controller-alive-p* nil)


#|
#|EXPORT|#				:controller-start
 |#
(defun controller-start (&key address ports cand-max learn-max
							  log-file log-stdout dicts date-formats time-formats)
  (when cand-max
	(setf *client-data-cand-limit-default* cand-max))
  (when date-formats
	(setf *digits-date-formats* (if (listp date-formats) date-formats (list date-formats))))
  (when time-formats
	(setf *digits-time-formats* (if (listp time-formats) time-formats (list time-formats))))
  (setf *digits-date-stampers* (dtcoder-make-stampers *digits-date-formats*))
  (setf *digits-time-stampers* (dtcoder-make-stampers *digits-time-formats*))

  (unless *controller-alive-p*
	(setf *clime-server-address* address)
	(logger-start log-file log-stdout)
	(let ((dictset (dictionary-set-load learn-max dicts)))
	  (transactor-start dictset))
	(dolist (port ports)
	  (port-listener-start address port))
	(setf *controller-alive-p* t)))


#|
#|EXPORT|#				:controller-stop
 |#
(defun controller-stop ()
  (when *controller-alive-p*
	(port-listener-stop-all)
	(sleep 0.01)
	(transactor-stop)
	(sleep 0.01)
	(logger-stop)
	(setf *clime-server-address* nil)
	(setf *controller-alive-p* nil)))

#|
#|EXPORT|#				:controller-alive-p
 |#
(defun controller-alive-p ()
  *controller-alive-p*)


