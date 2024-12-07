;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### service.logger.lisp
;;
;;　ロギングサービスを実装します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "service.logger"            :depends-on ("package"
#|ASD|#	                                                            "queue"
#|ASD|#	                                                            "service"
#|ASD|#	                                                            "utility"))
#|EXPORT|#				;service.logger.lisp
 |#

(in-package :clime)

#|
#|EXPORT|#				:*logger-no-debug*
 |#
(defparameter *logger-no-debug* nil)
(defparameter *logger-queue*    nil)
(defparameter *logger-service*  nil)
(defparameter *logger-suspend*  nil)

(defun logger-make-entry (log-type control-string args)
  `(,(get-universal-time) ,log-type ,control-string ,args))

(defun logger-output-entry (entry &optional (destination t))
  (destructuring-bind (timestamp log-type control-string args) entry
	(let ((control-string (concatenate 'string
						   "~4,'0D/~2,'0D/~2,'0D-~2,'0D:~2,'0D:~2,'0D	~A	" control-string "~%")))
	  (multiple-value-bind (sec min hour date month year) (decode-universal-time timestamp)
	  (apply #'format destination control-string year month date hour min sec log-type args))))
  (force-output destination))

(defmacro with-logger-suspend (&body body)
  `(progn
	 (setf *logger-suspend* t)
	 (logger-add-entry :info "logger service suspend.")
	 ,@body
	 (setf *logger-suspend* nil)
	 (logger-add-entry :info "logger service resume.")))
  

(defmacro with-logstream ((stream-sym file-name stdout-p) &body body)
  (let ((g-flag   (gensym "FLAG"))
		(g-file   (gensym "FILE"))
		(g-stdout (gensym "STDOUT")))
	`(let* ((,g-flag   t)
			(,g-file   (and ,file-name (open ,file-name :direction :output :if-exists :supersede)))
			(,g-stdout (and ,stdout-p  *standard-output*))
			(,stream-sym (cond
						   ((and ,g-file ,g-stdout) (make-broadcast-stream ,g-file ,g-stdout))
						   ((and (null ,g-file)
								 (null ,g-stdout))  (make-broadcast-stream))
						   (t (or ,g-file ,g-stdout)))))
	   (unwind-protect
			(multiple-value-prog1 (progn ,@body)
			  (setq ,g-flag nil))
		 (when ,g-file
		   (close ,g-file :ABORT ,g-flag))))))
  

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### logger-start 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{logger-start}} logfile-name log-stdout => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* logfile-name ---- a string or nil
;;* log-stdout ---- t or nil
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ロギングサービスを開始します。すでに開始済みである場合は `nil` が返り、新規に開始した
;;場合は `t` が返ります。
;;
;;${SEE_ALSO}
;;
;;* logger-alive-p 関数
;;* logger-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun logger-start (logfile-name log-stdout)
  (unless *logger-service*
	(let ((initialized nil))
	  (setf *logger-service*
			(service-start (:name "clime-logger")
			  (let ((lock (bt:make-lock))
					(cvar (bt:make-condition-variable)))
				(with-logstream (logfile logfile-name log-stdout)
				  (labels ((direct-log (log-type fmt &rest args)
							 (unless (and *logger-no-debug* (eq log-type :debug))
							   (logger-output-entry (logger-make-entry log-type fmt args) logfile)))
						   (notifier ()
							 (unless *logger-suspend*
							   (bt:with-lock-held (lock)
								 (bt:condition-notify cvar))))
						   (recur ()
							 (if (not (queue-empty-p *logger-queue*))
								 (progn
								   (logger-output-entry (queue-dequeue *logger-queue*) logfile)
								   (recur))
								 (if stop-flag
									 :killed
									 (progn
									   (direct-log :debug "logger service sleep...")
									   (setf (queue-notifier *logger-queue*) #'notifier)
									   (bt:with-lock-held (lock)
										 (bt:condition-wait cvar lock))
									   (setf (queue-notifier *logger-queue*) nil)
									   (direct-log :debug "logger service awake...")
									   (recur))))))
					(direct-log :info "logger service start.")
					(setf *logger-queue* (make-instance 'queue))
					(setf initialized t)
					(direct-log :info "logger service ready.")
					(prog1 (recur)
					  (direct-log :info "logger service stop.")))))))
	  (prog1 t
		(until initialized
		  (sleep 0.001))))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### logger-stop 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{logger-stop}} => no-value
;;
;;${ARGS_AND_VALS}
;;
;;* (none)
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　logger-start 関数によって開始したロギングサービスを停止します。
;;
;;${SEE_ALSO}
;;
;;* logger-start 関数
;;* logger-alive-p 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun logger-stop ()
  (when *logger-service*
	(service-stop *logger-service*)
	(with-slots (notifier) *logger-queue*
	  (when notifier
		(funcall notifier)))
	(service-join *logger-service*)
	(setf *logger-queue*  nil)
	(setf *logger-service* nil))
  (values))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### logger-alive-p 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{logger-alive-p}} => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ロギングサービスが実行中であるかどうかを返します。
;;
;;${SEE_ALSO}
;;
;;* logger-start 関数
;;* logger-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun logger-alive-p ()
  (and *logger-service*
	   (service-alive-p *logger-service*)))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### logger-add-entry 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{logger-add-entry}} log-type control-string &rest args => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* log-type ---- a keyword symbol (:info :warn :error :debug)
;;* control-string ---- a format control
;;* args ---- format arguments for control-string
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ロギングサービスにログ出力を依頼します。ロギングサービスが起動していない（logger-alive-p 関数
;;が `nil` を返す）状況では無視され、 `nil` が返されます。それ以外の場合は `t` が返ります。
;;
;;${SEE_ALSO}
;;
;;* logger-start 関数
;;* logger-alive-p 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun logger-add-entry (log-type control-string &rest args)
  (when *logger-queue*
	(unless (and *logger-no-debug* (eq log-type :debug))
	  (queue-enqueue *logger-queue* (logger-make-entry log-type control-string args)))
	t))

#| example...
(logger-start "./test.log")
(progn
  (logger-add-entry :info "Application initialized.")
  (logger-add-entry :warn "Nothing to do...so boread.")
  (logger-add-entry :info "Application completed..."))
(logger-stop)
 |#
