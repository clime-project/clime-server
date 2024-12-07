;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### service.port-listener.lisp
;;
;;　xxx
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "service.port-listener"     :depends-on ("package"
#|ASD|#	                                                            "client-data"
#|ASD|#	                                                            "protocols"
#|ASD|#	                                                            "service"
#|ASD|#	                                                            "service.logger"
#|ASD|#	                                                            "service.transactor"
#|ASD|#	                                                            "transaction"))
#|EXPORT|#				;service.port-listener.lisp
 |#

(in-package :clime)

(defparameter *port-listener-services*  nil)	;; list of '(id port service)


(defun port-listener-handler (port cdata lock cvar stream &optional input)
  (let ((line (or input (read-line stream nil nil))))
	(logger-add-entry :info "port ~A: receive ~S" port line)
	(let ((params (protocol-destructure-input line)))
	  (multiple-value-bind (response disconnect-p)
		  (if (not (null params))
			  (transaction-execute *transactor-queue* cdata params lock cvar)
			  (progn
				(logger-add-entry :error "port ~A: protocol error." port)
				(values "0" nil)))
		(prog1 disconnect-p
		  (logger-add-entry :info "port ~A:~A returns ~S" port
							(if disconnect-p " disconnected and" "") response)
		  (format stream "~A~%" response)
		  (force-output stream))))))

(let ((lock (bt:make-lock)))
  (defun port-listener-register-service (id port service)
	(bt:with-lock-held (lock)
	  (push (list id port service) *port-listener-services*)))

  (defun port-listener-remove-by-id (listener-id)
	(bt:with-lock-held (lock)
	  (setf *port-listener-services*
			(remove-if (lambda (info)
						 (eq listener-id (first info))) *port-listener-services*)))))

(defun port-listener-enum (callback)
  (dolist (info *port-listener-services*)
	(destructuring-bind (id port service) info
	  (funcall callback id port service)))
  (values))




;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### port-listener-alive-p 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{port-listener-alive-p}} listener => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* listener ---- an service object
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　リスナーサービスが実行中であるかどうかを返します。
;;
;;${SEE_ALSO}
;;
;;* port-listener-start 関数
;;* port-listener-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun port-listener-alive-p (listener)
  (and listener (service-alive-p listener)))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### port-listener-start 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{port-listener-start}} address port key => service
;;
;;${ARGS_AND_VALS}
;;
;;* address ---- a string
;;* port ---- a integer
;;* key ---- a string or nil
;;* service ---- a service object
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　クライアント向けのリスナーサービスを開始します。
;;
;;${SEE_ALSO}
;;
;;* port-listener-alive-p 関数
;;* port-listener-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun port-listener-start (address port)
  (let* ((name    (format nil "port-listener/~A:~A" address port))
		 (cdata   (client-data-create))
		 (info-id (gensym "LISTENER"))
		 (service (service-start (:name name)
					(let ((lock (bt:make-lock))
						  (cvar (bt:make-condition-variable)))
					  (labels ((recur1 (connection)
								 ;; exit when handler operates 'disconnect.'
								 (when (null (port-listener-handler port cdata lock cvar
																(usocket:socket-stream connection)))
								   (recur1 connection)))
							   (recur2 (listener)
								 (if stop-flag
									 :killed
									 (progn
									   (when (usocket:wait-for-input listener
																	 :ready-only t :timeout 0.01)
										 (logger-add-entry :debug "data has coming...")
										 (usocket:with-connected-socket
														 (connection (usocket:socket-accept listener))
										   (recur1 connection)))
									   (recur2 listener)))))
						(logger-add-entry :info "~A start." name)
						(prog1 (usocket:with-socket-listener (listener address port :reuseaddress t)
								 (recur2 listener))
						  (port-listener-remove-by-id info-id)
						  (logger-add-entry :info "~A service stop." name)))))))
	(port-listener-register-service info-id port service)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### port-listener-stop 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{port-listener-stop}} listener => no-value
;;
;;${ARGS_AND_VALS}
;;
;;* listener ---- an service object
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　port-listener-start 関数によって開始したリスナーサービスを停止します。
;;
;;${SEE_ALSO}
;;
;;* port-listener-start 関数
;;* port-listener-alive-p 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun port-listener-stop (listener)
  (when listener
	(service-stop listener)
	(service-join listener))
  (values))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### port-listener-stop-all 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{port-listener-stop-all}} => no-value
;;
;;${ARGS_AND_VALS}
;;
;;* (none)
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　全てのリスナーサービスを停止します。
;;
;;${SEE_ALSO}
;;
;;* port-listener-start 関数
;;* port-listener-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun port-listener-stop-all ()
  (port-listener-enum
   (lambda (id port service)
	 (declare (ignore id port))
	 (port-listener-stop service)))
  (values))

