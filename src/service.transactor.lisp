;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### service.transactor.lisp
;;
;;　API を処理するトランザクタサービスを実装します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#                             (:file "service.transactor"    :depends-on ("package"
#|ASD|#                                                                         "precision-stopwatch"
#|ASD|#                                                                         "protocols"
#|ASD|#                                                                         "queue"
#|ASD|#                                                                         "service"
#|ASD|#                                                                         "service.logger"
#|ASD|#                                                                         "transaction"
#|ASD|#                                                                         "utility"))
#|EXPORT|#              ;service.transactor.lisp
|#

(in-package :clime)


(defparameter *transactor-queue*     nil)
(defparameter *transactor-dictset*   nil)
(defparameter *transactor-service*   nil)
(defparameter *transactor-protocols* nil)


(defun transactor-make-protocolset ()
  (let ((hashtbl (make-hash-table)))
    (setf (gethash #\0 hashtbl) #'protocol-0-disconnect)
    (setf (gethash #\1 hashtbl) #'protocol-1-search)
    (setf (gethash #\2 hashtbl) #'protocol-2-version)
    (setf (gethash #\3 hashtbl) #'protocol-3-hostinfo)
    (setf (gethash #\4 hashtbl) #'protocol-4-notify-context)
    (setf (gethash #\5 hashtbl) #'protocol-5-register-word)
    (setf (gethash #\6 hashtbl) #'protocol-6-unregister-word)
    (setf (gethash #\7 hashtbl) #'protocol-7-save-dictionaries)
    (setf (gethash #\8 hashtbl) #'protocol-8-notify-selection)
    (setf (gethash #\C hashtbl) #'protocol-C-clear-context)
    (setf (gethash #\E hashtbl) #'protocol-E-enum-dictionaries)
    (setf (gethash #\I hashtbl) #'protocol-I-dictionary-info)
    (setf (gethash #\M hashtbl) #'protocol-M-mark-modified)
    (setf (gethash #\N hashtbl) #'protocol-N-next-candidates)
    (setf (gethash #\R hashtbl) #'protocol-R-set-cand-limit)
    (setf (gethash #\S hashtbl) #'protocol-S-search)
    (setf (gethash #\X hashtbl) #'protocol-X-search)
    (setf (gethash #\T hashtbl) #'protocol-T-register-word)
    (setf (gethash #\U hashtbl) #'protocol-U-unregister-word)
    hashtbl))

(defun transactor-exec-protocol (cdata params)
  (destructuring-bind (top &rest args) params
    (let ((handler (gethash top *transactor-protocols*)))
      (if (null handler)
          "0"
          (handler-case
              (let ((stopwatch (precision-stopwatch-start)))
                (multiple-value-bind (res quit) (apply handler *transactor-dictset* cdata args)
                  (multiple-value-bind (time unit) (funcall stopwatch)
                    (logger-add-entry :info "transaction takes ~F sec." (float (/ time unit))))
                  (values res quit)))
            (error ()
              (logger-add-entry :error "something goes wrong in ~S." params)
              "0"))))))

  


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### transactor-start 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{transactor-start}} dictset => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* dictset ---- a dictionary-set object
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　トランザクタを開始します。すでに開始済みである場合は `nil` が返り、新規に開始した場合は `t` が
;;返ります。
;;
;;${SEE_ALSO}
;;
;;* transactor-alive-p 関数
;;* transactor-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun transactor-start (dictset)
  (unless *transactor-service*
    (let ((initialized nil))
      (setf *transactor-dictset*  dictset)
      (setf *transactor-service*
            (service-start (:name "clime-transactor")
              (let ((lock (bt:make-lock))
                    (cvar (bt:make-condition-variable)))
                (labels ((notifier ()
                           (bt:with-lock-held (lock)
                             (bt:condition-notify cvar)))
                         (recur ()
                           (if (not (queue-empty-p *transactor-queue*))
                               (let ((tran (queue-dequeue *transactor-queue*)))
                                 (with-slots (cdata request response quit-p notifier) tran
                                   (logger-add-entry :debug "begin operate ~A" request)
                                   (multiple-value-bind (res quit)
                                       (transactor-exec-protocol cdata request)
                                     (setf response res)
                                     (setf quit-p   quit))
                                   (logger-add-entry :debug "response is ~A" response)
                                   (when (null notifier)
                                     (logger-add-entry :error "notifier is null!"))
                                   (funcall notifier)) ; wake up client service.
                                 (recur))
                               (if stop-flag
                                   :killed
                                   (progn
                                     (logger-add-entry :debug "transactor service sleep...")
                                     (setf (queue-notifier *transactor-queue*) #'notifier)
                                     (bt:with-lock-held (lock)
                                       (bt:condition-wait cvar lock))
                                     (setf (queue-notifier *transactor-queue*) nil)
                                     (logger-add-entry :debug "transactor service awake...")
                                     (recur))))))
                  (logger-add-entry :info "transactor service start.")
                  (setf *transactor-queue*     (make-instance 'queue))
                  (setf *transactor-protocols* (transactor-make-protocolset))
                  (setf initialized t)
                  (logger-add-entry :info "transactor service ready.")
                  (prog1 (recur)
                    (logger-add-entry :info "transactor service stop."))))))
      (prog1 t
        (until initialized
          (sleep 0.001))))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### transactor-stop 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{transactor-stop}} => no-value
;;
;;${ARGS_AND_VALS}
;;
;;* (none)
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　transactor-start 関数によって開始したトランザクタサービスを停止します。
;;
;;${SEE_ALSO}
;;
;;* transactor-start 関数
;;* transactor-alive-p 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun transactor-stop ()
  (when *transactor-service*
    (service-stop *transactor-service*)
    (with-slots (notifier) *transactor-queue*
      (when notifier
        (funcall notifier)))
    (service-join *transactor-service*)
    (setf *transactor-queue* nil)
    (setf *transactor-service*    nil))
  (values))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### transactor-alive-p 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{transactor-alive-p}} => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　トランザクタサービスが実行中であるかどうかを返します。
;;
;;${SEE_ALSO}
;;
;;* transactor-start 関数
;;* transactor-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun transactor-alive-p ()
  (and *transactor-service*
       (service-alive-p *transactor-service*)))


(defun transactor-get-dictset ()
  *transactor-dictset*)
