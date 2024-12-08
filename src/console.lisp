;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### console.lisp
;;
;;　xxx
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#                             (:file "console"               :depends-on ("package"
#|ASD|#                                                                         "client-data"
#|ASD|#                                                                         "protocols"
#|ASD|#                                                                         "service.logger"
#|ASD|#                                                                         "service.port-listener"))
#|EXPORT|#              ;console.lisp
|#

(in-package :clime)

(defparameter *console-cdata* nil)
(defparameter *console-lock*  nil)
(defparameter *console-cvar*  nil)

(defun console-initialize-if-need ()
  (unless *console-cdata*
    (setf *console-cdata* (client-data-create))
    (setf *console-lock*  (bt:make-lock))
    (setf *console-cvar*  (bt:make-condition-variable))))


(defun test-impl (request)
  (console-initialize-if-need)
  (let ((response (with-output-to-string (stream)
                    (port-listener-handler 0 *console-cdata*
                                           *console-lock* *console-cvar* stream request))))
    (when (char= #\newline (char response (1- (length response))))
      (setf response (subseq response 0 (1- (length response)))))
    (format t "request  : ~S~%" request)
    (format t "response : ~S~%" response)
    (format t "~%")
    response))


(defun test-enum-response-values (response delim callback)
  (labels ((recur (index cur)
             (let ((end (position delim response :start cur)))
               (if (null end)
                   (when (< cur (length response))
                     (funcall callback index (subseq response cur)))
                   (progn
                     (funcall callback index (subseq response cur end))
                     (recur (1+ index) (1+ end)))))))
    (recur 0 2)))

(defun test-dump-search-result (response old-p)
  (when (char= #\1 (aref response 0))
    (let ((delim (if old-p #\tab (aref response 1))))
      (test-enum-response-values response delim
                                 (lambda (index value)
                                   (format t "~A: ~S~%" index value))))))

#|
#|EXPORT|#              :test-raw-request
#|EXPORT|#              :test-raw-request
 |#
(defun test-raw-request (request)
  (with-logger-suspend
    (test-impl request))
  (values))

#|
#|EXPORT|#              :test-0-disconnect
 |#
(defun test-0-disconnect ()
  (with-logger-suspend
    (test-impl "0"))
  (values))

#|
#|EXPORT|#              :test-1-search
#|EXPORT|#              :test-S-search
#|EXPORT|#              :test-X-search
 |#
(defun test-1-search (pattern &optional exact-p)
  (with-logger-suspend
    (test-dump-search-result (test-impl (format nil "1~A~A" pattern (if exact-p "" " "))) t))
  (values))
(defun test-S-search (pattern)
  (with-logger-suspend
    (test-dump-search-result (test-impl (format nil "S~A" pattern)) nil))
  (values))
(defun test-X-search (pattern)
  (with-logger-suspend
    (test-dump-search-result (test-impl (format nil "X~A" pattern)) nil))
  (values))

#|
#|EXPORT|#              :test-N-next
 |#
(defun test-N-next ()
  (with-logger-suspend
    (test-impl "N"))
  (values))

#|
#|EXPORT|#              :test-2-version
#|EXPORT|#              :test-3-hostinfo
 |#
(defun test-2-version ()
  (with-logger-suspend
    (test-impl "2"))
  (values))
(defun test-3-hostinfo ()
  (with-logger-suspend
    (test-impl "3"))
  (values))

#|
#|EXPORT|#              :test-4-notify-context
 |#
(defun test-4-notify-context (context)
  (with-logger-suspend
    (test-impl (format nil "4~A" context)))
  (values))

#|
#|EXPORT|#              :test-5-register
#|EXPORT|#              :test-T-register
 |#
(defun test-5-register (pattern word)
  (with-logger-suspend
    (test-impl (format nil "5~A	~A" word pattern)))
  (values))
(defun test-T-register (pattern word)
  (let ((delim (protocol-decide-delimiter (list pattern word))))
    (with-logger-suspend
      (test-impl (format nil "T~C~A~C~A" delim pattern delim word))))
  (values))

#|
#|EXPORT|#              :test-6-unregister
#|EXPORT|#              :test-U-unregister
 |#
(defun test-6-unregister (pattern word)
  (with-logger-suspend
    (test-impl (format nil "6~A	~A" word pattern)))
  (values))
(defun test-U-unregister (word)
  (with-logger-suspend
    (test-impl (format nil "U~A" word)))
  (values))

#|
#|EXPORT|#              :test-7-savedict
 |#
(defun test-7-savedict ()
  (with-logger-suspend
    (test-impl "7"))
  (values))

#|
#|EXPORT|#              :test-8-notify-selection
 |#
(defun test-8-notify-selection (index)
  (with-logger-suspend
    (test-impl (format nil "8~A" index)))
  (values))

#|
#|EXPORT|#              :test-C-clear-context
 |#
(defun test-C-clear-context ()
  (with-logger-suspend
    (test-impl "C"))
  (values))

#|
#|EXPORT|#              :test-E-enum-dict
 |#
(defun test-E-enum-dict ()
  (with-logger-suspend
    (test-dump-search-result (test-impl "E") t))
  (values))

#|
#|EXPORT|#              :test-I-dict-info
 |#
(defun test-I-dict-info (index)
  (with-logger-suspend
    (let ((response (test-impl (format nil "I~A" index))))
      (when (char= #\1 (aref response 0))
        (let ((lst nil))
          (test-enum-response-values response #\tab
                                     (lambda (index value)
                                       (declare (ignore index))
                                       (push value lst)))
          (when (= 5 (length lst))
            (setf lst (nreverse lst))
            (format t "name : ~A~%" (first  lst))
            (format t "file : ~A~%" (second lst))
            (format t "type : :~A~%" (third  lst))
            (format t "kind : :~A~%" (fourth lst))
            (format t "size : ~A~%" (fifth  lst)))))))
  (values))

#|
#|EXPORT|#              :test-M-mark-modified
 |#
(defun test-M-mark-modified (index)
  (with-logger-suspend
    (test-impl (format nil "M~A" index)))
  (values))

#|
#|EXPORT|#              :test-R-set-cand-limit
 |#
(defun test-R-set-cand-limit (limit)
  (with-logger-suspend
    (test-impl (format nil "R~A" limit)))
  (values))
