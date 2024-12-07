;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### client-data.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#             (:file "client-data"               :depends-on ("package"))
#|EXPORT|#              ;client-data.lisp
 |#

(in-package :clime)

(defparameter *client-data-cand-limit-default* 40)

(defclass client-data ()
  ((context1   :initform nil :accessor client-data-context1   :initarg :context1)    ;; string
   (context2   :initform nil :accessor client-data-context2   :initarg :context2)    ;; string
   (cand-limit :initform nil :accessor client-data-cand-limit :initarg :cand-limit)  ;; number
   (cand-list  :initform nil :accessor client-data-cand-list  :initarg :cand-list)   ;; list
   ;; for protocol-N
   (last-mode    :initform nil :accessor client-data-last-mode    :initarg :last-mode)  ;; #\s,#\x or nil
   (last-pattern :initform nil :accessor client-data-last-pattern :initarg :last-pattern)
   (last-level   :initform 0   :accessor client-data-last-level   :initarg :last-level))) ;; integer

(defun client-data-create (&optional cand-limit)
  (make-instance 'client-data
                 :context1 nil :context2 nil :cand-list nil
                 :cand-limit (or cand-limit *client-data-cand-limit-default*)
                 :last-mode nil :last-pattern nil :last-level 0))
