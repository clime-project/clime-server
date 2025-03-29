;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### transaction.lisp
;;
;;　トランザクションとそのキューを定義します
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#                             (:file "transaction"           :depends-on ("package"
#|ASD|#                                                                         "service.logger"
#|ASD|#                                                                         "queue"))
#|EXPORT|#              ;transaction.lisp
|#

(in-package :clime)



;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### transaction クラス
;;
;;${DESCRIPTION}
;;
;;　トランザクションクラスを定義します。スロットの一覧は以下の通りです。
;;
;;| name       | type                       | accessor               | initarg     |
;;|------------|----------------------------|------------------------|-------------|
;;| `cdata`    | `client-data`              | `transaction-cdata`    | `:cdata`    |
;;| `request`  | `list` or `function`       | `transaction-request`  | `:request`  |
;;| `response` | `list` or `keyword symbol` | `transaction-response` | `:response` |
;;| `quit-p`   | `boolean`                  | `transaction-quit-p`   | `:quit-p`   |
;;| `notifier` | `function`                 | `transaction-notifier` | `:notifier` |
;;
;;${SEE_ALSO}
;;
;;* transaction-execute 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defclass transaction ()
  ((cdata    :initform nil :initarg :cdata    :accessor transaction-cdata)
   (request  :initform nil :initarg :request  :accessor transaction-request)
   (response :initform nil :initarg :response :accessor transaction-response)
   (quit-p   :initform nil :initarg :quit-p   :accessor transaction-quit-p)
   (notifier :initform nil :initarg :notifier :accessor transaction-notifier)))


(defparameter *transaction-request-count*   0)
(defparameter *transaction-create-count*    0)
(defparameter *transaction-reuse-count*     0)
(defparameter *transaction-release-count*   0)
(defparameter *transaction-pool*          nil)
(defparameter *transaction-log-counter*     0)
(defparameter *transaction-log-threshold*  20)

(let ((lock (bt:make-lock)))

  (defun transaction-stat-log ()
    (incf *transaction-log-counter*)
    (when (<= *transaction-log-threshold* *transaction-log-counter*)
      (setf *transaction-log-counter* 0)
      (logger-add-entry :info
                        "transaction stat : req=~A, create=~A, reuse=~A, release=~A"
                        *transaction-request-count*
                        *transaction-create-count*
                        *transaction-reuse-count*
                        *transaction-release-count*)))
      
  (defun transaction-reuse-or-create (cdata request notifier)
    (bt:with-lock-held (lock)
      (incf *transaction-request-count*)
      (prog1
          (if *transaction-pool*
              (let ((tran (pop *transaction-pool*)))
                (setf (transaction-cdata    tran) cdata)
                (setf (transaction-request  tran) request)
                (setf (transaction-response tran) nil)
                (setf (transaction-quit-p   tran) nil)
                (setf (transaction-notifier tran) notifier)
                (incf *transaction-reuse-count*)
                tran)
              (progn
                (incf *transaction-create-count*)
                (make-instance 'transaction :cdata    cdata
                                            :request  request
                                            :notifier notifier)))
        (transaction-stat-log))))
          

  (defun transaction-release (tran)
    (setf (transaction-cdata    tran) nil)
    (setf (transaction-request  tran) nil)
    (setf (transaction-response tran) nil)
    (setf (transaction-quit-p   tran) nil)
    (setf (transaction-notifier tran) nil)
    (bt:with-lock-held (lock)
      (incf *transaction-release-count*)
      (push tran *transaction-pool*)
      (transaction-stat-log))))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### transaction-execute 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{transaction-execute}} queue request lock cvar => response, quit-p
;;
;;${ARGS_AND_VALS}
;;
;;* queue ---- a queue object
;;* request ---- a string or a list
;;* lock ---- a bt:lock object
;;* cvar ---- a bt:conditional-variable object
;;* response ---- a string
;;* quit-p ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　トランザクションを実行します。この関数はトランザクション完了までブロックし、
;;レスポンスを直接返します。
;;
;;　レッド間の同期処理に使用する bg:lock および bt:conditional-variable オブジェクト
;;は呼び出し元で用意する必要がありますが、その使用はこの関数内に閉じています。これは、
;;単純にこれらのオブジェクトが再利用可能であるため、生成と破棄の繰り返しを避けるため
;;です。
;;
;;${SEE_ALSO}
;;
;;* transaction クラス
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun transaction-execute (queue cdata request lock cvar)
  (when queue
    (let ((tran (transaction-reuse-or-create cdata request
                                             (lambda ()
                                               (bt:with-lock-held (lock)
                                                 (bt:condition-notify cvar))))))
      (unless (transaction-notifier tran)
        (logger-add-entry :error "notifier is null when (make-instance transaction)!"))
      (bt:with-lock-held (lock)
        (queue-enqueue queue tran)
        (bt:condition-wait cvar lock))
      (let ((response (transaction-response tran))
            (quit-p   (transaction-quit-p   tran)))
        (transaction-release tran)
        (values response quit-p)))))


