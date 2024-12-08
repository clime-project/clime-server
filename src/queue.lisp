;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### queue.lisp
;;
;;　スレッドセーフに利用できるキュー構造を実装します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#                             (:file "queue"                 :depends-on ("package"))
#|EXPORT|#              ;queue.lisp
 |#

(in-package :clime)

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### queue クラス
;;
;;${DESCRIPTION}
;;
;;　データベースクラスを定義します。スロットの一覧は以下の通りです。
;;
;;| name       | type       | accessor         | memo      |
;;|------------|------------|------------------|-----------|
;;| `head`     | `cons`     | `queue-head`     |           |
;;| `tail`     | `cons`     | `queue-tail`     |           |
;;| `lock`     | `bt:lock`  | `queue-lock`     | read only |
;;| `notifier` | `function` | `queue-notifier` |           |
;;
;;${SEE_ALSO}
;;
;;* queue-enqueue 関数
;;* queue-dequeue 関数
;;
;;${NOTES}
;;
;;* ${{TODO}{前提とスレッド間制御の仕組みについて説明すること。}}$
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defclass queue ()
  ((head     :initform nil            :accessor queue-head)
   (tail     :initform nil            :accessor queue-tail)
   (lock     :initform (bt:make-lock) :reader   queue-lock)
   (notifier :initform nil            :accessor queue-notifier)))

(defmethod initialize-instance :after ((que queue) &rest initargs)
  (declare (ignore initargs))
  (with-slots (head tail) que
    (setf tail   (cons nil nil))
    (setf head   tail))
  que)

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### queue-empty-p 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{queue-empty-p}} => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　キューが空であるかどうかを返します。
;;
;;${SEE_ALSO}
;;
;;* queue-enqueue 関数
;;* queue-dequeue 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun queue-empty-p (que)
  (eq (queue-head que) (queue-tail que)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### queue-enqueue 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{queue-enqueue}} que item => item
;;
;;${ARGS_AND_VALS}
;;
;;* que ---- a queue object
;;* item ---- an object
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　キューに要素を追加します。結果として、item を返します。
;;
;;* ${{TODO}{BT:LOCK を使ったスレッド間制御の話、および notifier について説明すること。}}$
;;
;;${SEE_ALSO}
;;
;;* queue-dequeue 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun queue-enqueue (que item)
  (with-slots (tail lock) que
    (bt:with-lock-held (lock)
      (let ((new-tail (cons nil nil)))
        (setf (car tail) item)
        (setf (cdr tail) new-tail)
        (setf tail new-tail))
      (let ((notifier (queue-notifier que)))
        (when notifier
          (funcall notifier)))))
  item)

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### queue-dequeue 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{queue-dequeue}} que => item
;;
;;${ARGS_AND_VALS}
;;
;;* que ---- a queue object
;;* item ---- an object or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　キューから要素を取り出します。結果として、キューの先頭要素を返します。ただし、キューが空の
;;場合は `nil` を返します。
;;
;;${SEE_ALSO}
;;
;;* queue-enqueue 関数
;;* queue-empty-p 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun queue-dequeue (que)
  (unless (queue-empty-p que)
    (let ((cons (queue-head que)))
      (setf (queue-head que) (cdr cons))
      (car cons))))

