;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### service.lisp
;;
;;　`bordeaux-threads` を使用し、外部からの停止をサポートするスレッド「サービス」の実装を提供します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "service"                   :depends-on ("package"))
#|EXPORT|#				;service.lisp
 |#

(in-package :clime)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### service-start マクロ
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{service-start}} (${KEY} name stop-flag) ${BODY} body => service
;;
;;${ARGS_AND_VALS}
;;
;;* name ---- a string
;;* stop-flag ---- a symbol
;;* body ---- a code
;;* service ---- an object
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　サービススレッドを開始します。 `name` は `bordeaux-threads` に渡される `:name` パラメータになります。 
;;`stop-flag` はスレッド内部で停止指示を確認するためのフラグ変数の名前で、省略した場合は `'stop-flag` に
;;なります。外部からの停止指示を受け付けるには、スレッド内部でこのフラグ変数を定期的に確認し、非 `nil` になって
;;いた場合はスレッドを終了（関数から復帰）しなければなりません。
;;
;;${EXAMPLES}
;;
;;　このマクロを使用したサービススレッドの利用例を以下に示します。 `service-start` に渡した `body` 部分が
;;スレッド本体となる関数として扱われます。ここで `stop-flag` フラグを監視しながら 3 秒ごとに `.` を表示
;;しています。このスレッドは service-stop 関数の呼び出しにより、停止指示を出すことができます。
;;
;;```lisp
;;(defparameter *service*
;;    (service-start (:name "test")
;;      (labels ((recur ()
;;                 (if stop-flag
;;                     :killed
;;                     (progn
;;                       (format t ".")
;;                       (sleep 3)
;;                       (recur)))))
;;    	(recur))))
;;
;;(service-stop *service*)
;;(service-join *service*)
;;```
;;
;;${SEE_ALSO}
;;
;;* service-name 関数
;;* service-alive-p 関数
;;* service-stop 関数
;;* service-join 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defmacro service-start ((&key name (stop-flag 'stop-flag)) &body body)
  (let ((names (and name (list :name name))))
	`(let ((,stop-flag nil))
	   (cons (bt:make-thread (lambda () ,@body) ,@names)
			 (lambda ()
			   (setf ,stop-flag t))))))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### service-stop 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{service-stop}} service => no-value
;;
;;${ARGS_AND_VALS}
;;
;;* service ---- an object
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　service-start マクロによって開始したサービスに停止指示を出します。この関数呼び出しから復帰した時点で
;;サービスが停止していることは保証されません。サービスが実際に停止したかの確認には service-alive-p 関数
;;を使用してください。
;;
;;${SEE_ALSO}
;;
;;* service-start マクロ
;;* service-alive-p 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun service-stop (service)
  (funcall (cdr service))
  (values))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### service-name 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{service-name}} service => service-name
;;
;;${ARGS_AND_VALS}
;;
;;* service ---- an object
;;* service-name ---- a string
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　service-start マクロによって開始したサービスの名前を返します。
;;
;;${SEE_ALSO}
;;
;;* service-start マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun service-name (service)
  (bt:thread-name (car service))) 

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### service-alive-p 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{service-alive-p}} service => generalized-boolean
;;
;;${ARGS_AND_VALS}
;;
;;* service ---- an object
;;* generalized-boolean ---- t or nil
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　service-start マクロによって開始したサービスが実行中であるかどうかを返します。
;;
;;${SEE_ALSO}
;;
;;* service-start マクロ
;;* service-stop 関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun service-alive-p (service)
  (bt:thread-alive-p (car service)))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### service-join 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{service-join}} service => no-value
;;
;;${ARGS_AND_VALS}
;;
;;* service ---- an object
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　service-start マクロによって開始したサービスの終了を待ち合わせます。
;;
;;${SEE_ALSO}
;;
;;* service-start マクロ
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defun service-join (service)
  (bt:join-thread (car service))
  (values))
