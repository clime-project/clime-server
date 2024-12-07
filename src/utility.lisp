;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### utility.lisp
;;
;;　各種のユーティリティ関数／マクロを定義します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#             (:file "utility"                   :depends-on ("package"))
#|EXPORT|#              ;utility.lisp
 |#

(in-package :clime)


(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
         ,then-form
         ,else-form)))

(defmacro awhen (test-form &body body)
  `(let ((it ,test-form))
     (when it
       ,@body)))
