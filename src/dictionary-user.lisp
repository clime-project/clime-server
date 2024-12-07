;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### dictionary-user.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#             (:file "dictionary-user"           :depends-on ("package"
#|ASD|#                                                             "dict-entry"
#|ASD|#                                                             "dictionary"
#|ASD|#                                                             "dictionary-text"
#|ASD|#                                                             "utility"))
#|EXPORT|#              ;dictionary-user.lisp
 |#

(in-package :clime)



(defclass user-dictionary (text-dictionary) ())


(defun user-dictionary-create (title file-name kind okurigana-search)
  (let* ((lst (when (cl-fad:file-exists-p file-name)
                (with-open-file (in file-name :direction :input)
                  (read in))))
         (length (length lst))
         (contents (make-array length :initial-contents (dictionary-fix-words lst)
                                      :adjustable t :fill-pointer length))
         ;;念のために sort（ " や \ はエスケープするのでファイル内で sorted 状態維持は難しい）
         (dict (make-instance 'user-dictionary
                              :title title :kind kind :file-name file-name
                              :okurigana-search okurigana-search
                              :modified (zerop length)
                              :contents (stable-sort contents #'string< :key #'car))))
    (unless lst
      (dictionary-save dict))
    dict))

(defmethod dictionary-type ((dict user-dictionary))
  (declare (ignore dict))
  :user)

(defmethod dictionary-register-word ((dict user-dictionary) pattern word)
  (prog1 t
    (let ((new-entry (make-entry pattern word)))
      (with-slots (contents modified) dict
        (unless (find new-entry contents :test #'entry=)
          (vector-push-extend new-entry contents)
          (setf modified t)
          (setf contents (stable-sort contents #'string< :key #'car)))))))

(defmethod dictionary-unregister-word ((dict user-dictionary) pattern word callback)
  (with-slots (contents modified) dict
    (let ((count 0))
      (labels ((pred (entry)
                 (when (and (string= pattern (pattern-of entry))
                            (string= word    (word-of    entry)))
                   (prog1 t
                     (incf count)
                     (funcall callback dict entry)))))
        (setf contents (delete-if #'pred contents))
        (setf modified (or modified (< 0 count)))
        (values)))))

(defmethod dictionary-save ((dict user-dictionary))
  (awhen (slot-value dict 'modified)
    (prog1 it
      (with-slots (file-name contents modified) dict
        (with-open-file (out file-name :direction :output :if-exists :supersede)
          (labels ((recur (idx max)
                     (when (< idx max)
                       (let ((entry (aref contents idx)))
                         (format out "(~S . ~S)~%" (pattern-of entry) (word-of entry))
                         (recur (1+ idx) max)))))
            (format out ";;-*-MODE:lisp-*-~%")
            (format out "(~%")
            (recur 0 (length contents))
            (format out ")~%")
            (setf modified nil)))))))
