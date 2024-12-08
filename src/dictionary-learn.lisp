;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### dictionary-learn.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#                             (:file "dictionary-learn"      :depends-on ("package"
#|ASD|#                                                                         "dict-entry"
#|ASD|#                                                                         "dictionary"
#|ASD|#                                                                         "utility"))
#|EXPORT|#              ;dictionary-learn.lisp
 |#

(in-package :clime)



(defclass learn-dictionary (dictionary)
  ((contents   :initform nil :accessor learn-dict-contents   :initarg :contents)     ;; list
   (size-limit :initform nil :accessor learn-dict-size-limit :initarg :size-limit))) ;; number

(defun learn-dictionary-create (title file-name kind size-limit)
  (let* ((contents (when (cl-fad:file-exists-p file-name)
                     (with-open-file (in file-name :direction :input)
                       (read in))))
         (dict (make-instance 'learn-dictionary
                              :title title :kind kind :file-name file-name
                              :contents (dictionary-fix-words contents #'cdr)
                              :okurigana-search nil :size-limit (or size-limit 10000))))
    (unless contents
      (with-slots (modified) dict
        (setf modified t)
        (dictionary-save dict)))
    dict))


(defun dictionary-search-exact (dict pattern callback)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string pattern))
  (declare (type function callback))
  (let ((size-limit (learn-dict-size-limit dict)))
    (declare (type fixnum size-limit))
    (labels ((recur (index lst)
               (declare (type fixnum index))
               (if (null lst)
                   t
                   (let ((c-entry (car lst)))
                     (if (and (string= (the string (context-pattern-of c-entry)) pattern)
                              (not (funcall callback (entry-of c-entry))))
                         nil
                         (if (= size-limit (the fixnum (incf index)))
                             (prog1 t
                               (setf (cdr lst) nil))
                             (recur index (cdr lst))))))))
      (recur 0 (learn-dict-contents dict)))))

(defun dictionary-search-no-context (dict pattern callback)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string pattern))
  (declare (type function callback))
  (let ((ptn-length (length pattern))
        (size-limit (learn-dict-size-limit dict)))
    (declare (type fixnum ptn-length size-limit))
    (labels ((match-p (c-entry)
               (and (<= ptn-length (the fixnum (length (the string (context-pattern-of c-entry)))))
                    (string= (the string (context-pattern-of c-entry)) pattern :end1 ptn-length)))
             (recur (index lst)
               (declare (type fixnum index))
               (if (null lst)
                   t
                   (let ((c-entry (car lst)))
                     (if (and (match-p c-entry)
                              (not (funcall callback (entry-of c-entry))))
                         nil
                         (if (= size-limit (the fixnum (incf index)))
                             (prog1 t
                               (setf (cdr lst) nil))
                             (recur index (cdr lst))))))))
      (recur 0 (learn-dict-contents dict)))))

(defun dictionary-search-with-context (dict pattern callback context1 context2)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string pattern context1 context2))
  (declare (type function callback))
  (let ((lv0-result nil)
        (lv1-result nil)
        (ptn-length (length pattern))
        (size-limit (learn-dict-size-limit dict)))
    (declare (type fixnum ptn-length size-limit))
    (labels ((match-p (c-entry)
               (let ((ptn (context-pattern-of c-entry)))
                 (declare (type string ptn))
                 (and (<= ptn-length (the fixnum (length ptn)))
                      (string= ptn pattern :end1 ptn-length))))
             (chomp-contents (lst)
               (prog1 t
                 (setf (cdr lst) nil)))
             (recur-contents (index lst)
               (declare (type fixnum index))
               (if (null lst)
                   t
                   (let ((c-entry (car lst)))
                     (if (not (match-p c-entry))
                         (if (= size-limit (the fixnum (incf index)))
                             (chomp-contents lst)
                             (recur-contents index (cdr lst)))
                         (let ((lv (context-match-level c-entry context1 context2)))
                           (when (= lv 0)
                             (push (entry-of c-entry) lv0-result))
                           (when (= lv 1)
                             (push (entry-of c-entry) lv1-result))
                           (if (< lv 2)
                               (if (= size-limit (the fixnum (incf index)))
                                   (chomp-contents lst)
                                   (recur-contents index (cdr lst)))
                               (if (not (funcall callback (entry-of c-entry)))
                                   nil
                                   (if (= size-limit (the fixnum (incf index)))
                                       (chomp-contents lst)
                                       (recur-contents index (cdr lst))))))))))
             (recur-sub-result (lst)
               (if (null lst)
                   t
                   (if (not (funcall callback (car lst)))
                       nil
                       (recur-sub-result (cdr lst))))))
      (when (recur-contents 0 (learn-dict-contents dict))
        (when (recur-sub-result (nreverse lv1-result))
          (recur-sub-result (nreverse lv0-result)))))))


(defmethod dictionary-type ((dict learn-dictionary))
  (declare (ignore dict))
  :learn)

(defmethod dictionary-size ((dict learn-dictionary))
  (length (learn-dict-contents dict)))

(defmethod dictionary-search ((dict learn-dictionary) pattern callback exact-p context1 context2)
  (if exact-p
      (dictionary-search-exact dict pattern callback)
      (if (and (null context1) (null context2))
          (dictionary-search-no-context   dict pattern callback)
          (dictionary-search-with-context dict pattern callback (or context1 "") (or context2 "")))))

(defmethod dictionary-search-vague ((dict learn-dictionary) pattern matcher callback)
  ;;MEMO : 学習辞書では曖昧検索はしない
  (declare (ignore dict pattern matcher callback))
  t)

(defmethod dictionary-search-okurigana ((dict learn-dictionary) pattern callback)
  ;;MEMO : 学習辞書では送りがな検索はしない
  (declare (ignore dict pattern callback))
  t)

(defmethod dictionary-search-afterfix ((dict learn-dictionary) callback context1 context2)
  ;;MEMO : search-afterfix では size-limit 管理はしない（直前の search でやってるはずなので）
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string context1 context2))
  (declare (type function callback))
  (let ((lv1 nil))    ;; context2 のみマッチした context-entry のリスト
    (labels ((recur-contents (lst)
               (if (null lst)
                   t
                   (let* ((c-entry (car lst))
                          (level   (context-match-level c-entry context1 context2)))
                     (when (= level 1)
                       (push (entry-of c-entry) lv1))
                     (if (and (= level 2) (not (funcall callback (entry-of c-entry))))
                         nil
                         (recur-contents (cdr lst))))))
             (recur-lv1 (lst)
               (if (null lst)
                   t
                   (if (not (funcall callback (car lst)))
                       nil
                       (recur-lv1 (cdr lst))))))
      (when (recur-contents (learn-dict-contents dict))
        (recur-lv1 (nreverse lv1))))))

(defmethod dictionary-learn ((dict learn-dictionary) context1 context2 entry)
  (prog1 t
    (let ((new-entry (make-context-entry context1 context2 entry)))
      (with-slots (contents modified) dict
        (labels ((find-entry (prev lst)
                   (if (null lst)
                       (values nil nil)
                       (if (context-entry= (car lst) new-entry)
                           (values prev lst)
                           (find-entry (cdr prev) (cdr lst))))))
          (if (null contents)
              (push new-entry contents)
              (unless (context-entry= (car contents) new-entry)
                (multiple-value-bind (prev entry) (find-entry contents (cdr contents))
                  (if (null prev)
                      (push new-entry contents)
                      (progn
                        (setf (cdr prev)  (cdr entry))
                        (setf (cdr entry) contents)
                        (setf contents entry)))))))
        (setf modified t)))))

(defmethod dictionary-unregister-word ((dict learn-dictionary) pattern word callback)
  (with-slots (contents modified) dict
    (let ((count 0))
      (labels ((pred (c-entry)
                 (when (and (string= pattern (context-pattern-of c-entry))
                            (string= word    (context-word-of    c-entry)))
                   (prog1 t
                     (incf count)
                     (funcall callback dict c-entry)))))
        (setf contents (delete-if #'pred contents))
        (setf modified (or modified (< 0 count)))
        (values)))))

(defmethod dictionary-save ((dict learn-dictionary))
  (awhen (slot-value dict 'modified)
    (prog1 it
      (with-slots (file-name contents size-limit modified) dict
        (with-open-file (out file-name :direction :output :if-exists :supersede)
          (labels ((recur (index lst)
                     (when lst
                       (format out "~S~%" (car lst))
                       (if (= size-limit (incf index))
                           (setf (cdr lst) nil)
                           (recur index (cdr lst))))))
            (format out ";;-*-MODE:lisp-*-~%")
            (format out "(~%")
            (recur 0 contents)
            (format out ")~%")
            (setf modified nil)))))))

