;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### dictionary-lookup.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#             (:file "dictionary-lookup"         :depends-on ("package"
#|ASD|#                                                             "dict-entry"
#|ASD|#                                                             "dictionary"))
#|EXPORT|#              ;dictionary-lookup.lisp
 |#

(in-package :clime)



(defclass lookup-node ()
  ((ch   :initform nil :accessor lookup-node-char  :initarg :ch)    ;; character
   (top  :initform nil :accessor lookup-node-top   :initarg :top)   ;; number
   (end  :initform nil :accessor lookup-node-end   :initarg :end)   ;; number
   (arr  :initform nil :accessor lookup-node-arr)))                 ;; array or nil

(defmethod print-object ((node lookup-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (with-slots (ch top end) node
      (format stream "~C [~A ~A)" ch top end))))

(defclass lookup-dictionary (dictionary)
  ((contents :initform nil :accessor lookup-dict-contents  :initarg :contents)   ;; vector
   (topnodes :initform nil :accessor lookup-dict-topnodes  :initarg :topnodes))) ;; vector of lookup-node


(defun lookup-dictionary-create (title file-name kind okurigana-search &optional (max-depth 3))
  (let ((contents (with-open-file (in file-name :direction :input)
                    (read in))))
    (setf contents (dictionary-fix-words contents))
    ;;念のために sort しておく（エスケープが必要な " や \ の存在があるため、ファイル内で sorted 状態維持は難しい）
    (setf contents (stable-sort contents #'string< :key #'pattern-of))
    (labels ((make-arr ()
               (make-array 10 :fill-pointer 0 :adjustable t))
             (operate-entry (idx data depth arr)
               (when (< depth (length data))
                 (let ((ch  (char data depth)))
                   (unless (and (< 0 (length arr))
                                (char= ch (lookup-node-char (aref arr (1- (length arr))))))
                     (vector-push-extend
                      (make-instance 'lookup-node :ch ch :top idx :end idx) arr))
                   (let ((node (aref arr (1- (length arr)))))
                     (setf (lookup-node-end node) (1+ idx))
                     (when (< (1+ depth) max-depth)
                       (unless (lookup-node-arr node)
                         (setf (lookup-node-arr node) (make-arr)))
                       (operate-entry idx data (1+ depth)
                                      (lookup-node-arr node)))))))
             (recur (idx max top)
               (if (= idx max)
                   top
                   (progn
                     (operate-entry idx (pattern-of (aref contents idx)) 0 top)
                     (recur (1+ idx) max top)))))
      (let ((topnodes (recur 0 (length contents) (make-arr))))
        (make-instance 'lookup-dictionary :title title
                                          :kind kind
                                          :file-name file-name
                                          :okurigana-search okurigana-search
                                          :contents contents :topnodes topnodes)))))


(defmethod dictionary-type ((dict lookup-dictionary))
  (declare (ignore dict))
  :lookup)

(defmethod dictionary-size ((dict lookup-dictionary))
  (length (lookup-dict-contents dict)))


;;MEMO : 先頭文字から該当無しの場合は top end がそのまま返るので、呼出元でチェックすること
(defun lookupdict-filter-by-nodes (pattern ptn-length index arr top end)
  (if (or (null arr)
          (= index ptn-length))
      (values top end)
      (let* ((ch (char pattern index))
             (pos (position-if (lambda (node)
                                 (char= ch (lookup-node-char node))) arr)))
        (if (null pos)
            (values top end)
            (with-slots (arr top end) (aref arr pos)
              (lookupdict-filter-by-nodes pattern ptn-length (1+ index) arr top end))))))

(defun lookupdict-linear-search (contents test index end callback)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-vector contents))
  (declare (type function test callback))
  (declare (type fixnum index end))
  (if (= index end)
      t
      (let ((result (funcall test (svref contents index))))
        (if (null result)
            (lookupdict-linear-search contents test (1+ index) end callback)
            (if (not (funcall callback result))
                nil
                (lookupdict-linear-search contents test (1+ index) end callback))))))

(defmethod dictionary-search ((dict lookup-dictionary) pattern callback exact-p context1 context2)
  (declare (ignore context1 context2))
  (let ((pattern-length (length pattern)))
    (labels ((test-exact (entry)
               (when (string= (pattern-of entry) pattern)
                 entry))
             (test (entry)
               (unless (or (< (length (pattern-of entry)) pattern-length)
                           (string/= pattern (pattern-of entry) :end2 pattern-length))
                 entry)))
      (with-slots (contents topnodes) dict
        (multiple-value-bind (top end) (lookupdict-filter-by-nodes pattern pattern-length
                                                                   0 topnodes 0 (length contents))
          (if (and (zerop top)
                   (= end (length contents)))
              t
              (lookupdict-linear-search contents
                                        (if exact-p #'test-exact #'test) top end callback)))))))

(defmethod dictionary-search-vague ((dict lookup-dictionary) pattern matcher callback)
  (labels ((test (entry)
             (when (funcall matcher (pattern-of entry))
               entry)))
    (with-slots (contents topnodes) dict
      (multiple-value-bind (top end) (lookupdict-filter-by-nodes (subseq pattern 0 1) 1
                                                                 0 topnodes 0 (length contents))
        (if (and (zerop top)
                 (= end (length contents)))
            t
            (lookupdict-linear-search contents #'test top end
                                      (lambda (entry)
                                        (funcall callback (make-entry pattern (word-of entry))))))))))

(defmethod dictionary-search-okurigana ((dict lookup-dictionary) pattern callback)
  (labels ((test (entry)
             (dictionary-okurigana-match-p pattern entry)))
    (with-slots (contents topnodes) dict
      ;;MEMO : 送りがな検索の場合は先頭文字でのみ絞り込みを行う
      (multiple-value-bind (top end) (lookupdict-filter-by-nodes (subseq pattern 0 1) 1
                                                                 0 topnodes 0 (length contents))
        (if (and (zerop top)
                 (= end (length contents)))
            t
            (lookupdict-linear-search contents #'test top end callback))))))

(defmethod dictionary-save ((dict lookup-dictionary))
  (awhen (slot-value dict 'modified)
    (prog1 it
      (with-slots (file-name contents modified) dict
        ;; rename existing dictionary file.
        (rename-file file-name (concatenate 'string file-name ".bak"))
        (with-open-file (out file-name :direction :output :if-exists :supersede)
          (labels ((recur (idx max)
                     (when (< idx max)
                       (let ((entry (aref contents idx)))
                         (when (or (zerop idx)
                                   (entry/= entry (aref contents (1- idx))))
                           (format out "(~S . ~S)~%" (pattern-of entry) (word-of entry)))
                         (recur (1+ idx) max)))))
            (format out ";;-*-MODE:lisp-*-~%")
            (format out "#(~%")
            (recur 0 (length contents))
            (format out ")~%")
            (setf modified nil)))))))
