;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### dictionary-text.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "dictionary-text"           :depends-on ("package"
#|ASD|#	                                                            "dict-entry"
#|ASD|#	                                                            "dictionary"))
#|EXPORT|#				;dictionary-text.lisp
 |#

(in-package :clime)



(defclass text-dictionary (dictionary)
  ((contents :initform nil :accessor text-dict-contents :initarg :contents))) ;; vector


(defun text-dictionary-create (title file-name kind okurigana-search)
  (let ((contents (with-open-file (in file-name :direction :input)
					(read in))))
	(setf contents (dictionary-fix-words contents))
	;;念のために sort しておく（ " や \ はエスケープするのでファイル内で sorted 状態維持は難しい）
	(setf contents (stable-sort contents #'string< :key #'pattern-of))
	(make-instance 'text-dictionary
				   :title title :kind kind :file-name file-name
				   :okurigana-search okurigana-search :contents contents)))


(defun textdict-lower-bound (input arr top end)
  (if (= top end)
	  top
	  (let* ((mid     (ash (+ top end) -1))
			 (pattern (pattern-of (aref arr mid)))
			 (length  (min (length input) (length pattern))))
		;(format t "[~A ~A ~A) : ~A~%" top mid end pattern)
		(if (string< pattern input :end1 length)
			(textdict-lower-bound input arr (1+ mid) end)
			(textdict-lower-bound input arr top mid)))))

(defun textdict-upper-bound (input arr top end)
  (if (= top end)
	  top
	  (let* ((mid     (ash (+ top end) -1))
			 (pattern (pattern-of (aref arr mid)))
			 (length  (min (length input) (length pattern))))
		;(format t "[~A ~A ~A) : ~A~%" top mid end pattern)
		(if (string<= pattern input :end1 length)
			(textdict-upper-bound input arr (1+ mid) end)
			(textdict-upper-bound input arr top mid)))))


(defmethod dictionary-type ((dict text-dictionary))
  (declare (ignore dict))
  :text)

(defmethod dictionary-size ((dict text-dictionary))
  (length (text-dict-contents dict)))

(defun textdict-linear-search (contents test index end callback)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type vector contents))
  (declare (type function test callback))
  (declare (type fixnum index end))
  (if (= index end)
	  t
	  (let ((result (funcall test (aref contents index))))
		(if (null result)
			(textdict-linear-search contents test (1+ index) end callback)
			(if (not (funcall callback result))
				nil
				(textdict-linear-search contents test (1+ index) end callback))))))

(defmethod dictionary-search ((dict text-dictionary) pattern callback exact-p context1 context2)
  (declare (ignore context1 context2))
  (labels ((test (entry)
			 entry)
		   (test-exact (entry)
			 (when (string= pattern (pattern-of entry))
			   entry)))
	(with-slots (contents) dict
	  (let ((length (length contents)))
		(textdict-linear-search contents (if exact-p #'test-exact #'test)
								(textdict-lower-bound pattern contents 0 length)
								(textdict-upper-bound pattern contents 0 length) callback)))))

(defmethod dictionary-search-vague ((dict text-dictionary) pattern matcher callback)
  (labels ((test (entry)
			 (when (funcall matcher (pattern-of entry))
			   entry)))
	(with-slots (contents) dict
	  (let ((top (subseq pattern 0 1))
			(length (length contents)))
		(textdict-linear-search contents #'test
								(textdict-lower-bound top contents 0 length)
								(textdict-upper-bound top contents 0 length)
								(lambda (entry)
									(funcall callback (make-entry pattern (word-of entry)))))))))

(defmethod dictionary-search-okurigana ((dict text-dictionary) pattern callback)
  (labels ((test (entry)
			 (dictionary-okurigana-match-p pattern entry)))
	(with-slots (contents) dict
	  (let ((top    (subseq pattern 0 1))
			(length (length contents)))
		(textdict-linear-search contents #'test
								(textdict-lower-bound top contents 0 length)
								(textdict-upper-bound top contents 0 length) callback)))))

(defmethod dictionary-save ((dict text-dictionary))
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
