;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### protocols.lisp
;;
;;　xxx
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "protocols"                 :depends-on ("package"
#|ASD|#	                                                            "client-data"
#|ASD|#	                                                            "dict-entry"
#|ASD|#	                                                            "dictionary"
#|ASD|#	                                                            "dictionary-set"
#|ASD|#	                                                            "service.logger"
#|ASD|#	                                                            "utility"))
#|EXPORT|#				;protocols.lisp
|#

(in-package :clime)


;;MEMO : 分解失敗（空文字列、その他の理由）の場合は nil 返却
;;MEMO : それ以外の場合は分解結果をリストで返す
(defun protocol-destructure-input (line)
  (if (zerop (length line))
	  nil
	  (let ((top  (char   line 0))
			(rest (subseq line 1)))
		(labels ((no-param ()
				   (when (string= rest "")
					 (list top)))
				 (string-1 (&optional allow-empty)
				   (if (string/= rest "")
					   (list top rest)
					   (when allow-empty
						 (list top ""))))
				 (integer-1 ()
				   (when (string/= rest "")
					 (multiple-value-bind (value index) (parse-integer rest :junk-allowed t)
					   (when (= index (length rest))
						 (list top value)))))
				 (string-2-old ()
				   (when (string/= rest "")
					 (let ((index (position-if (lambda (ch)
												 (or (char= ch #\space) (char= ch #\tab))) rest)))
					   (when index
						 (list top (subseq rest 0 index) (subseq rest (1+ index)))))))
				 (string-2-new ()
				   (when (string/= rest "")
					 (let* ((delim (char rest 0))
							(index (position delim rest :start 1)))
					   (when index
						 (list top (subseq rest 1 index) (subseq rest (1+ index))))))))
		  (case top
			(#\0 (no-param))     ;; 0
			(#\1 (string-1))     ;; 1<PATTERN>
			(#\2 (no-param))     ;; 2
			(#\3 (no-param))     ;; 3
			(#\4 (string-1))     ;; 4<CONTEXT>
			(#\5 (string-2-old)) ;; 5<WORD> <PATTERN>
			(#\6 (string-2-old)) ;; 6<WORD> <PATTERN>
			(#\7 (no-param))     ;; 7
			(#\8 (integer-1))    ;; 8<N>
			(#\C (no-param))     ;; C
			(#\E (no-param))     ;; E
			(#\I (integer-1))    ;; I<INDEX>
			(#\M (integer-1))    ;; M<INDEX>
			(#\N (no-param))     ;; N
			(#\R (integer-1))    ;; R<LIMIT>
			(#\S (string-1 t))   ;; S<PATTERN>
			(#\X (string-1))     ;; X<PATTERN>
			(#\T (string-2-new)) ;; T@<PATTERN>@<WORD>
			(#\U (string-1))     ;; U<WORD>
			(otherwise nil))))))

(defun protocol-decide-delimiter (list &optional (key #'identity))
  (labels ((recur (delim lst)
 			 (if (null lst)
 				 delim
 				 (if (position delim (funcall key (car lst)))
 					 nil
 					 (recur delim (cdr lst))))))
	(dolist (delim '(#\| #\, #\! #\# #\$ #\% #\& #\' #\*
					 #\+ #\- #\. #\/ #\: #\; #\= #\? #\^
					 #\_ #\` #\~ #\" #\Space #\Tab #\Nul))
	  (when (recur delim list)
		(return-from protocol-decide-delimiter delim)))))

(defun protocol-make-search-response (top delim lst close-p)
  (with-output-to-string (out)
	(labels ((recur (lst)
			   (when lst
				 (format out "~C~A" delim (word-of (car lst)))
				 (recur (cdr lst)))))
	  (format out "~C" top)
	  (recur lst)
	  (format out "~C" delim)
	  (when close-p
		(format out "~C" delim)))))



;;MEMO: disconnect なので２値目が T になる
(defun protocol-0-disconnect (dictset cdata)
  (declare (ignore dictset))
  ;; コンテキストクリアも実施
  (with-slots (context1 context2) cdata
	(setf context1 nil)
	(setf context2 nil)
	(logger-add-entry :info "set context to NIL."))
  (values "1" t))

(defun protocol-1-search (dictset cdata pattern)
  ;;ToDo : pattern に長さゼロの文字列を与えるとマズくない？
  (let* ((len     (length pattern))
		 (exact-p (char/= #\space (char pattern (1- len))))
		 (pattern (if exact-p
					  pattern
					  (subseq pattern 0 (1- len)))))
	(with-slots (context1 context2 cand-limit cand-list) cdata
	  (multiple-value-bind (entries count)
		  (dictionary-set-search dictset pattern cand-limit exact-p context1 context2)
		(declare (ignore count))
		(setf cand-list entries)
		;;MEMO : 常に二値目は nil なので単一値返却で良い
		(if (null entries)
			"4"
			(protocol-make-search-response #\1 #\tab entries nil))))))

(defun protocol-2-version (dictset cdata)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore dictset cdata))
  *clime-server-version*)

(defun protocol-3-hostinfo (dictset cdata)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore dictset cdata))
  *clime-server-address*)

(defun protocol-4-notify-context (dictset cdata new-context)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore dictset))
  (prog1 "1"
	(with-slots (context1 context2) cdata
	  (setf context1 context2)
	  (setf context2 new-context)
	  (logger-add-entry :info "set context to ~S & ~S." context1 context2))))

(defun protocol-5-register-word (dictset cdata word pattern)
  (declare (ignore cdata))
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (if (dictionary-set-register-word dictset pattern word)
	  "1"
	  "0"))

(defun protocol-6-unregister-word (dictset cdata word pattern)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore cdata))
  (if (dictionary-set-unregister-word dictset pattern word)
	  "1"
	  "0"))

(defun protocol-7-save-dictionaries (dictset cdata)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore cdata))
  (dictionary-set-save dictset)
  "1")

(defun protocol-8-notify-selection (dictset cdata index)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (with-slots (context1 context2 cand-list) cdata
	(let ((entry (nth index cand-list)))
	  (if (null entry)
		  "0"
		  (labels ((set-context (word)
					 (setf context1 context2)
					 (setf context2 word)
					 (logger-add-entry :info "set context to ~S & ~S." context1 context2)))
			(if (null (pattern-of entry))
				(prog1 "1" ;; pattern が nil な候補は学習しない（数字列から生成された日付時刻文字列など）
				  (set-context nil))
				(if (null (dictionary-set-learn dictset context1 context2 entry))
					"0"
					(prog1 "1"
					  (set-context (word-of entry))))))))))

(defun protocol-C-clear-context (dictset cdata)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore dictset))
  (with-slots (context1 context2) cdata
	(setf context1 nil)
	(setf context2 nil)
	(logger-add-entry :info "set context to NIL."))
  "1")

(defun protocol-E-enum-dictionaries (dictset cdata)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore cdata))
  (with-output-to-string (out)
	(labels ((recur (dicts)
			   (when dicts
				 (format out "~A	" (dictionary-title (car dicts)))
				 (recur (cdr dicts)))))
	  (format out "1	")
	  (recur (dictionary-set-dicts dictset)))))

(defun protocol-I-dictionary-info (dictset cdata index)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore cdata))
  (if (< index 0)
	  "0"
	  (let ((dict (nth index (dictionary-set-dicts dictset))))
		(if (null dict)
			"0"
			(with-output-to-string (out)
			  (format out "1	~A	~A	~A	~A	~A"
					  (dictionary-title     dict)
					  (dictionary-file-name dict)
					  (dictionary-type      dict)
					  (dictionary-kind      dict)
					  (dictionary-size      dict)))))))

(defun protocol-M-mark-modified (dictset cdata index)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore cdata))
  (if (< index 0)
	  "0"
	  (let ((dict (nth index (dictionary-set-dicts dictset))))
		(if (null dict)
			"0"
			(prog1 "1"
			  (setf (dictionary-modified dict) T)
			  (logger-add-entry :info "mark ~A modified." (dictionary-file-name dict)))))))

(defun protocol-R-set-cand-limit (dictset cdata limit)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (declare (ignore dictset))
  (if (<= limit 0)
	  "0"
	  (prog1 "1"
		(setf (client-data-cand-limit cdata) limit)
		(logger-add-entry :info "set candidate limit to ~A." limit))))
  

(labels ((search-impl (dictset cdata pattern exact-p)
		   ;;MEMO : 常に二値目は nil なので単一値返却で良い
		   (with-slots (context1 context2 cand-limit cand-list) cdata
			 (multiple-value-bind (entries count)
				 (dictionary-set-search dictset pattern cand-limit exact-p context1 context2)
			   (setf cand-list entries)
			   (if (null entries)
				   nil
				   (let ((delim (protocol-decide-delimiter entries #'clime::word-of)))
					 (protocol-make-search-response #\1 delim entries (< count cand-limit)))))))
		 (finish-1st-search (mode pattern result cdata)
		   (with-slots (last-mode last-pattern last-level) cdata
			 (setf last-level 0)
			 (if (null result)
				 (prog1 "4"
				   (setf last-mode    nil)
				   (setf last-pattern nil))
				 (prog1 result
				   (setf last-mode    mode)
				   (setf last-pattern pattern))))))

  (defun protocol-S-search (dictset cdata pattern)
	(let ((result (search-impl dictset cdata pattern nil)))
	  (finish-1st-search #\s pattern result cdata)))

  (defun protocol-X-search (dictset cdata pattern)
	(let ((result (search-impl dictset cdata pattern t)))
	  (finish-1st-search #\x pattern result cdata)))

  (defun protocol-N-next-candidates (dictset cdata)
	(with-slots (cand-limit last-mode last-pattern last-level) cdata
	  (if (null last-mode)
		  "0"
		  (let ((org-limit cand-limit))
			(incf last-level)
			(incf cand-limit (* last-level cand-limit))
			(let ((result (search-impl dictset cdata last-pattern (char= last-mode #\x))))
			  (unless result
				(setf last-level   0)
				(setf last-mode    nil)
				(setf last-pattern nil))
			  (setf cand-limit org-limit)
			  (or result "4")))))))


(defun protocol-T-register-word (dictset cdata pattern word)
  (declare (ignore cdata))
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (if (dictionary-set-register-word dictset pattern word)
	  "1"
	  "0"))

(defun protocol-U-unregister-word (dictset cdata word)
  ;;MEMO : 常に二値目は nil なので単一値返却で良い
  (with-slots (cand-list) cdata
	(let ((entry (find-if (lambda (e)
							(string= word (word-of e))) cand-list)))
	  (if (null entry)
		  "0"
		  (if (dictionary-set-unregister-word dictset (pattern-of entry) word)
			  "1"
			  "0")))))


