;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### dictionary.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "dictionary"                :depends-on ("package"
#|ASD|#	                                                            "dict-entry"
#|ASD|#	                                                            "kana"))
#|EXPORT|#				;dictionary.lisp
 |#

(in-package :clime)


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary クラス
;;
;;${DESCRIPTION}
;;
;;　辞書の共通基底クラスを定義します。スロットの一覧は以下の通りです。
;;
;;| name               | type      | accessor                      | initarg             |
;;|--------------------|-----------|-------------------------------|---------------------|
;;| `title`            | `string`  | `dictionary-title`            | `:title`            |
;;| `kind`             | `keyword` | `dictionary-kind`             | `:kind`             |
;;| `file-name`        | `string`  | `dictionary-file-name`        | `:file-name`        |
;;| `okurigana-search` | `boolean` | `dictionary-okurigana-search` | `:okurigana-search` |
;;| `modified`         | `boolean` | `dictionary-modified`         | `:modified`         |
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defclass dictionary ()
  ((title            :initform nil :accessor dictionary-title            :initarg :title)
   (kind             :initform nil :accessor dictionary-kind             :initarg :kind)
   (file-name        :initform nil :accessor dictionary-file-name        :initarg :file-name)
   (okurigana-search :initform nil :accessor dictionary-okurigana-search :initarg :okurigana-search)
   (modified         :initform nil :accessor dictionary-modified         :initarg :modified)))


;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-title 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-title}} dict => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* result ---- a string
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ディクショナリのタイトルを文字列で返します。これはユーザーが指定する任意の情報
;;です。
;;
;;${SEE_ALSO}
;;
;;* dictionary-kind 関数
;;* dictionary-type 総称関数
;;* dictionary-size 総称関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
;;MEMO : このアクセサ関数は defclass 内で作成しているので、ここでは何もしない

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-kind 関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-kind}} dict => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* result ---- a keyword symbol
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ディクショナリの種類をキーワードシンボルで返します。これはユーザーが指定する任意の情報
;;ですが、一般に以下のようなものになります。
;;
;;* `:standard` : 標準辞書
;;* `:location` : 地名辞書
;;* `:personname` : 人名辞書
;;
;;${SEE_ALSO}
;;
;;* dictionary-title 関数
;;* dictionary-type 総称関数
;;* dictionary-size 総称関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
;;MEMO : このアクセサ関数は defclass 内で作成しているので、ここでは何もしない

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-type 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-type}} dict => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* result ---- a keyword symbol
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ディクショナリの実装の種類をキーワードシンボルで返します。これは以下のいずれかです。
;;
;;* `:learn` : 学習辞書
;;* `:text` : テキスト辞書
;;* `:user` : ユーザー辞書
;;* `:lookup` : ルックアップ辞書
;;
;;${SEE_ALSO}
;;
;;* dictionary-title 関数
;;* dictionary-kind 関数
;;* dictionary-size 総称関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-type  (dict))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-size 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-size}} dict => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* result ---- a integer
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　ディクショナリに格納されている辞書エントリの数を返します。
;;
;;${SEE_ALSO}
;;
;;* dictionary-title 関数
;;* dictionary-kind 関数
;;* dictionary-type 総称関数
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-size  (dict))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-search 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-search}} dict pattern callback exact-p context1 context2 => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* pattern ---- a string
;;* callback ---- a function
;;* context1 ---- a string or NIL
;;* context2 ---- a string or NIL
;;* exact-p ---- a boolean value
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-search (dict pattern callback exact-p context1 context2))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-search-vague 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-search-vague}} dict pattern matcher callback => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* pattern ---- a string
;;* matcher ---- a vague-matcher object
;;* callback ---- a function
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-search-vague (dict pattern matcher callback))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-search-okurigana 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-search-okurigana}} dict pattern callback => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* pattern ---- a string
;;* callback ---- a function
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-search-okurigana (dict pattern callback))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-search-afterfix 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-search-afterfix}} dict callback context1 context2 => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* callback ---- a function
;;* context1 ---- a string or NIL
;;* context2 ---- a string or NIL
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-search-afterfix (dict callback context1 context2)
  (:method (dict callback context1 context2)
	(declare (ignore dict callback context1 context2))
	t))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-register-word 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-register-word}} dict pattern word => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* pattern ---- a string
;;* word ---- a string
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-register-word (dict pattern word)
  (:method (dict pattern word)
	(declare (ignore dict pattern word))
	nil))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-unregister-word 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-unregister-word}} dict pattern word callback => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* pattern ---- a string
;;* word ---- a string
;;* callback ---- a function
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-unregister-word (dict pattern word callback)
  (:method (dict pattern word callback)
	(declare (ignore dict pattern word callback))
	(values)))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-learn 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-learn}} dict context1 context2 entry => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* context1 ---- a string
;;* context2 ---- a string
;;* entry ---- a object (cons of a pattern and a word)
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-learn (dict context1 context2 entry)
  (:method (dict context1 context2 entry)
	(declare (ignore dict context1 context2 entry))
	nil))

;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;#### dictionary-save 総称関数
;;
;;<!-- stack:push li class='syntax' -->
;;${SYNTAX}
;;
;;* ${{B}{dictionary-save}} dict => result
;;
;;${ARGS_AND_VALS}
;;
;;* dict ---- a dictionary object
;;* result ---- a boolean value
;;
;;<!-- stack:pop li -->
;;
;;${DESCRIPTION}
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;${NO_SEE_ALSO}
;;
;;${NO_NOTES}
;;
;;--------------------------------------------------------------------------------------- END TURNUP
(defgeneric dictionary-save (dict))


(defun dictionary-fix-words (contents &optional (get-entry #'identity))
  (labels ((to-string (value)
			 (make-string 1 :initial-element (code-char value))))
	(if (listp contents)
		(dolist (item contents)
		  (let ((entry (funcall get-entry item)))
			(when (numberp (cdr entry))
			  (setf (cdr entry) (to-string (cdr entry))))))
		(let ((count (length contents)))
		  (labels ((recur (index)
					 (when (< index count)
					   (let ((entry (funcall get-entry (aref contents index))))
						 (when (numberp (cdr entry))
						   (setf (cdr entry) (to-string (cdr entry)))))
					   (recur (1+ index)))))
			(recur 0))))
	contents))

(defun dictionary-okurigana-match-p (input entry)
  (with-entry (pattern word) entry
	(let* ((pattern-length (length pattern))
		   (last-char (char pattern (1- pattern-length))))
	  ;; * で終わる pattern のみを送りがな検索の対象とする
	  (when (and (char= last-char #\*) (< 1 pattern-length))
		;; 末尾の * を無視するように調整
		(decf pattern-length)
		(setf last-char (char pattern (1- pattern-length)))
		(labels ((make-entry-impl ()
				   (let ((suffix (subseq input (1- pattern-length))))
					 (multiple-value-bind (hira kana ascii err) (kana-convert-from-pattern suffix)
					   (declare (ignore kana ascii err))
					   (make-entry pattern (concatenate 'string word hira))))))
		  (if (char= last-char #\@)
			  ;; at sign で終わる pattern : あ行の送りがな（ex: dea' => 出会 ）
			  (when (and (<= pattern-length (length input))
						 (string= pattern input :end1 (1- pattern-length)
								  :end2 (1- pattern-length))
						 (position (char input (1- pattern-length)) "aeiou"))
				(make-entry-impl))
			  ;; 子音で終わる pattern : あ行以外の送りがな
			  (when (and (< pattern-length (length input))
						 (string= pattern input :end1 pattern-length :end2 pattern-length)
						 (position last-char "bcdfghjklmnpqrstvwxyz"))
				(make-entry-impl))))))))

