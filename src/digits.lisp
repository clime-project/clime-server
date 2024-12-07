;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### digits.lisp
;;
;;　日付・時刻のフォーマットを実装します。
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "digits"                    :depends-on ("package"
#|ASD|#	                                                            "datetime-coder"
#|ASD|#	                                                            "utility"))
#|EXPORT|#				;digits.lisp
|#

(in-package :clime)

(defparameter *digits-date-formats*  (list "yyyy/mm/dd"))
(defparameter *digits-time-formats*  (list "HH:MM:SS" "HH:MM"))
(defparameter *digits-date-stampers* nil)
(defparameter *digits-time-stampers* nil)

(defparameter *date-scanner* (ppcre:create-scanner "^(?:(\\d+)/)?(\\d+)/(\\d\\d?)$"))
(defparameter *time-scanner* (ppcre:create-scanner "^(\\d\\d?):(\\d\\d?)(?::(\\d\\d?))?$"))

;; 数字で始まっているかを調べる
(defun digits-starting-p (input)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string input))
  (if (zerop (length input))
	  nil
	  (<= (the fixnum #.(char-code #\0))
		  (the fixnum (char-code (char input 0)))
		  (the fixnum #.(char-code #\9)))))

;; 数字、:、/ 、その他の文字の数を数える
(defun digits-count-chars (input)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-string input))
  (labels ((recur (idx digit slash colon others)
			 (declare (type fixnum idx digit slash colon others))
			 (if (= idx (the fixnum (length input)))
				 (values digit slash colon others)
				 (let ((code (char-code (char input idx))))
				   (declare (type fixnum code))
				   (cond
					 ((<= (the fixnum #.(char-code #\0))
						  (the fixnum code)
						  (the fixnum #.(char-code #\9))) (incf digit))
					 ((=  (the fixnum code)
						  (the fixnum #.(char-code #\/))) (incf slash))
					 ((=  (the fixnum code)
						  (the fixnum #.(char-code #\:))) (incf colon))
					 (t                                   (incf others)))
				   (recur (1+ idx) digit slash colon others)))))
	(recur 0 0 0 0 0)))

(defun digits-get-days-of-month (year month)
  (labels ((leap-year ()
			 (cond
			   ((null year)            0)
			   ((zerop (mod year 400)) 1)
			   ((zerop (mod year 100)) 0)
			   ((zerop (mod year   4)) 1)
			   (t                      0))))
	(ecase month
	  (( 1)    31)
	  (( 2) (+ 28 (leap-year)))
	  (( 3)    31)
	  (( 4)    30)
	  (( 5)    31)
	  (( 6)    30)
	  (( 7)    31)
	  (( 8)    31)
	  (( 9)    30)
	  ((10)    31)
	  ((11)    30)
	  ((12)    31))))

(defun digits-date-impl (year mon date)
  (unless year
	(when (< 12 mon)
	  (setf year  mon)
	  (setf mon  date)
	  (setf date    1)))
  (when (and year (< year 1000))
	(setf year (+ 2000 year)))
  ;;MEMO : 暫定対処 - universal-time 制約により 1900年以前は無視する
  (when (and (or (null year) (< 1900 year))
			 (<= 1 mon 12)
			 (<= 1 date (digits-get-days-of-month year mon)))
	;;MEMO : year が nil になるパターンがあるが、呼び出し元で展開する
	(list :date year mon date)))

(defun digits-time-impl (hour minute second)
  (if (not (and (<= 0 hour 23) (<= 0 minute 59) (<= 0 second 59)))
	  nil
	  (list :time hour minute second)))

(defun digits-date (input)
  (multiple-value-bind (dummy1 dummy2 arr1 arr2)
					   (ppcre:scan *date-scanner* input)
	(declare (ignore dummy2))
	(if (null dummy1)
		nil
		(labels ((parse (idx)
				   (parse-integer input :start (aref arr1 idx) :end (aref arr2 idx))))
		  (awhen (digits-date-impl (and (aref arr1 0) (parse 0)) (parse 1) (parse 2))
			(list it))))))

(defun digits-time (input)
  (multiple-value-bind (dummy1 dummy2 arr1 arr2)
					   (ppcre:scan *time-scanner* input)
	(declare (ignore dummy2))
	(if (null dummy1)
		nil
		(labels ((parse (idx)
				   (parse-integer input :start (aref arr1 idx) :end (aref arr2 idx))))
		  (awhen (digits-time-impl (parse 0) (parse 1) (if (aref arr1 2) (parse 2) 0))
			(list it))))))

(defun digits-digit-sequence (input)
  (labels ((value (i)
			 (- (char-code (char input i)) #.(char-code #\0))))
	(symbol-macrolet ((v0 (value 0)) (v1 (value 1))
					  (v2 (value 2)) (v3 (value 3))
					  (v4 (value 4)) (v5 (value 5))
					  (v6 (value 6)) (v7 (value 7)))
	  (delete-if #'null (case (length input)
						  ((2) (list (digits-date-impl nil v0 v1)
									 (digits-time-impl  v0 v1 0)))
						  ((3) (list (digits-date-impl nil v0 (+ (* v1 10) v2))
									 (digits-date-impl nil (+ (* v0 10) v1) v2)
									 (digits-time-impl v0 (+ (* v1 10) v2) 0)
									 (digits-time-impl (+ (* v0 10) v1) v2 0)))
						  ((4) (list (list :integer (parse-integer input))
									 (digits-date-impl nil (+ (* v0 10) v1) (+ (* v2 10) v3))
									 (digits-time-impl (+ (* v0 10) v1) (+ (* v2 10) v3)  0)))
						  ;;MEMO : 5 桁以上の場合の「date/time の区切り位置の模索」はひとまずしない
						  ((6) (list (list :integer (parse-integer input))
									 (digits-date-impl (+ (* v0 10) v1)
													   (+ (* v2 10) v3) (+ (* v4 10) v5))
									 (digits-time-impl (+ (* v0 10) v1)
													   (+ (* v2 10) v3) (+ (* v4 10) v5))))
						  ((8) (list (list :integer (parse-integer input))
									 (digits-date-impl (+ (* v0 1000) (* v1 100) (* v2 10) v3)
													   (+ (* v4 10) v5) (+ (* v6 10) v7))))
						  (t   (list (list :integer (parse-integer input)))))))))

(defun digits-listup-data (input)
  (if (not (digits-starting-p input))
	  nil
	  (multiple-value-bind (digit slash colon others) (digits-count-chars input)
		(if (or (< 0 others)
				(< digit 2)
				(and (< 0 slash) (< 0 colon))
				(or  (< 2 slash) (< 2 colon))
				(not (< (+ slash colon) digit)))
			nil
			(let ((year nil))
			  (labels ((recur (lst acc)
						 (if (null lst)
							 (nreverse acc)
							 (let ((dt (car lst)))
							   (if (not (and (eq (first dt) :date) (null (second dt))))
								   (recur (cdr lst) (push (car lst) acc))
								   (progn
									 (unless year
									   (setf year (sixth (dtcoder-now))))
									 (setf acc (push (cons :date (cons     year  (cddr dt))) acc))
									 (setf acc (push (cons :date (cons (1+ year) (cddr dt))) acc))
									 (setf acc (push (cons :date (cons (1- year) (cddr dt))) acc))
									 (recur (cdr lst) acc)))))))
				(recur (if (not (zerop slash))
						   (digits-date input)
						   (if (not (zerop colon))
							   (digits-time input)
							   (digits-digit-sequence input))) nil)))))))
			  
(defun digits-candidates (input callback)
  (let ((lst (digits-listup-data input)))
	(when lst
	  (labels ((recur1 (stampers data)
				 (when stampers
				   (funcall callback (dtcoder-format-timestamp (car stampers) data))
				   (recur1 (cdr stampers) data)))
			   (recur2 (lst)
				 (when lst
				   (let* ((data (car lst))
						  (type (car data)))
					 (cond
					   ((eq type :date)
						(recur1 *digits-date-stampers* (apply #'dtcoder-make-date (cdr data))))
					   ((eq type :time)
						(recur1 *digits-time-stampers* (apply #'dtcoder-make-time (cdr data))))
					   ((eq type :integer)
						(funcall callback (format nil "~:D" (cadr data)))))
					 (recur2 (cdr lst))))))
	  (recur2 lst)))))
