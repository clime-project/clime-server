;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### vague-matcher.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#             (:file "vague-matcher"             :depends-on ("package"))
#|EXPORT|#              ;vague-matcher.lisp
 |#

(in-package :clime)

(defparameter +VAGUE-MATCHER-CHAR-RANGE+    128)    ;; support ASCII code only.
(defparameter +VAGUE-MATCHER-INITIAL-STATE+ (1+ (ash most-positive-fixnum -1)))

(defun vague-matcher-make-impl (accept-state epsilon-states char-states)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum accept-state epsilon-states))
  (lambda (target)
    (declare (type string target))
    (let ((length (length target)))
      (labels ((recur (idx states)
                 (declare (type fixnum idx states))
                 (if (= idx length)
                     (not (zerop (logand states accept-state)))
                     (if (not (zerop (logand states accept-state)))
                         t
                         (let* ((ch   (char target idx))
                                (code (char-code ch))
                                (mask (if (<= (the fixnum +VAGUE-MATCHER-CHAR-RANGE+) code)
                                          0
                                          (aref char-states code))))
                           (declare (type fixnum code mask))
                           (recur (the fixnum (1+ idx))
                                  (the fixnum (logior (the fixnum (logand states epsilon-states))
                                                      (the fixnum (ash (logand states mask) -1))))))))))
        (recur 0 +VAGUE-MATCHER-INITIAL-STATE+)))))


(defun vague-matcher-create (pattern)
  ;;最初に非 ASCII 文字が含まれていないかチェックしておく
  (if (position-if (lambda (ch)
                     (<= +VAGUE-MATCHER-CHAR-RANGE+ (char-code ch))) pattern)
      nil
      (let ((length         (length pattern))
            (epsilon-states 0)
            (char-states    (make-array +VAGUE-MATCHER-CHAR-RANGE+
                                        :element-type 'fixnum :initial-element 0)))
        (labels ((toggle-case (ch)
                   (cond
                     ((lower-case-p ch) (char-upcase   ch))
                     ((upper-case-p ch) (char-downcase ch))
                     (t ch)))
                 (set-ch-states (arr ch mask)
                   (setf (aref arr (char-code ch))
                         (logior (aref arr (char-code ch)) mask)))
                 (recur (idx mask)
                   (if (zerop mask) ;; means too long pattern...
                       nil
                       (if (= idx length)
                           (vague-matcher-make-impl mask epsilon-states char-states)
                           (let ((ch (char pattern idx)))
                             (set-ch-states char-states ch mask)
                             (set-ch-states char-states (toggle-case ch) mask)
                             (setf epsilon-states (logior epsilon-states (ash mask -1)))
                             (recur (1+ idx) (ash mask -1)))))))
          (recur 0 +VAGUE-MATCHER-INITIAL-STATE+)))))



;;(defparameter *matcher* (vague-matcher-create "ksrp"))
;;(time (funcall *matcher* "kusoripu"))
;; 
;;(require :cl-ppcre)
;;(defparameter *scanner* (cl-ppcre:create-scanner "^k.*s.*r.*p.*"))
;;(time (cl-ppcre:scan *scanner* "kusoripu"))
