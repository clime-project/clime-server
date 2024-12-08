;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### dict-entry.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#                             (:file "dict-entry"            :depends-on ("package"))
#|EXPORT|#              ;dict-entry.lisp
 |#

(in-package :clime)


;;------------------------------------------------------------------------------
;;
;; entry
;;
;;------------------------------------------------------------------------------
(declaim (inline make-entry))
(defun make-entry (pattern word)
  (cons pattern word))

(declaim (inline pattern-of))
(defun pattern-of (entry)
  (car entry))

(declaim (inline word-of))
(defun word-of (entry)
  (cdr entry))

(declaim (inline entry=))
(defun entry= (e1 e2)
  (and (string= (car e1) (car e2))
       (string= (cdr e1) (cdr e2))))

(declaim (inline entry/=))
(defun entry/= (e1 e2)
  (or (string/= (car e1) (car e2))
      (string/= (cdr e1) (cdr e2))))

(defmacro with-entry ((pattern-sym word-sym) entry &rest body)
  `(destructuring-bind (,pattern-sym . ,word-sym) ,entry
     ,@body))


;;------------------------------------------------------------------------------
;;
;; context-entry
;;
;;------------------------------------------------------------------------------
(declaim (inline make-context-entry))
(defun make-context-entry (context1 context2 entry)
  (cons context1 (cons context2 entry)))

(declaim (inline context1-of))
(defun context1-of (c-entry)
  (first c-entry))

(declaim (inline context2-of))
(defun context2-of (c-entry)
  (second c-entry))

(declaim (inline entry-of))
(defun entry-of (c-entry)
  (cddr c-entry))

(declaim (inline context-pattern-of))
(defun context-pattern-of (c-entry)
  (caddr c-entry))

(declaim (inline context-word-of))
(defun context-word-of (c-entry)
  (cdddr c-entry))

(declaim (inline context-entry=))
(defun context-entry= (e1 e2)
  (and (string= (context1-of        e1) (context1-of        e2))
       (string= (context2-of        e1) (context2-of        e2))
       (string= (context-pattern-of e1) (context-pattern-of e2))
       (string= (context-word-of    e1) (context-word-of    e2))))

(declaim (inline context-match-level))
(defun context-match-level (entry context1 context2)
  (cond
    ((string/= (context2-of entry) context2) 0)
    ((string/= (context1-of entry) context1) 1)
    (t                                       2)))

(defmacro with-context-entry ((context1-sym context2-sym pattern-sym word-sym) entry &rest body)
  `(destructuring-bind (,context1-sym ,context2-sym ,pattern-sym . ,word-sym) ,entry
     ,@body))
