;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### package.lisp
;;
;;　${{TODO}{まだ記述されていません。}}
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#				(:file "package")
#|EXPORT|#				;package.lisp
 |#

(provide :clime)

(defpackage		:clime
  (:use			:common-lisp)
  #+sb-package-locks
  (:lock t)
  (:export  	;--------------- BEGIN EXPORT
				;client-data.lisp
				;console.lisp
				:test-raw-request
				:test-0-disconnect
				:test-1-search
				:test-S-search
				:test-X-search
				:test-N-next
				:test-2-version
				:test-3-hostinfo
				:test-4-notify-context
				:test-5-register
				:test-T-register
				:test-6-unregister
				:test-U-unregister
				:test-7-savedict
				:test-8-notify-selection
				:test-C-clear-context
				:test-E-enum-dict
				:test-I-dict-info
				:test-M-mark-modified
				:test-R-set-cand-limit
				;controller.lisp
				:controller-start
				:controller-stop
				:controller-alive-p
				;datetime-coder.lisp
				;dict-entry.lisp
				;dictionary-learn.lisp
				;dictionary-lookup.lisp
				;dictionary-set.lisp
				;dictionary-text.lisp
				;dictionary-user.lisp
				;dictionary.lisp
				;digits.lisp
				;kana.lisp
				;package.lisp
				;precision-stopwatch.lisp
				;protocols.lisp
				;queue.lisp
				;service.lisp
				;service.logger.lisp
				:*logger-no-debug*
				;service.port-listener.lisp
				;service.transactor.lisp
				;transaction.lisp
				;utility.lisp
				;vague-matcher.lisp
				;--------------- END EXPORT
))


(in-package :clime)


(defparameter *clime-server-address*     nil)
(defparameter *clime-server-version* "0.001")


