;;;
;;; @see Land of Lisp, chapter 06
;;;

;;; スクリーンの表示
;;;  print は改行する、prin1 は改行しない 

(progn
  (print "Hello")
  (print "World"))
;; ==> "Hello"
;;     "World"

(progn
  (prin1 "Hello")
  (prin1 "World"))
;; ==> "Hello""World"

(defun say-hello ()
  (print "please type your name: ")
  (let ((name (read)))
	(print "nice to meet you, ")
	(print name)))

(defun add-five ()
  (print "please enter a number: ")
  (let ((num (read)))
	(print "when i add five i get")
	(print (+ 5 num))))


(print '3)     ;; ==> 3     数値
(print '3.4)   ;; ==> 3.4   数値
(print 'foo)   ;; ==> FOO   シンボル
(print "foo")  ;; ==> "foo" 文字列
(print '\#f)   ;; ==> #\f   文字


;;; 人間が見るのにやさしいprincという関数もある
(princ '3)       ;; ==> 3
(princ '3.4)     ;; ==> 3.4
(princ 'foo)     ;; ==> foo
(princ '"foo")   ;; ==> foo
(princ '\#f)     ;; ==> f

(defun say-hello2 ()
  (princ "pelase type your name: ")
  (let ((name (read)))
	(princ "nice to meet you, ")
	(princ name)))

