
;;; defparmeterでトップレベルで変数を定義する
;;; - この変数はグローバル変数となる
;;; - 変数名はcase sensitiveである
;;; - *...* は変数名の一部だが、Lisperはグローバル変数の場合は好んで*...* を付けるらしい
(defparameter *small* 1)
(defparameter *big* 100)

;;; defunでグローバル関数を定義する
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

;;; setfで変数の値を変更する
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

;;; ゲームを開始する関数
(defun start-over ()
   (defparameter *small* 1)
   (defparameter *big* 100)
   (guess-my-number))
