;;;
;;; @see Land of Lisp, chapter 06
;;;

;;; 第5章で作ったゲームエンジンをロードする
(load "05-building-text-game-engine")

;;; 超簡易的なREPLを作成
(defun easy-game-repl ()
  (loop (print (eval (read)))))

;;; ゲーム専用のREPLを作成
(defun game-repl ()
  (let ((command (game-read)))
	(unless (eq 'quit (car command))
	  (game-print (game-eval command))
	  (game-repl))))

;;; ()なし'なしでの入力もできるようにgame-readを実装する
(defun game-read ()
  (let ((command (read-from-string
			  (concatenate 'string "(" (read-line) ")"))))
;	(print command)
	(flet ((quote-it (x)
					 (list 'quote x)))
;					 (quote x)))
	  (cons (car command) (mapcar #'quote-it (cdr command))))))

;;; game-readの中のquote-itで (quote-it (x) (quote x)) としても上手く行かない
;;; 単にX (xがクォートされた結果) を返すだけになる
;;; 引数をクォートするために (list 'quote x) とする必要がある

;;; quoteはクォートする関数

'foo                ;; ==> FOO
(quote foo)         ;; ==> FOO  (上と同じ結果)
(quote 'foo)        ;; ==> 'FOO
(list 'quote 'foo)  ;; ==> 'FOO (上と同じ結果)

		   


