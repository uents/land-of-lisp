
;;; シンタックスとセマンティクス
;;; 
;;; - シンタックス
;;;   テキストが正しい文を構成するために必要な基本的な規則
;;; - セマンティクス
;;;   構成された文の意味


;;; Lispのシンタックスの構成要素
;;; 
;;; 以下の関数宣言はシンボルと括弧だけで書かれている。
;;; 実のところこれは括弧で区切られ入れ子になったリストにすぎない。
;;; Lispコードを構成する方法、括弧を使ってコードをリストにするしかない。

(defun square (n)
  (* n n))

;;; Common Lispのシンボルはcase sensitiveではない

(eq 'fooo 'FoOo) ;; ==> T


;;; コードモードとデータモード
;;;
;;; - コードモード
;;;   リストの先頭要素をコマンド、2番目以降の要素をフォームと解釈し実行するモード
;;;   Lispはデフォルトでコードモードになっている
;;; 
;;; - データモード
;;;   リストを全てデータとして扱うモード
;;;   リストの先頭にクォートを


;; コードモードの例: expt と + はコマンド、それ以外はフォーム
(expt 2 (+ 3 4)) ;; ==> 128

;; データモードの例: 全てデータ
'(expt 2 7) ;; ==> (expt 2 7)


;;; リストを扱う関数
;;; - リスト : Lispの場合、2つの要素をつなげるセル(コンスセル)がただただ長くなっただけのもの
;;; - cons : 2つのデータをつなげる関数 (データはどんなオブジェクトでもよい)
;;; - list : 複数のデータを一気につなげる関数
;;; - car  : 最初のコンスセルの値 (リストの最初の要素の値) を取り出す関数
;;; - cdr  : 2番目のコンスセル (リストの2番目以降の残りのリスト) を取り出す関数
;;; - cadr : 2番目のコンスセルの値 (リストの2番目の要素の値) を取り出す関数
;;;
;;; 他、cdar cddr cddar cadadr などなど

;; 以下の結果は同じ
'(pork beef chicken)
(cons 'pork (cons 'beef (cons 'chicken 'nil)))
(list 'pork 'beef 'chicken)

;; 以下の結果は同じ
(car (cdr '(pork beef chicken)))
(cadr '(pork beef chicken))


;;; リスト操作の練習
(car '((peas carrots tomatoes) (pork beef chicken)))       ;; -> (peas beef chicken)
(cdr '(peas carrots tomatoes))                             ;; -> (carrots tomatoes)
(cdr (car '((peas carrots tomatoes) (pork beef chicken)))) ;; -> (carrots tomatoes)
(cdar '((peas carrots tomatoes) (pork beef chicken)))      ;; -> (carrots tomatoes)

(cons (cons 'peas (cons 'carrots (cons 'tomatoes ())))
	  (cons (cons 'pork (cons 'beef (cons 'chicken ()))))) ;; -> ((peas carrots tomatoes) (pork beef chicken))

(cddr '((peas carrots tomatoes) (pork beef chicken) duck))   ;; -> (duck)
(caddr '((peas carrots tomatoes) (pork beef chicken) duck))  ;; -> duck
(cddar '((peas carrots tomatoes) (pork beef chicken) duck))  ;; -> (tomatoes)
(cadadr '((peas carrots tomatoes) (pork beef chicken) duck)) ;; -> beef
