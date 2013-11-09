
;;; Lispでは空リストが偽、それ以外は真
;;; なので、以下のような実装が可能
(defun my-length (list)
  (if list
	  (1+ (my-length (cdr list)))
	0))

(my-length '(list with four symbols))


;;; '() () nil 'nil は全て空リストとして扱われる
(eq '() nil)  ;; ==> T
(eq '() ())   ;; ==> T
(eq '() 'nil) ;; ==> T


;;; Lispは普通の関数呼び出しでは、関数名の後に続く式は、
;;; 関数自体が呼ばれる前に全て評価されるが、if はその規則に従わない
;;; 例えば以下を実行しても、0 除算は行われない
;;; (ただしコンパイルを行う場合は、評価されてしまうので注意)
(if (oddp 5)
	'odd-number
  (/ 1 0) ;; こっちの分岐は評価されない
)

;;; ifの分岐で複数のことを実行したい場合はprognを用いる
(defvar *number-is-odd* nil)
(if (oddp 5)
	(progn
	  (setf *number-is-odd* t)
	  'odd-number)
  'even-number)


;;; whenまたはunlessを使うと、prognなしで複数のコマンドを実行できる
;;; (これを暗黙のprognという)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)

(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)


;;; condを使うと複数の分岐が書けて、暗黙のprognも使える
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry)
		 (setf *arch-enemy* 'stupid-lisp-alien)
		 '(curse you lisp alien - you ate my pudding))
		((eq person 'johnny)
		 (setf *arch-enemy* 'useless-old-johnny)
		 '(i hope you choked on my pudding johnny))
		(t
		 '(why you eat my pudding stranger?))
		))

;;; caseによる分岐
;;; CやJavaのswitchのように比較対象になる値だけを並べればよい
;;;
;;; caseコマンドは比較にeqを使うことに注意
;;; (シンボルの値で分岐するため、文字列の値で分岐することはできない)
(defun pudding-eater2 (person)
  (case person
	((henry)
	 (setf *arch-enemy* 'stupid-lisp-alien)
	 '(curse you lisp alien - you ate my pudding))
	((johnny)
	 (setf *arch-enemy* 'useless-old-johnny)
	 '(i hope you choked on my pudding johnny))
	(otherwise
	 '(why you eat my pudding stranger?))
	))


;;; and/or はその名の通りの論理オペレータ
(and (oddp 3) (oddp 5) (oddp 7)) ;; ==> T
(or  (oddp 2) (oddp 3) (oddp 4)) ;; ==> T


;;; memberはリストの中に要素が含まれているかどうかを調べる関数
(if (member 1 '(3 4 1 5))
	'one-is-in-the-list
  'one-is-not-in-the-list)

(member 1 '(3 4 1 5)) ;; ==> (1 5) と部分リストを返す

;;; find-if は最初の引数に関数を受け取り、
;;; リストの要素を関数に与え、Tを返すまで順に見て行き、Tとなった要素を返す
(find-if #'oddp '(2 4 5 6)) # ==> 5

;;; よって以下のケースでは nil を返す...
;;; (そのため条件式としては使いにくい)
(find-if #'null '(2 4 nil 6)) # ==> NIL



;;; 比較についてのコンラッドのルール
;;; 1. シンボル同士は、常にeqで比較すべし
;;; 2. シンボル同士の比較でなければ、equalを使え

;;; eqはシンボル同士の比較しか結果が保証されていない

;; シンボル同士
(eq 'apple 'apple) ;; ==> T

;; リスト同士
(eq (list 1 2 3) (list 1 2 3)) ;; ==> NIL
(eq (list 1 2 3) (cons 1 (cons 2 (cons 3 ())))) ;; ==> NIL

;; 整数同士
(eq 5 5) ;; ==> T

;; 浮動小数点同士
(eq 2.5 2.5) ;; ==> NIL

;; 文字列同士
(eq "foo" "foo") ;; ==> NIL
(eq #\a #\a) ;; ==> T


;;; 基本的にはシンボル同士の比較でしか使えないeqに対し
;;; equalは全てのデータ型の比較で使うことができる

;; シンボル同士
(equal 'apple 'apple) ;; ==> T

;; リスト同士
(equal (list 1 2 3) (list 1 2 3)) ;; ==> T
(equal (list 1 2 3) (cons 1 (cons 2 (cons 3 ())))) ;; ==> T

;; 整数同士
(equal 5 5) ;; ==> T

;; 浮動小数点同士
(equal 2.5 2.5) ;; ==> T

;; 文字列同士
(equal "foo" "foo") ;; ==> T
(equal #\a #\a) ;; ==> T


;;; eqlはeqと似ているが、eqと違って数値と文字列も比較できる

;; シンボル同士
(eql 'apple 'apple) ;; ==> T

;; リスト同士
(eql (list 1 2 3) (list 1 2 3)) ;; ==> T
(eql (list 1 2 3) (cons 1 (cons 2 (cons 3 ())))) ;; ==> NIL

;; 整数同士
(eql 5 5) ;; ==> T

;; 浮動小数点同士
(eql 2.5 2.5) ;; ==> T

;; 文字列同士
(eql "foo" "foo") ;; ==> NIL
(eql #\a #\a) ;; ==> T


;;; equalpはequalとほぼ同じだが、少し複雑なケースにも対応できる

;; 大文字小文字の違いのある文字列比較
(equalp "Bob Smith" "bob smith") ;; ==> T
(equal "Bob Smith" "bob smith") ;; ==> NIL

;; 整数と浮動小数点の比較
(equalp 1 1.0) ;; ==> T
(equal 1 1.0) ;; ==> NIL







