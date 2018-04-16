

;; https://common-lisp.net/project/cl-heap/
(ql:quickload :cl-heap)

(defmacro while ((test &optional (result nil)) &body body)
  `(do ()
       ((not ,test) ,result)
     ,@body))




(defparameter *aluno* '(nome-completo "<your nome>"
			email-na-lista "<your email>"))



;;; QUESTAO 1

;; se sua resposta for f1 < f2 < f3 < f4 < f5 < f6, você deve
;; responder como abaixo. Ajuste a ordem dos números de acordo com sua
;; resposta.

(defun question-1 ()
  (list 1 2 3 4 5 6))


;;; QUESTAO 2

;; vc deve completar a resposta abaixo com as expressões adequadas
;; para termos o resultado pedido para cada algorítmo. O caso do
;; algoritmo 4 é um pouco mais delicado, vc ira precisar da função
;; auxiliar.

(defun nlogn (guess  &optional (err (expt 3 -100)))
  "<...>")

(defun question-2 ()
  (list :a1 (sqrt (* (expt 10 10) (* 60 60)))
	:a2 1
	:a3 1
	:a4 (nlogn "<...>")
	:a5 1
	:a6 1))


;;; QUESTAO 3

;; complete com a expressão correta da resposta. Se a resposta for
;; O(n^{log_2 10}) vc iria responder:

(defun question-3 (n)
  (expt n (log 10 2)))


;;; QUESTAO 4

;; complexidade : expressao em função de k e n.

(defun merge-1 (list1 list2 &optional (res nil))
  (cond ((null list1)
         (append (reverse res) list2))
        ((null list2)
	 (append (reverse res) list1))
        (t (if (<= (car list1) (car list2))
               (merge-1 (cdr list1) list2 (cons (car list1) res))
	       (merge-1 list1 (cdr list2) (cons (car list2) res))))))

(defun question-4-a (list-of-lists)
  ("<your code using merge-1>"))

(defun question-4-b (k n)
  ("<your expression here>"))

(defun linear-k-merge (lists)
  "Encontre o menor dos k itens (um de cada uma das listas
   ordenadas). Mova o menor para a lista de respostas e substitua-o
   pelo vizinho (o próximo elemento menor) na lista de origem de onde
   ele veio. Novamente, existem k itens, dentre os quais o menor deve
   ser selecionado. (Quando uma lista esgota, o último item movido não
   tem substituição, então, em seguida, encontramos o menor número em
   menos de k itens.)"
  "<your-code-here>")


(defun heap-merge (lists)
  "k itens (um de cada lista das listas ordenadas) são mantidos em um
   heap (sob disciplina: root = smallest - veja a página sobre o
   cl-heap). Mova o menor item para a lista de respostas, substitua o
   item movido no heap por seu menor vizinho na lista de origem da
   qual ele veio. Quando uma lista esgota, o último item movido não é
   substituído e a atualização do heap não é necessária e teremos
   menos k itens."
  "<your code using heap>")


(defun dc-merge (lists)
  "Recursivamente junte as primeiras k/2 listas e junte recursivamente
   as últimas k/2 listas. Em seguida, MERGE os dois resultados. Se k =
   2 então apenas um MERGE é necessário; se k = 1, apenas devolva a
   entrada."
  "<your divide-conquer code>")


;;; QUESTAO 5

(defun dijkstra (graph-as-lists s)
  "<your code here>")


;;; QUESTAO 6

(defun paths (graph-as-lists s t)
  "<your code here>")


;;; QUESTAO 7

;; vamos assumir que a rua é uma reta. As posições das casas são dadas
;; por números reais representando a distância da origem. A entrada é
;; uma lista de números e sua resposta será uma lista de numeros reais
;; representando a posição das antenas e o tamanho desta lista,
;; obviamente, indica quantas antenas forma necessárias.

(defun questao-7-a (casas)
  "<your code here>")


;; abaixo, n é o número de casas

(defun questao-7-b (n)
  "<expressao complexidade em funcao de n>")


;; QUESTAO 8


(defun questao-8 (list-of-clauses)
  "<your code here>")
