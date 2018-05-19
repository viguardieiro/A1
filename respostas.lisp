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
  (list 2 3 6 1 4 5))


;;; QUESTAO 2

;; vc deve completar a resposta abaixo com as expressões adequadas
;; para termos o resultado pedido para cada algorítmo. O caso do
;; algoritmo 4 é um pouco mais delicado, vc ira precisar da função
;; auxiliar.

(defun nlogn (guess  &optional (err (expt 3 -100)))
  (loop for y = 1 then x
	for x = guess then (/ (* (* 60 60) (expt 10 10)) (log x 2))
	while (> (abs (- x y)) err)
	finally (return x)))

(defun question-2 ()
  (list :a1 (sqrt (* (expt 10 10) (* 60 60)))
	:a2 (expt (* (expt 10 10) (* 60 60)) 1/3)
	:a3 (sqrt (* (expt 10 8) (* 60 60)))
	:a4 (nlogn-gabarito (expt 10 5))
	:a5 (log (* (expt 10 10) (* 60 60)) 2)
	:a6 (log (log (* (expt 10 10) (* 60 60)) 2) 2)))


;;; QUESTAO 3

;; complete com a expressão correta da resposta. Se a resposta for
;; O(n^{log_2 10}) vc iria responder:

(defun question-3 (n)
  (expt n (log 3 2)))


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
  (when list-of-lists
      (reduce #'merge-1 list-of-lists)))

(defun question-4-b (k n)
  (* (expt k 2) n))

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
  (if (cdr lists)
      (let* ((queue (make-instance 'cl-heap:priority-queue)))
	(mapc #'(lambda (list) (cl-heap:enqueue queue (list (car list) (cdr list)) (car list))) lists)
	(heap-aux queue))
      (car lists)))

(defun heap-aux (queue)
  (let* ((aux (cl-heap:dequeue queue))
	 (item (car aux))
	 (list (cadr aux)))
    (cond ((and (= (cl-heap:queue-size) 0) (not list)) nil)
	  ((not list) (cons item (heap-aux queue)))
	  (t (cl-heap:enqueue queue (list (car list) (cdr list)) (car list))
	     (cons item (heap-aux queue))))))


(defun dc-merge (lists &optional (k (length lists)))
  "Recursivamente junte as primeiras k/2 listas e junte recursivamente
   as últimas k/2 listas. Em seguida, MERGE os dois resultados. Se k =
   2 então apenas um MERGE é necessário; se k = 1, apenas devolva a
   entrada."
  (let ((middle (floor (/ k 2))))
    (cond ((< k 2) (car lists))
	  ((= k 2) (merge-1 (car lists) (cadr lists)))
	  (t (merge-1 (dc-merge (subseq lists 0 middle) middle)
		      (dc-merge (nthcdr middle lists) (- k middle)))))))


;;; QUESTAO 5

(defun dijkstra (graph-as-lists s)
  "<your code here>")


;;; QUESTAO 6

(defclass node ()
  ((obj     :accessor node-obj
	    :initarg :obj)
   (visited :accessor node-visited
	    :initarg :visited
	    :initform nil)
   (nbs     :accessor node-nbs
	    :initarg :nbs
	    :initform nil)))


(defun make-graph (alists &optional (test #'equal))
  (let ((tb (make-hash-table :test test)))
    (mapc (lambda (al)
	    (setf (gethash (car al) tb)
		  (make-instance 'node :obj (car al))))
	  alists)
    (mapcar (lambda (al)
	      (let ((u (gethash (car al) tb)))
		(dolist (v (cadr al))
		  (push (gethash v tb) (node-nbs u)))
		(setf (node-nbs u) (reverse (node-nbs u)))
		u))
	    alists)))


(defun make-clocker (n)
  (let ((clock n))
    (lambda () (incf clock))))


(defun explore (v clock target)
  (loop for u in (node-nbs v) do
       (if (equal (node-obj u) (node-obj target))
	   (funcall clock)
	   (explore u clock target))))

(defun paths (graph-as-lists u v)
  (let* ((node-order (mapcar #'car graph-as-lists))
	 (graph (sort (make-graph graph-as-lists #'equal)
		      (lambda (a b) (< (position a node-order :test #'equal)
				       (position b node-order :test #'equal)))
		      :key #'node-obj))
	 (source (find u graph :test #'(lambda (x y) (equal (node-obj y) x))))
	 (target (find v graph :test #'(lambda (x y) (equal (node-obj y) x))))
	 (clock (make-clocker -1)))
    (explore source clock target)
    (funcall clock)))

;;; QUESTAO 7

;; vamos assumir que a rua é uma reta. As posições das casas são dadas
;; por números reais representando a distância da origem. A entrada é
;; uma lista de números e sua resposta será uma lista de numeros reais
;; representando a posição das antenas e o tamanho desta lista,
;; obviamente, indica quantas antenas forma necessárias.

(defun questao-7-a (casas)
  (let* ((casas-ordenadas  (sort casas #'<))
	 (primeira-antena (+ 4 (car casas-ordenadas))))
    (cons primeira-antena (casas-aux (cdr casas-ordenadas) primeira-antena))))

(defun casas-aux (casas ultima-antena)
  (cond
    ((null casas) nil)
    ((<= (car casas) (+ 4 ultima-antena)) (casas-aux (cdr casas) ultima-antena))
    (t (let ((proxima-antena (+ 4 (car casas))))
	 (cons proxima-antena (casas-aux (cdr casas) proxima-antena))))))


;; abaixo, n é o número de casas

(defun questao-7-b (n)
  (n * (log n)))


;; QUESTAO 8

(defun questao-8 (list-of-clauses)
  (let ((values '())
        (flag 0))
    (dolist (x list-of-clauses)
      (when (listp (car x))
        (setq values (union values (list (cons (car (cdr x)) 1)) :key #'car))))
    (dolist (x list-of-clauses)
      (when (not (listp (car x)))
        (setq flag 1)
        (dolist (y x)
          (when (not (assoc y values))
            (setq flag 0)))
        (when (equal flag 1)
          (return-from questao-8 nil))))
    (dolist (x list-of-clauses)
      (if (listp (car x))
          (dolist (y (car x))
            (when (not (assoc y values))
              (setq values (union values (list (cons y 0)) :key #'car))))
        (when (not (assoc x values))
              (setq values (union values (list (cons (car x) 0)) :key #'car)))))
    values))
