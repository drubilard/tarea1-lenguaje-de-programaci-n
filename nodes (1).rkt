#lang racket

(define starGraph'(
    ("A" "B") ("A" "C")
    ("A" "D") ("A" "E")
    ("A" "F")
    ))
(define snowFlake'(
    ("A" "B") ("A" "C")
    ("A" "D") ("B" "E")
    ("B" "F") ("C" "G")
    ("C" "H") ("D" "I")
    ("D" "J")
    ))

(define (star n) ;;crea una grafo estrella de n nodos
  (if (< n 2) null (cons (cons 1 (cons n '() ) ) (star (- n 1)))))

(define (snow1 n) ;;crea una grafo estrella de n nodos
  (if (< n 5) null (cons (cons 2 (cons n '() ) ) (snow1 (- n 1)))))

(define (snow2 n) ;;crea una grafo estrella de n nodos
  (if (< n 8) null (cons (cons 3 (cons n '() ) ) (snow2 (- n 1)))))

(define (snow3 n) ;;crea una grafo estrella de n nodos
  (if (< n 11) null (cons (cons 4 (cons n '() ) ) (snow3 (- n 1)))))

(define (snowP) 
  (append (append (add 1 2 (snow1 7))
  (add 1 3 (snow2 10)))
  (add 1 4 (snow3 13)))
  )

(define (nodes graph) ;;imprime los nodos de uns grafo
   (remove-duplicates(flatten graph)))

(define (add a b graph)  ;;añade nodo a un grafo
  (let ([graph (append graph (cons (cons a (cons b '())) '()))]) graph)
          )

(define (borrar graph item) ;;borra un elemento de un grafo
  (cond ((null? graph)
         '())
        ((equal? item (car graph))
         (cdr graph))
        (else
         (cons (car graph) 
               (borrar (cdr graph) item)))))

(define (kids n graph) ;;devuelve los hijos de un nodo
  (map second
       (filter (lambda (edge) (equal? n (first edge)))
               graph)))

(define (dad n graph) ;;devuelve el padre de un nodo
  (map first
       (filter (lambda (edge) (equal? n (second edge)))
               graph)))

(define (leaf? n graph) ;;responde t si es hoja y f si no lo es
  (null? (kids n graph)))

(define (gkids n graph) ;;entrega los hijos de los hijos de un nodo
  (map (lambda (node) (kids node graph)) (kids n graph)))

(define (dijstar x y s) ;camino mas corto entre 2 nodos en estrella
  (if (= x y) 0 (if (= x 0) "error" (if (= y 0) "error" (if (> x s) "no existe el nodo x" (if (> y s) "no existe el nodo y" (if (= x 1) 1 (if (= y 1) 1 2)))))))
  )

