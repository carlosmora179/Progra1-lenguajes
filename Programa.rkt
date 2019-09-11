#lang racket

;Aca incia el codigo

(struct nodo(id nombre valor hermano hijo)#:transparent #:mutable)

(struct arbol(raiz)#:transparent #:mutable)


(define (list-all-nodes arbolito)
  (cond [(and(null? (nodo-hijo arbolito))(null? (nodo-hermano arbolito))) "fin"]
        [])

  )