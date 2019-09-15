#lang racket

;Aca incia el codigo

(struct nodo(id nombre valor hermano hijo)#:transparent #:mutable);estructura del nodo de los arboles

(struct arbol(raiz)#:transparent #:mutable);estructura del arbol que solo contiene un nodo

;creacion del arbol
;                      n10
;                  /   |   \
;              n9      n6      n5
;              /\      |       |
;            n8  n7    n3      n4
;            |         /\
;            n0      n2  n1

(define n0 (nodo 0 "nodo0" 4 null null))
(define n1 (nodo 1 "nodo1" 5 null null))
(define n2 (nodo 2 "nodo2" 6 n1 null))
(define n3(nodo 3 "nodo3" 7 null n2))
(define n4(nodo 4 "nodo4" 8 null null))
(define n5(nodo 5 "nodo5" 9 null n4))
(define n6(nodo 6 "nodo6" 10 n5 n3))
(define n7(nodo 7 "nodo7" 11 null null ))
(define n8(nodo 8 "nodo8" 12 n7 n0))
(define n9(nodo 9 "nodo9" 13 n6 n8))
(define n10(nodo 10 "nodo10" 14 null n9))
;fin de la creacion del arbol


;funcion que imprime el arbol con un recorrido a profundidad de izquierda a derecha
;entradas: nodo raiz del arbol a imprimir
;salidas: lista de los nodos impresos a profundidad.
(define (list_all_nodes arbolito)
  ;si esta vacio el nodo que llega
  (if (empty? arbolito)
      '();devuelvo esto al estar vacio
      ;si no esta vacio hago todo esto
      (append (list (nodo-nombre arbolito));aca meto el primero de esa rama
              (list_all_nodes (nodo-hijo arbolito));aca llamo con el hijo
              (list_all_nodes (nodo-hermano arbolito)) )));aca con el hermano


