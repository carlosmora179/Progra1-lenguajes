#lang racket

;Creamos las estructuras de los componentes que se nesesitan para poder jugar domino.
(struct ficha(valor1 valor2 usado1 usado2 promesing)#:transparent #:mutable)
(struct tablero(fichas)#:transparent #:mutable)

;Definimos las fichas 
 
;Definimos las 0 0 
(define f00 (ficha 0 0 #f #f #t)) 
(define f01 (ficha 0 1 #f #f #t)) 
(define f02 (ficha 0 2 #f #f #t)) 
(define f03 (ficha 0 3 #f #f #t)) 
(define f04 (ficha 0 4 #f #f #t)) 
(define f05 (ficha 0 5 #f #f #t)) 
(define f06 (ficha 0 6 #f #f #t)) 
 
;Definimos las 1 1 
(define f11 (ficha 1 1 #f #f #t)) 
(define f12 (ficha 1 2 #f #f #t)) 
(define f13 (ficha 1 3 #f #f #t)) 
(define f14 (ficha 1 4 #f #f #t)) 
(define f15 (ficha 1 5 #f #f #t)) 
(define f16 (ficha 1 6 #f #f #t)) 
 
;Definimos las 2 2 
(define f22 (ficha 2 2 #f #f #t)) 
(define f23 (ficha 2 3 #f #f #t)) 
(define f24 (ficha 2 4 #f #f #t)) 
(define f25 (ficha 2 5 #f #f #t)) 
(define f26 (ficha 2 6 #f #f #t)) 
 
;Definimos los 3 3 
(define f33 (ficha 3 3 #f #f #t)) 
(define f34 (ficha 3 4 #f #f #t)) 
(define f35 (ficha 3 5 #f #f #t)) 
(define f36 (ficha 3 6 #f #f #t)) 
 
;Definimos los 4 4 
(define f44 (ficha 4 4 #f #f #t)) 
(define f45 (ficha 4 5 #f #f #t)) 
(define f46 (ficha 4 6 #f #f #t)) 
 
;Definimos los 5 5 
(define f55 (ficha 5 5 #f #f #t)) 
(define f56 (ficha 5 6 #f #f #t)) 
 
;Definimos los 6 6 
(define f66 (ficha 6 6 #f #f #t)) 
 
;Definimos el tablero 
(define t (tablero (list f66 f56 f46 f55 f36 f45 f26 f35 f44 f16 f25 f34 f06 f15 f24 f33 f05 f14 f23 f04 f13 f22 f03 f12 f02 f11 f01 f00))) 
 
