#lang racket

;Creamos las estructuras de los componentes que se nesesitan para poder jugar domino.
(struct ficha(valor1 valor2 usado1 usado2 promesing)#:transparent #:mutable)
(struct tablero(fichas)#:transparent #:mutable)
