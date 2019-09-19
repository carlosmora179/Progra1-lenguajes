#lang racket

;Creamos las estructuras de los componentes que se nesesitan para poder jugar domino.
(struct ficha(valor1 valor2 conectado1 conectado2 promesing)#:transparent #:mutable)
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
 
;Funciones 
 
;Funcion eliminacion de una ficha en especifico. 
(define (eliminar_ficha nombre_ficha)
  ;Creo una condición donde verifico que si esta vacia la lista en la estructura, si lo esta retorna una lista vacía
  (cond 
    [(empty? (tablero-fichas t)) cons (tablero-fichas t) '()]
    ;Sino elimina la ficha especificada
    [else (set-tablero-fichas! t (remove f00 (tablero-fichas t)))]) 
  ) 
;Funcion de repartir fichas (no funciona)
(define (repartir_fichas lista)
  ;Condición que verifica si esta vacia solo retorna un espacio de strings si no recorre la lista y recorre uno por uno de la lista en formato = [6,6][5,6]
  (cond
    [(empty? lista) cons ""]
    [else  (string-append "["
                          (string-append (number->string (ficha-valor1 (first lista))))
                                                            (string-append ","
                                                                           (string-append (number->string (ficha-valor2 (first lista))) "]\n")))(repartir_fichas (cdr lista))]))
;Funcion que resulve el domino de manera backtracking (no funciona)
(define (resolver_domino table)
  ;condición que verifica que lista de la estructura no este vacía y sino llama a un auxiliar donde se le añade la lista normal, una lista y un indice
  (cond
    [(empty? (tablero-fichas table)) cons '()]
    [else (resolver_domino_aux (tablero-fichas table) '() 0 )]))

;Funcion auxiliar donde le entran tres parametros la lista sin resolver, en la que se va a resolver y un indice para recorrer la lista sin resolver
(define (resolver_domino_aux lvieja lnueva indice)
  ;hay una condicion que valida que aun no termine hasta que lvieja este vacía y asi retornarla
  (cond
    [(empty? lvieja) lnueva]
    ;hay otra condicion donde ve que el indice no supere el tamaño de lvieja porque si no eso significa que no encontro mas fichas competentes y por eso llama a un verificador
    ;para ver si es que ya no hay mas soluciones o un se puede
    [(< (length lvieja) indice) (verificador lvieja lnueva indice)]
    ;esta condicional ve si lnueva esta vacia para añadirla la primera asi de la nada ya que es la mas grande de fijo y eleminarla de lvieja y se hace llamada recursiva
    [(empty? lnueva) (set! lnueva (append lnueva (list (list-ref lvieja 0)))) (set! lvieja (remove (list-ref lvieja 0) lvieja)) (resolver_domino_aux lvieja lnueva 0)]
    ;vemos si se pueden conectar con el puerto uno de la ficha, y se verifica si alguna de las fichas coincide con la ultima de lnueva
    [(eqv? (ficha-conectado1 (last lnueva)) #f)(cond
                                                 ;aqui despues de entrar a la condicional se valida si el valor coincide
                                                 ;si coincide al ficha se agrega a la lnueva y se elimina de lvieja y bloquean los puertos se hace en las 4 condicionales siguinetes
                                           [(= (ficha-valor1 (last lnueva)) (ficha-valor1 (list-ref lvieja indice)))
                                            (set-ficha-conectado1! (list-ref lvieja indice) #t)
                                            (set-ficha-conectado1! (last lnueva) #t)
                                            (set! lnueva (append lnueva (list-ref lvieja indice)))
                                            (set! lvieja (remove (list-ref lvieja indice) lvieja))
                                            (resolver_domino_aux lvieja lnueva 0)]
                                           [(= (ficha-valor1 (last lnueva)) (ficha-valor2 (list-ref lvieja indice)))
                                            (set-ficha-conectado2! (list-ref lvieja indice) #t)
                                            (set-ficha-conectado1! (last lnueva) #t)
                                            (set! lnueva (append lnueva (list-ref lvieja indice)))
                                            (set! lvieja (remove (list-ref lvieja indice) lvieja))
                                            (resolver_domino_aux lvieja lnueva 0)]
                                           [else (resolver_domino_aux lvieja lnueva (+ indice 1))])]
    [(eqv? (ficha-conectado2 (last lnueva) ) #f)(cond
                                           [(= (ficha-valor2 (last lnueva)) ((ficha-valor1 (list-ref lvieja indice))))
                                            (set-ficha-conectado1! (list-ref lvieja indice) #t)
                                            (set-ficha-conectado2! (last lnueva) #t)
                                            (set! lnueva (append lnueva (list-ref lvieja indice)))
                                            (set! lvieja (remove (list-ref lvieja indice) lvieja))
                                            (resolver_domino_aux lvieja lnueva 0)]
                                           [(= (ficha-valor2 (last lnueva)) ((ficha-valor2 (list-ref lvieja indice))))
                                            (set-ficha-conectado2! (list-ref lvieja indice) #t)
                                            (set-ficha-conectado2! (last lnueva) #t)
                                            (set! lnueva (append lnueva (list-ref lvieja indice)))
                                            (set! lvieja (remove (list-ref lvieja indice) lvieja))
                                            (resolver_domino_aux lvieja lnueva 0)]
                                           ;este else solo llama recursivamente la funcion de resolver con el indice agrandado en el caso que no coincida nada
                                           [else (resolver_domino_aux lvieja lnueva (+ indice 1))])]))
    


(define (verificador lvieja lnueva indice)
  ;esta condicional lo que hace es que verifica que cumpla con ser promesing
  ;si vuelve a entrar la misma ficha ya termina ya que significa que ya recorrieron toda la lista y no logro encontrar ninguno que cumpla con lo establecido
  (cond
    ;se verifica si es promesing si no retorna la lista terminada hasta ese momento
    ;en el caso que sea promesing la ficha se borra de lnueva y se mete en lvieja de ultima y se restean los puertos
    ;luego se llama de nuevo resolver para que busque mas soluciones con el indice en 0.
    [(eqv? (ficha-promesing) (last lnueva) #t)
     (set-ficha-promesing! (last lnueva) (#f))
     (set-ficha-conectado1! (last lnueva) (#f))
     (set-ficha-conectado2! (last lnueva) (#f))
     (set! lvieja (append lvieja (last lnueva)))
     (remove (last lnueva) lnueva)
     (resolver_domino_aux lvieja lnueva 0)]
    [else (set-tablero-fichas! t lnueva)]))