#lang racket

;Aca incia el codigo

(struct nodo(id nombre valor hermano hijo)#:transparent #:mutable);estructura del nodo de los arboles

(struct arbol( raiz)#:transparent #:mutable);estructura del arbol que solo contiene un nodo

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
(define n11(nodo 11 "nodo11" 15 null null))
(define arbol1(arbol n10))
;fin de la creacion del arbol


;funcion que imprime el arbol con un recorrido a profundidad de izquierda a derecha
;entradas:arbol a imprimir
;salidas: lista de los nodos impresos a profundidad.
;ejecucion:>(list_all_nodes arbol)
(define (list_all_nodes arbolito)
  (if(arbol? arbolito);verificacion de la estructura
     (remove-duplicates(list_all_nodes_aux (arbol-raiz arbolito)));llamda a la funcion auxiliar de imprimir todo el arbol
     "error estructura de entrada no valida");mensaje de error en caso de no ser un arbol
  )

;Funcion auxiliar para listar todos los nodos
(define(list_all_nodes_aux raiz)

   ;si esta vacio el nodo que llega
  (if (empty? raiz)
      '();devuelvo esto al estar vacio
      ;si no esta vacio hago todo esto
      (append (list (nodo-nombre raiz));aca meto el primero de esa rama
              (list_all_nodes_aux (nodo-hijo raiz));aca llamo con el hijo
              (list_all_nodes_aux (nodo-hermano raiz)) )));aca con el hermano


;funcion para listar algunos nodos del arbol (ramas)
;entradas:arbol y nodo inicial
;salidas:lista con los nodos desde el epecificado
;ejecucion:>(list_some_nodes arbol nodoinicio)
(define (list_some_nodes arbolito nodoinicio)
  (if(and (arbol? arbolito) (nodo? nodoinicio) );verificacion de la estructura
     (list_some_nodes_aux (first (find_node arbol1 nodoinicio)) 0 );llamda a la funcion auxiliar de imprimir todo el arbol
     "error estructura de entrada no valida");mensaje de error en caso de no ser un arbol
  )
;funcion auxiliar para listar algunos nodos
(define (list_some_nodes_aux nodoinicio etapa )
  ;si esta vacio el nodo que llega
  (if(= etapa 0) (if (empty? nodoinicio)
      '();devuelvo esto al estar vacio
      ;si no esta vacio hago todo esto
      (append (list (nodo-nombre nodoinicio));aca meto el primero de esa rama
              (list_some_nodes_aux (nodo-hijo nodoinicio) (add1 etapa));aca llamo con el hijo
              
     )) (if (empty? nodoinicio)
      '();devuelvo esto al estar vacio
      ;si no esta vacio hago todo esto
      (append (list (nodo-nombre nodoinicio));aca meto el primero de esa rama
              (list_some_nodes_aux (nodo-hijo nodoinicio) (add1 etapa) );aca llamo con el hijo
              (list_some_nodes_aux (nodo-hermano nodoinicio) (add1 etapa)) )) ))



;funcion para buscar un nodo
;entradas: el arbol y el nodo a buscar
;salidas: lista con el nodo si existe si no empty
;ejecucion:>(find_node arbol nodoabuscar)
(define (find_node arbolito nodo)
  (if(and (arbol? arbolito) (nodo? nodo)) (find_node_aux(arbol-raiz arbolito) nodo);verificacion de valores de entrada
     "valores de entrada incorectos"))

 


;funcion auxiliar de find node
(define(find_node_aux raiz nodobuscado)

;verifica que sea el nodo buscado
 (cond [(equal? raiz nodobuscado) (list raiz )]
  ;si esta vacio
  [(empty? raiz)
      empty];devuelvo esto al estar vacio
      ;si no esta vacio hago todo esto
      [(append(find_node_aux (nodo-hijo raiz) nodobuscado);aca llamo con el hijo
              (find_node_aux (nodo-hermano raiz) nodobuscado) )]);aca con el hermano

  )

;funcion para insertar un nodo
;entradas:arbol nodo padre y el nodo que se vaa insertar
;salidas el arbol con el nuevo nodo insertado en su posicion
;ejecucion:>(insert_node arbol nodopadre nodohijo)
(define(insert_node arbolito padre nodohijo)
  (if (and(arbol? arbolito)(nodo? padre)(nodo? nodohijo));verificacion de entradas
      (insert_node_aux(first(find_node arbolito padre)) nodohijo )
      "error en las variables de entrada")

  )

;funcion auxiliar para insertar un hijo
(define(insert_node_aux nodopadre nodohijo )
  ;en caso de no tener hijos en padre se inserta
(if (null? (nodo-hijo nodopadre)) (set-nodo-hijo! nodopadre nodohijo)
    ;en caso de si tener se llama a una funcion insertar hermano
      ( insertar_hermano(nodo-hijo nodopadre) nodohijo))

  )

;funcion para insertar un hermano
(define(insertar_hermano hermanomayor hermanomenor)
  ;en caso de no tener hermanos se inserta
  (if (null? (nodo-hermano hermanomayor) ) (set-nodo-hermano! hermanomayor hermanomenor)
      ;en caso de si tener hermano se recorren hasta el ultimo recursivamente
       (insertar_hermano (nodo-hermano hermanomayor) hermanomenor))
  )





;funcion para eliminar un nodo
;entradas: el arbol y el nodo a eliminar
;salidas: el arbol con el nodo y su rama eliminado
;ejecucion:>(delete_node arbol nodoaeliminar)
(define (delete_node arbolito nodoeliminar)
;verificacion de valores de entrada
  (if(and (arbol? arbolito) (nodo? nodoeliminar))
     ;condicion para eliminar el hijo o el hermano del ancestro
     (if(= 0 (second(delete_node_aux(arbol-raiz arbolito) nodoeliminar (arbol-raiz arbolito) 0 )))
        ;si la segunda posicion de la lista es 0 es porque es un hijo
        (set-nodo-hijo! (first(delete_node_aux(arbol-raiz arbolito) nodoeliminar (arbol-raiz arbolito) 0 )) empty)
        ;si no es 0 es por que es un hermano
        (set-nodo-hermano! (first(delete_node_aux(arbol-raiz arbolito) nodoeliminar (arbol-raiz arbolito) 0 )) empty))
  
     "valores de entrada incorectos")

  )
;funcion auxiliar para eliminar nodo
;entradas la raiz, el nodod a eliminar, el ultimo en el que se estuvo y el estatus 0 si es hijo o 1 si es hermano
(define (delete_node_aux raiz nodoeliminar ultimovisitado estatus)
  (cond [(empty? raiz);si esta vacio
      empty ];devuelvo esto al estar vacio
  ;si no esta vacio hago todo esto
  [ (equal? raiz nodoeliminar) (list ultimovisitado estatus )];verifica que sea el nodo buscado y devuelve el ultimo visitado + el estatus
      
      [(append(delete_node_aux (nodo-hijo raiz) nodoeliminar raiz 0 );aca llamo con el hijo y cambio al ultimo visitado por el actual y el estatus por 0
              (delete_node_aux (nodo-hermano raiz) nodoeliminar raiz 1 ) )]);aca con el hermano cambio al ultimo visitado por el actual y el estatus por 1


  )



;funcion para sacar el ancestro de un nodo
;entradas: el arbol y el nodo a consultar
;salidas: una lista con el nodo ancestro
;ejecucion:>(ancestor arbol nodoaconsultar)
(define (ancestor arbolito nodo)
  (if(and (arbol? arbolito) (nodo? nodo)) (ancestor_aux(arbol-raiz arbolito) nodo empty) ;verificacion de valores de entrada
     "valores de entrada incorectos"))

 


;funcion auxiliar de ancestro
;cambios: se a;ade un ultimo padre para llevar el nodo padre
(define(ancestor_aux raiz nodobuscado ultimopadre)

 (cond [(empty? raiz);si esta vacio
      empty ];devuelvo esto al estar vacio
   ;si no esta vacio hago todo esto
  [ (equal? raiz nodobuscado) (list ultimopadre )];verifica que sea el nodo buscado y devuelve el ultimo padre
     
      [(append(ancestor_aux (nodo-hijo raiz) nodobuscado raiz );aca llamo con el hijo y cambio al ultimo padre por el actual
              (ancestor_aux (nodo-hermano raiz) nodobuscado ultimopadre ) )]);aca con el hermano ys e mantiene el ultimo padre

  )
  
  
;funcion para buscar el hermano derecho de un nodo
;entradas: arbol y el nodo a buscar su hermano
;salidas: el nodo correspondiente al hermano derecho del nodo buscado
;ejecucion:>(find_right_sibling arbol nodoaconsultar)
(define(find_right_sibling arbolito nodobuscado )
  (if(and (arbol? arbolito) (nodo? nodobuscado));verificacion de valores de entrada
     (nodo-hermano (first(find_node arbolito nodobuscado))) ;llamada a la busqueda del nodo y se devuelve su hermano derecho
     "valores de entrada incorectos")
  )

;funcion para buscar el hermano izquierdo de un nodo
;entradas: arbol y el nodo a buscar su hermano
;salidas: el nodo correspondiente al hermano izquierdo del nodo buscado
;ejecucion:>(find_left_sibling arbol nodoaconsultar)
(define(find_left_sibling arbolito nodobuscado)
  (if(and (arbol? arbolito) (nodo? nodobuscado)) (find_left_sibling_aux(arbol-raiz arbolito) nodobuscado empty) ;verificacion de valores de entrada
     "valores de entrada incorectos"))

  
;funcion auxiliar para encontrar el hermano izquierdo de un nodo
;cambios: ultimo visitado donde se guarda el hermano izquierdo del nodo
(define (find_left_sibling_aux raiz nodobuscar ultimovisitado )
  (cond [(empty? raiz);si esta vacio
      empty ];devuelvo esto al estar vacio
   ;si no esta vacio hago todo esto
  [ (equal? raiz nodobuscar) (list ultimovisitado)];verifica que sea el nodo buscado y devuelve el ultimo visitado
     
      [(append(find_left_sibling_aux (nodo-hijo raiz) nodobuscar empty);aca llamo con el hijo y cambio al ultimo visitado por vacio
              (find_left_sibling_aux (nodo-hermano raiz) nodobuscar raiz ) )]);aca con el hermano cambio al ultimo visitado por el actual


  )

  


