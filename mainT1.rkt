#lang racket (require racket/list)
(require data/spmatrix)
(require racket/system)
(require "treeNario.rkt")
(require "eval.rkt")
(require "minMax_Poda.rkt")

(define matrizGame (build-matrix 5 6 (lambda (i j) 0)))

;############### Generado automatico

(define (matriz-auto)
  (localize-pos (gna 0 5) (gna 0 6) 1) ;#1
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1)
  (localize-pos (gna 0 5) (gna 0 6) 1) ;#12 esto es el numero de fichas del jugador "n", son 12 fichas por cada jugador
  
  
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  (localize-pos (gna 0 5) (gna 0 6) 2)
  )

;pregunta si la posición es cero, si pregunta si es valida la posición si lo es cambia la posición por el numero sino, continua seleccionando aleatoriamente hasta que sea posible
(define (localize-pos row col num)
  (if (equal? (matrix-ref matrizGame row col) 0)
      (if (validate-pos matrizGame row col num)
          (matrix-set! matrizGame row col num)
          (localize-pos (gna 0 5) (gna 0 6) num)
          )
      (localize-pos (gna 0 5) (gna 0 6) num)
  )
)

;####### Generar numeros aleatorios entre minimo y el maximo
;sin entradas y sale 1 numero del minimo al maximo
(define (gna min max) ;generar numero aleatorio gna
  (inexact->exact (+ min (floor (* max (random)))))
  )

(define (return-matrix)
  matrizGame)

;#####################################################
;#################  BOARD  ###########################

;Función que crea el tablero inicial, utilizando la matriz global
;Sin entradas y la salida es un string del tablero
(define (draw-board)
  (display
"          TABLERO
      ****************
      ****1 2 3 4 5 6*
      ****************
 ")
  (display "     *1* ")
  (create-row (matrix-ref matrizGame 0 0)) (create-row (matrix-ref matrizGame 0 1)) (create-row (matrix-ref matrizGame 0 2))
  (create-row (matrix-ref matrizGame 0 3)) (create-row (matrix-ref matrizGame 0 4)) (create-row (matrix-ref matrizGame 0 5))
  (display "\n      *2* ") (create-row (matrix-ref matrizGame 1 0)) (create-row (matrix-ref matrizGame 1 1)) (create-row (matrix-ref matrizGame 1 2))
  (create-row (matrix-ref matrizGame 1 3)) (create-row (matrix-ref matrizGame 1 4)) (create-row (matrix-ref matrizGame 1 5)) (display "\n      *3* ")
  (create-row (matrix-ref matrizGame 2 0)) (create-row (matrix-ref matrizGame 2 1)) (create-row (matrix-ref matrizGame 2 2))
  (create-row (matrix-ref matrizGame 2 3)) (create-row (matrix-ref matrizGame 2 4)) (create-row (matrix-ref matrizGame 2 5)) (display "\n      *4* ")
  (create-row (matrix-ref matrizGame 3 0)) (create-row (matrix-ref matrizGame 3 1)) (create-row (matrix-ref matrizGame 3 2))
  (create-row (matrix-ref matrizGame 3 3)) (create-row (matrix-ref matrizGame 3 4)) (create-row (matrix-ref matrizGame 3 5)) (display "\n      *5* ")
  (create-row (matrix-ref matrizGame 4 0)) (create-row (matrix-ref matrizGame 4 1)) (create-row (matrix-ref matrizGame 4 2))
  (create-row (matrix-ref matrizGame 4 3)) (create-row (matrix-ref matrizGame 4 4)) (create-row (matrix-ref matrizGame 4 5)) (display "\n\n ")
   )

;imprime y crea una posición del tablero
(define (create-row num)
  (display (create-pos num))
  )

;Función encargada de devolver un string dependiendo de lo que entre si es cero un cuadro vacio sino un numero
(define (create-pos num)
  (if (equal? num 0) (integer->char #x25EF)
      
      (if (equal? num 1)
          (integer->char #x2460)
          (integer->char #x2461)
          );(string-append " "(number->string num))
      ))