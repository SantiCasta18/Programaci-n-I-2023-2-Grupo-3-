#lang racket

; Utilizamos la libreria "2htdp" que es adecuada para manejar eventos 

(require 2htdp/universe)       
(require 2htdp/image)

; Este es nuestro juego de Pacman

; Constantes

(define (not-wall? cv)
  (not (string=? "wall" cv)))

(define SCREEN-W 400)
(define SCREEN-H 380)
(define TOCK 0.2)

(define E "empty") 
(define D "dot")   
(define W "wall") 
(define C "cherry")

; Tablero inicial (Vectores que definen la ubicación de los objetos)

(define INIT-BOARD
  (vector (vector W W W W W W W W W W W W W)
          (vector W C D D D D D D D D D C W)
          (vector W D W D W W W W W D W D W)
          (vector W D D D W D W D W D D D W)
          (vector W D W D D D D D D D W D W)
          (vector W D C W W D W D W W C D W)
          (vector E D D D D D E D D D D D E)
          (vector W D C W D W D W D W C D W)
          (vector W D W D D D W D D D W D W)
          (vector W D C D W D W D W D C D W)
          (vector W D W D D D W D D D W D W)
          (vector W D W D W D D D W D W D W)
          (vector W D D C D D D D D C D D W)
          (vector W W W W W W W W W W W W W)))

; Velocidades de pacman y ghost

(define PM-SPEED 1)
(define GT-SPEED 0.01)

; Tablero pequeño

(define SMALL-BOARD
  (vector (vector E E E)
          (vector E E E)))

; Ahora definimos el tamaño de las celdas

(define CELL-SIZE 20)

; Medidas tablero inicial

(define BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref INIT-BOARD 0))))
(define BOARD-HEIGHT (* CELL-SIZE (vector-length INIT-BOARD)))

; Medidas tablero pequeño

(define SMALL-BOARD-WIDTH  (* CELL-SIZE (vector-length (vector-ref SMALL-BOARD 0))))
(define SMALL-BOARD-HEIGHT (* CELL-SIZE (vector-length SMALL-BOARD)))

(define SCORE-HEIGHT    30)
(define SCORE-TEXT-SIZE 20)

; Animaciones de pacman
; overlay/align es una función para juntar imagenes

(define PM (overlay/align "right" "middle" 
                          (rotate -45 (right-triangle 14 14 "solid" "black")) 
                          (circle 10 "solid" "yellow"))) ; Esta es la forma de Pacman base

(define R-PM PM)
(define U-PM (rotate 90 R-PM))
(define L-PM (rotate 180 R-PM))
(define D-PM (rotate 270 R-PM))
(define C-PM (circle 10 "solid" "yellow")) ; En esta cierra su boca

; Creacion de la cereza
; Above es una función que une objetos en linea vertical partiendo de su centro

(define CHERRY (above (rotate -30 (rectangle 1 5 "solid" "green")) 
                      (circle 5 "solid" "red")))

(define GT (circle 10 "solid" "violet")) ; Aqui se define al fantasma (Ghost) el cual sera solo uno

(define MTC  (rectangle CELL-SIZE CELL-SIZE "solid" "black")) ; Celda vacia
(define DTC  (overlay (circle 3  "solid" "yellow") MTC))       ; Punto en celda
(define WALL (rectangle CELL-SIZE CELL-SIZE "solid" "white"))  ; Bloque (pared)

; empty-scene crea una escena vacía, o sea un rectángulo blanco con un contorno negro

(define MTB 
  (empty-scene BOARD-WIDTH
               (+ BOARD-HEIGHT (* 2 SCORE-HEIGHT))
               "black"))

(define SMALL-MTB
  (empty-scene SMALL-BOARD-WIDTH
               (+ SMALL-BOARD-HEIGHT SCORE-HEIGHT)))

; La puntuación parte desde el comienzo del juego
; Los puntos devorados por Pacman equivalenn al aumento de la puntuación

; La celda puede definirse como:
; - "vacio"
; - "punto"
; - "pared"
; - "cereza"
; Es el contenido de la celda en la tabla

; Direction de Pacman is one of:
; - "U" (hacia arriba)
; - "D" (hacia la izquierda)
; - "L" (hacia abajo)
; - "F" (hacia la derecha)

(define INIT-SCORE  0)

(define-struct pos (x y))

; En este caso pos es (make-pos Natural Natural)
; Define una posicion en el tablero
; El uso de local es útil para evitar colisiones de nombres de variables y
; para controlar el alcance de las variables

(define MAP-LIST
  (local [(define i 0)
          (define j 0)
          (define lst empty)
          (define bd INIT-BOARD)]
    (begin 
      (let loopi()
        (when (< i 14)
          (begin
            (set! bd (vector-ref INIT-BOARD i))
            (let loopj()
              (when (< j 13)
                (begin
                  (if (not-wall? (vector-ref bd j))
                      (set! lst (append lst (list (make-pos j i)) ))
                      void)
                  (set! j (add1 j))
                  (loopj))))
            (set! i (add1 i))
            (set! j 0)
            (loopi))))
      lst)))

(define-struct sprite (x y dir))

; La función Sprite significa (make-sprite Natural Natural Direction)
; La posición en coordenadas del tablero, y la dirección de un sprite
; Los sprites sirven para representar objetos individuales

(define INIT-PM (make-sprite 6 6 "U"))

; El tablero (TAB) es (vector-of (vector-of VallorCelda))
; Es el Tablero principal del juego

(define RENDER-TEST-BOARD (vector (vector W E)
                                  (vector D E)))

(define-struct ghost (x y dir))

; Ghost sera el fantasma, por lo cual se definira como (make-ghost Natural Natural Direction)
; Ghost cazara continuamente a Pacman

(define INIT-GT (make-ghost 6 1 "D"))

(define-struct gs (pm board board-image score time gt))

; EstadodelJuego es (make-gs Sprite Board Image punt Natural Ghost)
; Son todas las partes del juego Pac-man; Pac-man, (el actual)
; El tablero, (el actual) tab-image, y la puntuación actual
; el tiempo progresara en segundos desde que el juego emoiece
; ght es el fantasma (Ghost)

(define MTB-GS (make-gs INIT-PM INIT-BOARD MTB INIT-SCORE 0 INIT-GT))


;-------------------------------------------------------------------------------

; Valores para testear

; Sprite:

;A continuación se definen los respectivos tamaños del sprite de cada apariencia de PAC
; y su dirección

(define R-SPRITE (make-sprite 1 1 "R"))
(define L-SPRITE (make-sprite 1 1 "L"))
(define U-SPRITE (make-sprite 1 1 "U"))
(define D-SPRITE (make-sprite 1 1 "D"))

(define R-GT (make-ghost 2 2 "R")) ; El sprite de ghost

; Tablero:

(define EE-BOARD (vector (vector W W W W)
                         (vector W E E W)
                         (vector W W W W)))

(define ED-BOARD (vector (vector W W W W)
                         (vector W E D W)
                         (vector W W W W)))

(define DD-BOARD (vector (vector W W W W)
                         (vector W D D W)
                         (vector W W W W)))

; EstadoJuego:
; EMTE-GS definido previamente mas arriba

(define END-GS (make-gs R-SPRITE EE-BOARD SMALL-MTB 0 0 R-GT))


;-------------------------------------------------------------------------------

; Funciones:

; -> EstadoJuego
; La función que hace que el juego corra

(define (main)
  (local [(define INIT-GS (make-gs INIT-PM
                                   INIT-BOARD
                                   (render-board INIT-BOARD)
                                   INIT-SCORE
                                   0
                                   INIT-GT))]
    (big-bang INIT-GS
              (on-tick tick TOCK)
              (to-draw render)
              (on-key key-handler)
              (stop-when game-over? last-scene))))

; display de la ultima escena (son los textos que se mostraran en caso  de derrota o de victoria)

(define (final-score ttime tscore)
  (- tscore  ttime))

(define (last-scene gs)
  (cond [(goal? (ghost->pos (gs-gt gs)) (sprite->pos (gs-pm gs)))
         (overlay/align "middle" "middle" 
                        (above (text "GAME OVER." 20 "red")
                               (text "Gracias por jugar!" 20 "white")
                               (text "Tiempo total:" 20 "aqua")
                               (text (number->string (ceiling (* TOCK (gs-time gs)))) 20 "aqua")
                               (text "Puntuación:" 20 "yellow")
                               (text (number->string (gs-score gs)) 20 "yellow")
                               (text "Puntuación final:" 20 "red")
                               (text (number->string (final-score (ceiling (* TOCK (gs-time gs))) (gs-score gs))) 20 "yellow"))
                        (empty-scene SCREEN-W SCREEN-H "black"))]        
        [else (overlay/align "middle" "middle" 
                             (above (text "PACMAN LO HA LOGRADO." 20 "white")
                                    (text "Gracias por jugar!" 20 "white")
                                    (text "Tiempo total:" 20 "aqua")
                                    (text (number->string (ceiling (* TOCK (gs-time gs)))) 20 "aqua")
                                    (text "Puntuación:" 20 "yellow")
                                    (text (number->string (gs-score gs)) 20 "yellow")
                                    (text "Puntuación final:" 20 "red")
                                    (text (number->string (final-score (ceiling (* TOCK (gs-time gs))) (gs-score gs))) 20 "yellow"))
                             (empty-scene SCREEN-W SCREEN-H "black"))]))

; Pos Pos (listof Pos) -> (listof Pos) o arroja falso
; busca el camino más corto entre inicio y meta, falso (f) si no existe camino en bd (tablero)

(define (A-star start goal bd)
  (local [; ListaDeTrabajo es (make-ldt Pos Pos Numero Numero Numero)
          ; La ListaDeTrabajo es usanda al interior de "todo"
          ; pos is the position , parent is the parent position, 
          ; f es f-punt, g es g-punt, h es h-punt
          (define-struct wle (pos parent f g h))
          
          ; (listof ListaDeTrabajo) -> (listof Pos)
          ; Devuelve la ruta desde las entradas de ListaDeTrabajo
          ; reverse se utiliza para invertir el orden de los elementos en una lista
          ; λ (lambda) se utiliza para definir funciones anónimas, Estas funciones
          ; no tienen un nombre asociado y se utilizan directamente en expresiones
          (define (get-path rsf)
            (if (false? rsf)
                false
                (reverse (map (λ (p) (wle-pos p)) rsf))))
          
          ; Pos Pos -> Numero
          ; calcular la puntuación heurística
          (define (h-score p1 p2)
            (+ (abs (- (pos-x p1) (pos-x p2)))
               (abs (- (pos-y p1) (pos-y p2)))))
          
          ; Puntuación heuristica inicial
          (define h0 (h-score start goal))
          
          ; Pos (listof Pos) -> Booleano
          ; devuelve true si lop contiene pos, false en caso contrario
          ; ¿no se puede utilizar el built-in de función incorporado?
          ; La función rest se utiliza para obtener todos los elementos de una lista excepto el primero
          ; first se utiliza para obtener el primer elemento de una lista.
          (define (pos-member? pos lop)
            (cond [(empty? lop) false]
                  [else
                   (if(goal? pos (first lop))
                      true
                      (pos-member? pos (rest lop)))]))
          
          ; Pos (listof Pos) -> (listof Pos)
          ; Producen las posiciones vecinas de p que están en bd
          (define (neighbour p bd)
            (local [(define x (pos-x p))
                    (define y (pos-y p))
                    ;; determina si n esta en el tablero bd
                    (define (in? n) 
                      (local [(define (in-board? n bd)
                                (cond [(empty? bd) false]
                                      [else
                                       (if (goal? n (first bd))
                                           true
                                           (in-board? n (rest bd)))]))]
                        (in-board? n bd)))]
              (filter in? 
                      (list 
                       (make-pos (add1 x) y)
                       (make-pos (sub1 x) y)
                       (make-pos x (add1 y))
                       (make-pos x (sub1 y))))))
          
          (define (remove-first-n lst n)
            (cond [(or (zero? n) (empty? lst)) lst]
                  [else
                   (remove-first-n (rest lst) (sub1 n))]))
          
          
          ; Pos Pos Numero Numero Numero (listof ldt) (listof Pos) (listof ldt) -> (listof ldt) or false
          (define (fn-for-pos p parent f g h todo visited rsf)
            ; no es posible utilizar equal? para comparacion, usamos goal? en cambio
            ; map es utilizada para aplicar una función a cada elemento de una lista
            ; (o de otra estructura de datos) y devolver una nueva lista que contiene los resultados de aplicar la función a cada elemento
            (if (goal? p goal)
                (cons (make-wle goal parent f g h) rsf)
                (if (pos-member? p visited)
                    (fn-for-lop todo visited rsf)                              
                    (fn-for-lop (append (map (λ (n) (local [; hn es la puntuacion heuristica para n (el termino "heuristica"
                                                            ; lo utilizamos para denominar a la IA implementada dentro del codigo)
                                                            (define hn (h-score n goal))
                                                            ; g-punt para n
                                                            (define gn (add1 g))
                                                            ; f = g+ h
                                                            (define fn (+ gn hn))]
                                                      (make-wle n p fn gn hn)))
                                             (begin ;(display (vecino p bd))
                                               (neighbour p bd)))
                                        todo) 
                                (cons p visited)
                                (if (> (length rsf) g)
                                    (cons (make-wle p parent f g h)
                                          (remove-first-n rsf (- (length rsf) g)))
                                    (cons (make-wle p parent f g h) rsf))))))
          ; (listof ldt) (listof Pos) (listof ldt) -> false o (listof ldt)
          (define (fn-for-lop todo visited rsf)
            (cond [(empty? todo) false]
                  [else
                   (local [; (listof ldt) -> (listof ldt)
                           ; ordena la ListaDeTrabajo con el crecimiento de f-punt, si son iguales se ordenan con h
                           (define (sort-f lst)
                             (sort 
                              (sort lst
                                    (λ (x y) (<= (wle-f x) (wle-f y))))
                              (λ (x y) (< (wle-h x) (wle-h y)))))
                           (define (sort l comp)
                             (local [(define (sort l)
                                       (cond [(empty? l) empty]
                                             [else
                                              (insert (first l)
                                                      (sort (rest l)))]))
                                     (define (insert i lst)
                                       (cond [(empty? lst) (list i)]
                                             [else
                                              (if (comp i (first lst))
                                                  (cons i lst)
                                                  (cons (first lst) (insert i (rest lst))))]))]
                               (sort l)))
                           ; La ListaDeTrabajo ordenada
                           (define sorted-todo (sort-f todo))
                           ; selleciona la entrada con el f-punt más bajo
                           (define current (first sorted-todo))]
                     (fn-for-pos (wle-pos current)
                                 (wle-parent current)
                                 (wle-f current)
                                 (wle-g current)
                                 (wle-h current)
                                 (rest sorted-todo)
                                 visited
                                 rsf))]))]
    (get-path (fn-for-pos start false h0 0 h0 empty empty empty))))

;-------------------------------------------------------------------------------

; on-tick manipulador:


; EstadoJuego: -> EstadoJuego:
; Avances del juego

(define (tick gs)
  (local [(define pm          (gs-pm gs))
          (define board       (gs-board gs))
          (define board-image (gs-board-image gs))
          (define score       (gs-score gs))
          (define game-time   (gs-time gs))
          (define ghost        (gs-gt gs))
          (define new-pm          (tick-pm pm board game-time))
          (define new-board       (tick-board board new-pm))
          (define new-board-image (tick-board-image board board-image new-pm))
          (define new-score       (tick-score new-pm board score))
          (define new-time        (tick-time score new-pm board game-time))
          (define new-ghost       (tick-ghost ghost new-pm new-board game-time))]
    
    (make-gs new-pm
             new-board
             new-board-image
             new-score
             new-time
             new-ghost)))

; Natural Sprite tab Natural -> Natural
; actualizar el contador de tiempo

(define (tick-time score new-pm last-board t)
  (local [(define pos (board-ref last-board (sprite-x new-pm) (sprite-y new-pm)))]
    (cond [(=  score 0.0) 0.0]
          [else
           (ceiling (add1 t))])))

; Ghost Sprite Board Time -> Ghost
; actualiza la posición y dirección del fantasma para perseguir a Pacman

(define (tick-ghost ghost pm bd t)
  (if (zero? (modulo t 6))
      ghost
      (if (integer? t)
          (if (goal? (ghost->pos ghost) (sprite->pos pm))
              ghost
              (local [(define p 
                        (local [(define search-result (A-star (ghost->pos ghost) (sprite->pos pm) MAP-LIST))]
                          (if (false? search-result)
                              (ghost->pos ghost)
                              (second search-result))))
                      (define x (pos-x p))
                      (define y (pos-y p))]
                (make-ghost x
                            y
                            (ghost-dir ghost))))
          ghost)))

; Sprite tab Time -> Sprite
; actualiza la posición de Pac-man en función de su dirección

(define (tick-pm pm bd t)
  (if (and (string=? "L" (sprite-dir pm)) (= (sprite-x pm) 0))
      (make-sprite 12 (sprite-y pm) "L")
      (if (and (string=? "R" (sprite-dir pm)) (= (sprite-x pm) 12))
          (make-sprite 0 (sprite-y pm) "R")
          (make-sprite (checked-move-x (sprite-x pm) (sprite-y pm) (sprite-dir pm) bd)
                       (checked-move-y (sprite-x pm) (sprite-y pm) (sprite-dir pm) bd)
                       (sprite-dir pm)))))


; Natural Natural Direction Board -> Natural
; mueve x en direccion dir, a menos que se encuentre con una pared en bd o dir no está en la dirección x
; SUPUESTO: se supone que x, y está al menos a una celda de cualquier borde de bd

(define (checked-move-x x y dir bd)
  (cond [(string=? "L" dir) (restrict-move (sub1 x) y x (sub1 x) bd)]
        [(string=? "R" dir) (restrict-move (add1 x) y x (add1 x) bd)]
        [else x]))

; Natural Natural Direction Board -> Natural
; mueve y en direction dir, a menos que se encuentre con una pared en bd o dir no está en la dirección y
; SUPUESTO: se supone que x, y está al menos a una celda de cualquier borde de bd


(define (checked-move-y x y dir bd)
  (cond [(string=? "U" dir) (restrict-move x (sub1 y) y (sub1 y) bd)]
        [(string=? "D" dir) (restrict-move x (add1 y) y (add1 y) bd)]
        [else y]))

; Natural Natural Natural Natural tab -> Natural
; produce new-coord si bd no contiene un muro en check-x, check-y; de lo contrario produce old-coord


(define (restrict-move check-x check-y old-coord new-coord bd)
  (if (string=? (board-ref bd check-x check-y) "wall")
      old-coord
      new-coord))

; Tab Sprite -> Tab
; si la celda en la posición de Pacman no está vacía, crea un nuevo tablero en el que lo esté


(define (tick-board bd pm)
  (if (string=? "empty" (board-ref bd (sprite-x pm) (sprite-y pm)))
      bd
      (new-board-w-empty-at (sprite-x pm) (sprite-y pm) bd)))

; Numero Numero Tab -> Tab
; produce un nuevo tablero con los mismos valores de celda que bd, excepto que tiene "empty" (vacio) en x, y


(define (new-board-w-empty-at x0 y0 bd)
  (map-board (lambda (x y cv) ; Natural Natural CellValue -> CellValue
               (if (and (= x x0) (= y y0))
                   "empty"
                   cv))
             bd))

; Tab Image Sprite -> Image
; actualiza la imagen del tablero con una celda vacía en x, y si Pac-man está en una celda con un punto o una cereza


(define (tick-board-image bd board-image pm)
  (local [(define x (sprite-x pm))
          (define y (sprite-y pm))]
    (if (or (string=? (board-ref bd x y) "dot")
            (string=? (board-ref bd x y) "cherry")) ;cherry se agrega
        (place-cell-image MTC x y board-image)
        board-image)))

; Sprite Board punt Time -> punt
; Incrementa por 1 si Pacman esta en una celda que contiene un punto
; Incrementa por 5 si Pacman esta en una celda que contiene una  cereza

(define (tick-score new-pm last-board score)
  (local [(define pos (board-ref last-board (sprite-x new-pm) (sprite-y new-pm)))]
    (cond [(string=? "dot" pos)
           (+ 5 score)]
          [(string=? "cherry" pos)
           (+ 10 score)]
          [else
           score])))

; Sprite Sprite Tab -> Sprite
; Actualiza la posición del sprite de Ghost


; Sprite -> Pos
; Toma la x y la y de un sprite

(define (sprite->pos sp)
  (make-pos (sprite-x sp)
            (sprite-y sp)))

; Ghost -> Pos
; Toma la posición de Ghost

(define (ghost->pos gt)
  (make-pos (ghost-x gt)
            (ghost-y gt)))

; Pos Pos -> Booleano
; Determina si dos pos son iguales

(define (goal? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))))

(define (upward    p) (make-pos       (pos-x p)  (sub1 (pos-y p))))
(define (downward  p) (make-pos       (pos-x p)  (add1 (pos-y p))))
(define (leftward  p) (make-pos (sub1 (pos-x p))       (pos-y p)))
(define (rightward p) (make-pos (add1 (pos-x p))       (pos-y p)))

; Tab Pos -> (listof Pos)

(define (next-moves m p)
  (filter (λ (p1) 
            (not (solid? m p1)))
          (filter (λ (p2) 
                    (valid? m p2))
                  (list (leftward p)      
                        (rightward p)  
                        (upward p)        ;permitiría alinear
                        (downward p)))))
(define (valid? m p)
  (local [(define s (vector-length m))]
    (and (<= 0 (pos-x p) (add1 s))
         (<= 0 (pos-y p) (add1 s)))))
(define (solid? m p)
  (is-solid? (board-ref m (pos-x p) (pos-y p))))
(define (is-solid? v)
  (cond [(string=? v "empty") false]
        [(string=? v "wall") true]        
        [(string=? v "dot") false]
        [(string=? v "cherry") false]))

;-------------------------------------------------------------------------------

; EstadoJuego KeyEvent -> EstadoJuego
; Actualiza la dirección de Pacman en base a "key"


;; GameState KeyEvent -> GameState
;; updates pac-man's direction based on key

(define (key-handler gs key)
  (make-gs (new-dir-pm (gs-pm gs) key)
           (gs-board gs)
           (gs-board-image gs)
           (gs-score gs)
           (gs-time gs)
           #;(new-dir-ghost (gs-gt gs) key)
           (gs-gt gs)))

; Sprite KeyEvent -> Sprite
; Produce las caras de Pacman en una nueva dirección en base a "key"

(define (new-dir-pm pm key)
  (cond [(key=? "up"    key) (make-sprite (sprite-x pm) (sprite-y pm) "U")]
        [(key=? "down"  key) (make-sprite (sprite-x pm) (sprite-y pm) "D")]      
        [(key=? "left"  key) (make-sprite (sprite-x pm) (sprite-y pm) "L")]
        [(key=? "right" key) (make-sprite (sprite-x pm) (sprite-y pm) "R")]
        [else pm]))


#;
(define (new-dir-ghost ghost key)
  (cond [(key=? "w" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "U")]
        [(key=? "s" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "D")]      
        [(key=? "a" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "L")]
        [(key=? "d" key) (make-ghost (ghost-x ghost) (ghost-y ghost) "R")]
        [else ghost]))


;-------------------------------------------------------------------------------

; stop-when - control:


; EstadoJuego -> Booleano
; Determina si Pacman ha comido todos los puntos, incluyendo cerezas

(define (game-over? gs)
  (or (zero? (count-dots (gs-board gs)))
      (goal? (ghost->pos (gs-gt gs)) (sprite->pos (gs-pm gs)))))


; Board -> Natural
; cuenta el número de puntos en el tablero

(define (count-dots bd)
  (foldr-board (lambda (x y cv b) ;Natural Natural CellValue Natural -> Natural
                 (if (string=? "dot" cv)
                     (add1 b)
                     b))
               0
               bd))


;-------------------------------------------------------------------------------


; to-draw - control:


; EstadoJuego -> Image
; Dibuja el juego


(define (render gs)
  (overlay/align "middle"
                 "center"
                 (above/align "middle"
                              (text "JUEGO DE PACMAN" 20 "yellow")
                              (render-time (ceiling (* 0.25 (gs-time gs)))
                                           (render-ghost (gs-gt gs)
                                                         (render-pm (gs-pm gs)
                                                                    (render-score (gs-score gs)
                                                                                  (gs-board-image gs))
                                                                    (gs-time gs))
                                                         (gs-time gs))))
                 (empty-scene SCREEN-W SCREEN-H "black")))

; Board -> Image
; Dibuja el tablero


(define (render-board bd)
  (foldr-board (lambda (x y cv b)
                 (place-cell-image (cell-image cv) x y b))
               MTB
               bd))

; Sprite Image Natural -> Image
; añade la imagen de pac-man a img (si el tiempo es de segundos impares, renderiza pacman de boca cerrada)


(define (render-pm pm img t)
  (local [(define PM
            (cond [(odd? t) C-PM] 
                  [(string=? "U" (sprite-dir pm)) U-PM]
                  [(string=? "D" (sprite-dir pm)) D-PM]
                  [(string=? "L" (sprite-dir pm)) L-PM]
                  [(string=? "R" (sprite-dir pm)) R-PM]))]
    (place-cell-image PM (sprite-x pm) (sprite-y pm) img)))

; punt Image -> Image
; Agrega la puntuación como imagen

(define (render-ghost ghost img t)
  (local [(define GT-IMG
            (cond [(= 1 (modulo t 4)) (circle 10 "solid" "tomato")]
                  [(= 2 (modulo t 4)) (circle 10 "solid" "pink")]
                  [(= 3 (modulo t 4)) (circle 10 "solid" "tomato")]
                  [else
                   GT]))]
    (place-cell-image GT-IMG (ghost-x ghost) (ghost-y ghost) img)))

; ValorCelda -> Image
; Dibuja una celda del tablero


(define (render-score score img) 
  (local [(define score-text
            (text (string-append "Puntuación: " (number->string score)) SCORE-TEXT-SIZE "yellow"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ BOARD-HEIGHT (/ SCORE-HEIGHT 2))
                 img)))
(define (render-time score img) 
  (local [(define score-text
            (text (string-append "Tiempo: " (number->string score)) SCORE-TEXT-SIZE "aqua"))]
    (place-image score-text
                 (/ BOARD-WIDTH 2)
                 (+ 5 BOARD-HEIGHT SCORE-HEIGHT)
                 img)))

; ValorCelda -> Image
; Dibuja una celda del tablero
(define (cell-image cv)
  (cond [(string=? cv "empty") MTC] 
        [(string=? cv "dot")   DTC]
        [(string=? cv "wall")  WALL]
        [(string=? cv "cherry") CHERRY]))


;-------------------------------------------------------------------------------

; Operaciones en el tablero y otras ayudas


; Tablero Natural Natural -> ValorCelda
; busca el valor de una celda del tablero


(define (board-ref bd x y)
  (vector-ref (vector-ref bd y) x))

; (ref-tab EE-TAB 1 1)
; (ref-tab ED-TAB 2 1)
; (ref-tab DD-TAB 3 1)

; (Natural Natural ValorCelda -> ValorCelda) tab -> tab
; el análogo de map para tableros, la función se llama para
; cada posición en el tablero para producir un valor de celda para esa
; posición en un nuevo tablero resultante


(define (map-board fn bd)
  (build-vector (vector-length bd)
                (lambda (y)
                  (build-vector (vector-length (vector-ref bd y))
                                (lambda (x)
                                  (fn x y (board-ref bd x y)))))))

; (Natural Natural ValorCelda X -> X) X tab -> X
; el análogo de foldr para tableros, la función se llama para
; cada posición en el tablero para producir un único valor

(define (foldr-board fn base bd)
  (local [(define nrows (vector-length bd))
          (define ncols (vector-length (vector-ref bd 0)))
          
          (define (rows y b)
            (cond [(= y nrows) b]
                  [else
                   (rows (add1 y)
                         (cols 0 y b))]))
          (define (cols x y b)
            (cond [(= x ncols) b]
                  [else
                   (cols (add1 x)
                         y
                         (fn x y (board-ref bd x y) b))]))]
    (rows 0 base)))

; Image Natural Natural Image -> Image
; agrega cell-img a tab-image para x, y coordinadas del tablero


(define (place-cell-image cell-img x y board-image)
  (place-image cell-img
               (+ (* x CELL-SIZE) (/ CELL-SIZE 2))
               (+ (* y CELL-SIZE) (/ CELL-SIZE 2))
               board-image))
(main)
