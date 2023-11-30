(require (lib "graphics.ss" "graphics"))
(open-graphics)

(define ventana (open-viewport "Botón" (make-posn 500 500)))

(define medio (make-posn 160 250))


(define (button posn eleccion color)
  ((draw-solid-rectangle ventana) posn 111 111 color)
  ((draw-string ventana) (make-posn (+ (posn-x posn) 25) (+ (posn-y posn) 60)) eleccion))



(define (tomarDecision c)
  (cond
    [(and (>= (posn-x (mouse-click-posn c)) 165) (<= (posn-x (mouse-click-posn c)) 276)
          (>= (posn-y (mouse-click-posn c)) 15) (<= (posn-y (mouse-click-posn c)) 126))
     1]
    [(and (>= (posn-x (mouse-click-posn c)) 44) (<= (posn-x (mouse-click-posn c)) (+ 44 111))
          (>= (posn-y (mouse-click-posn c)) 74) (<= (posn-y (mouse-click-posn c)) (+ 74 111)))
     2]
    [(and (>= (posn-x (mouse-click-posn c)) 22) (<= (posn-x (mouse-click-posn c)) (+ 22 111))
          (>= (posn-y (mouse-click-posn c)) 205) (<= (posn-y (mouse-click-posn c)) (+ 205 111)))
     3]
    [(and (>= (posn-x (mouse-click-posn c)) 103) (<= (posn-x (mouse-click-posn c)) (+ 103 111))
          (>= (posn-y (mouse-click-posn c)) 340) (<= (posn-y (mouse-click-posn c)) (+ 340 111)))
     4]
    [(and (>= (posn-x (mouse-click-posn c)) 230) (<= (posn-x (mouse-click-posn c)) (+ 230 111))
          (>= (posn-y (mouse-click-posn c)) 340) (<= (posn-y (mouse-click-posn c)) (+ 340 111)))
     5]
    [(and (>= (posn-x (mouse-click-posn c)) 310) (<= (posn-x (mouse-click-posn c)) 415)
          (>= (posn-y (mouse-click-posn c)) 205) (<= (posn-y (mouse-click-posn c)) (+ 205 111)))
     6]
    [(and (>= (posn-x (mouse-click-posn c)) 290) (<= (posn-x (mouse-click-posn c)) 401)
          (>= (posn-y (mouse-click-posn c)) 74) (<= (posn-y (mouse-click-posn c)) (+ 74 111)))
     7]
    [else 8]))

(define (esperarClick)
  (define c (get-mouse-click ventana))
  (tomarDecision c))

(define (puntuacion count j)
  (button (make-posn 165 15) "ROCK" "brown")
  (button (make-posn 44 74) "FIRE" "red")
  (button (make-posn 22 205) "SCISSORS" "gray")
  (button (make-posn 103 340) "SPONGE" "brown")
  (button (make-posn 230 340) "PAPER" (make-rgb 0.94 0.94 0.94))
  (button (make-posn 310 205) "AIR" (make-rgb 0.3 0.94 0.3))
  (button (make-posn 290 74) "WATER" "blue")
  (define pc (random 7))
  (displayln count)
  (if (and (> count -3) (< count 3))
      (cond
        [(= j pc)
         (begin
           ((draw-string ventana) borrar "EMPATE" "black")
           (sleep 0.5)

           (puntuacion (esperarClick) count))]
        [(and (= j 1) (or (or (= pc 2) (= pc 3)) (= pc 4)))
         (begin
           ((draw-string ventana) medio "OBTUVISTE PUNTO" "black")
           (sleep 0.5)

           (puntuacion (+ count 1) (esperarClick)))]
        [(and (= j 2) (or (or (= pc 3) (= pc 4)) (= pc 5)))
         (begin
           ((draw-string ventana) medio "OBTUVISTE PUNTO" "black")
           (sleep 0.5)
           
           (puntuacion (+ count 1) (esperarClick)))]
        [(and (= j 3) (or (or (= pc 4) (= pc 5)) (= pc 6)))
         (begin
           ((draw-string ventana) medio "OBTUVISTE PUNTO" "black")
           (sleep 0.5)
           
           (puntuacion (+ count 1) (esperarClick)))]
        [(and (= j 4) (or (or (= pc 5) (= pc 6)) (= pc 7)))
         (begin
           ((draw-string ventana) medio "OBTUVISTE PUNTO" "black")
           (sleep 0.5)
           
           (puntuacion (+ count 1) (esperarClick)))]
        [(and (= j 5) (or (or (= pc 6) (= pc 7)) (= pc 1)))
         (begin
           ((draw-string ventana) medio "OBTUVISTE PUNTO" "black")
           (sleep 0.5)
           
           (puntuacion (+ count 1) (esperarClick)))]
        [(and (= j 6) (or (or (= pc 7) (= pc 1)) (= pc 2)))
         (begin
           ((draw-string ventana) medio "OBTUVISTE PUNTO" "black")
           (sleep 0.5)
           
           (puntuacion (+ count 1) (esperarClick)))]
        [(and (= j 7) (or (or (= pc 1) (= pc 2)) (= pc 3)))
         (begin
           ((draw-string ventana) medio "OBTUVISTE PUNTO" "black")
           (sleep 0.5)
           
           (puntuacion (+ count 1) (esperarClick)))]
        [(= j 8)
         (begin
           ((draw-string ventana) medio "ACCION NO VALIDA" "black")
           (sleep 0.5)
           
           (puntuacion count (esperarClick)))]
        [else
         (begin
           ((draw-string ventana) medio "PERDISTE PUNTO" "black")
           (sleep 0.5)
           
           (puntuacion (- count 1) (esperarClick)))]))
  (if (= count -3)
      (begin
        ((draw-solid-rectangle ventana) (make-posn 0 0) 500 500 "red")
        ((draw-string ventana) medio "¡¡¡PERDISTE LA PARTIDA!!!" "black")
        (sleep 4)
        (close-graphics))
      (begin
        ((draw-solid-rectangle ventana) (make-posn 0 0) 500 500 "green")
        ((draw-string ventana) medio "¡¡¡GANASTE LA PARTIDA!!!" "black")
        (sleep 4)
        (close-graphics))))
(puntuacion 0 (esperarClick))
