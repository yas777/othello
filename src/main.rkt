#lang racket

(provide (all-defined-out))
(require racket/vector)
(require lang/posn)
;(require "universe.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PARAMETERS WE ARE FREE TO SET
;(define (f) (update-grid-size grid_size))
(define delay_time 300)
(define num_players 1)
(define depth_search 2)
(define initial_player 1)
(define current_player initial_player)
(define grid_size 8)
(define player-name "team")
(define ip-address "198")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; delay FUNCTION

(define (delay1 process time)
  (define initial-time (current-inexact-milliseconds))
  (define (delay-help)
    (unless (>= (- (current-inexact-milliseconds) initial-time) time) (delay-help)))
  (define temp_evaluate process)
  (delay-help)
  temp_evaluate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2D-VECTOR DEFINITION

(define (make-2d-vector r c)
  (build-vector r 
    (lambda (x) (make-vector c #f))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; grid-copy FUNCTION

(define (grid-copy grid) (vector-map! (λ (v) (vector-copy v)) (vector-copy grid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main_grid, main_outline DEFINITIONS

(define k (/ grid_size 2))

(define (grid-gen n)
  (let ((copy (make-2d-vector n n))
        (k (/ n 2)))
    (2d-vector-set! copy (- k 1) (- k 1) 1)
    (2d-vector-set! copy (- k 1) k 0)
    (2d-vector-set! copy k (- k 1) 0)
    (2d-vector-set! copy k k 1)
    copy))
(define (outline-gen n)
  (define k (/ n 2))
  (list
   (make-posn (+ k 1) (- k 2)) (make-posn (+ k 1) (- k 1))
   (make-posn (+ k 1) k) (make-posn (+ k 1) (+ k 1))
   (make-posn k (+ k 1)) (make-posn (- k 1) (+ k 1))
   (make-posn (- k 2) (+ k 1)) (make-posn (- k 2) k)
   (make-posn (- k 2) (- k 1)) (make-posn (- k 2) (- k 2))
   (make-posn (- k 1) (- k 2)) (make-posn k (- k 2))))
(define main_grid (grid-gen 8))

(define main_outline (outline-gen 8))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (setter! thing new)
;  (begin (display thing)
;         (if (equal? thing 'main_grid-outline-grid_size)
;             (begin (setter! grid_size new)
;                    (setter! main_grid (grid-gen new))
;                    (setter! main_outline (outline-gen new)))
;             (set! thing new))))
;(define (grid_size-set! n)
;  (set! grid_size n))
(define (num_players-set! n)
  (set! num_players n))
(define (depth_search-set! n)
  (set! depth_search n))
;(define (set-main_grid! n)
;  (set! main_grid (grid-gen n)))
;(define (set-main_outline! n)
;  (set! main_outline (outline-gen n)))
(define (set-total-grid-initial-player l)
  (set-main_grid-outline-grid_size (car l))
  (set! initial_player (cadr l))
  (set! current_player (cadr l)))
(define (set-main_grid-outline-grid_size n)
  (begin (set! grid_size n)
         (set! k (/ n 2))
         (set! main_grid (grid-gen n))
         (set! main_outline (outline-gen n))
         (set! restart_grid main_grid)
         (set! restart_outline main_outline)
         (set! 1player_undo_grid main_grid)
         (set! 1player_undo_outline main_outline)
         (set! 2player_undo_grid main_grid)
         (set! 2player_undo_outline main_outline)))
(define (player-name-set! name)
  (set! player-name name))
(define (ip-address-set! str)
  (set! ip-address str))
(define (intial_player-set! x)
  (set! initial_player x)
  (set! current_player x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UNDO Functions

(define 1player_undo_grid main_grid)
(define 1player_undo_outline main_outline)
(define 2player_undo_grid main_grid)
(define 2player_undo_outline main_outline)

(define (undo_grids_outlines_reset)
  (set! 1player_undo_grid main_grid)
  (set! 1player_undo_outline main_outline)
  (set! 2player_undo_grid main_grid)
  (set! 2player_undo_outline main_outline))

(define (undo-game)
  (cond [(= num_players 1) (begin (set! main_grid 1player_undo_grid)
                                  (set! main_outline 1player_undo_outline))]
        [(= num_players 2) (begin (cond [(not (equal? main_grid 2player_undo_grid))
                                         (update-current_player)])
                                  (set! main_grid 2player_undo_grid)
                                  (set! main_outline 2player_undo_outline))]
        [else (error "Oops! There should be only 1 or 2 players")]))

(define (update-current_player) (set! current_player (- 1 current_player)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESTART FUNCTIONS

(define restart_grid (grid-copy main_grid))

(define restart_outline main_outline)

(define (restart-game) (let [(copied_grid (grid-copy restart_grid))]
                         (set! main_outline restart_outline)
                         (set! main_grid copied_grid)
                         (set! current_player initial_player)
                         (undo_grids_outlines_reset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UPDATE-FUNCTIONS

(define (update-all-grid position player) (begin (update-outline position) (update-grid position player)))

(define (update-grid position player)
  (let [(converted (convert main_grid player position))]
    (if (equal? converted "INVALID MOVE") "INVALID MOVE"
        (begin
          (set! 1player_undo_grid 2player_undo_grid)
          (set! 2player_undo_grid main_grid)
          (set! main_grid converted)))))

(define (update-outline position)
  (begin
    (set! 1player_undo_outline 2player_undo_outline)
    (set! 2player_undo_outline main_outline)
    (set! main_outline (next-outline main_grid position main_outline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Functions start here

; -> list
(define (search-list grid player position)
  (define (search-list_help position direc1 direc2 adjacent_check)
    (define not_pass #f)
    (let [(i (posn-x position))
          (j (posn-y position))]
      (if (or (and (= 1 direc1) (= i (- grid_size 1))) (and (= -1 direc1) (= i 0))
              (and (= 1 direc2) (= j (- grid_size 1))) (and (= -1 direc2) (= j 0)))
        not_pass
        (let [(next_pos (2d-vector-ref grid (+ i direc1) (+ j direc2)))]
          (cond [(or (equal? next_pos #f) (and adjacent_check (equal? next_pos player))) not_pass]
                [(and (not adjacent_check) (equal? next_pos player)) (list direc1 direc2 (make-posn (+ i direc1) (+ j direc2)))]
                [else (search-list_help (make-posn (+ i direc1) (+ j direc2)) direc1 direc2 #f)])))))
  (let [(x (posn-x position))
        (y (posn-y position))]
    (if (number? (2d-vector-ref grid x y)) (list #f #f #f #f #f #f #f #f)
        (list (search-list_help position 1 0 #t) (search-list_help position 1 1 #t)
              (search-list_help position 0 1 #t) (search-list_help position -1 1 #t)
              (search-list_help position -1 0 #t) (search-list_help position -1 -1 #t)
              (search-list_help position 0 -1 #t) (search-list_help position 1 -1 #t)))))

(define (convert grid_copy player position)
  (define grid (grid-copy grid_copy))
  (let [(i (posn-x position))
        (j (posn-y position))]
    (define (convert_help e)
      (cond [(not (equal? #f e))
             (let* [(direc1 (car e))
                    (direc2 (cadr e))
                    (final_pos (caddr e))
                    (x1 (posn-x final_pos))
                    (y1 (posn-y final_pos))]
               (define (change x y)
                 (cond ((not (and (= x x1) (= y y1))) (begin (2d-vector-set! grid x y player)
                                                             (change (+ x direc1) (+ y direc2))))))
               (change i j))]))
    (let [(l (search-list grid player position))]
      (define (f l1) (cond ((not (null? l1)) (begin (convert_help (car l1)) (f (cdr l1)))))) 
      (if (equal? l (list #f #f #f #f #f #f #f #f)) "INVALID MOVE"
          (begin (f l) grid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (valid? position)
  (let [(x (posn-x position))
        (y (posn-y position))]
    (and (>= x 0) (>= y 0) (< x grid_size) (< y grid_size))))

(define (not-occupied? grid)
  (λ (position)
    (let [(x (posn-x position))
          (y (posn-y position))]
      (boolean? (2d-vector-ref grid x y)))))

(define (find grid position)
  (let* [(x (posn-x position))
         (y (posn-y position))
         (possible_list (list (make-posn (+ x 1) y)
                              (make-posn (+ x 1) (+ y 1))
                              (make-posn x (+ y 1))
                              (make-posn (- x 1) (+ y 1))
                              (make-posn (- x 1) y)
                              (make-posn (- x 1) (- y 1))
                              (make-posn x (- y 1))
                              (make-posn (+ x 1) (- y 1))))]
    (filter (not-occupied? grid) (filter valid? possible_list))))
;; Think about filter. the error it caused when using (and (not-occupied? grid) valid?)).

(define (next-outline grid position outline)
  (define (not-in-outline? position)
    (boolean? (member position outline)))
  (append (filter not-in-outline? (find grid position)) (remove position outline)))

(define (next-moves grid outline player)
  (define (next-valid-move? position)
    (not (equal? (search-list grid player position) (list #f #f #f #f #f #f #f #f))))
  (filter next-valid-move? outline))

(define (score grid_copy player)
  (define grid (grid-copy grid_copy))
  (define (sum-vec v)
    (define (sum-vec_help v count ans)
      (if (= count -1) ans
          (sum-vec_help v (- count 1) (+ ans (vector-ref v count)))))
    (sum-vec_help v (- grid_size 1) 0))
  (define (score-vec v)
    (define (score-vec_help v count ans)
      (cond [(= count -1) ans]
            [(equal? (vector-ref v count) player) (score-vec_help v (- count 1) (+ ans 1))]
            ;[(equal? (vector-ref v count) 0) (score-vec_help v (- count 1) (+ ans 1))]
            [else (score-vec_help v (- count 1) ans)]))
    (score-vec_help v (- grid_size 1) 0))
  (sum-vec (vector-map! score-vec grid)))
;;;;;; DO ABSTRACTION HERE.. HIGH SCOPE..

(define (p a)
  (if (= a 1) -1 1))

(define (g a)
  (if (= a 1) 1 -1))

(define (score1 gr n)
 (define (f i)
   (define (g j)
     (if (= j n) (f (+ i 1))
         (let* ((a (2d-vector-ref gr i j)))
           (if (number? a) (+ (* (p a) (position-value n (make-posn i j))) (g (+ j 1)))
               (g (+ j 1))))))
  (if (= i n) 0
      (g 0)))
  (f 0))
  

(define (position-score-grid n)
  (define sample-grid (make-2d-vector n n))
 (define (f i)
   (define (g j)
     (if (= j n) (f (+ i 1))
         (begin (2d-vector-set! sample-grid i j
                                (position-value n (make-posn i j))) (g (+ j 1)))))
  (if (= i n) sample-grid
      (g 0)))
  (f 0))

(define (no-of-occupied gr n)
  (define (f i)
    (define (g j)
      (if (= j n) (f (+ i 1))
           (if (number? (2d-vector-ref gr i j)) (+ 1 (g (+ j 1)))
               (g (+ j 1)))))
    (if (= i n) 0
        (g 0)))
  (f 0))

(define (score-helper gr n player a b c d e k)
  (define (f i)
    (define (g j)
      (if (= (+ j (* k i)) n) (+ (* 15 (* (+ 0.5 (abs (- i (/ (- n 1) 2))))
                                          (+ 0.5 (abs (- i (/ (- n 1) 2)))))) (f (+ i 1)))
          (if (let* ((a (2d-vector-ref gr (+ (* a i) (* b j)) (+ (* c i) (* d j)))))
                (and (equal? player a) (number? a))) (g (+ j 1))
              (f (+ i 1)))))
    (if (= i n) 0
          (g 0)))
  (f e))
(define (no-of-horizontal gr n player) (score-helper gr n player 1 0 0 1 0 0))
(define (no-of-vertical gr n player) (score-helper gr n player 0 1 1 0 0 0))
(define (no-of-dia-left gr n player) (score-helper gr n player 1 1 0 1 1 1))
(define (no-of-dia-right gr n player) (score-helper gr n player 0 1 1 1 0 1))



(define (position-value n pos)
  (define cs (+ 10 (* 20 (- n 6))))
  (let* ((x (posn-x pos))
         (y (posn-y pos)))
    (cond [(and (or (= x 0) (= x (- n 1))) (or (= y 0) (= y (- n 1)))) 10000]
          [(or (and (or (= x 0) (= x (- n 1))) (or (= y 1) (= y (- n 2))))
               (and (or (= y (- n 1)) (= y 0)) (or (= x 1) (= x (- n 2))))) -3000]
          [(and (or (= x 1) (= x (- n 2))) (or (= y 1) (= y (- n 2)))) -5000]
          [(or (and (and (> x 1) (< x (- n 1))) (or (= y 0) (= y (- n 1))))
               (and (and (> y 1) (< y (- n 1))) (or (= x 0) (= x (- n 1))))) 900]
          [(or (and (and (> x 1) (< x (- n 1))) (or (= y 1) (= y (- n 2))))
               (and (and (> y 1) (< y (- n 1))) (or (= x 1) (= x (- n 2))))) -500]
          
          [(and (or (= x (- (/ n 2) 1)) (= x (/ n 2)))
                (or (= y (- (/ n 2) 1)) (= y (/ n 2)))) cs]
          [else (let* ((a (abs (- x (/ (- n 1) 2))))
                       (b (abs (- y (/ (- n 1) 2)))))
                  (- cs (+  (* (max (- a (/ 1 2))
                                    (- b (/ 1 2))) 20)
                     (* (abs (- a b)) 20))))])))

(define (score_predicate grid depth_search)
  (cond [(> depth_search 2) (score_weightage grid 600 800 1100)]
        [else (score_weightage grid 380 470 600)]))

(define (score_weightage grid a b c)
  (let* ((n grid_size)
         (s (no-of-occupied grid n)))
    (cond [(< s (/ n 6)) a]
          [(and (>= s (/ n 6)) (< s (/ n 2.8))) b]
          [else c])))
 
 (define (total_score grid outline player)
   (+ (* (score_weightage grid 0.19 0.21 0.23) (score1 grid grid_size))
      (* (length (next-moves grid outline player)) (* 0.20 (g player)))
      (* 0.65 (no-of-horizontal grid grid_size 1))
      (* -1 (* 0.65 (no-of-horizontal grid grid_size 0)))
      (* 0.65 (no-of-vertical grid grid_size 1))
      (* -1 (* 0.65 (no-of-vertical grid grid_size 0)))
      (* 0.65 (no-of-dia-right grid grid_size 1))
      (* -1 (* 0.65 (no-of-dia-right grid grid_size 0)))
      (* 0.65 (no-of-dia-left grid grid_size 1))
      (* -1 (* 0.65 (no-of-dia-left grid grid_size 0)))
      (* (score_predicate grid depth_search) (differ grid))))
          

(define (differ grid) (- (score grid (- 1 initial_player)) (score grid initial_player)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The GOD IS HERE

(define (minimax depth grid outline)
  (define (minimax_help depth grid outline player temp alpha beta)
    (define (mini l)
      (define (mini_help l ans)
        (if (null? l) ans (mini_help (cdr l) (min (car l) ans))))
      (mini_help l +inf.0))
    (define (maxi l)
      (define (maxi_help l ans)
        (if (null? l) ans (maxi_help (cdr l) (max (car l) ans))))
      (maxi_help l -inf.0))
    (define (f max_min p player x)
      (let [(next_list (next-moves grid outline player))]
        (if (null? next_list) (* (differ grid) (total_score grid outline player))
            (max_min (map (λ (pos) (if (>= alpha beta) x
                                       (let* [(best-val (minimax_help depth
                                                 (convert grid player pos)
                                                 (next-outline grid pos outline) (- 1 player)
                                                 (+ 1 temp) alpha beta))]
                                         (begin0 best-val (set! x (p x best-val))))))
                          
                          next_list)))))
    (cond [(= depth temp) (total_score grid outline player)]
          [(odd? temp) (f maxi max 0 alpha)]
          [(even? temp) (f mini min 1 beta)]))
  (minimax_help depth grid outline initial_player 0 -inf.0 +inf.0))

(define (best-move depth grid outline)
  (let [(l (next-moves grid outline (- 1 initial_player)))]
    (define (best-move_help depth grid outline l ans max)
      (if (null? l) ans
        (let* [(pos (car l))
              (val (minimax depth
                            (convert grid (- 1 initial_player) pos)
                            (next-outline grid pos outline)))]
          (if (< max val) (best-move_help depth grid outline (cdr l) pos val)
              (best-move_help depth grid outline (cdr l) ans max)))))
    (best-move_help depth grid outline l "GAME_OVER" -inf.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;THE END IS HERE

