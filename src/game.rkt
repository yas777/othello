#lang racket

(require 2htdp/universe)
(require (except-in 2htdp/image make-pen make-color))
(require racket/gui)
(require rsound)
(require lang/posn)
(require "images.rkt")
(require "universe.rkt")
(require "main.rkt")

;;;;;;;;;;;;;;ALL VARIABLE DEFINTIONS;;;
(define sound? #t)
(define button_click (rs-read "images/button_click.wav"))
(define temp "GAME")
(define down_status "NO")
(define legal-moves? #t)
;;;;;;;;;;;;;;;;;;;;;
(define (p x y) (make-posn (+ (* 90 x) 45) (+ (* 90 y) 45)))

(define (grid_image) (place-images (build-list (* k k) (λ (x) backg))
                                 (append* (build-list k (λ (y) (build-list k (λ (x) (p (+ (* 2 x) 0.5)
                                                                                       (+ (* 2 y) 0.5)))))))
                                 (empty-scene (* grid_size 90) (* grid_size 90))))

(define (2dvector->image-list matrix)
  (define (value->image e)
    (cond [(boolean? e) t]
          [(= e 0) b]
          [(= e 1) w]
          [else (error "Somewhere went wrong, need 0, 1 or #f")]))
  (define (list->image-list l ans)
    (if (null? l) ans
        (list->image-list (cdr l) (cons (value->image (car l)) ans))))
  (append* (map (λ (v) (reverse (list->image-list (vector->list v) '()))) (vector->list matrix))))

(define empty_frame (empty-scene 990 720))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Game Score box

(define score_white_icon (scale 3 r_w))
(define score_black_icon b)
(define (score_value player) (text/font (number->string (score main_grid player))
                                       60 "black" "DISTRICTPRO"
                                       'modern 'normal 'light #f))
(define (score_white_value) (score_value 1))
(define (score_black_value) (score_value 0))

(define (current_indicator_pos)
  (if (= current_player 1) (make-posn 795 100) (make-posn 915 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (2-lan-game_image_list)
  (list current_indicator
        score_white_icon
        score_black_icon
        (score_white_value)
        (score_black_value)
        (scale (/ 8 grid_size) (place-images (show-color-l) (show-pos-l) (grid_image)))
        (grid_size_image)))
(define (game_image_list)
  (list current_indicator
        score_white_icon
        score_black_icon
        (score_white_value)
        (score_black_value)
        (scale (/ 8 grid_size) (place-images (show-color-l) (show-pos-l) (grid_image)))
        undo_image restart_image mainmenu_image))
(define (game_position_list) (list (current_indicator_pos)
                                 (make-posn 795 100) (make-posn 915 100)
                                 (make-posn 795 200) (make-posn 915 200)
                                 (make-posn 360 360) (make-posn 855 525)
                                 (make-posn 855 600) (make-posn 855 675)))
(define (2-lan-game_position_list) (list (current_indicator_pos)
                                 (make-posn 795 100) (make-posn 915 100)
                                 (make-posn 795 200) (make-posn 915 200)
                                 (make-posn 360 360) (make-posn 855 525)))
;;;;;;;;;;;;;;;ALL IMAGES;;;;;;;;;;
(define undo_image undo_normal)
(define restart_image restart_normal)
(define mainmenu_image mainmenu_normal)
(define replay_image replay_normal)
(define play_image play1)
(define settings_image settings)
(define credits_image credits)
(define howtoplay_image howto)
(define 2-lan_image 2-lan)
(define (grid_size_image) (text/font (string-append "GRID SIZE : " (number->string grid_size))
                                     20 "black" "DISTRICTPRO"
                                     'modern 'normal 'light #f))
(define back_s_image back_s)
(define credits_screen_image t)

;;;;;ALL BUTTON AREAS;;;;;;;;;;;;;;
(define (button-area x0 y0)
    (lambda (x-button y-button)
      (λ (x y)
      (and (< (abs (- x x-button)) x0) (< (abs (- y y-button)) y0)))))
(define (game-button-area x-button y-button) ((button-area 120 30) x-button y-button))
(define (welcome-button-area x-button y-button) ((button-area 150 38) x-button y-button))
(define undo-button-area (game-button-area 855 525))
(define restart-button-area (game-button-area 855 600))
(define mainmenu-button-area (game-button-area 855 675))
(define player-button-area (button-area 51 20))
(define grid_size-button-area (button-area 23 19))
(define sound-button-area (button-area 28 19))
(define first-move-area (button-area 37 19))
(define legal-button-area (button-area 36 19))
(define difficulty-button-area (button-area 19 19))
(define back-button-area (button-area 45 27))
(define (game-area x y) (and (> x 0) (< x 720) (> y 0) (< y 720)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (color-l) (2dvector->image-list main_grid))
(define (pos-l) (append* (build-list grid_size (λ (y) (build-list grid_size (λ (x) (p x y)))))))

(define (show-color-l) (if legal-moves? (append (build-list (length (next_possible)) (λ (x) (if (= current_player 1) r_w r_b)))
                                                (color-l)) (color-l)))
(define (show-pos-l) (if legal-moves? (append (next_possible) (pos-l)) (pos-l)))
(define (next_possible) (map (λ (pos) (p (posn-y pos) (posn-x pos))) (next-moves main_grid main_outline current_player)))


(define (winner) (if (> (- (score main_grid 0) (score main_grid 1)) 0) "BLACK WINS" "WHITE WINS"))
(define (score-display) (string-append "BY LEAD SCORE OF "
                                       (number->string (abs (- (score main_grid 0) (score main_grid 1))))))


;;;;;;;;;RESET FUNCTIONS ;;;;;;;;;;;
(define (reset-all) (undo-set! undo_normal) (mainmenu-set! mainmenu_normal)
  (restart-set! restart_normal) (replay-set! replay_normal) (play-set! play1)
  (settings-set! settings) (howtoplay-set! howto) (credits-set! credits) (2-lan-set! 2-lan)
  (back_s-set! back_s)
  (credits_screen-set! t))

;;;;;;;;COMMON BUTTON ABSTRACTIONS

(define (update str inp s1 s2 f f_hover f_press action)
  (define (hover) (f f_hover))
  (define (press) (f f_press))
  (cond [(mouse=? inp "button-down") (begin (set! down_status str)
                                            (when sound? (play button_click))
                                            (press)
                                            s1)]
        [(and (not (equal? down_status str)) (mouse=? inp "button-up"))
         (set! down_status "NO") (reset-all) (hover) s1]
        [(mouse=? inp "button-up") (begin (set! down_status "NO")
                                          (hover)
                                          (action)
                                          s2)]
        [(equal? down_status str) (begin (press)
                                         s1)]
        [else (hover) s1]))

;;;;;;;;;;ABSTRACTED BUTTONS

(define (undo-set! imag)
  (set! undo_image imag))
(define (mainmenu-set! imag)
  (set! mainmenu_image imag))
(define (restart-set! imag)
  (set! restart_image imag))
(define (replay-set! imag)
  (set! replay_image imag))
(define (play-set! image)
  (set! play_image image))
(define (settings-set! image)
  (set! settings_image image))
(define (credits-set! image)
  (set! credits_image image))
(define (howtoplay-set! image)
  (set! howtoplay_image image))
(define (2-lan-set! image)
  (set! 2-lan_image image))
(define (back_s-set! image)
  (set! back_s_image image))
(define (credits_screen-set! image)
  (set! credits_screen_image image))
(define (play_update inp)
  (update "play1" inp "WELCOME" "GAME" play-set! play_hover play_hover undo-game))
(define (credits_update inp)
  (update "credits" inp "WELCOME" "CREDITS" credits-set! credits_hover credits_hover undo-game))
(define (howtoplay_update inp)
  (update "howtoplay" inp "WELCOME" "HOW_TO_PLAY" howtoplay-set! howto_hover howto_hover undo-game))
(define (settings_update inp)
  (update "settings" inp "WELCOME" "SETTINGS" settings-set! settings_hover settings_hover undo-game))
(define (2-lan_update inp)
  (update "2-lan" inp "WELCOME" "2-lan" 2-lan-set! 2-lan_hover 2-lan_hover undo-game))
(define (undo_update inp)
  (update "UNDO" inp "GAME" "GAME" undo-set! undo_hover undo_press undo-game))
(define (restart_update inp)
  (update "RESTART" inp "GAME" "GAME" restart-set! restart_hover restart_press restart-game))
(define (mainmenu_update stat inp)
  (update "MAINMENU" inp stat "WELCOME" mainmenu-set! mainmenu_hover mainmenu_press restart-game))
(define (replay_update inp)
  (update "REPLAY" inp "GAME_OVER" "GAME" replay-set! replay_hover replay_press restart-game))

(define (back_settings_update inp)
  (update "B-S" inp "SETTINGS" "WELCOME" back_s-set! back_s_hover back_s_hover undo-game))
(define (credits_back_settings_update inp)
  (update "C-S" inp "CREDITS" "WELCOME" credits_screen-set! credits_screen_back credits_screen_back undo-game))


;;;;;;;;;;;;;;;;ALL FRAMES;;;;;;;;;;
(define (welcome_frame)
  (place-images (list logo
                      play_image settings_image
                      howtoplay_image credits_image
                      2-lan_image)
                (list (make-posn 495 160)
                      (make-posn 248 420) (make-posn 742 420)
                      (make-posn 248 540) (make-posn 742 540)
                      (make-posn 495 670))
                empty_frame))
(define (credits_frame)
  (place-images (list credits_screen_image credits_scr)
                (list (make-posn 495 594)
                      (make-posn 495 360)) empty_frame))
(define (howtoplay_frame)
  (place-images (list how_to_play)
                (list (make-posn 495 360)) empty_frame))
(define (2-lan-game_frame)
  (place-images (2-lan-game_image_list)
                (2-lan-game_position_list)
                empty_frame))
(define (game_frame)
  (place-images (game_image_list)
                (game_position_list)
                empty_frame))

(define (settings_frame)
  (place-images (list settings_logo
                      1player 2player
                      grid_size?
                      grid_size-6 grid_size-8 grid_size-10 grid_size-12
                      grid_size-14 grid_size-16 grid_size-18 grid_size-20
                      sound-toggle
                      sound-yes sound-no
                      first_move?
                      first_color-white first_color-black
                      show_legal_moves?
                      legal_moves-yes legal_moves-no
                      difficulty?
                      difficulty-1 difficulty-2 difficulty-3 difficulty-4 difficulty-5
                      back_s_image)
                (list (make-posn 495 107)
                      (make-posn 330 260) (make-posn 660 260)
                      (make-posn 150 340)
                      (make-posn 350 340) (make-posn 400 340) (make-posn 450 340) (make-posn 500 340)
                      (make-posn 550 340) (make-posn 600 340) (make-posn 650 340) (make-posn 700 340)
                      (make-posn 150 420)
                      (make-posn 350 420) (make-posn 470 420)
                      (make-posn 150 500)
                      (make-posn 350 500) (make-posn 470 500)
                      (make-posn 150 580)
                      (make-posn 350 580) (make-posn 470 580)
                      (make-posn 150 660)
                      (make-posn 350 660) (make-posn 400 660) (make-posn 450 660) (make-posn 500 660) (make-posn 550 660)
                      (make-posn 880 660))
                      empty_frame))
;;;;;;;;;;;;;;ALL FRAME UPDATES ;;;;;;;;;;;;;;;
(define (howtoplay_frame_update x y inp)
  (cond [(and (mouse=? "button-down" inp)
              (((button-area 48 27) 495 680) x y)) "WELCOME"]
        [else "HOW_TO_PLAY"]))

(define (credits_frame_update x y inp)
  (cond [(((button-area 48 27) 495 594) x y) (credits_back_settings_update inp)]
        [else (reset-all) "CREDITS"]))
(define (welcome_frame_update x y inp)
  (cond [((welcome-button-area 248 420) x y) (play_update inp)]
        [((welcome-button-area 742 420) x y) (settings_update inp)]
        [((welcome-button-area 248 540) x y) (howtoplay_update inp)]
        [((welcome-button-area 742 540) x y) (credits_update inp)]
        [((welcome-button-area 495 670) x y) (2-lan_update inp)]
        [(and (not (equal? down_status "NO"))
                    (mouse=? inp "button-up")) (begin (reset-all)
                                                      (set! down_status "NO") "WELCOME")]
        [(mouse=? inp "move") (begin (reset-all)
                                                 "WELCOME")]
        [else "WELCOME"]))
(define (game_frame_update x y inp)
  (begin0
    (if (mainmenu-button-area x y) (mainmenu_update "GAME" inp)
        (begin
          (cond [(null? (next-moves main_grid main_outline current_player)) "GAME_OVER"]
                [(undo-button-area x y) (undo_update inp)]
                [(restart-button-area x y) (restart_update inp)]
                [(mainmenu-button-area x y) (mainmenu_update "GAME" inp)]
                [(and (not (equal? down_status "NO"))
                      (mouse=? inp "button-up")) (begin (reset-all)
                                                        (set! down_status "NO") "GAME")]
                [(mouse=? inp "move") (begin (reset-all)
                                             "GAME")]
                [(mouse=? inp "drag") (reset-all)
                                      (cond [(equal? down_status "UNDO") (set! undo_image undo_press)]
                                            [(equal? down_status "RESTART") (set! restart_image restart_press)]
                                            [(equal? down_status "MAINMENU") (set! mainmenu_image mainmenu_press)])
                                      "GAME"]
              [(game-area x y) (game_update x y inp)]
              [else "GAME"])))))

(define (game_update x y inp)
  (cond [(mouse=? inp "button-down")
         (begin (cond [sound? (play ding)])
                (cond [(equal? num_players 1)
                       (begin 
                  (set! temp (1-player (make-posn (exact-floor (/ y (/ 720 grid_size)))
                                                  (exact-floor (/ x (/ 720 grid_size))))))
                  "GAME")]
                      [(equal? num_players 2) (2-player (make-posn (quotient y (/ 720 grid_size))
                                                                   (quotient x (/ 720 grid_size))))]
                      [(equal? num_players '2-lan)
                       (2-lan-player "GAME"
                                     (make-posn (quotient y (/ 720 grid_size))
                                         (quotient x (/ 720 grid_size))))]))]
        [(and (mouse=? inp "button-up") (= num_players 1))
         (begin 
           (computer-input temp (make-posn (/ y (/ 720 grid_size))
                                           (/ x (/ 720 grid_size)))))]
        [else "GAME"]))


;;;;;;;;;;;;;;;SETTINGS FRAME DEFINTIONS;;;

(define 1player 1player_select)
(define 2player 2player_normal)

(define grid_size-6  grid_size-6_normal)
(define grid_size-8  grid_size-8_select)
(define grid_size-10  grid_size-10_normal)
(define grid_size-12 grid_size-12_normal)
(define grid_size-14  grid_size-14_normal)
(define grid_size-16  grid_size-16_normal)
(define grid_size-18  grid_size-18_normal)
(define grid_size-20 grid_size-20_normal)

(define sound-yes sound-yes_select)
(define sound-no sound-no_normal)

(define first_color-white first_color-white_select)
(define first_color-black first_color-black_normal)

(define legal_moves-yes legal_moves-yes_select)
(define legal_moves-no legal_moves-no_normal)

(define difficulty-1 difficulty-1_normal)
(define difficulty-2 difficulty-2_normal)
(define difficulty-3 difficulty-3_select)
(define difficulty-4 difficulty-4_normal)
(define difficulty-5 difficulty-5_normal)
;;;;;;;;;;;;;;;;;;;;;;;SETTINGS RESETS;;;;

(define (reset-players)
  (set! 1player 1player_normal)
  (set! 2player 2player_normal))

(define (reset-all-grid-sizes)
  (set! grid_size-6  grid_size-6_normal)
  (set! grid_size-8  grid_size-8_normal)
  (set! grid_size-10  grid_size-10_normal)
  (set! grid_size-12 grid_size-12_normal)
  (set! grid_size-14  grid_size-14_normal)
  (set! grid_size-16  grid_size-16_normal)
  (set! grid_size-18  grid_size-18_normal)
  (set! grid_size-20 grid_size-20_normal))

(define (reset-all-sound)
  (set! sound-yes sound-yes_normal)
  (set! sound-no sound-no_normal))

(define (reset-all-first_move)
  (set! first_color-white first_color-white_normal)
  (set! first_color-black first_color-black_normal))

(define (reset-all-legal_moves)
  (set! legal_moves-yes legal_moves-yes_normal)
  (set! legal_moves-no legal_moves-no_normal))

(define (reset-difficulty)
  (set! difficulty-1 difficulty-1_normal)
  (set! difficulty-2 difficulty-2_normal)
  (set! difficulty-3 difficulty-3_normal)
  (set! difficulty-4 difficulty-4_normal)
  (set! difficulty-5 difficulty-5_normal))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (settings_frame_update x y inp)
  (begin0 (cond
            [(and ((player-button-area 330 260) x y) (mouse=? inp "button-down"))
             (reset-players) (set! 1player 1player_select) (num_players-set! 1) "SETTINGS"]
            [(and ((player-button-area 660 260) x y) (mouse=? inp "button-down"))
                 (reset-players) (set! 2player 2player_select) (num_players-set! 2) "SETTINGS"]
            
                [(and ((grid_size-button-area 350 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-6 grid_size-6_select) (set-main_grid-outline-grid_size 6) "SETTINGS"]
                [(and ((grid_size-button-area 400 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-8 grid_size-8_select) (set-main_grid-outline-grid_size 8) "SETTINGS"]
                [(and ((grid_size-button-area 450 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-10 grid_size-10_select) (set-main_grid-outline-grid_size 10) "SETTINGS"]
                [(and ((grid_size-button-area 500 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-12 grid_size-12_select) (set-main_grid-outline-grid_size 12) "SETTINGS"]
                [(and ((grid_size-button-area 550 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-14 grid_size-14_select) (set-main_grid-outline-grid_size 14) "SETTINGS"]
                [(and ((grid_size-button-area 600 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-16 grid_size-16_select)(set-main_grid-outline-grid_size 16) "SETTINGS"]
                [(and ((grid_size-button-area 650 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-18 grid_size-18_select) (set-main_grid-outline-grid_size 18) "SETTINGS"]
                [(and ((grid_size-button-area 700 340) x y) (mouse=? inp "button-down"))
                 (reset-all-grid-sizes) (set! grid_size-20 grid_size-20_select) (set-main_grid-outline-grid_size 20) "SETTINGS"]
                
                [(and ((sound-button-area 350 420) x y) (mouse=? inp "button-down"))
                 (reset-all-sound) (set! sound-yes sound-yes_select) (set! sound? #t) "SETTINGS"]
                [(and ((sound-button-area 470 420) x y) (mouse=? inp "button-down"))
                 (reset-all-sound) (set! sound-no sound-no_select) (set! sound? #f) "SETTINGS"]
                
                [(and ((first-move-area 350 500) x y) (mouse=? inp "button-down"))
                 (reset-all-first_move) (begin (set! first_color-white first_color-white_select)
                                               (intial_player-set! 1)) "SETTINGS"]
                [(and ((first-move-area 470 500) x y) (mouse=? inp "button-down"))
                 (reset-all-first_move) (begin (set! first_color-black first_color-black_select)
                                               (intial_player-set! 0)) "SETTINGS"]

                [(and ((legal-button-area 350 580) x y) (mouse=? inp "button-down"))
                 (reset-all-legal_moves) (set! legal-moves? #t) (set! legal_moves-yes legal_moves-yes_select) "SETTINGS"]
                [(and ((legal-button-area 470 580) x y) (mouse=? inp "button-down"))
                 (reset-all-legal_moves) (set! legal-moves? #f) (set! legal_moves-no legal_moves-no_select) "SETTINGS"]
                
                [(and (= num_players 1) ((difficulty-button-area 350 660) x y) (mouse=? inp "button-down"))
                 (reset-difficulty) (depth_search-set! 0) (set! difficulty-1 difficulty-1_select) "SETTINGS"]
                [(and (= num_players 1) ((difficulty-button-area 400 660) x y) (mouse=? inp "button-down"))
                 (reset-difficulty) (depth_search-set! 1) (set! difficulty-2 difficulty-2_select) "SETTINGS"]
                [(and (= num_players 1) ((difficulty-button-area 450 660) x y) (mouse=? inp "button-down"))
                 (reset-difficulty) (depth_search-set! 2) (set! difficulty-3 difficulty-3_select) "SETTINGS"]
                [(and (= num_players 1) ((difficulty-button-area 500 660) x y) (mouse=? inp "button-down"))
                 (reset-difficulty) (depth_search-set! 3) (set! difficulty-4 difficulty-4_select) "SETTINGS"]
                [(and (= num_players 1) ((difficulty-button-area 550 660) x y) (mouse=? inp "button-down"))
                 (reset-difficulty) (depth_search-set! 4) (set! difficulty-5 difficulty-5_select) "SETTINGS"]
                
                [((back-button-area 880 660) x y) (back_settings_update inp) ]
                [else (reset-all) "SETTINGS"]) (update-l (list grid_size initial_player))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (game_over_frame_update x y inp)
  (define replay_button_area (game-button-area 855 600))
  (define mainmenu_button_area (game-button-area 855 675))
  (cond [(replay_button_area x y) (replay_update inp)]
        [(mainmenu_button_area x y) (mainmenu_update "GAME_OVER" inp)]
        [else "GAME_OVER"]))

(define (game_over_frame)
  (place-images (list (text/font (winner) 40 "black"
                              "DISTRICTPRO" 'swiss 'normal 'light #f)
                       score_white_icon
                       score_black_icon
                       (score_white_value)
                       (score_black_value)
                      (scale (/ 8 grid_size) (place-images (show-color-l) (show-pos-l) (grid_image)))
                      replay_image mainmenu_image)
                (list (make-posn 855 340)
                      (make-posn 795 100) (make-posn 915 100)
                      (make-posn 795 200) (make-posn 915 200)
                      (make-posn 360 360)
                      (make-posn 855 600)
                      (make-posn 855 675))
                empty_frame))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (1-player position_inp)
  (begin
    (let [(human-updated (update-all-grid position_inp initial_player))]
      (cond ((equal? human-updated "INVALID MOVE") "INVALID MOVE")
            (else (begin (update-current_player) "GAME"))))))


(define (computer-input stat position_inp)
  (if (equal? stat "INVALID MOVE") "GAME"
    (let [(next (delay1 (best-move depth_search main_grid main_outline) 220))]
      (if (equal? next "GAME_OVER") "GAME_OVER"
          (begin (update-all-grid next (- 1 initial_player))
                 (update-current_player) "GAME")))))


(define (2-player position_inp)
  (begin
    (let [(updated (update-all-grid position_inp current_player))]
      (if (equal? updated "INVALID MOVE") "GAME"
          (begin
            (update-current_player)
            (if (null? (next-moves main_grid main_outline current_player)) "GAME_OVER"
                "GAME"))))))

(define (2-lan-player stat position_inp)
  (begin
    (let [(updated (update-all-grid position_inp current_player))]
      (if (equal? updated "INVALID MOVE") "GAME"
          (begin
            (update-current_player)
            (if (null? (next-moves main_grid main_outline current_player)) "GAME OVER"
                "waiting for other player's move..."))))))

(define (main_frame stat)
  (begin (cond [(equal? stat "WELCOME") (welcome_frame)]
        [(and (equal? num_players '2-lan) (equal? stat "GAME")) (2-lan-game_frame)]
        [(equal? stat "GAME") (game_frame)]
        [(or (equal? stat "waiting for other player....")
             (equal? stat "waiting for other player's move...")
             (equal? stat "other player is offline")
             (equal? stat "sry"))
         (place-images (list (text/font stat 15 "black"
                              "DISTRICTPRO" 'swiss 'normal 'light #f))
                       (list (make-posn 855 450))
                       (2-lan-game_frame))]
             
        [(equal? stat "GAME_OVER") (game_over_frame)]
        [(equal? stat "SETTINGS") (settings_frame)]
        [(equal? stat "HOW_TO_PLAY") (howtoplay_frame)]
        [(equal? stat "CREDITS") (credits_frame)]
        [else empty_frame])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (world-make)
  (big-bang "WELCOME"
            (name "Reversi")
            (to-draw main_frame)
            (stop-when (λ (stat) (begin0 (equal? stat "2-lan")
                                         (cond ((equal? stat "2-lan")
                                                (begin (num_players-set! '2-lan)
                                                       (showframe)))))))
            (on-mouse (λ (stat x y inp)
                        (cond [(or (mouse=? inp "enter")
                                   (mouse=? inp "leave")) stat]
                              [(equal? stat "WELCOME") (welcome_frame_update x y inp)]
                              [(equal? stat "GAME") (game_frame_update x y inp)]
                              [(equal? stat "GAME_OVER") (game_over_frame_update x y inp)]
                              [(equal? stat "SETTINGS") (settings_frame_update x y inp)]
                              [(equal? stat "HOW_TO_PLAY") (howtoplay_frame_update x y inp)]
                              [(equal? stat "CREDITS") (credits_frame_update x y inp)]
                              [else (error "Wrong stat passed")])))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-world nam)
  (big-bang "GAME"
            (name nam)
            (to-draw main_frame)
            (on-receive receive_msg_update)
            (on-mouse (λ (stat x y inp)
                        (cond [(and (equal? stat "GAME") (mouse=? inp "button-down"))
                               (make-package (game_frame_update x y inp)
                                             (list (quotient y (/ 720 grid_size))
                                                   (quotient x (/ 720 grid_size))))]
                              [else stat])))
            (register ip-address)))

(define (receive_msg_update state sexp)
         (cond
           [(and (list? sexp) (string? (cadr sexp)))
            (set-total-grid-initial-player (car sexp)) (cadr sexp)]
           [(equal? sexp "waiting for other player....") "waiting for other player...."]
           [(equal? sexp "waiting for other player's move...") "waiting for other player's move..."]
           [(equal? sexp "other player is offline") "other player is offline"]
           [(equal? sexp "sry") "sry"]
           [else (if (equal? sexp "start") "GAME"
                     (let*([x (car sexp)]
                           [y (cadr sexp)]
                           [position_inp (make-posn x y)])
                       (begin (2-player position_inp) "GAME")))]))

;;;;;;;;;;;;;;;;;;;;;;GUI;;;;;;;;;;
(define frame (new frame% [label "player-details"]
                          [width 300]
                          [height 300]))
(define button1 (new button%
                    [parent frame]
                    [label "RUN"]
                    [callback (lambda (button event)
                                (run-universe))]))

(define name-gui (new text-field%
                  [label "NAME"]
                  [parent frame]))

(define Ip-address (new text-field%
                        [label "IP-ADDRESS"]
                        [parent frame]))

(define button (new button%
                    [parent frame]
                    [label "PLAY"]
                    [callback (lambda (button event)
                                (begin (player-name-set! (send name-gui get-value))
                                       (ip-address-set! (send Ip-address get-value))
                                       (create-world player-name)))]))

(define msg (new message% [parent frame]
                          [label "Note: If u want to be host 
              please press the run 
              button first and then fill
              the text-boxes before
              clicking play button "]))
(define (showframe)
  (send frame show #t))
;;;;;;;;;;;;;;;;;;;;;;;;;FINAL CALL
(world-make)

;;;;;;;;;;;;;;;;;;;;;;;;;THE END;;;;;;;;
