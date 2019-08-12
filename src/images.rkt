#lang racket

(require 2htdp/image)

(provide (all-defined-out))

(define backg (bitmap "images/background.png"))
(define current_indicator (bitmap "images/blue_box.png"))

(define w (bitmap "images/white.png"))
(define b (bitmap "images/black.png"))
(define t (bitmap "images/trans.png"))
(define r_w (bitmap "images/ring_white.png"))
(define r_b (bitmap "images/ring_black.png"))
(define score_white (bitmap "images/score_white.png"))

(define mainmenu_normal (bitmap "images/mainmenu_normal.png"))
(define mainmenu_hover (bitmap "images/mainmenu_hover.png"))
(define mainmenu_press (bitmap "images/mainmenu_press.png"))
(define restart_normal (bitmap "images/restart_normal.png"))
(define restart_hover (bitmap "images/restart_hover.png"))
(define restart_press (bitmap "images/restart_press.png"))
(define undo_normal (bitmap "images/undo_normal.png"))
(define undo_hover (bitmap "images/undo_hover.png"))
(define undo_press (bitmap "images/undo_press.png"))
(define replay_normal (scale 0.708 (bitmap "images/replay_normal.png")))
(define replay_hover (scale 0.708 (bitmap "images/replay_hover.png")))
(define replay_press (scale 0.708 (bitmap"images/replay_press.png")))
(define logo (bitmap "images/logo.png"))
(define play1 (bitmap "images/play.png"))
(define play_hover (bitmap "images/play_hover.png"))
(define settings (bitmap "images/settings.png"))
(define settings_hover (bitmap "images/settings_hover.png"))
(define credits (bitmap "images/credits.png"))
(define credits_hover (bitmap "images/credits_hover.png"))
(define howto (bitmap "images/howto.png"))
(define howto_hover (bitmap "images/howto_hover.png"))
(define 2-lan (bitmap "images/2-lan.png"))
(define 2-lan_hover (bitmap "images/2-lan_hover.png"))

(define how_to_play (bitmap "images/how_to_play.png"))

(define credits_scr (bitmap "images/credits-screen.png"))

(define settings_logo (bitmap "images/settings_head.png"))

(define back_s (bitmap "images/settings-back_normal.png"))
(define back_s_hover (bitmap "images/settings-back_select.png"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define 1player_normal (bitmap "images/1player_normal.png"))
(define 2player_normal (bitmap "images/2player_normal.png"))
(define 1player_select (bitmap "images/1player_select.png"))
(define 2player_select (bitmap "images/2player_select.png"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define grid_size? (text/font "GRID SIZE:" 25 "black"
                           "DISTRICTPRO" 'modern 'normal 'light #f))


(define grid_size-6_normal (bitmap "images/grid_size-6_normal.png"))
(define grid_size-8_normal (bitmap "images/grid_size-8_normal.png"))
(define grid_size-10_normal (bitmap "images/grid_size-10_normal.png"))
(define grid_size-12_normal (bitmap "images/grid_size-12_normal.png"))
(define grid_size-14_normal (bitmap "images/grid_size-14_normal.png"))
(define grid_size-16_normal (bitmap "images/grid_size-16_normal.png"))
(define grid_size-18_normal (bitmap "images/grid_size-18_normal.png"))
(define grid_size-20_normal (bitmap "images/grid_size-20_normal.png"))

(define grid_size-6_select (bitmap "images/grid_size-6_select.png"))
(define grid_size-8_select (bitmap "images/grid_size-8_select.png"))
(define grid_size-10_select (bitmap "images/grid_size-10_select.png"))
(define grid_size-12_select (bitmap "images/grid_size-12_select.png"))
(define grid_size-14_select (bitmap "images/grid_size-14_select.png"))
(define grid_size-16_select (bitmap "images/grid_size-16_select.png"))
(define grid_size-18_select (bitmap "images/grid_size-18_select.png"))
(define grid_size-20_select (bitmap "images/grid_size-20_select.png"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define sound-toggle (text/font "SOUND: " 25 "black"
                           "DISTRICTPRO" 'modern 'normal 'light #f))
(define sound-yes_normal (bitmap "images/sound-yes_normal.png"))
(define sound-yes_select (bitmap "images/sound-yes_select.png"))
(define sound-no_normal  (bitmap "images/sound-no_normal.png"))
(define sound-no_select  (bitmap "images/sound-no_select.png"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define first_move? (text/font "FIRST MOVE: " 25 "black"
                           "DISTRICTPRO" 'modern 'normal 'light #f))
(define first_color-white_normal (bitmap "images/first_color-white_normal.png"))
(define first_color-black_normal (bitmap "images/first_color-black_normal.png"))
(define first_color-white_select (bitmap "images/first_color-white_select.png"))
(define first_color-black_select (bitmap "images/first_color-black_select.png"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define show_legal_moves? (text/font "LEGAL MOVES: " 25 "black"
                           "DISTRICTPRO" 'modern 'normal 'light #f))
(define legal_moves-yes_normal (bitmap "images/legal_moves-yes_normal.png"))
(define legal_moves-no_normal (bitmap "images/legal_moves-no_normal.png"))
(define legal_moves-yes_select (bitmap "images/legal_moves-yes_select.png"))
(define legal_moves-no_select (bitmap "images/legal_moves-no_select.png"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define difficulty? (text/font "DIFFICULTY: " 25 "black"
                           "DISTRICTPRO" 'modern 'normal 'light #f))

(define difficulty-1_normal (bitmap "images/difficulty-1_normal.png"))
(define difficulty-2_normal (bitmap "images/difficulty-2_normal.png"))
(define difficulty-3_normal (bitmap "images/difficulty-3_normal.png"))
(define difficulty-4_normal (bitmap "images/difficulty-4_normal.png"))
(define difficulty-5_normal (bitmap "images/difficulty-5_normal.png"))

(define difficulty-1_select (bitmap "images/difficulty-1_select.png"))
(define difficulty-2_select (bitmap "images/difficulty-2_select.png"))
(define difficulty-3_select (bitmap "images/difficulty-3_select.png"))
(define difficulty-4_select (bitmap "images/difficulty-4_select.png"))
(define difficulty-5_select (bitmap "images/difficulty-5_select.png"))

(define credits_screen_back (bitmap "images/credits_back_hover.png"))
