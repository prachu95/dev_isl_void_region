;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ver_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Proposal :

               1. It is a single player game. 

               2. It is a game which ramps up as and when a user completes a given level. 

               3. Two different shaped objects come from two sides of the screen. 

               4. Centre region is blacked out. 

               5. User can see the two objects until it enters the centre (defined region) 

               6. He has to click a button the exact time at which the two objects collide. ( Calculate and Guess )

               7. If he guess's it right he advances to the next level. Else he'll be asked to play the same level again. 

               8. If he doesn't click and the objects leave the screen new objects will come. 

               9. Or if presses the button at the wrong time. We can display an error message and show an option to replay the same level. 

               10. The level increases by speeding up the pace at which objects come in and by increasing the size of the void region. 

               11. Will create multiple shaped objects and try to make sure that two same objects never come together.
                   The object selection will be randomized.

|#


(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/abstraction)

(define world-width 1200)
(define world-height 600)
(define EMPTY (empty-scene world-width world-height))

; Initial width to start of with. Will be drawn to the right and left of the widhth's centre. 
(define void_region_width (* world-width (/ 1 8)))

; Void region height = World's height.
(define void_region_height world-height)

; Void region size ramp up factor.
(define void_region_ramp_up 0.1)

; Initial speed of object
(define initial_object_speed 300)

; Speed ramp up factor
(define object_speed_ramp_up 0.1)

; Initial level to start with
(define start_level 1)

; Left object starting postion
(define left_obj_starting_posn 0)

; Right object starting postion
(define right_obj_starting_posn world-width)

; Initial game status
(define initial_game_status #true)




;; a World is
;;   (make-world number  -- level
;;               posn    -- left image position
;;               posn    -- right image position
;;               boolean -- True if running
;;                       -- False if button clicked 
(define-struct world (level left_pos right_pos status))



;; Function Wish list


; draw_shapes : World -> Image
; A random number will be used to draw the shape.
; Using that random number the respective shapes will be printed
; In the respective left and right positions as defined in the world.

(define (draw_shapes world)
  (cond
    [...]
    [else...]))
  
; key: World Key Event -> World
; This function would check if a key was pressed or not.
; Considering SpaceBar for the game play
; Toggles the world-status and calls
; the status check function. 

(define (key world key_event)
  (cond
    [...]
    [else...]))


; draw_void_region : World -> Image
; To draw the void region based on the level from the world.
; Using the level and the "void_region_ramp_up" rate draws the void region.
; Game stops when the void_region is equalto or greater than the width. That's the max-level.
; Call win_game function accordingly


(define (draw_void_region world)
  (cond
    [...]
    [else...]))


; tick : world -> world
; Updates the world by updating the
; positions of the left and right image respectively.
; When objects reach the corner's the positions are reset. 

(define (tick world)
  (cond
    [...]
    [else...]))


; game_speed : world -> number
; Using the level from the world the game speed
; is calculated and returned as a fraction
; which is used in the big bang. All this is done
; using the "initial_object_speed" & "object_speed_ramp_up"

(define (game_speed world)
  (cond
    [...]
    [else...]))


; win_game : world -> Image
; Toggles the world status to false
; Displays that the player has won the game

(define (win_game world))

; world-stop : world -> boolean
; Check the status and returns the opposite
; for the world to continue.

(define (world-stop world)
  (cond
    [...]
    [else...]))


; draw_level : world -> Image
; Using the level from the world
; & using the "world-width" and "world-height"
; the level is displayed for the user to
; keep a tab. In the top right corner. 

(define (draw_level world))


; status_check : world -> world
; All this only if world-status is false. 
; Using the positions of the right and
; left image, it checks if it has intersected
; or not. If true it updates the level and resets
; image positions and the game play to true.
; If it did not intersect, then it resets the positions
; and changes the world-status to true. ( Basically the
; same as former without updating the level).

(define (status_check world)
  (cond
    [...]
    [else...]))

; Big bang definition
(big-bang (make-world start_level left_obj_starting_posn right_obj_starting_posn initial_game_status)
          [on-tick tick game_speed]
          [on-key key]
          [to-draw draw]
          [stop-when world-stop])