;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ver_15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


#| -- List of to do's


--> Update image colour and image type during level update. 
--> TEST cases pending for "shape_drawer"
                           "draw_object_left"
                           "draw_object_right"
                           "intersection_checker"
                           "status_check"

--> Can add after creating the world.
--> Change EMPTY to respective draw worlds in "shape_drawer"
-->   This is while updating the level and so on.  (colour (random available_colour))
-->  Upadare shape and colour for both right and left when updating the level.
-->  Use world in the void region instead of level. Change define void_region also. 
-->  Change Scene parameter in all draw functions according to signature.

--> Change the definition of tick to world and replace with respective values.
--> First do with level and then shift to make world.

--> Write the "win" function
--> Write the "on-stop" function. 
--> Modify "draw_void_region" to call "win" function to stop the game. 

--> Key
--> Status fn
--> Tick FN
--> Win fun
--> On-Stop FN 



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
(define left_obj_starting_posn (make-posn (/ world-height 2) 0))

; Right object starting postion
(define right_obj_starting_posn (make-posn (/ world-height 2) world-width))

; Initial current game status
(define initial_current_game_status #true)

; Initial Final game status
(define initial_final_game_status #false)

; Object width
(define object_width (* 0.05 world-width))

; Object Height
(define object_height (* 0.05 world-height))

; Number of available shapes
(define available_shapes 1)

; Number of available colours
(define available_color 6)

; Initial void regiom x axis
(define initial_void_region_x (/ world-width 2))

; Initial void regiom y axis
(define initial_void_region_y (/ world-height 2))

; Level Display X Coordinate
(define level_disp_x (- world-width 100))

; Level Display Y Coordinate
(define level_disp_y (* world-height 0.125))


;; a Shape is
;;    (make-Shape  number -- shape type(defined in a shape function called using structural decomposition)
;;                 posn   -- image position
;;                 color  -- image color)

(define-struct Shape ( type pos colour ))



;; a World is
;;   (make-world number  -- level
;;               Shape   -- left image
;;               Shape   -- right image 
;;               boolean -- True if running
;;                       -- False if button clicked
;;               boolean -- False if game is not over
;;                       -- True if game is over)

(define-struct world (level left_img right_img current_game_status final_game_status))

#|

;; Function Wish list


; world-stop : world -> boolean
; Check world-status and stops
; accordingly. 

(define (world-stop world)
  (cond
    [...]
    [else...]))


; win_game : world -> Image
; Toggles the world-status to false
; Displays that the player has won the game
; and ends the game by calling world-stop.
; Called by win-game.
(define (win_game world))






***************************************************************************************************************************************************************************************************
**************************************************************************************************************************************************************************************************
**************************************************************************************************************************************************************************************************

; key: World Key_Event -> World
; This function would check if a key was pressed or not.
; Considering SpaceBar for the game play
; Toggles the "current_game_status" in the world 
; and calls the "status_check" function.
; { This is called by on-key event in Big-Bang }
; { This calls the "status_check" function }

; Strategy : Structural Decomposition

; Example : If a key is pressed it stops the game execution and checks if there was an intersection
;           between the two images.If, yes then it would update the level and other parameters and
;           restart the game. 

(define (key world key_event)
  (cond
    [(string=? key_event " ") status_check((make-world
                                           world-level
                                           (make-Shape world-left_img-type  world-left_img-pos  world-right_img-colou)
                                           (make-Shape world-right_img-type world-right_img-pos world-right_img-colour)
                                           (not world-current_game_status)
                                           world-final_game_status))]
    [else world]))



; color_selector : Number -> String
; Returns the colour assigned to
;; to the given number.


; Strategy : Structural Decomposition

; Example :
;           (color_selector 1) -> would return red
;           (color_selector 2) -> would return yellow


(define (color_selector option)
  (cond
    [(= 0 option) "purple"]
    [(= 1 option) "red"]
    [(= 2 option) "yellow"]
    [(= 3 option) "blue"]
    [(= 4 option) "orange"]
    [(= 5 option) "grey"]
    [(= 6 option) "green"] ))

(check-expect (color_selector 0) "purple")
(check-expect (color_selector 2) "yellow")
(check-expect (color_selector 6) "green") 




;; shape_drawer : Number Boolean World -> Image
;; Abstract function for "draw_object_left" &
;; "draw_object_right", the type would define the
;; object and the boolean is to decide the if the call is from
;; "draw_object_left" or "draw_object_right". This draws the image accordingly
;; using the parameters from the world. 
;; true -> left_img
;; false -> right_img
;; { Helper function called by "draw_object_right" and "draw_object_left"}

;; Stratergy : Structural Decomposition

;; Example :
;;             (shape_drawer 0 #true world)
;;                            
;;             --> This would draw an ellipse as the left side image
;;                 getting all parameters from the world. 



(define (shape_drawer type selecter world)
  (cond
    [(= 0 type)
     (cond
       [(equal? #true selecter) (place-image (ellipse object_width object_height "solid" world-left_img-colour)
                                       (posn-x world-left_img-pos) (posn-y world-left_img-pos)
                                       EMPTY)]
       [else (place-image (ellipse object_width object_height "solid" world-right_img-colour)
                                       (posn-x world-right_img-pos) (posn-y world-right_img-pos)
                                       EMPTY)])]
    [else
     (cond
       [(equal? #true selecter) (place-image (rectangle object_width object_height "solid" world-left_img-colour)
                                       (posn-x world-left_img-pos) (posn-y world-left_img-pos)
                                       EMPTY)]
       [else (place-image (rectangle object_width object_height "solid" world-left_img-colour)
                                       (posn-x world-right_img-pos) (posn-y world-right_img-pos)
                                       EMPTY)])])) 



;; This make-world will come in handy.
===================================================================================================================================================================
(make-world
                                                         world-level
                                                         (make-Shape (random available_shapes) world-left_img-pos (colour_selecter (random available_colour)))
                                                         (make-Shape world-right_img-type world-right_img-pos world-right_img-colour)
                                                         world-current_game_status
                                                         world-final_game_status)))

===================================================================================================================================================================
; intersection_checker : world -> boolean
; Checks if the right and the left objects
; intersected or not.
; Returs #true if it did or #false
; { This is called by "status_check" (helper function)}


; Strategy : Structural Decomposition

; Example : Both Images are in iniotial position and this function returns #false
;           If it intersected and then this function would return #true.

(define (intersection_checker world)
  (cond
    [(<= (posn-x world-left_img-pos) (+ (posn-x world-left_img-pos) (object_width)) (- (posn-x world-right_img-pos) (object_width)) (posn-x world-right_img-pos)) #true]
    [else #false]))



; status_check : world -> world

; All this only if world-status is false. 
; Using the positions of the right and
; left image, it checks if it has intersected
; or not. If true it updates the level and resets
; image positions.
; If it did not intersect, then it just resets the positions
;(Basically the same as former without updating the level).
; Called by the "key" event, when a button is clicked( only when
; True -> False during the button click )
; Displays game status to user and prompts by displaying
; appropriate message.
; { This is called by "key" }
; { This calls "draw_level" }

(define (status_check world)
  (cond 
    [ (intersection_checker world) (draw_level (make-world (+ 1 world-level)
                                                           (make-Shape (random (available_shapes)) left_obj_starting_pos  (color_selector (random (available_color))))
                                                           (make-Shape (random (available_shapes)) right_obj_starting_pos (color_selector (random (available_color))))
                                                           world-current_game_status
                                                           world-final_game_status))]

    [else                          (draw_level (make-world  world-level
                                                            (make-Shape (random (available_shapes)) left_obj_starting_pos  (color_selector (random (available_color))))
                                                            (make-Shape (random (available_shapes)) right_obj_starting_pos (color_selector (random (available_color))))
                                                            world-current_game_status
                                                            world-final_game_status))]))


; draw_level : world -> Image
; Using the level from the world
; & using the "world-width" and "world-height"
; the level is displayed for the user to
; keep a tab. In the top right corner.
; This uses "EMPTY" while placing image.
; "EMPTY" sets the scene based on the world-width and world-height.
; {This is called by Big Bang to draw }
; {This calls "draw_void_region" }

; Stratergy : Function Composition

; Example :
;             This displays the game score to the user in the right top corner. 

;(define (draw_level world)
(define (draw_level level)
  (place-image (text (number->string level) 50 "black")
               level_disp_x level_disp_y
               EMPTY))



; draw_void_region : World -> Image
; To draw the void region based on the level from the world.
; Using the world-level and the "void_region_ramp_up" rate draws the void region.
; Game stops when the void_region is equalto or greater than the width. That's the max-level.
; Call "win_game" function accordingly.
; {This will be called by "draw_level"}
; {This calls "draw_object_left"}

; Strategy : Function Composition

;(define (draw_void_region world)
(define (draw_void_region level)
  (place-image (rectangle (+ (* (* level void_region_ramp_up) void_region_width) void_region_width) void_region_height "solid" "LightSlateGray")
                (- initial_void_region_x (/ 2 (+ (* (* level void_region_ramp_up) void_region_width) void_region_width))) initial_void_region_y
                EMPTY))




; draw_object_left : World -> Image
; A random number will be used to draw the shape. 
; Using that random number the respective shapes will be placed (like switch case,
; we may have  multiple predefined shapes with set "object_width" & "object_height")
; In the respective left position as defined in the world the image is drawn.
; For the final scene parameter while placing the image, this calls "draw_shape_right"
;{This is called by "draw_void_region"}
;{This calls "draw_object_right"}

; Strategy : Function Composition

;; Example :
;;          This would draw the left side image on the screen with the
;;          help of shape_drawer where we use the concept of abstraction. 

(define (draw_object_left world)
  (shape_drawer (world-left_img-type) #true world))




; draw_object_right : World -> Image
; A random number will be used to draw the shape. 
; Using that random number the respective shapes will be placed (like switch case,
; we may have  multiple predefined shapes with set "object_width" & "object_height")
; In the respective right position as defined in the world the image is drawn.
; For the final scene parameter while placing the image, this calls "draw_void_region"
; { This is called by "draw_object_left"}
; { This calls EMPTY SCENE }

; Strategy : Function Composition

;; Example :
;;          This would draw the right side image on the screen with the
;;          help of shape_drawer where we use the concept of abstraction. 


(define (draw_object_right world)
   (shape_drawer (world-right_img-type) #false world))

***************************************************************************************************************************************************************************************************
**************************************************************************************************************************************************************************************************
**************************************************************************************************************************************************************************************************
|#


; tick : world -> world
; Updates the world by updating the
; positions of the left and right image respectively.(Using the level,
; "initial_object_speed" & "object_speed_ramp_up", the positions
; are updated accordingly. ( This would make the shapes move faster
; as the level increases )
; When objects reach the corner's the positions are reset. 

;(define (tick world)
(define (tick level)
  (cond
    [...]
    [else...]))





#|
; Big bang definition
(big-bang (make-world
                       start_level
                       (make-Shape (random available_shapes) left_obj_starting_posn "red")
                       (make-Shape (random available shapes) right_obj_starting_posn "blue")
                       initial_current_game_status
                       initial_final_game_status)

          [on-tick tick 1/250]
          [on-key key]
          [to-draw draw_object_left]
          [stop-when world-stop])

|#