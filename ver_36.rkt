;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ver_36) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


--> TEST cases pending for "shape_drawer"
                           "draw_object_left"
                           "draw_object_right"
                           "intersection_checker"
                           "status_check"



-->  Upadare shape and colour for both right and left when updating the level.

 



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

; Constant Position Update for objects 
(define const_obj_pos_update 20)

; Speed ramp up factor
(define object_speed_ramp_up 2)

; Initial level to start with
(define start_level 1)

; Left object starting postion
(define left_obj_starting_posn (make-posn 0 (/ world-height 2)))

; Right object starting postion
(define right_obj_starting_posn (make-posn world-width (/ world-height 2)))

; Initial current game status
(define initial_current_game_status #false)

; Object width
(define object_width (* 0.05 world-width))

; Object Height
(define object_height (* 0.05 world-height))

; Number of available shapes
(define available_shapes 10)

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
(define-struct world(level left_img right_img current_game_status))


; win_game : -> Image
; Displays that the player has won the game
; Called by "draw_void_region"
(define win_game (place-image
                  (text "Wheyy..!! You Won the Game...!!!" 25 "black")
                  (/ world-width 2) (/ world-height 2)
                  EMPTY))


; common_posn_updater : posn number symbol -> posn
; Abstraction function for "left_obj_posn_updater"
; & "left_obj_posn_updater".

; Strategy : Function Composition

; Example : If a position (10, 300, +) in  the 4th level is given as an input
;           the output will be (38, 300)
;           If a position (100, 300, -) in  the 4th level is given as an input
;           the output will be (72, 300)


(define (common_posn_updater pos level symbol)
  (make-posn (symbol (posn-x pos) const_obj_pos_update (* object_speed_ramp_up level))
             (posn-y pos)))

;; Test Cases:
(check-expect (common_posn_updater (make-posn 0 0) 0 +) (make-posn 20 0))
(check-expect (common_posn_updater (make-posn 0 0) 1 +) (make-posn 22 0))
(check-expect (common_posn_updater (make-posn 400 300) 10 +) (make-posn 440 300))
(check-expect (common_posn_updater (make-posn 900 300) 20 +) (make-posn 960 300))
(check-expect (common_posn_updater (make-posn 0 0) 0 -) (make-posn -20 0))
(check-expect (common_posn_updater (make-posn 0 0) 1 -) (make-posn -22 0))
(check-expect (common_posn_updater (make-posn 400 300) 10 -) (make-posn 360 300))
(check-expect (common_posn_updater (make-posn 900 300) 20 -) (make-posn 840 300))
 


; left_obj_posn_updater : posn number -> posn
; Updates the position of the left object, using the level,
; "initial_object_speed" & "object_speed_ramp_up".
; { Helper function for "tick" }

; Strategy : Function Composition

; Example : If a position (10, 300) in  the 4th level is given as an input
;           the output will be (38, 300)

(define (left_obj_posn_updater pos level)
  (common_posn_updater pos level +))

;; Test cases :

(check-expect (left_obj_posn_updater (make-posn 0 0) 0) (make-posn 20 0))
(check-expect (left_obj_posn_updater (make-posn 0 0) 1) (make-posn 22 0))
(check-expect (left_obj_posn_updater (make-posn 400 300) 10) (make-posn 440 300))
(check-expect (left_obj_posn_updater (make-posn 900 300) 20) (make-posn 960 300))



; right_obj_posn_updater : posn -> posn
; Updates the position of the right object, using the level,
; "initial_object_speed" & "object_speed_ramp_up".
; { Helper function for "tick" }
; {Uses  "common_posn_updater" }

; Strategy : Function Composition

; Example :  If a position (100, 300) in  the 4th level is given as an input
;            the output will be (72, 300)

(define (right_obj_posn_updater pos level)
  (common_posn_updater pos level -))

;; Test cases :

(check-expect (right_obj_posn_updater (make-posn 0 0) 0) (make-posn -20 0))
(check-expect (right_obj_posn_updater (make-posn 0 0) 1) (make-posn -22 0))
(check-expect (right_obj_posn_updater (make-posn 400 300) 10) (make-posn 360 300))
(check-expect (right_obj_posn_updater (make-posn 900 300) 20) (make-posn 840 300))

; world_posn_updater: world posn posn -> world
; Updates the left and right object posns
; with the given posns.
;{Called by tick}

; Strategy : Function Composition

; Example : Gets a world and the initial posns
;           as the two posns, then it replaces the
;           world posns with the initial posns.


(define (world_posn_updater world posn_l posn_r)
  (make-world       (world-level world)
                    (make-Shape (Shape-type (world-left_img world))  posn_l  (Shape-colour (world-left_img world)))
                    (make-Shape (Shape-type (world-right_img world)) posn_r (Shape-colour (world-right_img world)))
                    (world-current_game_status world)))

;; Test Cases

(check-expect (world_posn_updater (make-world
                                               start_level
                                               (make-Shape 1 left_obj_starting_posn "red")
                                               (make-Shape 2 right_obj_starting_posn "blue")
                                               initial_current_game_status)
                                  (make-posn 100 100) (make-posn 300 300))
                                  (make-world
                                               start_level
                                               (make-Shape 1 (make-posn 100 100) "red")
                                               (make-Shape 2 (make-posn 300 300) "blue")
                                               initial_current_game_status))

(check-expect (world_posn_updater (make-world
                                               start_level
                                               (make-Shape 1 (make-posn 250 300) "red")
                                               (make-Shape 2 (make-posn 300 250) "blue")
                                               initial_current_game_status)
                                  left_obj_starting_posn right_obj_starting_posn)
                                  (make-world
                                               start_level
                                               (make-Shape 1 left_obj_starting_posn "red")
                                               (make-Shape 2 right_obj_starting_posn "blue")
                                               initial_current_game_status))




  
  

; tick : world -> world
; Updates the world by updating the
; positions of the left and right image respectively.(Using the 
; "left_obj_posn_updater" & "right_obj_posn_updater"
; This would make the shapes move faster
; as the level increases
; When objects reach the corner's the positions are reset. 

; Strategy : Structural Decomposition

; Example : Updates the position of the object so that it moves,
;           changes the position basically. 


(define (tick world)
  (cond
    [(world-current_game_status world)
     (cond
       [(or (>= (posn-x (Shape-pos (world-left_img world))) world-width)
            (<= (posn-x (Shape-pos (world-right_img world))) 0))

        (world_posn_updater world
                            left_obj_starting_posn
                            right_obj_starting_posn)]

       [else
        (world_posn_updater world
                            (left_obj_posn_updater  (Shape-pos (world-left_img world))  (world-level world))
                            (right_obj_posn_updater (Shape-pos (world-right_img world)) (world-level world)))])]                                                       
                                                                                   
    [else world]))


;; Test Cases:
(check-expect (tick (make-world 1
                                (make-Shape 1 (make-posn 0 300) "red")
                                (make-Shape 2 (make-posn 1200 300) "yellow")
                                #false))
              (make-world 1
                          (make-Shape 1 (make-posn 0 300) "red")
                          (make-Shape 2 (make-posn 1200 300) "yellow")
                          #false))


(check-expect (tick (make-world 1
                                (make-Shape 1 (make-posn 1220 300) "red")
                                (make-Shape 2 (make-posn 1200 300) "yellow")
                                #true))
              (make-world 1
                          (make-Shape 1 (make-posn 0 300) "red")
                          (make-Shape 2 (make-posn 1200 300) "yellow")
                          #true))

(check-expect (tick (make-world 1
                                (make-Shape 1 (make-posn 1100 300) "red")
                                (make-Shape 2 (make-posn -20 300) "yellow")
                                #true))
              (make-world 1
                          (make-Shape 1 (make-posn 0 300) "red")
                          (make-Shape 2 (make-posn 1200 300) "yellow")
                          #true))

(check-expect (tick (make-world 10
                                (make-Shape 1 (make-posn 400 300) "red")
                                (make-Shape 2 (make-posn 400 300) "yellow")
                                #true))
              (make-world 10
                          (make-Shape 1 (make-posn 440 300) "red")
                          (make-Shape 2 (make-posn 360 300) "yellow")
                          #true))
              
 


; void_region_calc : world -> Number
; Calcs the next void region width
; using the exsisting world values &
; ramp_up factors.

; Strategy : Function Composition

; Example : Calculates the next void_region value
;           using the exsisting value.


(define (void_region_calc world)
  (+ (* (* (world-level world) void_region_ramp_up) void_region_width) void_region_width))



;; Test Cases
(check-expect (void_region_calc (make-world 1
                                                   (make-Shape 1 (make-posn 0 300) "red")
                                                   (make-Shape 2 (make-posn 1200 300) "yellow")
                                                   #true))
              165)

(check-expect (void_region_calc (make-world 10
                                                   (make-Shape 1 (make-posn 0 300) "red")
                                                   (make-Shape 2 (make-posn 1200 300) "yellow")
                                                   #true))
              300)

(check-expect (void_region_calc (make-world 100
                                                   (make-Shape 1 (make-posn 0 300) "red")
                                                   (make-Shape 2 (make-posn 1200 300) "yellow")
                                                   #true))
              1650)
              



; void_region_out_of_bounds : world -> boolean
; Returns #true if the new void region will
; go beyond the world-width.

; Strategy : Function Composition

; Example : Returns #true if the the next void_regiom
;           is greater than the world-width

(define (void_region_out_of_bounds world)
  (>= (void_region_calc world)
      world-width))


;; Test Cases:
(check-expect (void_region_out_of_bounds (make-world 1
                                                     (make-Shape 1 (make-posn 0 300) "red")
                                                     (make-Shape 2 (make-posn 1200 300) "yellow")
                                                     #true))
              #false)

(check-expect (void_region_out_of_bounds (make-world 100
                                                     (make-Shape 1 (make-posn 0 300) "red")
                                                     (make-Shape 2 (make-posn 1200 300) "yellow")
                                                     #true))
              #true)

  

; world-stop : world -> boolean
; Stops when the void region is >=
; the world-width
(define (world-stop world)
  (void_region_out_of_bounds world))


;; Test Cases:
(check-expect (world-stop (make-world 100
                                      (make-Shape 1 (make-posn 0 300) "red")
                                      (make-Shape 2 (make-posn 1200 300) "yellow")
                                      #true))
              #true)

(check-expect (world-stop (make-world 10
                                      (make-Shape 1 (make-posn 0 300) "red")
                                      (make-Shape 2 (make-posn 1200 300) "yellow")
                                      #true))
              #false)


; key: World Key_Event -> World
; This function would check if a key was pressed or not.
; Considering SpaceBar for the game play
; Toggles the "current_game_status" in the world 
; and calls the "status_check" function.
; { This is called by on-key event in Big-Bang }
; { This calls the "status_check" function }

; Strategy : Structural Decomposition

; Example : Calls "status_check" and toggles "world-current_game_status"
;           If #true is made #false and vice versa. 

(define (key world key_event)
  (cond
    [(string=? key_event " ") (status_check (make-world (world-level world)
                                                        (world-left_img world)
                                                        (world-right_img world)
                                                   (not (world-current_game_status world)))
                                            (random available_shapes)
                                            (random available_color)
                                            (random available_shapes)
                                            (random available_color))]
    [else world]))



;; Test Case written but this one is isolated as it has all the random
;; functions.


(check-expect (key (make-world 10
                               (make-Shape 1 left_obj_starting_posn "red")
                               (make-Shape 2 right_obj_starting_posn "yellow")
                               #true)
                   "a")

              (make-world 10
                               (make-Shape 1 left_obj_starting_posn "red")
                               (make-Shape 2 right_obj_starting_posn "yellow")
                               #true))

 

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
(check-expect (color_selector 1) "red")
(check-expect (color_selector 2) "yellow")
(check-expect (color_selector 3) "blue")
(check-expect (color_selector 4) "orange")
(check-expect (color_selector 5) "grey")
(check-expect (color_selector 6) "green") 



;; shape_drawer : Number String -> Image
;; Returns the respective shape
;; according to the number with the
;; given colour. 
;; { Helper function called by "draw_object_right" and "draw_object_left"}

;; Stratergy : Structural Decomposition

;; Example : (shape_drawer 0 "red")
;;            will return a red coloured ellipse 
         

(define (shape_drawer type colour)
  (cond
    [(= type 0) (ellipse object_width object_height "solid" colour)]
    [else (rectangle object_width object_height "solid" colour)]))


;; Test Cases:
(check-expect (shape_drawer 0 "red")
              (ellipse object_width object_height "solid" "red"))

(check-expect (shape_drawer 1 "blue")
              (rectangle object_width object_height "solid" "blue"))


    


; intersection_checker : world -> boolean
; Checks if the right and the left objects
; intersected or not.
; Returs #true if it did or #false
; { This is called by "status_check" (helper function)}


; Strategy : Structural Decomposition

; Example : Both Images are in iniotial position and this function returns #false
;           If it intersected and then this function would return #true.

(define (intersection_checker world)
  (and (<= (- (posn-x (Shape-pos (world-right_img world)))
              (posn-x (Shape-pos (world-left_img world))))
           object_width)

       (and (>= (posn-x (Shape-pos (world-left_img world)))
                (- initial_void_region_x (/ 2 (void_region_calc world))))

            (<= (posn-x (Shape-pos (world-left_img world)))
                (+ initial_void_region_x (void_region_calc world))))))


;; Test Cases:

(check-expect (intersection_checker (make-world 1
                                                (make-Shape 1 (make-posn 0 300) "red")
                                                (make-Shape 2 (make-posn 1200 300) "yellow")
                                                #true))
              #false)

(check-expect (intersection_checker (make-world 1
                                                (make-Shape 1 (make-posn 1190 300) "red")
                                                (make-Shape 2 (make-posn 1200 300) "yellow")
                                                #true))
              #false)

(check-expect (intersection_checker (make-world 1
                                                (make-Shape 1 (make-posn 1200 300) "red")
                                                (make-Shape 2 (make-posn 0 300) "yellow")
                                                #true))
              #false)


(check-expect (intersection_checker (make-world 1
                                                (make-Shape 1 (make-posn 600 300) "red")
                                                (make-Shape 2 (make-posn 600 300) "yellow")
                                                #true))
              #true)

(check-expect (intersection_checker (make-world 10
                                                (make-Shape 1 (make-posn 600 300) "red")
                                                (make-Shape 2 (make-posn 600 300) "yellow")
                                                #true))
              #true)

(check-expect (intersection_checker (make-world 69
                                                (make-Shape 1 (make-posn 600 300) "red")
                                                (make-Shape 2 (make-posn 600 300) "yellow")
                                                #true))
              #true)



       
           
         
; world_level_updater: world number number number number number -> world
; Updates the level and returns  the world
;{Called by status_check}

; Strategy : Function Composition

; Example : Gets a world and the level,
;           then it replaces the world's
;           level with the given level. 


(define (world_level_updater world level left_shape left_color right_shape right_color)

  (make-world    level
                 (make-Shape left_shape left_obj_starting_posn  (color_selector left_color))
                 (make-Shape right_shape right_obj_starting_posn (color_selector right_color))
                 (world-current_game_status world)))


;; Test Cases
(check-expect (world_level_updater (make-world
                                               start_level
                                               (make-Shape 1 left_obj_starting_posn "red")
                                               (make-Shape 2 right_obj_starting_posn "blue")
                                               #true)
                                  10 1 1 2 2)
                                  (make-world
                                               10
                                               (make-Shape 1 left_obj_starting_posn "red")
                                               (make-Shape 2 right_obj_starting_posn "yellow")
                                               #true))

(check-expect (world_level_updater (make-world
                                               start_level
                                               (make-Shape 1 left_obj_starting_posn "red")
                                               (make-Shape 2 right_obj_starting_posn "blue")
                                               #false)
                                  start_level 2 2 1 1)
                                  (make-world
                                               start_level
                                               (make-Shape 2 left_obj_starting_posn "yellow")
                                               (make-Shape 1 right_obj_starting_posn "red")
                                               #false))




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


(define (status_check world left_shape left_color right_shape right_color)
  (cond 
    [ (intersection_checker world) (world_level_updater world
                                                        (+ 1 (world-level world))
                                                        left_shape
                                                        left_color
                                                        right_shape
                                                        right_color)] 

    [else                          (world_level_updater world
                                                        (world-level world)
                                                        left_shape
                                                        left_color
                                                        right_shape
                                                        right_color)]))


;; Test Cases:

(check-expect (status_check (make-world 1
                                        (make-Shape 4 (make-posn 600 300) "blue")
                                        (make-Shape 6 (make-posn 600 300) "white")
                                        #true)
                            1 1 2 2)
              (make-world 2
                          (make-Shape 1 (make-posn 0 300) "red")
                          (make-Shape 2 (make-posn 1200 300) "yellow")
                          #true))

(check-expect (status_check (make-world 50
                                        (make-Shape 4 (make-posn 600 300) "blue")
                                        (make-Shape 6 (make-posn 600 300) "white")
                                        #true)
                            1 1 2 2)
              (make-world 51
                          (make-Shape 1 (make-posn 0 300) "red")
                          (make-Shape 2 (make-posn 1200 300) "yellow")
                          #true))

(check-expect (status_check (make-world 1
                                        (make-Shape 4 (make-posn 0 300) "blue")
                                        (make-Shape 6 (make-posn 1200 300) "white")
                                        #true)
                            1 1 2 2)
              (make-world 1
                          (make-Shape 1 (make-posn 0 300) "red")
                          (make-Shape 2 (make-posn 1200 300) "yellow")
                          #true))

(check-expect (status_check (make-world 50
                                        (make-Shape 4 (make-posn 0 300) "blue")
                                        (make-Shape 6 (make-posn 1200 300) "white")
                                        #true)
                            1 1 2 2)
              (make-world 50
                          (make-Shape 1 (make-posn 0 300) "red")
                          (make-Shape 2 (make-posn 1200 300) "yellow")
                          #true))




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

(define (draw_level world)
  (place-image (text (number->string (world-level world)) 50 "black")
               level_disp_x level_disp_y
               (draw_void_region world)))

;; Test Cases :

(check-expect (draw_level (make-world 1
                                      (make-Shape 0 (make-posn 0 300) "red")
                                      (make-Shape 2 (make-posn 1200 300) "yellow")
                                      #true))
              (place-image (text (number->string 1) 50 "black")
                           level_disp_x level_disp_y
                           (place-image (rectangle 165 600 "solid" "LightSlateGray")
                                        (- initial_void_region_x (/ 2 165)) initial_void_region_y
                                        (place-image (ellipse object_width object_height "solid" "red")
                                                     (posn-x (make-posn 0 300)) (posn-y (make-posn 0 300))
                                                     (place-image (rectangle object_width object_height "solid" "yellow")
                                                                  (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                                                                  EMPTY)))))

(check-expect (draw_level (make-world 72
                                      (make-Shape 0 (make-posn 0 300) "red")
                                      (make-Shape 2 (make-posn 1200 300) "yellow")
                                      #true))
              (place-image (text (number->string 72) 50 "black")
                           level_disp_x level_disp_y
                           (place-image
                            (text "Wheyy..!! You Won the Game...!!!" 25 "black")
                            (/ world-width 2) (/ world-height 2)
                            EMPTY)))
              



; draw_void_region : World -> Image
; To draw the void region based on the level from the world.
; Using the world-level and the "void_region_ramp_up" rate draws the void region.
; Game stops when the void_region is equalto or greater than the width. That's the max-level.
; Call "win_game" function accordingly.
; {This will be called by "draw_level"}
; {This calls "draw_object_left"}

; Strategy : Structural Decomposition

(define (draw_void_region world)
  (cond
    [(void_region_out_of_bounds world) win_game]
    [else (place-image (rectangle (void_region_calc world) void_region_height "solid" "LightSlateGray")
                       (- initial_void_region_x (/ 2 (void_region_calc world))) initial_void_region_y
                       (draw_object_left world))]))


;; Test Case:
(check-expect (draw_void_region (make-world 1
                                            (make-Shape 0 (make-posn 0 300) "red")
                                            (make-Shape 2 (make-posn 1200 300) "yellow")
                                            #true))
              (place-image (rectangle 165 600 "solid" "LightSlateGray")
                           (- initial_void_region_x (/ 2 165)) initial_void_region_y
                           (place-image (ellipse object_width object_height "solid" "red")
                                        (posn-x (make-posn 0 300)) (posn-y (make-posn 0 300))
                                        (place-image (rectangle object_width object_height "solid" "yellow")
                                                     (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                                                     EMPTY))))

(check-expect (draw_void_region (make-world 10
                                            (make-Shape 0 (make-posn 0 300) "red")
                                            (make-Shape 2 (make-posn 1200 300) "yellow")
                                            #true))
              (place-image (rectangle 300 600 "solid" "LightSlateGray")
                           (- initial_void_region_x (/ 2 300)) initial_void_region_y
                           (place-image (ellipse object_width object_height "solid" "red")
                                        (posn-x (make-posn 0 300)) (posn-y (make-posn 0 300))
                                        (place-image (rectangle object_width object_height "solid" "yellow")
                                                     (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                                                     EMPTY))))

(check-expect (draw_void_region (make-world 72
                                            (make-Shape 0 (make-posn 0 300) "red")
                                            (make-Shape 2 (make-posn 1200 300) "yellow")
                                            #true))
              (place-image
               (text "Wheyy..!! You Won the Game...!!!" 25 "black")
               (/ world-width 2) (/ world-height 2)
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
  (place-image (shape_drawer (Shape-type (world-left_img world)) (Shape-colour (world-left_img world)))
               (posn-x (Shape-pos (world-left_img world))) (posn-y (Shape-pos (world-left_img world)))
               (draw_object_right world)))


;; Test Cases:

(check-expect (draw_object_left (make-world 1
                                            (make-Shape 0 (make-posn 0 300) "red")
                                            (make-Shape 2 (make-posn 1200 300) "yellow")
                                            #true))
              (place-image (ellipse object_width object_height "solid" "red")
                           (posn-x (make-posn 0 300)) (posn-y (make-posn 0 300))
                           (place-image (rectangle object_width object_height "solid" "yellow")
                                        (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                                        EMPTY)))

(check-expect (draw_object_left (make-world 1
                                            (make-Shape 0 (make-posn 0 300) "red")
                                            (make-Shape 0 (make-posn 1200 300) "yellow")
                                            #true))
              (place-image (ellipse object_width object_height "solid" "red")
                           (posn-x (make-posn 0 300)) (posn-y (make-posn 0 300))
                           (place-image (ellipse object_width object_height "solid" "yellow")
                                        (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                                        EMPTY)))

(check-expect (draw_object_left (make-world 1
                                            (make-Shape 2 (make-posn 0 300) "red")
                                            (make-Shape 2 (make-posn 1200 300) "yellow")
                                            #true))
              (place-image (rectangle object_width object_height "solid" "red")
                           (posn-x (make-posn 0 300)) (posn-y (make-posn 0 300))
                           (place-image (rectangle object_width object_height "solid" "yellow")
                                        (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                                        EMPTY)))


 



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
  (place-image (shape_drawer (Shape-type (world-right_img world)) (Shape-colour (world-right_img world)))
               (posn-x (Shape-pos (world-right_img world))) (posn-y (Shape-pos (world-right_img world)))
               EMPTY))

;; Test Cases:
(check-expect (draw_object_right (make-world 1
                                             (make-Shape 1 (make-posn 0 300) "red")
                                             (make-Shape 2 (make-posn 1200 300) "yellow")
                                             #true))
              (place-image (rectangle object_width object_height "solid" "yellow")
                           (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                           EMPTY))

(check-expect (draw_object_right (make-world 1
                                             (make-Shape 1 (make-posn 0 300) "red")
                                             (make-Shape 0 (make-posn 1200 300) "yellow")
                                             #true))
              (place-image (ellipse object_width object_height "solid" "yellow")
                           (posn-x (make-posn 1200 300)) (posn-y (make-posn 1200 300))
                           EMPTY))
              

; Big bang definition
#;(big-bang (make-world
           start_level
           (make-Shape (random available_shapes) left_obj_starting_posn (color_selector (random available_color)))
           (make-Shape (random available_shapes) right_obj_starting_posn (color_selector (random available_color)))
           initial_current_game_status)

          [on-tick tick 1/30]
          [on-key key]
          [to-draw draw_level]
          [stop-when world-stop])

