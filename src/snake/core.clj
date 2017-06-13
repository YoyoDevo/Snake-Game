(ns snake.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;set up the world
(def world { :width 100 :height 100})

;define a snake
(def snake_vector (atom []))

;snake starting position
(swap! snake_vector conj [50 50])

;; define a direction to move a snake
(def direction (atom []))

;sets initial value for direction of snake to the right (x=1, y=0)
(swap! direction conj 1 0)

;define food
(def food (atom #{}))

;spawns food 50% of the time the update function is ran
(defn food_gen []
  (when (< (rand) 0.5)
    (swap! food conj [(rand-int (world :width)) (rand-int (world :width))])))

;removes food when eaten by snake
(defn food_eat []
  (swap! food disj (first @snake_vector)))

;allows the snake to wrap to the other side when it reaches an edge
(defn wrap[i m]
  (loop [x i] (cond (< x 0) (recur (+ x m)) (>= x m) (recur (- x m)) :else x)))

;allows the snake to change directions based on keyboard input
(defn turn[state event]
  (case (:key event)
    (:up) (if (not= [0 1] @direction) (reset! direction [0 -1])  @direction)
    (:down) (if (not= [0 -1] @direction) (reset! direction [0 1])  @direction)
    (:left) (if (not= [1 0] @direction) (reset! direction [-1 0])  @direction)
    (:right) (if (not= [-1 0] @direction) (reset! direction [1 0])  @direction)))

;reset the snake if it dies
(defn death []
  (if (apply distinct? @snake_vector)
    @snake_vector
    (reset! snake_vector (into [] (take 1 @snake_vector)))))

;count length of snake and show score
(defn score [] (str "Score: " (count @snake_vector)))

;snake is green and food is green
(def green_color (atom '(0 255 54)))

;move the snake in the direction it's facing
(defn move []
  (let [new-values (into [] (map + [(wrap (first (first @snake_vector)) (world :width)) (wrap (second (first @snake_vector)) (world :height))] @direction))]
    (if (contains? @food (first @snake_vector))
      (do
        (food_eat)
        (swap! snake_vector conj new-values))
      (reset! snake_vector (drop-last 1 (conj @snake_vector new-values))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; QUIL FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;frame-rate is basically the speed of the snake and speed to spawn food
(defn setup []
  (q/smooth)
  (q/frame-rate 30))

;generates food 50% of the time, moves snake 1 coordinate, checks for death, updates score
(defn update [state]
  (food_gen)
  (move)
  (death)
  (score))

;draws the current state of the game
(defn draw [state]
  (let [w (/ (q/width) (world :width))
        h (/ (q/height) (world :height))]
    (q/background 0 0 0)
    (doseq [[x y] @food]
      (q/fill (first @green_color) (second @green_color) (last @green_color))
      (q/stroke (first @green_color) (second @green_color) (last @green_color))
      (q/rect (* w x) (* h y) w h))
    (doseq [[x y] @snake_vector]
      (q/fill (first @green_color) (second @green_color) (last @green_color))
      (q/stroke (first @green_color) (second @green_color) (last @green_color))
      (q/rect (* w x) (* h y) w h)))
  (q/fill 100 255 100)
  (q/text-size 25)
  (q/text (score) 10 30 ))

(q/defsketch snake
             :title "Snake"
             :size [700 700]
             :setup setup
             :update update
             :draw draw
             :key-pressed turn
             :features [:keep-on-top]
             :middleware [m/fun-mode])