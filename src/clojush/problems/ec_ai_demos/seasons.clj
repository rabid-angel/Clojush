;; squirrel_play.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Nic McPhee, mcphee@morris.umn.edu, 2016

(ns clojush.problems.ec-ai-demos.seasons
  (:use [clojush.pushgp.pushgp]
        [clojush.random]
        [clojush pushstate interpreter]
        clojush.instructions.common))

;;;;;;;;;;;;
;; The squirrels in Palo Alto spend most of the day playing. In particular,
;; they play if the temperature is between 60 and 90 (inclusive). Unless it
;; is summer, then the upper limit is 100 instead of 90.
;; Given an int temperature and a boolean is_summer, return true if the
;; squirrels play and false otherwise.
;; Taken from CodingBat: http://codingbat.com/prob/p135815

(def input-set
  [;Winter
   [7, "N"]
   [7, "N"]
   [8, "N"]
   [8, "N"]
   [9, "N"]
   [9, "N"]
   ;Spring
   [10, "G"]
   [10, "G"]
   [11, "G"]
   [11, "G"]
   [12, "G"]
   [12, "G"]
   ;Summer
   [13, "G"]
   [13, "G"]
   [14, "G"]
   [14, "G"]
   [15, "G"]
   [15, "G"]
   ;Fall
   [10, "R"]
   [10, "Y"]
   [11, "R"]
   [11, "Y"]
   [12, "R"]
   [12, "Y"]])

(defn expected-output
  [inputs]
  (let [[light-hours leaf-color] inputs]
    (cond
      (< light-hours 10) "Winter"
      (>= light-hours 13) "Summer"
      (and (<= 10 light-hours 12)
           (= leaf-color "G")) "Spring"
      (and (<= 10 light-hours 12)
           (or (= leaf-color "R") (= leaf-color "Y"))) "Fall")))

; Make a new push state, and then add every
; input to the special `:input` stack.
; You shouldn't have to change this.
(defn make-start-state
  [inputs]
  (reduce (fn [state input]
            (push-item input :input state))
          (make-push-state)
          inputs))

; The only part of this you'd need to change is
; which stack(s) the return value(s) come from.
(defn actual-output
  [program inputs]
  (let [start-state (make-start-state inputs)
        end-state (run-push program start-state)
        result (top-item :string end-state)]
    result))

(defn all-errors
  [program]
  (doall
    (for [inputs input-set]
      (let [expected (expected-output inputs)
            actual (actual-output program inputs)]
        (if (= actual :no-stack-item)
          1000
          (if (= expected actual)
            0
            1))))))

(def atom-generators
  (concat
    ; Include all the instructions that act on integers and booleans
    ; Could have :exec here, but I just am limiting things to exec-if
    (registered-for-stacks [:integer :boolean :string :exec])
    (list 'exec_if 'exec_if 'exec_if 'exec_if 'exec_if 'exec_if 'exec_if 'exec_if 'exec_if 'exec_if)
    ; A bunch of random numbers in case that's useful.
    (list (fn [] (lrand-int 24)))
    ; The three numeric constants that are specified in the problem statement
    (list "Spring" "Summer" "Fall" "Winter" "R" "G" "Y" "N" "R" "G" "Y" "N" "R" "G" "Y" "N" 10 13 10 13 10 13 10 13)
    ; The two inputs
    (list 'in1 'in2)))

(def argmap
  {:error-function all-errors
   :atom-generators atom-generators
   :population-size 1000
   })
