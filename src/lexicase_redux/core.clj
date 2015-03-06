(ns lexicase-redux.core)
(use 'midje.sweet
     '(clojush interpreter pushstate)
     '(clojush.instructions boolean code common numbers random-instructions string char vectors tag zip return input-output))


; The point here is to explore a framework in which Clojush Individuals
; (whatever they are) are not evaluated on any given rubric until the
; value is needed. Very, very lazy in other words.

; I'm also working in a rather linear, narrative style: 
; interspersing tests and code, changing it as I go
; rather than revising it in place. 

; The selection algorithm in question is lexicase selection, used in Clojush for
; selecting parents for reproduction.

; the old one (currently in Clojush) is:
;
; (defn lexicase-selection
;   "Returns an individual that does the best on the fitness cases when considered one at a
;    time in random order.  If trivial-geography-radius is non-zero, selection is limited to parents within +/- r of location"
;   [pop location {:keys [trivial-geography-radius]}]
;   (let [lower (mod (- location trivial-geography-radius) (count pop))
;         upper (mod (+ location trivial-geography-radius) (count pop))
;         popvec (vec pop)
;         subpop (if (zero? trivial-geography-radius)
;                  pop
;                  (if (< lower upper)
;                    (subvec popvec lower (inc upper))
;                    (into (subvec popvec lower (count pop))
;                          (subvec popvec 0 (inc upper)))))]
;     (loop [survivors (retain-one-individual-per-error-vector subpop)
;            cases (lshuffle (range (count (:errors (first subpop)))))]
;       (if (or (empty? cases)
;               (empty? (rest survivors)))
;         (lrand-nth survivors)
;         (let [min-err-for-case (apply min (map #(nth % (first cases))
;                                                (map #(:errors %) survivors)))]
;           (recur (filter #(= (nth (:errors %) (first cases)) min-err-for-case)
;                          survivors)
;                  (rest cases)))))))


; let's see if we can grow this using Midje tests and REPL-like interactive code

; testing midje (should be happy in parallel REPL `autotest`)
(fact "I can totally add integers, and Midje notices it!"
  (+ 66 11) => 77)

; Individuals
; An Individual is a prospective solution
; (aka "Answer" in Tozier's agnostic ontology)
(defrecord Individual [uniqueID script errors])

; Taken from https://gist.github.com/gorsuch/1418850
(defn uuid []
  (str (java.util.UUID/randomUUID)))

; Individuals have a unique id, a script, and an errors map

(defn make-individual [script]
         (Individual. (uuid)
                      script
                      {}))

(def testing-dude (make-individual '(1 2 integer_add)))

(fact "an Individual should be able to report its attributes"
      (nil? (:uniqueID testing-dude)) => false
      (:script testing-dude) => '(1 2 integer_add)
      (:errors testing-dude) => {})

;; Errors and rubrics:
;;
;; errors are created established by Rubrics, functions of an Individual
;; which return a numeric value
;; if a specified error doesn't have an associated Rubric, then the result is nil

; an example rubric
(def length-rubric
  (fn script-length [individual]
    (count (:script individual))))

(fact "applying the length-rubric to testing-dude should return his script length"
      (length-rubric testing-dude) => 3)

; a random rubric
(def random-rubric
  (fn random-int [individual]
    (rand-int 100)))

(fact "calling the random-rubric on testing-dude should return a number"
      (integer? (random-rubric testing-dude)) => true)

;; getting and setting errors with rubrics: a bit of infrastructure
;;
;; getting an existing rubric value from an individual's error hash

(fact "if an error is set already in an individual, I can get it"
      (let [error-dude
            (assoc-in testing-dude
                      [:errors :foo] 
                      (length-rubric testing-dude))]
            (:foo (:errors error-dude)) => 3))
      
;; but that's an awful way to get it, so how about...

(defn get-error [individual error-key]
      (error-key (:errors individual)))

(fact "I can get a particular error from an individual's error map with `get-error`"
      (let [error-dude
            (assoc-in testing-dude
                      [:errors :foo] 
                      (length-rubric testing-dude))]
            (get-error error-dude :foo) => 3))

;; it's also a pretty awful way to set a value, so...
(defn set-error [individual error-key error-value]
      (assoc-in individual
                [:errors error-key]
                error-value))

(fact "I can set a particular error in an error map with `set-error`"
      (let [error-dude (set-error testing-dude :bar 8182)]
           (get-error error-dude :bar) => 8182))

;; however, set-error is creating a new dude, not _saving_ the error in one...
; (fact "When I set an error in an individual, it should stick"
;       (set-error testing-dude :baz 8182)
;       (get-error testing-dude :baz) => 8182) ;; fails

(defn set-error [individual error-key error-value]
      (assoc-in individual
                [:errors error-key]
                error-value))


;; what I need is a different setup of an Individual...
;; (which feels weird), because so very imperative
(defn make-individual [script]
      (atom (Individual. (uuid)
                         script
                         {})))

;; make a new testing-dude
(def testing-dude (make-individual '(1 2 integer_subtract)))

;; revise set-error to use the approriate magic words
(defn set-error [this-individual error-key error-value]
      (swap! this-individual (assoc-in :errors [error-key] error-value)))

(set-error testing-dude :quux 8182) ;; fails

(fact "When I set an error in an individual, it sticks"
      (get-error testing-dude :quux) => 8182)




























;; "convenience function" for setting up push interpreter with inputs...
(defn load-an-arg [arg my-state]
  (push-item arg :input my-state))

(defn build-loaded-push-state [inputs]
      "returns a push-state with specified arguments in :input stack (in order)"
      (reduce 
        (fn [the-state the-input] (load-an-arg the-input the-state))
        (make-push-state)
        inputs))

(fact "I can create a push-state with one argument on the :input stack"
      (:input (load-an-arg 1 (make-push-state))) => (just [1]))

(fact "I can create a push-state with loads of arguments"
      (:input (build-loaded-push-state [1 2 3])) => (just [3 2 1])
      (:input (build-loaded-push-state [1 -1.3 false])) => (just [false -1.3 1])
      (:input (build-loaded-push-state ["foo"])) => (just ["foo"])
      )

(println (run-push '(in1 in2) (build-loaded-push-state [1 7 8 7.3]))) ;; works
; (println (run-push '(in1 in3) (build-loaded-push-state [1 '(3) 8 7.3]))) ;; nope
; (println (run-push '(in1 in3) (build-loaded-push-state [1 (3) 8 7.3]))) ;; nope
; (println (run-push '(in1 in3) (build-loaded-push-state [1 ('(3)) 8 7.3]))) ;; nope
; (println (run-push '(in1 in2) (build-loaded-push-state [1 (3) 8 7.3]))) ;; nope
(println (run-push '(in1 in2) (build-loaded-push-state [1 (quote (3)) 8 7.3]))) ;; maybe?




; (fact "I can create a push-state with code points as arguments"
;       (:input (build-loaded-push-state '(1 quote ( 2 ) 3))) => (just ["foo"])
;       )

; (fact "an Individual's error can be set by a rubric, with rubric 'name' as a key"
;       (false) => "not sure how to proceed")

;; An experiment in dumb-ass search:
;;
;; 1. I make 100 random individuals
;; - they have no errors
;; - they have executable scripts
;; 2. I make 100 rubrics 
;; - for 100 I/O training cases, for example
;; - maybe also a script complexity one
;; 3. using lexicase selection, I pick 2 "parents"
;; - filling in the errors only as needed
;; 4. I _add_ 2 new offspring to the population by random crossover of parents
;; - they have no errors
;; 5. GOTO 3
;; - NOTE: no culling from the population!