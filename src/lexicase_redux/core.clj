(ns lexicase-redux.core)
(use 'midje.sweet)

; The point here is to explore a framework in which Clojush Individuals
; (whatever they are) are not evaluated on any given rubric until the
; value is needed. Very, very lazy in other words.

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

; Iaken from https://gist.github.com/gorsuch/1418850
(defn uuid []
  (str (java.util.UUID/randomUUID)))

; Individuals have a unique id, a script, and an errors map

(defn make-individual [script]
         (Individual. (uuid)
                      script
                      {}))

(def testing-dude (make-individual "foo bar"))

(fact "an Individual should be able to report its attributes"
      (nil? (:uniqueID testing-dude)) => false
      (:script testing-dude) => "foo bar"
      (:errors testing-dude) => {})

;; Errors and rubrics
;; errors are created established by Rubrics, functions of an Individual
;; which return a numeric value
;; if a specified error doesn't have an associated Rubric, then the result is nil

; an example rubric
(def length-rubric
  (fn script-length [individual]
    (count (:script individual))))

(fact "applying the length-rubric to testing-dude should return his script length"
      (length-rubric testing-dude) => 7)

; a random rubric
(def random-rubric
  (fn random-int [individual]
    (rand-int 100)))

(fact "applying the length-rubric to testing-dude should return a number"
      (integer? (random-rubric testing-dude)) => true)



; (fact "an Individual's error can be set by a rubric, with the rubric name as a key")


; (defn get-error 
;       "returns the error associated with a given key, or nil"
;       [which-error individual]
;       (deref (which-error (:errors individual))))


; (defn set-rubric
;       "assigns a new rubric function to the :errors map of an individual"
;       [which-error individual rubric]
;       (assoc-in individual [:errors which-error] (future (rubric individual))))

; ; (assoc-in dude [:errors :err999] 7)
; ; (get-error :err999 dude)

; ; However, if we're going to be lazy about evaluation, then...
; ; (get-error :err813 dude)
; (fact "it should be OK to ask for an Individual's error that doesn't exist because it'll be set"
;   (let [dude (set-rubric :err813 dude (fn [_] 8.13))]
;     (get-error :err813 dude) => 8.13))


; (facts "about `set-rubric`"
;       (fact "it should be OK to set an Individual's error that doesn't exist"
;         (get-error :err44 (set-rubric :err44 dude (fn [_] 8.1))) => 8.1)
;       (fact "it should be OK to overwrite an existing :error value"
;         (get-error :err1 (set-rubric :err1 dude (fn [_] -12))) => -12))

; ;; observation: This doesn't feel like how I should be setting up "laziness" in a Clojurish sense.
; ;; What I think I would prefer is a way to bind the "things that evaluate" to :errors (somewhere)
; ;; and then call those error-makers only when a `get` is received.
; ;; But I don't know how to do that, so I'm (temporarily) willing to push
; ;; responsibility upstream to the calling process....


; ; I make 100 individuals
; ;   they have no errors
; ; I make 100 rubrics

; ; I'll need a population of those

; ; This just uses integers for the keys in the error vector. It's not
; ; hard to turn those into keywords (like :err3) if we'd prefer.
; (defn make-random-dude [num-errors]
;   (Individual. (uuid)
;                (reduce #(assoc %1 %2 (rand-int 10)) {} (range num-errors))))

; ; (defn make-population [popsize num-errors]
; ;   (repeatedly popsize #(make-random-dude num-errors)))

