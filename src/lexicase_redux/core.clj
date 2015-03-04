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
      (length-rubric testing-dude) => 7)

; a random rubric
(def random-rubric
  (fn random-int [individual]
    (rand-int 100)))

(fact "applying the length-rubric to testing-dude should return a number"
      (integer? (random-rubric testing-dude)) => true)


(fact "an Individual's error can be set by a rubric, with rubric 'name' as a key"
      (false) => "not sure how to proceed")

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