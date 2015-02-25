(ns lexicase-redux.core)
(use 'midje.sweet)

; the old one:
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


; let's see if we can grow this

(fact "I can totally add integers, and Midje notices it!"
  (+ 66 11) => 77)

; Taken from https://gist.github.com/gorsuch/1418850
(defn uuid []
  (str (java.util.UUID/randomUUID)))

; I'll need an individual proxy, with some errors
(defrecord Individual [uniqueID errors])

(def dude (Individual. 99 {:err1 1, :err2 2, :err3 3}))

(fact "dude should be able to report his ID and errors in detail"
      (:uniqueID dude) => 99
      (:errors dude) => {:err1 1, :err2 2, :err3 3},
      (:err2 (:errors dude) 1) => 2)

(defn get-error 
      "returns the error associated with a given key, or nil"
      [which-error individual]
      (which-error (:errors individual)))

; However, if we're going to be lazy about evaluation, then...
(fact "it should be OK to ask for an Individual's error that doesn't exist"
      (get-error :err813 dude) => nil)

(defn set-error 
      "assigns a new value to the :errors map of an individual"
      [which-error individual new-value]
      (assoc-in individual [:errors which-error] new-value))

(facts "about `set-error`"
      (fact "it should be OK to set an Individual's error that doesn't exist"
        (get-error :err44 (set-error :err44 dude 8.1)) => 8.1)
      (fact "it should be OK to overwrite an existing :error value"
        (get-error :err1 (set-error :err1 dude -12)) => -12))

;; observation: This doesn't feel like how I should be setting up "laziness" in a Clojurish sense.
;; What I think I would prefer is a way to bind the "things that evaluate" to :errors (somewhere)
;; and then call those error-makers only when a `get` is received.
;; But I don't know how to do that, so I'm (temporarily) willing to push
;; responsibility upstream to the calling process....


; I'll need a population of those

; This just uses integers for the keys in the error vector. It's not
; hard to turn those into keywords (like :err3) if we'd prefer.
(defn make-random-dude [num-errors]
  (Individual. (uuid)
               (reduce #(assoc %1 %2 (rand-int 10)) {} (range num-errors))))

(defn make-population [popsize num-errors]
  (repeatedly popsize #(make-random-dude num-errors)))

(facts "about `make-population`"
       (let [popsize 100
             num-errors 10
             population (make-population popsize num-errors)]
         (fact "every individual should have a different unique id"
               (count (distinct (map :uniqueID population))) => popsize)
         ; Because I wanted a computed description for this fact, I had to
         ; use the :midje/description map entry.
         ; https://github.com/marick/Midje/wiki/Metadata#quoting-and-metadata
         (fact {:midje/description (format "every individual should have %d error values" num-errors)}
               (every? #(= num-errors (count (:errors %))) population) => true)))

