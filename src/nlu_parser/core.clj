(ns nlu-parser.core)

(defrecord Atom [cat])
(defrecord Functor [ret dir arg])

(defn is-functor? [t] (= (class t) Functor))
(defn is-atom? [t] (= (class t) Atom))

;; (def andie [(Functor. (Atom. 'S)
;;                      "/"
;;                        (Functor. (Atom. 'S) "\\" (Atom. 'NP)))
;;             (fn [x] (x 'andie))])
;; (def see [(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP))
;;                      "/"
;;                      (Atom. 'NP))
;;           (fn [x] (fn [y] ['loves y x]))])

;; (def steve [(Atom. 'NP)
;;             'steve])

;; "andie" (Functor. (Atom. 'S)
;;                                 "/"
;;                                 (Functor. (Atom. 'S) "\\" (Atom. 'NP)))

;; Returns the result of applying the forward type-raising combinator to atom
(defn forward-tr [atom]
  (Functor. (Atom. 'T) "/" (Functor. (Atom. 'T) "\\" atom)))

(defn backward-tr [atom]
  (Functor. (Atom. 'T) "\\" (Functor. (Atom. 'T) "/" atom)))

(def lexicon {"andie" [(Atom. 'NP)] 
              "saw" [(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP))
                               "/"
                               (Atom. 'NP))
                     (Atom. 'N)]
              "steve" [(Atom. 'NP)]
              "loves" [(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))]
              "the" [(Functor. (Atom. 'NP) "/" (Atom. 'N))]
              "dog" [(Atom. 'N)]
              "John" [(Atom. 'NP)]
              "bit" [(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))]})

;; returns a string representation of the complex or simple type
(defn type-to-string [t]
  (cond (is-functor? t)
        (str "(" (type-to-string (:ret t)) (:dir t) (type-to-string (:arg t)) ")")
        (is-atom? t)
        (:cat t)))

;; compose Types t1 and t2
(defn compose [t1 t2]
  (do
    (println "COMPOSING" (type-to-string t1) "and" (type-to-string t2)) 
    (cond (and (is-functor? t1) (is-functor? t2))
          (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
            (Functor. (:ret t1) "/" (:arg t2))
            (Functor. (:ret t2) "\\" (:arg t1)))
          (is-functor? t1) (:ret t1)
          :else          (:ret t2))))

(defn unifiable? [t1 t2]
  (if (and (is-functor? t2) (is-functor? t1)
           (= (Atom. 'T) (:ret (:arg t1))))
    (if (is-functor? (:ret t2))
      (= (:arg (:ret t2)) (:arg (:arg t1)))
      (= (:arg t2) (:arg (:arg t1))))
    false))

;; returns the unification of t1 with t2 (replaces 'T with the return value of t2)
(defn unify [t1 t2]
  (do (println "UNIFYING:" (type-to-string t1) "with" (type-to-string t2))
      (if (is-functor? (:ret t2))
        (assoc (assoc t1 :arg (:ret t2)) :ret (:ret (:ret t2)))
        (assoc (assoc t1 :arg t2) :ret (:ret t2)))))

;; returns true if two Types are combinable by composition combinators
(defn comp-combinable? [t1 t2]
  (cond (and (is-functor? t1) (is-functor? t2))
        (cond
         ;; > Forward composition combinator
         (and (= (:dir t2) "/") (= (:dir t1) "/"))
          (= (:arg t1) (:ret t2))
         ;; < Backward composition combinator
         (and (= (:dir t2) "\\") (= (:dir t1) "\\"))
         (= (:ret t1) (:arg t2)))
        :else false))

;; returns true if two Types are combinable by application combinators
(defn app-combinable? [t1 t2]
  (cond 
   ;; > Forward application combinator
   (is-functor? t1)
   (and (= (:dir t1) "/") (= (:arg t1) t2))
   ;; < Backward application combinator
   (is-functor? t2)
   (and (= (:dir t2) "\\") (= (:arg t2) t1))
   :else false))
 

;; (defn reduce-stack [s]
;; (let [t1 (first s)
;;       t2 (second s)]
;;   (if (composable? (first t2) (first t1))
;;     [(compose (first t2)(first t1))
;;      ((second t2) (second t1))])))

;;(lexicon (first sentence))

;; returns the collection of lexical entries for the word, including type-raised atoms
(defn get-lexical-entries [word]
  (let [entries (lexicon word)]
    (loop [accum entries
           ents entries]
      (let [e (first ents)]
        (if (nil? e)
          accum
          (if (is-atom? e)
            (recur (cons (forward-tr e) (cons (backward-tr e) accum))
                   (rest ents))
            (recur accum (rest ents))))))))

;; returns a random entry for the word in the lexicon
(defn random-lookup [word]
  (let [entries (get-lexical-entries word)
        selection (nth entries (rand-int (count entries)))]
    (do
      (println "SELECTING:" (type-to-string selection) "for" word)
      selection)))

;; SHIFT: push the first word's lexical entry onto the stack
(defn shift [sentence stack]
  (let [s1 (first sentence)]
    (if (not (nil? s1))
      (let [entry (random-lookup s1)]
        (do (println  "PUSHING:" (type-to-string entry) "onto"
                      (reverse (map type-to-string stack)))
            (cons entry stack)))
      stack)))

;; non-deterministic parse 
(defn non-det-parse [sentence stack]
  (do (println "\nSTACK:" (reverse (map type-to-string stack)))
      (println "INPUT:" sentence)
      (let [s (shift sentence stack)
            sent (rest sentence)
            t1 (first s)
            t2 (second s)]
        (do 
          ;; sentence empty, all words have been pushed to stack
          (if (empty? sent)
            (cond (= (count s) 1)
                  (println "found valid parse:" (:cat (first s)))
                  
                  (or (comp-combinable? t2 t1) (app-combinable? t2 t1))
                  (non-det-parse sent (cons (compose t2 t1) (rest (rest s))))
                  
                  (and (and (= (:dir t2) "/") (= (:dir t1) "/"))
                       (unifiable? t2 t1))
                  (non-det-parse sent (cons (compose (unify t2 t1) t1) (rest (rest s))))
                  
                  (and (and (= (:dir t2) "\\") (= (:dir t1) "\\"))
                       (unifiable? t1 t2))
                  (non-det-parse sent (cons (compose t2 (unify t1 t2)) (rest (rest s))))
                  
                  :else (do (println "failed to find a valid parse")
                            s))
            
            ;; sentence non-empty
            (if (> (count s) 1)
              (if (composable? t2 t1)
                (non-det-parse sent (cons (compose t2 t1) (rest (rest s))))
                (non-det-parse sent s))
              (non-det-parse sent s)))))))


(println "\n---------------------------")

(def example3 '("andie" "loves" "steve"))
(def example1 '("the" "dog" "bit" "John"))
(def example2 '("andie" "saw" "the" "dog"))

(non-det-parse example2 '())

;; DONE type raising, Unification?
;; DONE multiple entries for lexical items, choosing mechanism (must demonstrate ambiguity)
;; TODO BFS search through stack-space
;; TODO expand lexicon to a specific domain


;; The parser: consults the lexicon to find out the possible lexical categories and
;; semantic representations for the input word

;; type-raising is assumed to be in the lexicon, and only on atomic entries.
