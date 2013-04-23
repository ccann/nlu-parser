;; @author  ccann
;; COMP150NLD: Situated Natural Language Understanding on Robots
;; Assignment 2: Parser
;;
;;
;; NOTE: type-raising is assumed to be in the lexicon, and only on atomic entries.
;;
;; TODO BFS search through stack-space
;; TODO expand lexicon to a specific domain
;; TODO add semantics?? need higher-order representations in lexicon


(ns nlu-parser.core)
(use '[clojure.string :only (join split)])

(defrecord Atom [cat])
(defrecord Functor [ret dir arg])

(defn is-functor? [t] (= (class t) Functor))
(defn is-atom? [t] (= (class t) Atom))

;; Returns the result of applying the forward type-raising combinator to atom
(defn forward-tr [atom]
  (Functor. (Atom. 'T) "/" (Functor. (Atom. 'T) "\\" atom)))

(defn backward-tr [atom]
  (Functor. (Atom. 'T) "\\" (Functor. (Atom. 'T) "/" atom)))

(def lexicon {"andie" {(Atom. 'NP) 'andie
                       (forward-tr (Atom. 'NP)) (fn [x] (x 'andie))}
              
              "saw" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                     (fn [y] (fn [x] ['see y x]))
                     (Atom. 'N) (fn [x] ['saw x])}

              "steve" {(Atom. 'NP) 'steve
                       (forward-tr (Atom. 'NP)) (fn [x] (x 'steve))}
              
              "loves" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                       (fn [y] (fn [x] ['love y x]))}
              
              "the" {(Functor. (Atom. 'NP) "/" (Atom. 'N))
                     (fn [p] ['def p])}

              "flowers" {(Atom. 'N) (fn [x] ['flowers x])
                         (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                         (fn [q] (fn [x] [(q 'flowers) x]))}
              
              "sent" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'PP))
                      (fn [y] (fn [x] ['summon y x]))}
              
              "for" {(Functor. (Atom. 'PP) "/" (Atom. 'NP)) (fn [x] x)}
              
              "patient" {(Atom. 'N) (fn [x] ['patient x])
                         (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                         (fn [q] (fn [x] [(q 'patient) x]))}
              
              "dog" {(Atom. 'N) (fn [x] ['dog x])}

              "john" {(Atom. 'NP) 'john
                       (forward-tr (Atom. 'NP)) (fn [x] (x 'john))}

              "bit" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                     (fn [y] (fn [x] ['bite y x]))}})

;; need to create hooks for situatedness. Specifically -- an input
;; vector consisting of potentially recognized visual targets in
;; lexical key form (strings). If this vector is non-empty, bias the
;; lexical choice of the entry to N or NP form.
(def objects-detected #{"saw" "table"})


;; returns a string representation of the complex or simple type
(defn type-to-string [t]
  (cond (is-functor? t)
        (str "(" (type-to-string (:ret t)) (:dir t) (type-to-string (:arg t)) ")")
        (is-atom? t)
        (:cat t)))

;; compose Types t1 and t2
(defn combine [t1 t2 m1 m2]
  (do
    (println "COMBINING" (type-to-string t1) "and" (type-to-string t2)) 
    (cond (and (is-functor? t1) (is-functor? t2))
          (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
            [(Functor. (:ret t1) "/" (:arg t2)) (m1 m2)]
            [(Functor. (:ret t2) "\\" (:arg t1)) (m2 m1)])
          (is-functor? t1) [(:ret t1) (m1 m2)]
          :else          [(:ret t2) (m2 m1)])))

(defn unifiable? [t1 t2]
  (if (and (is-functor? t2) (is-functor? t1)
           (= (Atom. 'T) (:ret (:arg t1))))
    (if (is-functor? (:ret t2))
      (= (:arg (:ret t2)) (:arg (:arg t1)))
      (and (= (:arg t2) (:arg (:arg t1))) (= (:dir (:arg t1)) (:dir t2))))
    false))

;; returns the unification of t1 with t2 (replaces 'T with the return value of t2)
(defn unify [t1 t2]
  (if (is-functor? (:ret t2))
        (assoc (assoc t1 :arg (:ret t2)) :ret (:ret (:ret t2)))
        (assoc (assoc t1 :arg t2) :ret (:ret t2))))

;; returns true if two Types are combinable by composition combinators
(defn comp-combinable? [t1 t2]
  (cond (and (is-functor? t1) (is-functor? t2))
        (cond
         ;; > Forward composition combinator
         (and (= (:dir t1) "/") (= (:dir t2) "/"))
          (= (:arg t1) (:ret t2))
         ;; < Backward composition combinator
         (and (= (:dir t1) "\\") (= (:dir t2) "\\"))
         (= (:ret t1) (:arg t2)))
        :else false))

;; returns true if two Types are combinable by application combinators
(defn app-combinable? [t1 t2]
  (cond 
   ;; > Forward application combinator
   (and (is-functor? t1) (is-atom? t2))
   (and (= (:dir t1) "/") (= (:arg t1) t2))
   ;; < Backward application combinator
   (and (is-atom? t1) (is-functor? t2))
   (and (= (:dir t2) "\\") (= (:arg t2) t1))
   :else false))
 

;; (defn reduce-stack [s]
;; (let [t1 (first s)
;;       t2 (second s)]
;;   (if (composable? (first t2) (first t1))
;;     [(compose (first t2)(first t1))
;;      ((second t2) (second t1))])))

;;COMBINING ((S\NP)/NP) and (NP/N)

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

(defn get-lexical-entries-2 [word]
  (keys (lexicon word)))

;; returns a random entry for the word in the lexicon
(defn random-lookup-with-hooks [word]
  (if (contains? objects-detected word)
    (when (contains? (set (get-lexical-entries-2 word)) (Atom. 'N))
      (do
        (println "SELECTING:" (type-to-string (Atom. 'N)) "for" word)
        (Atom. 'N)))
    (let [entries (get-lexical-entries-2 word)
          selection (nth entries (rand-int (count entries)))]
      (do
        (println "SELECTING:" (type-to-string selection) "for" word)
        selection))))
  
;; SHIFT: push the first word's lexical entry onto the stack
(defn shift [sentence stack sem]
  (let [s1 (first sentence)]
    (if (not (nil? s1))
      (let [entry (random-lookup-with-hooks s1)]
        (do (println  "PUSHING:" (type-to-string entry) "onto"
                      (reverse (map type-to-string stack)))
            [(cons entry stack) (cons ((lexicon s1) entry) sem)]))
      [stack sem])))

(defn shift-semantics [sentence sem]
  (let [m1 (first sem)]
    (if (not (nil? m1))
      ())))

;; returns true if t1 and t2 are reducible
(defn reducible? [t1 t2]
  (or (comp-combinable? t1 t2)
      (app-combinable? t1 t2)
      (if (and (is-functor? t1) (is-functor? t2))
        (if (and (and (= (:dir t2) "/") (= (:dir t1) "/"))
                 (unifiable? t1 t2))
          (comp-combinable? (unify t1 t2) t2)
          (if (and (and (= (:dir t2) "\\") (= (:dir t1) "\\"))
                 (unifiable? t2 t1))
            (comp-combinable? t1 (unify t2 t1))
            false))
        false)))

;; returns the reduction of t1 and t2 -- call reducible? first as a check
(defn reduce-stack [t1 t2 m1 m2]
  (if (or (comp-combinable? t1 t2) (app-combinable? t1 t2))
    (combine t1 t2 m1 m2)
    (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
      (do (println "UNIFYING:" (type-to-string t1) "with" (type-to-string t2))
          (combine (unify t1 t2) t2 m2 m1))
      (do (println "UNIFYING:" (type-to-string t2) "with" (type-to-string t1))
          (combine t1 (unify t2 t1) m1 m2)))))

;; returns true if the sentence has a valid parse from random lexical selections
(defn non-det-parse [sentence stack sem]
  (let [t1 (second stack)
        t2 (first stack)
        m1 (second sem)
        m2 (first sem)]
    (do (println "\nSTACK:" (reverse (map type-to-string stack)))
        (println "INPUT:" sentence)
        (if (not (empty? sentence))
          (if (< (count stack) 2)
            (let [new-stack-sem (shift sentence stack sem)]
              ;; recurse on shifted stack and rest of sentence
              (non-det-parse (rest sentence) (first new-stack-sem) (second new-stack-sem)))
            ;; else if reducible, recurse on reduced stack
            (if (reducible? t1 t2)
              (let [ [t m] (reduce-stack t1 t2 m1 m2) ]
                (non-det-parse sentence (cons t (drop 2 stack))
                               (cons m (drop 2 sem))))

              ;; if not reducible, recurse on shifted stack and rest of sentence
              (let [new-stack-sem (shift sentence stack sem)] 
                (non-det-parse (rest sentence) (first new-stack-sem) (second new-stack-sem)))))
          (if (< (count stack) 2)
            (do (println "found valid parse:" (type-to-string t2))
                ;;(println sem)
                true)
            (if (reducible? t1 t2)
              ;; if reducible, recurse on reduced stack
              (let [ [t m] (reduce-stack t1 t2 m1 m2)]
                (non-det-parse sentence (cons t (drop 2 stack))
                               (cons m (drop 2 sem))))
              ;; otherwise return failure
              (do (println "failed to find a valid parse")
                  false)))))))




(println "\n---------------------------")

(def example3 "andie loves steve")
(def example1 "the dog bit john")
(def example2 "andie saw the dog")
(def example4 "the flowers sent for the patient")

(non-det-parse (split example2 #"\s") '() '())



