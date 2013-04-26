;; @author  ccann
;; COMP150NLD: Situated Natural Language Understanding on Robots
;; Assignment 2: Parser
;;
;;
;; NOTE: type-raising is assumed to be in the lexicon, and only on atomic entries.
;;
;; TODO get semantics working completely (it doesn't work in the MS dissertation, so...)

(ns nlu-parser.core)
(use '[clojure.string :only (join split)])

;; FUNDAMENTAL TYPES 
(defrecord Atom [cat])
(defrecord Functor [ret dir arg])

(defn is-functor? [t] (= (class t) Functor))
(defn is-atom? [t] (= (class t) Atom))

;; Returns the result of applying the forward type-raising combinator to atom
(defn forward-tr [atom]
  (Functor. (Atom. 'T) "/" (Functor. (Atom. 'T) "\\" atom)))

(defn backward-tr [atom]
  (Functor. (Atom. 'T) "\\" (Functor. (Atom. 'T) "/" atom)))

;; returns a string representation of the complex or simple type
(defn type-to-string [t]
  (cond (is-functor? t)
        (str "(" (type-to-string (:ret t)) (:dir t) (type-to-string (:arg t)) ")")
        (is-atom? t)
        (:cat t)))

;; LEXICON 
(def lexicon {"#77" {(Atom. 'NP) '77
                       (forward-tr (Atom. 'NP)) (fn [x] (x '77))}
              
              "thanked" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                     (fn [y] (fn [x] ['thank y x]))
                     (Atom. 'N) (fn [x] ['thank x])}

              "Jagr" {(Atom. 'NP) 'jagr
                       (forward-tr (Atom. 'NP)) (fn [x] (x 'jagr))}
              
              "checked" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                       (fn [y] (fn [x] ['check y x]))}
              
              "the" {(Functor. (Atom. 'NP) "/" (Atom. 'N))
                     (fn [p] ['def p])}

              "fans" {(Atom. 'N) (fn [x] ['fans x])
                         (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                         (fn [q] (fn [x] [(q 'fans) x]))}
              
              "saved" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'PP))
                       (fn [y] (fn [x] ['save y x]))}

              "skated" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'PP))
                        (fn [y] (fn [x] ['skate y x]))}

              "skate" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'PP))
                       (fn [y] (fn [x] ['skate y x]))
                       (Atom. 'N) (fn [x] ['skate x])}
              
              "for" {(Functor. (Atom. 'PP) "/" (Atom. 'NP)) (fn [x] x)}
              
              "skater" {(Atom. 'N) (fn [x] ['skater x])
                         (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                         (fn [q] (fn [x] [(q 'skater) x]))}

              "player" {(Atom. 'N) (fn [x] ['player x])
                         (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                         (fn [q] (fn [x] [(q 'player) x]))}

              "players" {(Atom. 'N) (fn [x] ['players x])
                         (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                         (fn [q] (fn [x] [(q 'players) x]))}
              
              "puck" {(Atom. 'N) (fn [x] ['puck x])}

              "Henrik" {(Atom. 'NP) 'henrik
                       (forward-tr (Atom. 'NP)) (fn [x] (x 'henrik))}

              "shot" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                     (fn [y] (fn [x] ['shoot y x]))}})


;; set of objects from vision that are located in the immediate environment
(def objects-detected #{"puck" "skate"})


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
 
;; (defn combine [t1 t2 m1 m2]
;;   (do
;;     (println "COMBINING" (type-to-string t1) "and" (type-to-string t2)) 
;;     (cond (and (is-functor? t1) (is-functor? t2))
;;           (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
;;             [(Functor. (:ret t1) "/" (:arg t2)) (m1 m2)]
;;             [(Functor. (:ret t2) "\\" (:arg t1)) (m2 m1)])
;;           (is-functor? t1) (do (println "YO" (type-to-string (:ret t1))) [(:ret t1) (m1 m2)])
;;           :else        (do (println "YO2 "(type-to-string (:ret t2))) [(:ret t2) (m1 m2)]))))

;; compose Types t1 and t2
(defn combine [t1 t2]
  (do
    (println "COMBINING" (type-to-string t1) "and" (type-to-string t2)) 
    (cond (and (is-functor? t1) (is-functor? t2))
          (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
            (Functor. (:ret t1) "/" (:arg t2))
            (Functor. (:ret t2) "\\" (:arg t1)))
          (is-functor? t1)  (:ret t1)
          :else             (:ret t2))))

;; (defn get-lexical-entries [word]
;;   (let [entries (lexicon word)]
;;     (loop [accum entries
;;            ents entries]
;;       (let [e (first ents)]
;;         (if (nil? e)
;;           accum
;;           (if (is-atom? e)
;;             (recur (cons (forward-tr e) (cons (backward-tr e) accum))
;;                    (rest ents))
;;             (recur accum (rest ents))))))))

;; returns the collection of lexical entries for the word, including type-raised atoms
(defn get-lexical-entries [word]
  (keys (lexicon word)))

;; returns a random entry for the word in the lexicon
(defn random-lookup-with-hooks [word]
  (if (contains? objects-detected word)
    (when (contains? (set (get-lexical-entries word)) (Atom. 'N))
      (do
        (println "SELECTING:" (type-to-string (Atom. 'N)) "for" word)
        (Atom. 'N)))
    (let [entries (get-lexical-entries word)
          selection (nth entries (rand-int (count entries)))]
      (do
        (println "SELECTING:" (type-to-string selection) "for" word)
        selection))))


;; (defn shift [sentence stack sem]
;;   (let [s1 (first sentence)]
;;     (if (not (nil? s1))
;;       (let [entry (random-lookup-with-hooks s1)]
;;         (do (println  "PUSHING:" (type-to-string entry) "onto"
;;                       (reverse (map type-to-string stack)))
;;             [(cons entry stack) (cons ((lexicon s1) entry) sem)]))
;;       [stack sem])))

;; SHIFT: push the first word's lexical entry onto the stack
(defn shift [sentence stack]
  (let [s1 (first sentence)]
    (if (not (nil? s1))
      (let [entry (random-lookup-with-hooks s1)]
        (do (println  "PUSHING:" (type-to-string entry) "onto"
                      (reverse (map type-to-string stack)))
            (cons entry stack)))
      stack)))


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


;; (defn reduce-stack [t1 t2 m1 m2]
;;   (do (println "begin")
;;       (if (or (comp-combinable? t1 t2) (app-combinable? t1 t2))
;;         (do (println "first if")
;;             (combine t1 t2 m1 m2)
;;             )
;;         (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
;;           (do (println "UNIFYING:" (type-to-string t1) "with" (type-to-string t2))
;;               (combine (unify t1 t2) t2 m2 m1))
;;           (do (println "UNIFYING:" (type-to-string t2) "with" (type-to-string t1))
;;               (combine t1 (unify t2 t1) m1 m2))))))

;; returns the reduction of t1 and t2 -- call reducible? first as a check
(defn reduce-stack [t1 t2]
  (if (or (comp-combinable? t1 t2) (app-combinable? t1 t2))
    (combine t1 t2)
    (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
      (do (println "UNIFYING:" (type-to-string t1) "with" (type-to-string t2))
          (combine (unify t1 t2) t2))
      (do (println "UNIFYING:" (type-to-string t2) "with" (type-to-string t1))
          (combine t1 (unify t2 t1))))))


;; (defn nd-sr-parse [sentence stack sem]
;;   (let [t1 (second stack)
;;         t2 (first stack)
;;         m1 (second sem)
;;         m2 (first sem)]
;;     (do (println "\nSTACK:" (reverse (map type-to-string stack)))
;;         (println "INPUT:" sentence)
;;         (if (not (empty? sentence))
;;           (if (< (count stack) 2)
;;             (let [new-stack-sem (shift sentence stack sem)]
;;               ;; recurse on shifted stack and rest of sentence
;;               (nd-sr-parse (rest sentence) (first new-stack-sem) (second new-stack-sem)))
;;             ;; else if reducible, recurse on reduced stack
;;             (if (reducible? t1 t2)
;;               (let [ [t m] (reduce-stack t1 t2 m1 m2) ]
;;                 (do (println "ONE:" t m)
;;                     (nd-sr-parse sentence (cons t (drop 2 stack))
;;                                  (cons m (drop 2 sem)))))

;;               ;; if not reducible, recurse on shifted stack and rest of sentence
;;               (let [new-stack-sem (shift sentence stack sem)] 
;;                 (nd-sr-parse (rest sentence) (first new-stack-sem) (second new-stack-sem)))))
;;           (if (< (count stack) 2)
;;             (do (println "found valid parse:" (type-to-string t2))
;;                 ;;(println sem)
;;                 true)
;;             (if (reducible? t1 t2)
;;               ;; if reducible, recurse on reduced stack
;;               (let [ [t m] (reduce-stack t1 t2 m1 m2)]
;;                 (do (println "TWO" t m)
;;                     (nd-sr-parse sentence (cons t (drop 2 stack))
;;                                  (cons m (drop 2 sem)))))
;;               ;; otherwise return failure
;;               (do (println "failed to find a valid parse")
;;                   false)))))))

;; Non-deterministic shift-reduce parse
;; returns true if the sentence has a valid parse from random lexical selections
(defn nd-sr-parse [sentence stack]
  (let [t1 (second stack)
        t2 (first stack)]
    (do (println "\nSTACK:" (reverse (map type-to-string stack)))
        (println "INPUT:" sentence)
        (if (not (empty? sentence))
          (if (< (count stack) 2)
            (let [new-stack (shift sentence stack)]
              ;; recurse on shifted stack and rest of sentence
              (nd-sr-parse (rest sentence) new-stack))
            ;; else if reducible, recurse on reduced stack
            (if (reducible? t1 t2)
              (let [t (reduce-stack t1 t2)]
                (nd-sr-parse sentence (cons t (drop 2 stack))))

              ;; if not reducible, recurse on shifted stack and rest of sentence
              (let [new-stack (shift sentence stack)] 
                (nd-sr-parse (rest sentence) new-stack))))
          (if (< (count stack) 2)
            (do (println "found valid parse:" (type-to-string t2))
                true)
            (if (reducible? t1 t2)
              ;; if reducible, recurse on reduced stack
              (let [ t (reduce-stack t1 t2)]
                (nd-sr-parse sentence (cons t (drop 2 stack))))
              ;; otherwise return failure
              (do (println "failed to find a valid parse")
                  false)))))))

(println "\n---------------------------")

(def example3 "#77 hit Jagr")
(def example1 "the puck hit Henrik")
(def example2 "Henrick saved the shot")
(def example4 "the players skate for the fans")

(nd-sr-parse (split example4 #"\s") '() )



