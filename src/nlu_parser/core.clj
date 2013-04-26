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
(defn tts [t]
  (cond (is-functor? t)
        (str "(" (tts (:ret t)) (:dir t) (tts (:arg t)) ")")
        (is-atom? t)
        (:cat t)))

;; LEXICON 
(def hockey-lexicon {"77" {(Atom. 'NP) '77
                            (forward-tr (Atom. 'NP)) (fn [x] (x '77))
                            (backward-tr (Atom. 'NP)) (fn [x] (x '77))}
              
                     "loved" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                                (fn [y] (fn [x] ['thank y x]))}

                     "jagr" {(Atom. 'NP) 'jagr
                             (forward-tr (Atom. 'NP)) (fn [x] (x 'jagr))
                             (backward-tr (Atom. 'NP)) (fn [x] (x 'jagr))}
              
                     ;; "checked" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                     ;;            (fn [y] (fn [x] ['check y x]))}
              
                     ;; "the" {(Functor. (Atom. 'NP) "/" (Atom. 'N))
                     ;;        (fn [p] ['def p])}

                     ;; "fans" {(Atom. 'N) (fn [x] ['fans x])
                     ;;         (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                     ;;         (fn [q] (fn [x] [(q 'fans) x]))}
              
                     ;; "saved" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'PP))
                     ;;          (fn [y] (fn [x] ['save y x]))}

                     ;; "skated" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'PP))
                     ;;           (fn [y] (fn [x] ['skate y x]))}

                     ;; "skate" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'PP))
                     ;;          (fn [y] (fn [x] ['skate y x]))
                     ;;          (Atom. 'N) (fn [x] ['skate x])}
                     
                     ;; "for" {(Functor. (Atom. 'PP) "/" (Atom. 'NP)) (fn [x] x)}
              
                     ;; "skater" {(Atom. 'N) (fn [x] ['skater x])
                     ;;           (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                     ;;           (fn [q] (fn [x] [(q 'skater) x]))}

                     ;; "player" {(Atom. 'N) (fn [x] ['player x])
                     ;;           (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                     ;;           (fn [q] (fn [x] [(q 'player) x]))}

                     ;; "players" {(Atom. 'N) (fn [x] ['players x])
                     ;;            (Functor. (Atom. 'N) "/" (Functor. (Atom. 'N) "\\" (Atom. 'N)))
                     ;;            (fn [q] (fn [x] [(q 'players) x]))}
              
                     ;; "puck" {(Atom. 'N) (fn [x] ['puck x])}

                     ;; "henrik" {(Atom. 'NP) 'henrik
                     ;;           (forward-tr (Atom. 'NP)) (fn [x] (x 'henrik))}

                     ;; "shot" {(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
                     ;;         (fn [y] (fn [x] ['shoot y x]))
                     })                                      



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

;; compose Types t1 and t2
(defn combine [t1 t2]
  (do
    (println "COMBINING" (tts t1) "and" (tts t2)) 
    (cond (and (is-functor? t1) (is-functor? t2))
          (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
            (Functor. (:ret t1) "/" (:arg t2))
            (Functor. (:ret t2) "\\" (:arg t1)))
          (is-functor? t1)  (:ret t1)
          :else             (:ret t2))))


;; returns the reduction of t1 and t2 -- call reducible? first as a check
(defn ccg-reduce-types [t1 t2]
  (if (or (comp-combinable? t1 t2) (app-combinable? t1 t2))
    (combine t1 t2)
    (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
      (do (println "UNIFYING:" (tts t1) "with" (tts t2))
          (combine (unify t1 t2) t2))
      (do (println "UNIFYING:" (tts t2) "with" (tts t1))
          (combine t1 (unify t2 t1))))))

;; SHIFT: push the  onto the stack
(defn ccg-push [entry stack]
  (if (not (nil? entry))
    (let [result (cons entry stack)]
      (do (println "PUSHING:" (tts entry) "onto" (reverse (map tts stack)))
          result))
    (let [result stack]
      (do (println "ERROR!: trying to push nil onto stack")
          result))))

(defn lookup [word lex]
  (keys (lex word)))

(defn ccg-shift [word ws lex]
  (let [entries (lookup word lex)]
    (if (empty? ws)
      (for [e entries] [e])
      (apply concat (for [stack ws]
                      (for [e entries] (ccg-push e stack)))))))

(defn ccg-reduce [stack]
  (let [t2 (first stack)
        t1 (second stack)]
    (if (reducible? t1 t2)
      (ccg-reduce (cons (ccg-reduce-types t1 t2) (drop 2 stack)))
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

(defn ccg-parse [sent workspace lex]
  (if (not (empty? sent))
    (let [word (first sent)
          ws (ccg-shift word workspace lex)]
      (ccg-parse (rest sent)       
                 (for [stack ws]
                   (ccg-reduce stack))
                 lex))
    (let [succs (filter (partial = [(Atom. 'S)]) workspace)]
      (if (empty? succs)
        (println "FAILURE: could not find a valid parse.")
        (println "SUCCESS: found" (count succs) "valid parses amidst"
                 (count workspace) "possible parses.")))))



;; set of objects from vision that are located in the immediate environment
(def objects-detected #{"puck" "skate"})

(println "\n---------------------------")

(def example3 "jagr loved 77")
(def answer (ccg-parse (split example3 #"\s") '() hockey-lexicon))

