(ns nlu-parser.core)

(defrecord Atom [cat])
(defrecord Functor [ret dir arg])

;; (def andie [(Functor. (Atom. 'S)
;;                        "/"
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
  (cond (= (class t) Functor)
        (str "(" (type-to-string (:ret t)) (:dir t) (type-to-string (:arg t)) ")")
        (= (class t) Atom)
        (:cat t)))



;; compose Types t1 and t2
(defn compose [t1 t2]
  (let [c1 (class t1)
        c2 (class t2)]
    (cond (and (= c2 Functor) (= c1 Functor))
          (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
            (Functor. (:ret t1) "/" (:arg t2))
            (Functor. (:ret t2) "\\" (:arg t1)))
          (= c1 Functor) (:ret t1)
          :else          (:ret t2))))

;; returns true if two Types are composable by composition or appplication combinators
(defn composable? [t1 t2]
  (let [c1 (class t1)
        c2 (class t2)]
    (cond (and (= c2 Functor) (= c1 Functor))
          (cond
           ;; > Forward composition combinator
           (and (= (:dir t2) "/") (= (:dir t1) "/"))
           (= (:arg t1) (:ret t2))
           
           ;; < Backward composition combinator
           (and (= (:dir t2) "\\") (= (:dir t1) "\\"))
           (= (:ret t1) (:arg t2)))

          ;; > Forward application combinator
          (= c1 Functor)
          (and (= (:dir t1) "/") (= (:arg t1) t2))

          ;; < Backward application combinator
          (= c2 Functor)
          (and (= (:dir t2) "\\") (= (:arg t2) t1))

          :else false)))

;; (defn reduce-stack [s]
;; (let [t1 (first s)
;;       t2 (second s)]
;;   (if (composable? (first t2) (first t1))
;;     [(compose (first t2)(first t1))
;;      ((second t2) (second t1))])))

;;(lexicon (first sentence))

;; returns a random entry for the word in the lexicon
(defn random-lookup [word]
  (let [entries (lexicon word)]
    (nth entries (rand-int (count entries)))))

;; SHIFT: push the first word's lexical entry onto the stack
(defn shift [sentence stack]
  (let [s1 (first sentence)]
    (do
      ;;(println (str "pushing: " s1))
      (if (not (nil? s1))
        (cons (random-lookup (first sentence)) stack)
        stack))))

;; non-deterministic parse 
(defn non-det-parse [sentence stack]
  (let [s (shift sentence stack)
        sent (rest sentence)
        t1 (first s)
        t2 (second s)]
    (do (println "\nParse list: " sentence)
        (println "Stack: " (reverse (map type-to-string s)))

        ;; sentence empty, all words have been pushed to stack
        (if (empty? sent)
          (cond (= (count s) 1)
                (println "found valid parse: " (:cat (first s)))
                
                (composable? t2 t1)
                (do
                  (println "composing " (type-to-string t2) " and " (type-to-string t1))
                  (non-det-parse sent
                                 (cons (compose t2 t1) (rest (rest s)))))
                
                :else (do (println "failed to find a valid parse")
                          s))
          
          ;; sentence non-empty
          (if (> (count s) 1)
            (if (composable? t2 t1)
              (do
                (println "composing " (type-to-string t2) " and " (type-to-string t1))
                (non-det-parse sent
                               (cons (compose t2 t1) (rest (rest s)))))
              (non-det-parse sent s))
            (non-det-parse sent s))))))


(println "\n---------------------------")

(def example3 '("andie" "loves" "steve"))
(def example1 '("the" "dog" "bit" "John"))
(def example2 '("andie" "saw" "the" "dog"))

(non-det-parse example2 '())

;; TODO type raising, Unification?
;; TODO multiple entries for lexical items, choosing mechanism (must demonstrate ambiguity)
;; TODO BFS search through stack-space
;; TODO expand lexicon to a specific domain


;; functional composition: allows two functional categories to combine partially.
;; type-raising: converts atomic or otherwise simpler categories into more complex
;; functional categories
;; proper names: NP
;; transitive verbs: (S\NP)/NP
;; left-branching derivations produce well-formed semantic representations
;; This is a side-effect of type-raising and functional composition

;; The parser: consults the lexicon to find out the possible lexical categories and
;; semantic representations for the input word
