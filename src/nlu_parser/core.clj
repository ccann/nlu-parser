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

(def lexicon {"andie" (Functor. (Atom. 'S)
                                "/"
                                (Functor. (Atom. 'S) "\\" (Atom. 'NP)))
              "see" (Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP))
                              "/"
                              (Atom. 'NP))
              "steve" (Atom. 'NP)
              "the" (Functor. (Atom. 'NP) "/" (Atom. 'N))
              "dog" (Atom. 'N)
              "John" (Atom. 'NP)
              "bit" (Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP)) "/" (Atom. 'NP))
              })

;; returns a string representation of the complex or simple type
(defn type-to-string [t]
  (cond (= (class t) Functor)
        (str "(" (type-to-string (:ret t)) (:dir t) (type-to-string (:arg t)) ")")
        (= (class t) Atom)
        (:cat t)))

(def example1 ["the" "dog" "bit" "John"])

;; returns true if Types t1 and t2 are equivalent 
(defn equivalent? [t1 t2]
  (let [c1 (class t1)
        c2 (class t2)]
    (cond
     ;; comparing two functors
     (and (= c2 Functor) (= c1 Functor))
     (and (= (:ret t1) (:ret t2))
          (= (:arg t1) (:arg t2))
          (= (:dir t1) (:dir t2)))
     ;; comparing two atoms
     (not (and (= c1 Functor) (= c2 Functor)))
     (= t1 t2)
     ;; comparing functor with atom
     (= c1 Functor)
     (= (:arg t1) t2)
     ;; comparing atom with functor
     :else
     (= t1 (:arg t2)))))

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

;; returns true if Type t1 is composable with Type t2, false otherwise
(defn composable? [t1 t2]
  (let [c1 (class t1)
        c2 (class t2)]
    (cond (and (= c2 Functor) (= c1 Functor))
          (cond
           ;; composition combinator alpha
           (and (= (:dir t2) "/") (= (:dir t1) "/"))
           (equivalent? (:arg t1) (:ret t2))
           
           ;; composition combinator beta
           (and (= (:dir t2) "\\") (= (:dir t1) "\\"))
           (equivalent? (:ret t1) (:arg t2)))

          ;; comparing LHS functor with RHS atom
          (= c1 Functor)
          (and (= (:dir t1) "/") (equivalent? (:arg t1) t2))

          ;; comparing RHS functor with LHS atom
          (= c2 Functor)
          (and (= (:dir t2) "\\") (equivalent? (:arg t2) t1))

          :else false)))

;; (defn reduce-stack [s]
;; (let [t1 (first s)
;;       t2 (second s)]
;;   (if (composable? (first t2) (first t1))
;;     [(compose (first t2)(first t1))
;;      ((second t2) (second t1))])))

;; SHIFT: push the first word's lexical entry onto the stack
(defn shift-stack [sentence stack]
  (let [s1 (first sentence)]
    (do
      ;;(println (str "pushing: " s1))
      (if (not (nil? s1))
        (cons (lexicon (first sentence)) stack)
        stack))))

;; non-deterministic parse 
(defn non-det-parse [sentence stack]
  (let [s (shift-stack sentence stack)
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
(non-det-parse example1 '())

;; TODO type raising, Unification?
;; TODO multiple entries for lexical items, choosing mechanism (must demonstrate ambiguity)
;; TODO BFS search through stack-space
;; TODO expand lexicon to a specific domain
