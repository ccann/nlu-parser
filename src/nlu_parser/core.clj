(ns nlu-parser.core)


(defrecord Atom [cat])
(defrecord Functor [ret dir arg])

;; the Stack in the workspace

(def andie [(Functor. (Atom. 'S)
                       "/"
                       (Functor. (Atom. 'S) "\\" (Atom. 'NP))
                       )
             #(%1 'andie)])
(def see [(Functor. (Functor. (Atom. 'S) "\\" (Atom. 'NP))
                     "/"
                     (Atom. 'NP))
          #('see %2 %1)])

(def steve [(Atom. 'NP)
            'steve])

(def stack (conj (conj '() andie) see))

;; test if two terms, f1 and f2, are equivalent
(defn equivalent [f1 f2]
  (cond
   ;; comparing two functors
   (and (= (class f2) Functor) (= (class f1) Functor))
   (and (= (:ret f1) (:ret f2))
        (= (:arg f1) (:arg f2))
        (= (:dir f1) (:dir f2)))
   ;; comparing two atoms
   (not (and (= (class f2) Functor) (= (class f1) Functor)))
   (= f1 f2)
   ;; comparing a functor with an atom
   :else false))


;; do composition on functions f1 and f2
(defn compose [f1 f2]
  (do
    (println (str "composing " f1 " " f2))
    (cond
     ;; composition combinator alpha
     (and (= (:dir f2) "/") (= (:dir f1) "/"))
     (if (equivalent (:arg f1) (:ret f2))
       (Functor. (:ret f1) "/" (:arg f2)))
     ;; composition combinator beta
     (and (= (:dir f2) "\\") (= (:dir f1) "\\"))
     (if (equivalent (:ret f1) (:arg f2))
       (Functor. (:ret f2) "\\" (:arg f1))))))

(def first-pass (compose (first (last stack)) (first (first stack))))
(compose first-pass (first steve))

;; something doesn't make sense here... we're accessing :dir of the atom in compose. We
;; should be checking if f1 and f2 ARE functors before we do the rule.
