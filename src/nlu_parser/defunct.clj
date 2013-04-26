;; (defn combine [t1 t2 m1 m2]
;;   (do
;;     (println "COMBINING" (tts t1) "and" (tts t2)) 
;;     (cond (and (is-functor? t1) (is-functor? t2))
;;           (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
;;             [(Functor. (:ret t1) "/" (:arg t2)) (m1 m2)]
;;             [(Functor. (:ret t2) "\\" (:arg t1)) (m2 m1)])
;;           (is-functor? t1) (do (println "YO" (tts (:ret t1))) [(:ret t1) (m1 m2)])
;;           :else        (do (println "YO2 "(tts (:ret t2))) [(:ret t2) (m1 m2)]))))


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

;; ;; returns the collection of lexical entries for the word, including type-raised atoms
;; (defn get-lexical-entries [word]
;;   (keys (hockey-lexicon word)))

;; ;; returns a random entry for the word in the lexicon
;; (defn random-lookup-with-hooks [word]
;;   (if (contains? objects-detected word)
;;     (when (contains? (set (get-lexical-entries word)) (Atom. 'N))
;;       (do
;;         (println "SELECTING:" (tts (Atom. 'N)) "for" word)
;;         (Atom. 'N)))
;;     (let [entries (get-lexical-entries word)
;;           selection (nth entries (rand-int (count entries)))]
;;       (do
;;         (println "SELECTING:" (tts selection) "for" word)
;;         selection))))


;; (defn shift [sentence stack sem]
;;   (let [s1 (first sentence)]
;;     (if (not (nil? s1))
;;       (let [entry (random-lookup-with-hooks s1)]
;;         (do (println  "PUSHING:" (tts entry) "onto"
;;                       (reverse (map tts stack)))
;;             [(cons entry stack) (cons ((lexicon s1) entry) sem)]))
;;       [stack sem])))

;; shift: push the first word's lexical entry onto the stack
;; (defn shift [sentence stack]
;;   (let [s1 (first sentence)]
;;     (if (not (nil? s1))
;;       (let [entry (random-lookup-with-hooks s1)]
;;         (do (println  "PUSHING:" (tts entry) "onto"
;;                       (reverse (map tts stack)))
;;             (cons entry stack)))
;;       stack)))


;; (defn reduce-stack [t1 t2 m1 m2]
;;   (do (println "begin")
;;       (if (or (comp-combinable? t1 t2) (app-combinable? t1 t2))
;;         (do (println "first if")
;;             (combine t1 t2 m1 m2)
;;             )
;;         (if (and (= (:dir t2) "/") (= (:dir t1) "/"))
;;           (do (println "UNIFYING:" (tts t1) "with" (tts t2))
;;               (combine (unify t1 t2) t2 m2 m1))
;;           (do (println "UNIFYING:" (tts t2) "with" (tts t1))
;;               (combine t1 (unify t2 t1) m1 m2))))))


;; (defn nd-sr-parse [sentence stack sem]
;;   (let [t1 (second stack)
;;         t2 (first stack)
;;         m1 (second sem)
;;         m2 (first sem)]
;;     (do (println "\nSTACK:" (reverse (map tts stack)))
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
;;             (do (println "found valid parse:" (tts t2))
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
;; (defn nd-sr-parse [sentence stack]
;;   (let [t1 (second stack)
;;         t2 (first stack)]
;;     (do (println "\nSTACK:" (reverse (map tts stack)))
;;         (println "INPUT:" sentence)
;;         (if (not (empty? sentence))
;;           (if (< (count stack) 2)
;;             (let [new-stack (shift sentence stack)]
;;               ;; recurse on shifted stack and rest of sentence
;;               (nd-sr-parse (rest sentence) new-stack))
;;             ;; else if reducible, recurse on reduced stack
;;             (if (reducible? t1 t2)
;;               (let [t (reduce-stack t1 t2)]
;;                 (nd-sr-parse sentence (cons t (drop 2 stack))))

;;               ;; if not reducible, recurse on shifted stack and rest of sentence
;;               (let [new-stack (shift sentence stack)] 
;;                 (nd-sr-parse (rest sentence) new-stack))))
;;           (if (< (count stack) 2)
;;             (do (println "found valid parse:" (tts t2))
;;                 true)
;;             (if (reducible? t1 t2)
;;               ;; if reducible, recurse on reduced stack
;;               (let [ t (reduce-stack t1 t2)]
;;                 (nd-sr-parse sentence (cons t (drop 2 stack))))
;;               ;; otherwise return failure
;;               (do (println "failed to find a valid parse")
;;                   false)))))))
