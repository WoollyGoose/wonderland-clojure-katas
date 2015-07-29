(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

;This solution generates a doublets word trail based on the following:
; 1) Create a list of possible words that have the same number of letters as the starting word.
; 2) Select the possible words that differ only one letter from previous word in the word trail.
; 3) If step 2 returns more than one word, select the word that differs least from the target word (word2).
; 4) For the next iteration, conj selected word to word-trail and make sure the possible-words list excludes
;    word-trail words.

(defn next-word-finder
"This function filters the 'possible-words' that differ one letter from the 'test-word'
(the last word in the word-trail)."
  [test-word possible-words]
  (filter (fn [w] (= (- (count test-word) 1)
                     (reduce + (map #(if (= % %2) 1 0) test-word w))))
          possible-words))

(defn word-selector
  "This function selects the word, from a word list, that differs least
  from the target word. This is used when the 'next-words' list contains
  more than one word."
  [target-word next-word-list]
  (last (sort-by (fn [w] (reduce + (map #(if (= % %2) 1 0) target-word w)))
                 next-word-list)))

(defn doublets [word1 word2]
  (loop [poss-words (filter #(= (count %) (count word1)) words)
         word-trail [word1]]
    (let [next-words (next-word-finder (last word-trail) poss-words)]
      (cond
        (not= (count word1) (count word2)) []
        (= (last word-trail) word2) word-trail
        (< 1 (count next-words)) (recur (remove (set word-trail) poss-words)
                                        (conj word-trail (word-selector word2 next-words)))
        (= 1 (count next-words)) (recur (remove (set word-trail) poss-words)
                                        (conj word-trail (first next-words)))))))
