(ns alphabet-cipher.coder)

(def abc "abcdefghijklmnopqrstuvwxyz")
(def cycle-abc (cycle abc))
(def abc-map (zipmap abc (range 26)))


(defn encode [keyword message]
  (let [enc-key (take (count message) (cycle keyword))
        key-msg (partition 2 (interleave enc-key message))]
    (apply str
           (for [[k m] key-msg]
             (last (take (+ 1 (abc-map m))
                         (drop (abc-map k) cycle-abc)))))))

(defn decode [keyword message]
  (let [dec-key (take (count message) (cycle keyword))
        key-msg (partition 2 (interleave dec-key message))]
    (apply str
           (for [[k m] key-msg]
             (last (take (+ (- 26 (abc-map k)) (+ 1 (abc-map m)))
                         cycle-abc))))))
