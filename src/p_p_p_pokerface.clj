(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        letters {\T 10, \J 11, \Q 12, \K 13, \A 14}] 
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (letters r))))

(defn suit [card]
  (let [[_ s] card] (str s)))

(defn- n-of-a-kind? [hand n]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (> (count (filter #(== % n) freqs)) 0)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
    (> (count (filter #(== % 5) freqs)) 0)))

(defn full-house? [hand]
  (let [ranks  (map rank hand)
        freqs  (vals (frequencies ranks))
        pairs  (count (filter #(== % 2) freqs))
        threes (count (filter #(== % 3) freqs))]
    (== pairs threes 1)))

(defn two-pairs? [hand]
  (let [ranks  (map rank hand)
        freqs  (vals (frequencies ranks))
        pairs  (count (filter #(== % 2) freqs))]
    (== pairs 2)))

(defn straight? [hand]
   (let [ranks           (sort (map rank hand))
         min-ranks       (apply min ranks)
         ranks-ace-first (sort (replace {14 1} ranks))
         min-ace-first   (apply min ranks-ace-first)]
    (or (= ranks (range min-ranks (+ min-ranks 5)))
      (= ranks-ace-first (range min-ace-first (+ min-ace-first 5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
