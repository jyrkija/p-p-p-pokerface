(ns p-p-p-pokerface)

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r)
    )
  )

(defn suit [[_ s]] (str s))

(defn rank-frequencies [hand] (frequencies (map rank hand)))

(defn same-rank-counts [hand] (vals (rank-frequencies hand)))

(defn suits [hand] (frequencies (map suit hand)))

(defn same-suit-counts [hand] (vals (suits hand)))

(defn pair? [hand] (contains? (set (same-rank-counts hand)) 2))

(defn three-of-a-kind? [hand] (contains? (set (same-rank-counts hand)) 3))

(defn four-of-a-kind? [hand] (contains? (set (same-rank-counts hand)) 4))

(defn flush? [hand] (contains? (set (same-suit-counts hand)) 5))

(defn full-house? [hand] (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand] (= 2 (get (frequencies (same-rank-counts hand)) 2)))

(defn replace-ace [ranks] (replace {14 1} ranks))

(defn ranks-straight? [ranks] (let [smallest (apply min ranks)]
                                (= (sort ranks) (range smallest (+ smallest 5)))))

(defn straight? [hand] (let [ranks (map rank hand)]
                         (or (ranks-straight? ranks) (ranks-straight? (replace-ace ranks)))
                         ))

(defn straight-flush? [hand] (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand] (let [checkers #{[high-card? 0]
                                    [pair? 1]
                                    [two-pairs? 2]
                                    [three-of-a-kind? 3]
                                    [straight? 4]
                                    [flush? 5]
                                    [full-house? 6]
                                    [four-of-a-kind? 7]
                                    [straight-flush? 8]}
                         matches (fn [check] ((first check) hand))]
                     (apply max (map second (filter matches checkers)))
  ))
