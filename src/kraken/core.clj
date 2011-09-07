(ns kraken.core)
;; Interpreting quality inherence as a relation between
;; a particular and a quality universal (i.e. no tropes)
;; Quality patterns are represented as vectors of quality universals

;; utility
(defn in? [x s] (some #(= x %) s))

;; primitives
(defn lebesgue-measure [x] 1)
(defn has-quality? [x q] true)
(defn proper-sub-universal-of? [x b] true)
(defn parts [x] x)
(defn partition? [x] true)
(defn quality-list [] [1 2 3])
(defn quality-pattern [x] (filter #(has-quality? x %) (quality-list)))

;; formal definitions
(defn exactly-the-same-size? [x y] (= (lebesgue-measure x)
                                      (lebesgue-measure y)))
(defn roughly-the-same-size? [x y w]
  (let [min (/ 1 (+ 1 w))
        max (+1 w)
        ratio (/ (lebesgue-measure x) (lebesgue-measure y))]
    (and (<= min ratio)
         (<= ratio max))))
(defn negligible-in-size? [x y w]
  (let [ratio (/ (lebesgue-measure x) (lebesgue-measure y))
        max (/ w (+ 1 w))]
    (<= ratio max)))
(defn same-scale? [x y w]
  (let [min (/ w (+ 1 w))
        ratio (/ (lebesgue-measure x) (lebesgue-measure y))
        max (/ (+ 1 w) w)]))

(defn region-sum [a &b] (reduce + (map lebesque-measure (conj (list a) b))))
(defn homogeneous?
  "the area of the sum of parts of x that have a quality in Q is roughly the same size as the area of x"
  [x Q w]
  (let [r-list (parts x)
        has-quality? (fn (r) (and (has-quality? r q) (in? q Q)))
        q-region (region-sum (filter has-quality? r-list))]
    (roughly-the-same-size? x q-region w)))

(defn sub-pattern? [q1 q2] (every? #(in? % q2) q1))
(defn proper-sub-pattern? [q1 q2] (and (sub-pattern? q1 q2) (not= q1 q2)))
(defn species-pattern?
"every member of q1 is a proper sub-universal of some member of q2"
  [q1 q2]
  (let [f (fn [q Q] (some #(proper-sub-universal-of q %) Q))]
    (every? #(f % q2) q1)))
(defn genus-pattern? [q2 q1] (species-pattern? q1 q2))

(defn w-Q-partition? [p x w Q]
  (and (partition? p x)
       (every? #(homogeneous? % Q w) p)
       (distinct? (map quality-pattern p))))
