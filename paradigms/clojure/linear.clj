
;;
;; @author Saveliy Bakturin
;; <p>
;; Don't write off, if you don't wanna be banned!
;;

(defn v+ [a, b] (mapv + a b))
(defn v- [a, b] (mapv - a b))
(defn v* [a, b] (mapv * a b))
(defn vd [a, b] (mapv / a b))
(defn scalar [a, b] (apply + (v* a b)))
(defn det [a, b, c, d] (- (* a d) (* b c)))
(defn vect [a, b] (vector (det (get a 1) (get a 2) (get b 1) (get b 2))
                          (* -1 (det (get a 0) (get a 2) (get b 0) (get b 2)))
                          (det (get a 0) (get a 1) (get b 0) (get b 1))))
(defn v*s [a, b] (mapv * a (iterate * b)))
(defn convert [a] (into [] a))
(defn m+ [a, b] (mapv v+ a b))
(defn m- [a, b] (mapv v- a b))
(defn m* [a, b] (mapv v* a b))
(defn md [a, b] (mapv vd a b))
(defn m*s [a, b] (convert (for [x a] (v*s x b))))
(defn m*v [a, b] (convert (for [x a] (scalar b x))))
(defn transpose [a] (convert (apply map vector a)))
(defn m*m [a, b] (convert (for [y a] ((fn [v] (mapv (partial scalar v) (transpose b))) y))))
(defn c+ [a, b] (mapv m+ a b))
(defn c- [a, b] (mapv m- a b))
(defn c* [a, b] (mapv m* a b))
(defn cd [a, b] (mapv md a b))
