
;;
;; @author Saveliy Bakturin
;; <p>
;; Don't write off, if you don't wanna be banned!
;;

(defn constant [x] (fn [m] (do x)))
(defn variable [x] (fn [m] (do (m (str x)))))
(defn impl [a, m] (double (do (a m))))
(defn op [f, a, b, m] (f (impl a m) (impl b m)))
(defn add [a, b] (fn [m] (op + a b m)))
(defn subtract [a, b] (fn [m] (op - a b m)))
(defn multiply [a, b] (fn [m] (op * a b m)))
(defn divide [a, b] (fn [m] (/ (double (impl a m)) (double (impl b m)))))
(defn negate [a] (fn [m] (op - (constant 0) a m)))
(defn exp [a] (fn [m] (Math/exp (impl a m))))
(defn ln [a] (fn [m] (Math/log (Math/abs ^double (impl a m)))))

(def ops {'+ add '- subtract '/ divide '* multiply 'negate negate 'exp exp 'ln ln})
(defn convertFunction [s] (cond (symbol? s) (variable s)
                                (number? s) (constant s)
                                (= (count s) 2) (let [a (nth s 1) c (nth s 0)] ((ops c) (convertFunction a)))
                                (= (count s) 3) (let [a (nth s 1) b (nth s 2) c (nth s 0)] ((ops c) (convertFunction a) (convertFunction b)))))
(defn parseFunction [s] (cond (string? s) (let [parsed (read-string s)]
                                            (cond (number? parsed) (constant parsed)
                                                  (symbol? parsed) (variable parsed)
                                                  :else (convertFunction parsed)))
                              (symbol? s) (convertFunction (str s))))

;=== //Georgiy Korneev// ===;

(defn proto-get
  "Returns object property respecting the prototype chain"
  ([obj key] (proto-get obj key nil))
  ([obj key default]
   (cond
     (contains? obj key) (obj key)
     (contains? obj :prototype) (proto-get (obj :prototype) key default)
     :else default)))

(defn proto-call
  "Calls object method respecting the prototype chain"
  [this key & args]
  (apply (proto-get this key) this args))

(defn method
  "Creates method"
  [key] (fn [this & args] (apply proto-call this key args)))

(defn field [key]
  "Creates field"
  (fn
    ([this] (proto-get this key))
    ([this def] (proto-get this key def))))

(defn constructor
  [ctor prototype]
  (fn [& args] (apply ctor {:prototype prototype} args)))

;=== \\Georgiy Korneev\\ ===;

(def get-left (field :left))
(def get-right (field :right))
(def get-middle (field :middle))
(def get-func (field :func))
(def get-eval (field :unary))
(def get-diff (field :der))

(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))
(def toStringSuffix (method :toStringSuffix))

(declare Constant)
(declare Variable)
(declare Negate)
(declare Exp)
(declare Ln)
(declare Add)
(declare Subtract)
(declare Multiply)
(declare Divide)

(def AtomPrototype
  {
   :evaluate       (fn [this m] ((get-eval this) m))
   :toString       (fn [this] (str (get-middle this)))
   :diff           (fn [this m] ((get-diff this) m))
   :toStringSuffix (fn [this] (str (get-middle this)))
   })

(defn ConstantPrototype [this a]
  (assoc this
    :middle a
    :unary (fn [m] a)
    :der (fn [m] (Constant 0))))

(defn VariablePrototype [this a]
  (assoc this
    :middle a
    :unary (fn [m] (m (str (clojure.string/lower-case (first a)))))
    :der (fn [m] (cond (= m (str a)) (Constant 1)
                       :else (Constant 0)))))

(def UnaryPrototype
  {
   :evaluate       (fn [this m] ((get-eval this) (evaluate (get-middle this) m)))
   :toString       (fn [this] (str "(" (get-func this) " " (toString (get-middle this)) ")"))
   :diff           (fn [this m] ((get-diff this) m))
   :toStringSuffix (fn [this] (str "(" (toStringSuffix (get-middle this)) " " (get-func this) ")"))
   })

(defn NegatePrototype [this a]
  (assoc this
    :middle a
    :unary (fn [v] (* -1 v))
    :func "negate"
    :der (fn [m] (Multiply (Constant -1) (diff a m)))))

(defn ExpPrototype [this a]
  (assoc this
    :middle a
    :unary (fn [v] (Math/exp v))
    :func "exp"
    :der (fn [m] (Multiply (diff a m) (Exp a)))))

(defn LnPrototype [this a]
  (assoc this
    :middle a
    :unary (fn [v] (Math/log (Math/abs ^double v)))
    :func "ln"
    :der (fn [m] (Multiply (diff a m) (Divide (Constant 1) a)))))

(def BinaryPrototype
  {
   :evaluate       (fn [this m] (cond
                                  (and (= (get-func this) "/") (or (= (evaluate (get-right this) m) 0) (= (evaluate (get-right this) m) 0.0))) ##Inf
                                  :else ((resolve (symbol (get-func this))) (evaluate (get-left this) m) (evaluate (get-right this) m))))
   :toString       (fn [this] (str "(" (get-func this) " " (toString (get-left this)) " " (toString (get-right this)) ")"))
   :diff           (fn [this m] ((get-diff this) m))
   :toStringSuffix (fn [this] (str "(" (toStringSuffix (get-left this)) " " (toStringSuffix (get-right this)) " " (get-func this) ")"))
   })

(defn AddPrototype [this a b]
  (assoc this
    :left a
    :right b
    :func "+"
    :der (fn [m] (Add (diff a m) (diff b m)))))

(defn SubtractPrototype [this a b]
  (assoc this
    :left a
    :right b
    :func "-"
    :der (fn [m] (Subtract (diff a m) (diff b m)))))

(defn MultiplyPrototype [this a b]
  (assoc this
    :left a
    :right b
    :func "*"
    :der (fn [m] (Add (Multiply (diff a m) b) (Multiply a (diff b m))))))

(defn DividePrototype [this a b]
  (assoc this
    :left a
    :right b
    :func "/"
    :der (fn [m] (Divide (Subtract (Multiply (diff a m) b) (Multiply a (diff b m))) (Multiply b b)))))

(defn build-ato [f a] ((constructor f AtomPrototype) a))
(defn build-una [f a] ((constructor f UnaryPrototype) a))
(defn build-bin [f a b] ((constructor f BinaryPrototype) a b))

(defn build-con [a] (build-ato ConstantPrototype a))
(defn build-var [a] (build-ato VariablePrototype a))

(defn build-neg [a] (build-una NegatePrototype a))
(defn build-exp [a] (build-una ExpPrototype a))
(defn build-log [a] (build-una LnPrototype a))

(defn build-add [a b] (build-bin AddPrototype a b))
(defn build-sub [a b] (build-bin SubtractPrototype a b))
(defn build-mul [a b] (build-bin MultiplyPrototype a b))
(defn build-div [a b] (build-bin DividePrototype a b))

(defn Constant [a] (build-con a))
(defn Variable [a] (build-var a))
(defn Negate [a] (build-neg a))
(defn Exp [a] (build-exp a))
(defn Ln [a] (build-log a))
(defn Add [a b] (build-add a b))
(defn Subtract [a b] (build-sub a b))
(defn Multiply [a b] (build-mul a b))
(defn Divide [a b] (build-div a b))

(def objects {'+  Add '- Subtract '/ Divide '* Multiply 'negate Negate 'exp Exp 'ln Ln
              "+" '+ "-" '- "/" '/ "*" '* "negate" 'negate "exp" 'exp "ln" 'ln})

(defn convertObjects [s] (cond (symbol? s) (Variable (str (name s)))
                               (number? s) (Constant s)
                               (= (count s) 2) (let [a (nth s 1) c (nth s 0)] ((objects c) (convertObjects a)))
                               (= (count s) 3) (let [a (nth s 1) b (nth s 2) c (nth s 0)] ((objects c) (convertObjects a) (convertObjects b)))))

(defn parseObject [s] (let [parsed (read-string s)] (convertObjects parsed)))

;=== //Georgiy Korneev// ===;

(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)

(defn _empty [value] (partial -return value))

(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))

(defn _map [f]
  (fn [result]
    (if (-valid? result)
      (-return (f (-value result)) (-tail result)))))

(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        ((_map (partial f (-value ar)))
         ((force b) (-tail ar)))))))

(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))

(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0001})) (str input \u0001)))))
(mapv (_parser (_combine str (_char #{\a \b}) (_char #{\x}))) ["ax" "ax~" "bx" "bx~" "" "a" "x" "xa"])

(defn +char [chars] (_char (set chars)))
(defn +map [f parser] (comp (_map f) parser))
(def +ignore (partial +map (constantly 'ignore)))

(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))

(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))

(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))

(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))

(defn +or [p & ps]
  (reduce (partial _either) p ps))

(defn +opt [p]
  (+or p (_empty nil)))

(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))

(defn +plus [p] (+seqf cons p (+star p)))

(defn +str [p] (+map (partial apply str) p))

(def +parser _parser)

;=== \\Georgiy Korneev\\ ===;

(def *space (+char "\u0020\t\n\r"))
(def *whitespace (+ignore (+star *space)))
(def *alphabet (+char "xyzXYZ"))
(def *var (+str (+plus *alphabet)))
(def *digit (+char ".-0123456789"))
(def *number (+str (+plus *digit)))
(def *op (+char "+-/*"))
(def *bracket-start (+ignore (+char "(")))
(def *bracket-end (+ignore (+char ")")))
(def *constant (+map (comp Constant read-string) *number))
(def *variable (+map Variable *var))
(def *negate (+seqf str (+char "n") (+char "e") (+char "g") (+char "a") (+char "t") (+char "e")))
(def *operator-binary (+map (comp objects str) *op))
(def *operator-unary (+map objects *negate))
(declare *parser-suffix)

(def *expression (+or (+seqf #(do ((objects (nth %& 1)) (nth %& 0))) *whitespace *bracket-start *whitespace
                             (delay *parser-suffix) *whitespace *operator-unary
                             *whitespace *bracket-end *whitespace)
                      (+seqf #(do ((objects (nth %& 2)) (nth %& 0) (nth %& 1))) *whitespace *bracket-start *whitespace
                             (delay *parser-suffix) *whitespace (delay *parser-suffix) *whitespace *operator-binary
                             *whitespace *bracket-end *whitespace)))

(def *parser-suffix (+seqn 0 *whitespace (+or *variable *constant *expression) *whitespace))

(def *suffix (+parser *parser-suffix))

(defn parseObjectSuffix [expr] (*suffix expr))
