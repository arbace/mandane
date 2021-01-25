(ns mandane.core
    (:refer-clojure :only [])
)

(ns mandane.bore
    (:refer-clojure :only [*in* *out*])
    (:require [clojure.core :as -])
)

(-/defmacro refer! [ns s]
    (-/let [f (-/fn [%] (-/let [v (-/ns-resolve (-/the-ns ns) %) n (-/vary-meta % -/merge (-/select-keys (-/meta v) [:macro]))] `(def ~n ~(-/var-get v))))]
        (if (-/symbol? s) (f s) (-/cons 'do (-/map f s)))
    )
)

(refer! clojure.core [+ - = aget alength and apply aset atom case char char? cond conj cons declare defmacro defn deref false? first fn fn? identical? int let list long long-array loop memoize neg? next nil? not number? or seq seq? seqable? sequential? str string? swap! symbol symbol? the-ns time true? var? when])

(defn &throw! [& s] (throw (Error. (apply str s))))

(defn &bits [s] (char (Long/parseLong (if (number? s) (str s) s), 2)))
(def  &bits?     char?)
(def  &bits=     =)

(defn &zero  []       (char 0))
(defn &zero? [x] (= x (char 0)))

(defn &inc [x] (char (+ (long x) 1)))
(defn &dec [x] (char (- (long x) 1)))

(defn &read   [_]   (let [x (.read *in*)] (when (not (neg? x)) (char x))))
(defn &unread [_ c] (.unread *in*, (int c)))
(defn &append [_ x] (.append *out*, x) _)
(defn &flush  [_]   (.flush *out*) _)

(def -apply       apply)
(def -conj        conj)
(def -first       first)
(def -fn?         fn?)
(defn &identical? [x y] (cond (and (char? x) (char? y)) (= x y) (and (number? x) (number? y)) (= x y) 'else (identical? x y)))
(def -next        next)
(def -seq         seq)
(def -seq?        seq?)
(def -seqable?    seqable?)
(def -sequential? sequential?)
(def -str         str)
(def -string?     string?)
(def -symbol?     symbol?)
(def -var?        var?)

(defn -var-find [s] (.findInternedVar (the-ns 'mandane.core), (symbol s)))
(defn -var-get  [v] (.get v))

(def Mandane'ram (long-array (+ 16777216 16777216 16777216)))
(def Mandane'gc (atom (alength Mandane'ram)))

(defn &anew [n] (let [a (swap! Mandane'gc - n)] (if (neg? a) (&throw! "ram non più at " a) a)))

(defn &aget [a i]
    (let [x (aget Mandane'ram (+ a i))]
        (case x -65537 nil -65538 false -65539 true (if (neg? x) (char (- 0 x 1)) x))
    )
)

(defn &aset [a i x]
    (let [x (cond (nil? x) -65537 (false? x) -65538 (true? x) -65539 (char? x) (- 0 (long x) 1) 'else x)]
        (aset Mandane'ram (+ a i) x)
    )
)

(defn &car [s] (&aget s 1))
(defn &cdr [s] (&aget s 2))

(defn &volatile-cas-cdr! [a x y] (when (&identical? (&aget a 2) x) (&aset a 2 y) a))
(defn &volatile-get-cdr  [a]     (&aget a 2))
(defn &volatile-set-cdr! [a x]   (&aset a 2 x) a)

(defn &cons  [car cdr] (let [a (&anew 3)] (&aset a 0 false) (&aset a 1 car) (&aset a 2 cdr) a))
(defn &meta  [car cdr] (let [a (&anew 3)] (&aset a 0 true)  (&aset a 1 car) (&aset a 2 cdr) a))
(defn &meta! [car cdr] (let [a (&anew 3)] (&aset a 0 true)  (&aset a 1 car) (&volatile-set-cdr! a cdr)))

(def &array? number?)

(defn &cons? [x] (and (&array? x) (not (&aget x 0))))
(defn &meta? [x] (and (&array? x)      (&aget x 0)))

(ns mandane.core
    (:refer-clojure :only [])
    (:require [mandane.bore :as -])
)

(-/refer! mandane.bore [and &append &bits &bits? &bits= &car &cdr cond &cons &cons? &dec declare defn &flush fn &identical? &inc let loop &meta &meta! &meta? or &read &throw! &unread &volatile-cas-cdr! &volatile-get-cdr &volatile-set-cdr! when &zero &zero?])

(defn -eager! [s] (if (-/-fn? s) (-/-apply s nil) s))

(-/defmacro lazy-seq [& s] (-/list '-eager! (-/cons 'fn (-/cons [] s))))
(-/defmacro &do      [& s] (-/cons 'do s))

(def &'list              (&bits '1110000000000000))
(def &'string            (&bits '1110000000000001))
(def &'symbol            (&bits '1110000000000010))
(def &'closure           (&bits '1110000000000011))
(def &'atom              (&bits '1110000000000100))

(def &'let               (&bits '1110000000001000))
(def &'if                (&bits '1110000000001001))
(def &'apply             (&bits '1110000000001010))
(def &'fn                (&bits '1110000000001011))
(def &'quote             (&bits '1110000000001100))
(def &'binding           (&bits '1110000000001101))
(def &'var-get           (&bits '1110000000001110))
(def &'var-set!          (&bits '1110000000001111))

(def &'bits              (&bits '1110000000010001))
(def &'bits?             (&bits '1110000000010010))
(def &'bits=             (&bits '1110000000010011))
(def &'zero              (&bits '1110000000010100))
(def &'zero?             (&bits '1110000000010101))
(def &'inc               (&bits '1110000000010110))
(def &'dec               (&bits '1110000000010111))
(def &'identical?        (&bits '1110000000011000))
(def &'read              (&bits '1110000000011001))
(def &'unread            (&bits '1110000000011010))
(def &'append            (&bits '1110000000011011))
(def &'flush             (&bits '1110000000011100))
(def &'car               (&bits '1110000000011101))
(def &'cdr               (&bits '1110000000011110))
(def &'cons?             (&bits '1110000000011111))
(def &'meta?             (&bits '1110000000100000))
(def &'cons              (&bits '1110000000100001))
(def &'meta              (&bits '1110000000100010))
(def &'meta!             (&bits '1110000000100011))
(def &'volatile-cas-cdr! (&bits '1110000000100100))
(def &'volatile-get-cdr  (&bits '1110000000100101))
(def &'volatile-set-cdr! (&bits '1110000000100110))
(def &'throw!            (&bits '1110000000100111))

(defn &some? [x] (if (&identical? x nil) false true))

(defn &1 [s] (&car                   s))
(defn &2 [s] (&car             (&cdr s)))
(defn &3 [s] (&car       (&cdr (&cdr s))))
(defn &4 [s] (&car (&cdr (&cdr (&cdr s)))))

(declare apply)

(defn &eval [e m]
    (if (&meta? e)
        (let [a (&car e) s (&cdr e)]
            (cond
                (&bits= a &'let)        (&eval (&cdr s) (&cons (&eval (&car s) m) m))

                (&bits= a &'if)         (&eval (if (&1 m) (&car s) (&2 s)) (&cdr m))

                (&bits= a &'apply)
                    (let [
                        s
                            (loop [i (&car s) s nil m m]
                                (if (&zero? i)
                                    s
                                    (recur (&dec i) (&cons (&car m) s) (&cdr m))
                                )
                            )
                        c (&car s) s (&cdr s)
                    ]
                        (when (and (&meta? c) (&bits= (&car c) &'closure))
                            (let [
                                g (&car (&cdr c)) n (&cdr (&cdr c))
                                n
                                    (loop [n (if (&1 g) (&cons c n) n) p (&2 g) s s]
                                        (if (&zero? p)
                                            (if (&3 g) (&cons s n) n)
                                            (recur (&cons (&car s) n) (&dec p) (&cdr s))
                                        )
                                    )
                            ]
                                (&eval (&4 g) n)
                            )
                        )
                    )
                (&bits= a &'fn)         (&meta &'closure (&cons s m))

                (&bits= a &'quote)      (&car s)
                (&bits= a &'binding)
                    (let [i (&car s)]
                        (when (&some? m)
                            (if (&zero? i)
                                (&car m)
                                (&eval (&meta &'binding (&cons (&dec i) nil)) (&cdr m))
                            )
                        )
                    )

                (&bits= a &'var-get)    (&volatile-get-cdr (&car s))
                (&bits= a &'var-set!)   (let [_ (&volatile-set-cdr! (&car s) (&1 m))] nil)

                (&bits= a &'bits)       (&bits (apply -/-str (&1 m)))
                (&bits= a &'bits?)      (&bits? (&1 m))
                (&bits= a &'bits=)      (&bits= (&2 m) (&1 m))

                (&bits= a &'zero)       (&zero)
                (&bits= a &'zero?)      (&zero? (&1 m))

                (&bits= a &'inc)        (&inc (&1 m))
                (&bits= a &'dec)        (&dec (&1 m))

                (&bits= a &'identical?) (&identical? (&2 m) (&1 m))

                (&bits= a &'read)       (&read (&1 m))
                (&bits= a &'unread)     (&unread (&2 m) (&1 m))
                (&bits= a &'append)     (&append (&2 m) (&1 m))
                (&bits= a &'flush)      (&flush (&1 m))

                (&bits= a &'car)        (&car (&1 m))
                (&bits= a &'cdr)        (&cdr (&1 m))

                (&bits= a &'cons?)      (&cons? (&1 m))
                (&bits= a &'meta?)      (&meta? (&1 m))

                (&bits= a &'cons)       (&cons (&2 m) (&1 m))
                (&bits= a &'meta)       (&meta (&2 m) (&1 m))
                (&bits= a &'meta!)      (&meta! (&2 m) (&1 m))

                (&bits= a &'volatile-cas-cdr!) (&volatile-cas-cdr! (&3 m) (&2 m) (&1 m))
                (&bits= a &'volatile-get-cdr)  (&volatile-get-cdr (&1 m))
                (&bits= a &'volatile-set-cdr!) (&volatile-set-cdr! (&2 m) (&1 m))

                (&bits= a &'throw!)     (&throw! "oops!")
                'else                   e
            )
        )
        e
    )
)

(defn identical? [x y] (&identical? x y))

(defn identity [x] x)

(defn nil?   [x] (identical? x nil))
(defn true?  [x] (identical? x true))
(defn false? [x] (identical? x false))
(defn not    [x] (if x false true))
(defn some?  [x] (not (nil? x)))

(declare cons?)    (declare Cons''seq)
(declare list?)    (declare List''seq)
(declare string?)  (declare String''seq)
(declare symbol?)  (declare Symbol''seq)
(declare closure?) (declare Closure''seq)

(defn seq [s]
    (cond
        (nil? s)         nil
        (cons? s)       (Cons''seq s)
        (list? s)       (List''seq s)
        (string? s)     (String''seq s)
        (symbol? s)     (Symbol''seq s)
        (closure? s)    (Closure''seq s)
        (-/-seqable? s) (-/-seq s)
        (-/-symbol? s)  (-/-seq (-/-str s))
        (-/-fn? s)      (-/-apply s nil)
        'else           (&throw! "seq not supported on " s)
    )
)

(declare Cons''first)

(defn first [s]
    (let [s (seq s)]
        (cond
            (nil? s)     nil
            (cons? s)   (Cons''first s)
            (-/-seq? s) (-/-first s)
            'else       (&throw! "first not supported on " s)
        )
    )
)

(declare Cons''next)

(defn next [s]
    (let [s (seq s)]
        (cond
            (nil? s)     nil
            (cons? s)   (Cons''next s)
            (-/-seq? s) (-/-next s)
            'else       (&throw! "next not supported on " s)
        )
    )
)

(defn second [s] (first (next s)))
(defn third  [s] (first (next (next s))))
(defn fourth [s] (first (next (next (next s)))))

(defn last [s] (let [r (next s)] (if (some? r) (#_recur last r) (first s))))

(defn reduce [f r s] (let [s (seq s)] (if (some? s) (#_recur reduce f (f r (first s)) (next s)) r)))

(defn reduce-kv [f r kvs] (let [kvs (seq kvs)] (if (some? kvs) (#_recur reduce-kv f (f r (first kvs) (second kvs)) (next (next kvs))) r)))

(defn Atom'new [init] (&meta! &'atom init))

(defn atom? [x] (and (&meta? x) (&bits= (&car x) &'atom)))

(defn Atom''deref [this]
    (&volatile-get-cdr this)
)

(defn Atom''swap! [this, f, s]
    (loop []
        (let [o (&volatile-get-cdr this) o' (apply f o s)]
            (if (&volatile-cas-cdr! this o o') o' (recur))
        )
    )
)

(defn Atom''reset! [this, o']
    (&volatile-set-cdr! this o')
    o'
)

(defn atom [x] (Atom'new x))

(defn deref [a]
    (cond
        (atom? a)   (Atom''deref a)
        (-/-var? a) (-/-var-get a)
        'else       (&throw! "deref not supported on " a)
    )
)

(defn swap! [a f & s]
    (cond
        (atom? a) (Atom''swap! a, f, s)
        'else     (&throw! "swap! not supported on " a)
    )
)

(defn reset! [a x]
    (cond
        (atom? a) (Atom''reset! a, x)
        'else     (&throw! "reset! not supported on " a)
    )
)

(defn Cons'new [car, cdr] (&cons car cdr))

(defn cons? [x] (&cons? x))

(defn Cons''seq   [this]            this)
(defn Cons''first [this]      (&car this))
(defn Cons''next  [this] (seq (&cdr this)))

(defn List'new [s] (&meta &'list s))

(defn list? [x] (and (&meta? x) (&bits= (&car x) &'list)))

(defn List''seq [this] (seq (&cdr this)))

(declare reverse)

(defn cons [x s] (Cons'new x, (if (-/-seq? s) (reverse (reverse s)) s)))

(defn cons* [x & s] (cons x s))

(defn conj [s x] (cons x s))

(defn spread [s]
    (cond
        (nil? s)         nil
        (nil? (next s)) (seq (first s))
        'else           ((if (-/-seq? s) -/cons cons) (first s) (spread (next s)))
    )
)

(defn reverse [s] (reduce conj nil s))

(defn list [& s] (List'new (if (-/-seq? s) (reverse (reverse s)) s)))

(defn some [f s]
    (let [s (seq s)]
        (when (some? s)
            (or (f (first s)) (#_recur some f (next s)))
        )
    )
)

(defn some-kv [f kvs]
    (let [kvs (seq kvs)]
        (when (some? kvs)
            (or (f kvs) (#_recur some-kv f (next (next kvs))))
        )
    )
)

(defn map [f s]
    (lazy-seq
        (let [s (seq s)]
            (when (some? s)
                (cons (f (first s)) (map f (next s)))
            )
        )
    )
)

(defn ConsMap'new [car, cadr, cddr]
    (cons car (cons cadr cddr))
)

(declare =)

(defn ConsMap''find [this, key]
    (some-kv (fn [e] (when (= (first e) key) e)) this)
)

(defn ConsMap''contains? [this, key]
    (some? (ConsMap''find this, key))
)

(defn ConsMap''get [this, key]
    (second (ConsMap''find this, key))
)

(defn ConsMap''assoc [this, key, val]
    (if (and (#_= identical? (first this) key) (#_= identical? (second this) val))
        this
        (ConsMap'new key, val, this)
    )
)

(defn ConsMap''keys [this]
    (lazy-seq
        (let [s (seq this)]
            (when (some? s)
                (cons (first s) (ConsMap''keys (next (next s))))
            )
        )
    )
)

(defn ConsMap''vals [this]
    (lazy-seq
        (let [s (seq this)]
            (when (some? s)
                (cons (second s) (ConsMap''vals (next (next s))))
            )
        )
    )
)

(defn assoc [m k v]
    (cond
        (nil? m)  (ConsMap'new k, v, nil)
        (cons? m) (ConsMap''assoc m, k, v)
        'else     (&throw! "assoc not supported on " m)
    )
)

(defn cons-map [& kvs] (reduce-kv assoc nil kvs))

(defn contains? [m k]
    (cond
        (nil? m)   false
        (cons? m) (ConsMap''contains? m, k)
        'else     (&throw! "contains? not supported on " m)
    )
)

(defn get [m k]
    (cond
        (nil? m)   nil
        (cons? m) (ConsMap''get m, k)
        'else     (&throw! "get not supported on " m)
    )
)

(defn update [m k f & s] (assoc m k (apply f (get m k) s)))

(defn memoize1 [f]
    (let [m (atom nil)]
        (fn [x]
            (let [e (ConsMap''find (deref m), x)]
                (if (some? e)
                    (second e)
                    (let [r (f x)]
                        (swap! m assoc x r)
                        r
                    )
                )
            )
        )
    )
)

(defn memoize [f]
    (let [m (atom nil)]
        (fn [& s]
            (let [e (ConsMap''find (deref m), s)]
                (if (some? e)
                    (second e)
                    (let [r (apply f s)]
                        (swap! m assoc s r)
                        r
                    )
                )
            )
        )
    )
)

(def String'new (memoize1 (fn [s] (&meta &'string s))))

(defn string? [x] (and (&meta? x) (&bits= (&car x) &'string)))

(defn String''seq [this] (seq (&cdr this)))

(defn String''equals [this, that]
    (or (identical? this that)
        (and (or (string? that) (-/-string? that))
            (= (seq this) (seq that))
        )
    )
)

(def Symbol'new (memoize1 (fn [s] (&meta &'symbol s))))

(defn symbol? [x] (and (&meta? x) (&bits= (&car x) &'symbol)))

(defn Symbol''seq [this] (seq (&cdr this)))

(defn Symbol''equals [this, that]
    (or (identical? this that)
        (and (or (symbol? that) (-/-symbol? that))
            (= (seq this) (seq that))
        )
    )
)

(def -symbol! (-/memoize (fn [s] (Symbol'new (reverse (reverse (-/-str s)))))))

(defn ! [s] (if (-/-symbol? s) (-symbol! s) s))

(defn bits [s] (&bits s))

(defn bits? [x] (&bits? x))

(def Unicode'newline     (bits    '1010))
(def Unicode'escape      (bits   '11011))
(def Unicode'space       (bits  '100000))
(def Unicode'exclamation (bits  '100001))
(def Unicode'quotation   (bits  '100010))
(def Unicode'hash        (bits  '100011))
(def Unicode'percent     (bits  '100101))
(def Unicode'ampersand   (bits  '100110))
(def Unicode'apostrophe  (bits  '100111))
(def Unicode'lparen      (bits  '101000))
(def Unicode'rparen      (bits  '101001))
(def Unicode'asterisk    (bits  '101010))
(def Unicode'comma       (bits  '101100))
(def Unicode'minus       (bits  '101101))
(def Unicode'slash       (bits  '101111))
(def Unicode'0           (bits  '110000))
(def Unicode'1           (bits  '110001))
(def Unicode'2           (bits  '110010))
(def Unicode'3           (bits  '110011))
(def Unicode'4           (bits  '110100))
(def Unicode'5           (bits  '110101))
(def Unicode'6           (bits  '110110))
(def Unicode'7           (bits  '110111))
(def Unicode'8           (bits  '111000))
(def Unicode'9           (bits  '111001))
(def Unicode'equals      (bits  '111101))
(def Unicode'greater     (bits  '111110))
(def Unicode'question    (bits  '111111))
(def Unicode'A           (bits '1000001))
(def Unicode'B           (bits '1000010))
(def Unicode'C           (bits '1000011))
(def Unicode'D           (bits '1000100))
(def Unicode'E           (bits '1000101))
(def Unicode'F           (bits '1000110))
(def Unicode'G           (bits '1000111))
(def Unicode'H           (bits '1001000))
(def Unicode'I           (bits '1001001))
(def Unicode'J           (bits '1001010))
(def Unicode'K           (bits '1001011))
(def Unicode'L           (bits '1001100))
(def Unicode'M           (bits '1001101))
(def Unicode'N           (bits '1001110))
(def Unicode'O           (bits '1001111))
(def Unicode'P           (bits '1010000))
(def Unicode'Q           (bits '1010001))
(def Unicode'R           (bits '1010010))
(def Unicode'S           (bits '1010011))
(def Unicode'T           (bits '1010100))
(def Unicode'U           (bits '1010101))
(def Unicode'V           (bits '1010110))
(def Unicode'W           (bits '1010111))
(def Unicode'X           (bits '1011000))
(def Unicode'Y           (bits '1011001))
(def Unicode'Z           (bits '1011010))
(def Unicode'lbracket    (bits '1011011))
(def Unicode'backslash   (bits '1011100))
(def Unicode'rbracket    (bits '1011101))
(def Unicode'underscore  (bits '1011111))
(def Unicode'grave       (bits '1100000))
(def Unicode'a           (bits '1100001))
(def Unicode'b           (bits '1100010))
(def Unicode'c           (bits '1100011))
(def Unicode'd           (bits '1100100))
(def Unicode'e           (bits '1100101))
(def Unicode'f           (bits '1100110))
(def Unicode'g           (bits '1100111))
(def Unicode'h           (bits '1101000))
(def Unicode'i           (bits '1101001))
(def Unicode'j           (bits '1101010))
(def Unicode'k           (bits '1101011))
(def Unicode'l           (bits '1101100))
(def Unicode'm           (bits '1101101))
(def Unicode'n           (bits '1101110))
(def Unicode'o           (bits '1101111))
(def Unicode'p           (bits '1110000))
(def Unicode'q           (bits '1110001))
(def Unicode'r           (bits '1110010))
(def Unicode's           (bits '1110011))
(def Unicode't           (bits '1110100))
(def Unicode'u           (bits '1110101))
(def Unicode'v           (bits '1110110))
(def Unicode'w           (bits '1110111))
(def Unicode'x           (bits '1111000))
(def Unicode'y           (bits '1111001))
(def Unicode'z           (bits '1111010))

(defn Closure'new [fun, env] (&meta &'closure (cons fun env)))

(defn closure? [x] (and (&meta? x) (&bits= (&car x) &'closure)))

(defn Closure''fun [this] (first (&cdr this)))
(defn Closure''env [this] (next  (&cdr this)))

(defn Closure''apply [this, args]
    (let [
        fun (Closure''fun this) env (Closure''env this)
        env
            (loop [env (if (first fun) (cons this env) env) pars (second fun) args (seq args)]
                (if (&zero? pars)
                    (if (third fun) (cons args env) env)
                    (recur (cons (first args) env) (&dec pars) (next args))
                )
            )
    ]
        (&eval (fourth fun) env)
    )
)

(defn Closure''seq [this]
    (Closure''apply this, nil)
)

(defn apply [f & s]
    (let [s (spread s)]
        (cond
            (closure? f) (Closure''apply f, s)
            (atom? f)    (#_recur apply (deref f) s)
            (-/-fn? f)   (-/-apply f (if (-/-seq? s) s (reduce -/-conj (-/list) (reverse s))))
            'else        (&throw! "apply not supported on " f)
        )
    )
)

(def Mandane'ns (atom nil))

(defn Var'new []
    (atom nil)
)

(defn Var'find [s]
    (or
        (get (deref Mandane'ns) s)
        (when (and (= (first s) Unicode'minus) (= (second s) Unicode'slash))
            (-/-var-find (apply -/-str (next (next s))))
        )
    )
)

(defn Var'lookup [s]
    (or
        (get (deref Mandane'ns) s)
        (let [v (Var'new)]
            (swap! Mandane'ns assoc s v)
            v
        )
    )
)

(defn Var''get [this]
    (deref this)
)

(defn Var''set [this, root]
    (reset! this root)
    nil
)

(defn sequential? [x] (or (cons? x) (list? x)))

(defn Sequential''equals [this, that]
    (or (identical? this that)
        (and (or (sequential? that) (-/-sequential? that))
            (loop [s (seq this) z (seq that)]
                (if (some? s)
                    (and (some? z) (= (first s) (first z)) (recur (next s) (next z)))
                    (nil? z)
                )
            )
        )
    )
)

(defn = [x y]
    (or (identical? x y)
        (cond
            (or (nil? x) (true? x) (false? x))       false
            (bits? x)                               (&bits= x y)
            (or (sequential? x) (-/-sequential? x)) (Sequential''equals x, y)
            (or (string? x)     (-/-string? x))     (String''equals x, y)
            (or (symbol? x)     (-/-symbol? x))     (Symbol''equals x, y)
            'else                                   (&throw! "= not supported on " x)
        )
    )
)

(def Mandane'in  (atom nil))
(def Mandane'out (atom nil))

(defn append' [a x]
    (let [f'append (if (atom? a) (fn [%1 %2] (&append %1 %2)) conj)]
        (cond
            (bits? x)                                   (f'append a x)
            (or (string? x) (-/-string? x) (symbol? x)) (reduce f'append a x)
            'else                                       (&throw! "append' not supported for " x)
        )
    )
)

(defn escape-str [c]
    (cond
        (= c Unicode'newline)   "\\n"
        (= c Unicode'quotation) "\\\""
        (= c Unicode'backslash) "\\\\"
        'else                   c
    )
)

(defn append-str [a x]
    (let [
        a (append' a "\"")
        a (reduce (fn [%1 %2] (append' %1 (escape-str %2))) a x)
        a (append' a "\"")
    ]
        a
    )
)

(defn append-sym [a x]
    (append' a x)
)

(defn append* [a b f'append c d q]
    (let [
        a (append' a b)
        a
            (let [s (seq q)]
                (if (some? s)
                    (loop [a a s s]
                        (let [a (f'append a (first s)) s (next s)]
                            (if (some? s) (recur (append' a c) s) a)
                        )
                    )
                    a
                )
            )
        a (append' a d)
    ]
        a
    )
)

(declare append)

(defn append-seq [a x] (append* a "(" append " " ")" x))

(defn append [a x]
    (cond
        (nil? x)        (append' a "nil")
        (true? x)       (append' a "true")
        (false? x)      (append' a "false")
        (bits? x)       (append' a "bits")
        (sequential? x) (append-seq a x)
        (string? x)     (append-str a x)
        (symbol? x)     (append-sym a x)
        (atom? x)       (append' a "atom")
        (closure? x)    (append' a "closure")
        (-/-seq? x)     (append' a "-seq")
        (-/-string? x)  (append' a "-string")
        (-/-symbol? x)  (append' a "-symbol")
        (-/-fn? x)      (append' a "-fn")
        'else           (&throw! "append not supported on " x)
    )
)

(defn append! [a x]
    (if (or (bits? x) (string? x) (-/-string? x)) (append' a x) (append a x))
)

(defn str [& s]
    (loop [sb nil s s]
        (if (some? s)
            (let [x (first s)]
                (recur (append! sb x) (next s))
            )
            (String'new (reverse sb))
        )
    )
)

(defn space   [] (append' Mandane'out Unicode'space)   nil)
(defn newline [] (append' Mandane'out Unicode'newline) nil)

(defn flush [] (&flush Mandane'out) nil)

(defn pr [& s]
    (when (some? s)
        (loop [x (first s) s (next s)]
            (append Mandane'out x)
            (when (some? s)
                (space)
                (recur (first s) (next s))
            )
        )
    )
    nil
)

(defn print [& s]
    (when (some? s)
        (loop [x (first s) s (next s)]
            (append! Mandane'out x)
            (when (some? s)
                (space)
                (recur (first s) (next s))
            )
        )
    )
    nil
)

(defn prn     [& s] (apply pr    s) (newline) (flush) nil)
(defn println [& s] (apply print s) (newline) (flush) nil)

(defn QuoteExpr'create [form]
    (if (or (nil? form) (true? form) (false? form))
        form
        (&meta &'quote (cons form nil))
    )
)

(defn QuoteExpr'parse [form, scope]
    (cond
        (nil? (next form))         (&throw! "too few arguments to quote")
        (some? (next (next form))) (&throw! "too many arguments to quote")
    )
    (QuoteExpr'create (second form))
)

(declare Compiler'analyze)

(defn IfExpr'parse [form, scope]
    (cond
        (nil? (next (next (next form))))         (&throw! "too few arguments to if")
        (some? (next (next (next (next form))))) (&throw! "too many arguments to if")
    )
    (let [
        test (Compiler'analyze (second form), scope)
        then (Compiler'analyze (third form), scope)
        else (Compiler'analyze (fourth form), scope)
    ]
        (&meta &'let (cons test (&meta &'if (cons then (cons else nil)))))
    )
)

(defn ApplyExpr'parse [form, scope]
    (let [
        f (fn f [m e & s] (if s (&meta &'let (cons (Compiler'analyze (first s), m) (apply f (cons (! '&_) m) e (next s)))) (&meta e nil)))
        m scope e (first form) s (next form)
    ]
        (cond
            (= e '&do)
                (when s
                    ((fn f [m s]
                        (if s
                            (&meta &'let (cons (Compiler'analyze (first s), m) (f (cons (! '&_) m) (next s))))
                            (&meta &'binding (cons (&zero) nil))
                        )
                    ) m s)
                )

            (= e '&bits)       (f m &'bits (first s))
            (= e '&bits?)      (f m &'bits? (first s))
            (= e '&bits=)      (f m &'bits= (first s) (second s))

            (= e '&zero)       (f m &'zero)
            (= e '&zero?)      (f m &'zero? (first s))

            (= e '&inc)        (f m &'inc (first s))
            (= e '&dec)        (f m &'dec (first s))

            (= e '&identical?) (f m &'identical? (first s) (second s))

            (= e '&read)       (f m &'read (first s))
            (= e '&unread)     (f m &'unread (first s) (second s))
            (= e '&append)     (f m &'append (first s) (second s))
            (= e '&flush)      (f m &'flush (first s))

            (= e '&car)        (f m &'car (first s))
            (= e '&cdr)        (f m &'cdr (first s))

            (= e '&cons?)      (f m &'cons? (first s))
            (= e '&meta?)      (f m &'meta? (first s))

            (= e '&cons)       (f m &'cons (first s) (second s))
            (= e '&meta)       (f m &'meta (first s) (second s))
            (= e '&meta!)      (f m &'meta! (first s) (second s))

            (= e '&volatile-cas-cdr!) (f m &'volatile-cas-cdr! (first s) (second s) (third s))
            (= e '&volatile-get-cdr)  (f m &'volatile-get-cdr (first s))
            (= e '&volatile-set-cdr!) (f m &'volatile-set-cdr! (first s) (second s))

            (= e '&throw!)     (f m &'throw!)

            'else
                ((fn f [m n s]
                    (if s
                        (&meta &'let (cons (Compiler'analyze (first s), m) (f (cons (! '&_) m) (&inc n) (next s))))
                        (&meta &'apply (cons n nil))
                    )
                ) m (&zero) (cons e s))
        )
    )
)

(defn FnExpr'parse [form, scope]
    (let [
        self (second form) ? (symbol? self) self (when ? self) form (if ? (next (next form)) (next form))
        _
            (loop [pars nil etal nil variadic? false s (seq (first form))]
                (if (some? s)
                    (let [x (first s)]
                        (if (symbol? x)
                            (cond
                                (= x (! '&))
                                    (if (not variadic?)
                                        (recur pars etal true (next s))
                                        (&throw! "overkill variadic parameter list")
                                    )
                                (some? etal)
                                    (&throw! "excess variadic parameter " x)
                                'else
                                    (if (not variadic?)
                                        (recur (cons x pars) etal variadic? (next s))
                                        (recur         pars  x    variadic? (next s))
                                    )
                            )
                            (&throw! "function parameters must be symbols")
                        )
                    )
                    (if (or (not variadic?) (some? etal))
                        (list (reverse pars) etal)
                        (&throw! "missing variadic parameter")
                    )
                )
            )
        pars (first _) etal (second _)
        scope
            (loop [scope (if (some? self) (cons self scope) scope) s (seq pars)]
                (if (some? s)
                    (recur (cons (first s) scope) (next s))
                    (if (some? etal) (cons etal scope) scope)
                )
            )
        body (ApplyExpr'parse (cons (! '&do) (next form)), scope)
    ]
        (&meta &'fn (cons (some? self) (cons (loop [n (&zero) s (seq pars)] (if (some? s) (recur (&inc n) (next s)) n)) (cons (some? etal) (cons body nil)))))
    )
)

(defn Scope'index-of [scope, name]
    (loop [i (&zero) s (seq scope)]
        (when (some? s)
            (if (= (first s) name)
                i
                (recur (&inc i) (next s))
            )
        )
    )
)

(defn DefExpr'parse [form, scope]
    (cond
        (nil? (next (next form)))         (&throw! "too few arguments to def")
        (some? (next (next (next form)))) (&throw! "too many arguments to def")
    )
    (let [s (second form)]
        (if (symbol? s)
            (let [
                var (Var'lookup s)
                any (Compiler'analyze (third form), scope)
            ]
                (&meta &'let (cons any (&meta &'var-set! (cons var nil))))
            )
            (&throw! "first argument to def must be a symbol")
        )
    )
)

(defn Compiler'macro [m]
    (cond
        (= m 'declare)  (fn [x]     (list (! 'def) x nil))
        (= m 'when)     (fn [? & s] (list (! 'if) ? (cons (! '&do) s) nil))
        (= m 'cond)     (fn [& s]   (when s (list (! 'if) (first s) (second s) (cons (! 'cond) (next (next s))))))
        (= m 'and)      (fn [& s]   (if s (let [x (first s) s (next s)] (if s (list (! 'let) (list (! '&and) x) (list (! 'if) (! '&and) (cons (! 'and) s) (! '&and))) x)) true))
        (= m 'or)       (fn [& s]   (when s (let [x (first s) s (next s)] (if s (list (! 'let) (list (! '&or) x) (list (! 'if) (! '&or) (! '&or) (cons (! 'or) s))) x))))
        (= m 'let)      (fn [a & s] (if (seq a) (list (list (! 'fn) (list (first a)) (cons (! 'let) (cons (next (next a)) s))) (second a)) (cons (! '&do) s)))
        (= m 'loop)     (fn [a & s] (cons (cons (! 'fn) (cons (! 'recur) (cons (ConsMap''keys a) s))) (ConsMap''vals a)))
        (= m 'defn)     (fn [f & s] (list (! 'def) f (cons (! 'fn) s)))
        (= m 'lazy-seq) (fn [& s]   (cons (! 'fn) (cons #_[] nil s)))
    )
)

(defn Compiler'special [s]
    (cond
        (= s 'def)   DefExpr'parse
        (= s 'fn)    FnExpr'parse
        (= s 'if)    IfExpr'parse
        (= s 'quote) QuoteExpr'parse
    )
)

(defn Compiler'analyze [form, scope]
    (cond
        (or (and (list? form) (seq form)) (cons? form) (and (-/-seq? form) (-/-seq form)))
            (let [
                f'macro (Compiler'macro (first form))
                me (if (some? f'macro) (apply f'macro (next form)) form)
            ]
                (if (#_= identical? me form)
                    (let [
                        f'parse (or (Compiler'special (first form)) ApplyExpr'parse)
                    ]
                        (f'parse form scope)
                    )
                    (#_recur Compiler'analyze me, scope)
                )
            )
        (symbol? form)
            (or
                (let [i (Scope'index-of scope, form)]
                    (when (some? i)
                        (&meta &'binding (cons i nil))
                    )
                )
                (let [v (Var'find form)]
                    (when (some? v)
                        (&meta &'var-get (cons v nil))
                    )
                )
                (&throw! "unable to resolve symbol " form)
            )
        'else
            (QuoteExpr'create form)
    )
)

(defn eval [form] (&eval (Compiler'analyze form, nil) nil))

(declare Reader'macro)

(defn Reader'isMacro [c]
    (some? (Reader'macro c))
)

(defn Reader'isTerminatingMacro [c]
    (and (Reader'isMacro c) (not (= c Unicode'hash)) (not (= c Unicode'apostrophe)))
)

(defn Reader'isLetter [c]
    (or
        (= c Unicode'a) (= c Unicode'A)
        (= c Unicode'b) (= c Unicode'B)
        (= c Unicode'c) (= c Unicode'C)
        (= c Unicode'd) (= c Unicode'D)
        (= c Unicode'e) (= c Unicode'E)
        (= c Unicode'f) (= c Unicode'F)
        (= c Unicode'g) (= c Unicode'G)
        (= c Unicode'h) (= c Unicode'H)
        (= c Unicode'i) (= c Unicode'I)
        (= c Unicode'j) (= c Unicode'J)
        (= c Unicode'k) (= c Unicode'K)
        (= c Unicode'l) (= c Unicode'L)
        (= c Unicode'm) (= c Unicode'M)
        (= c Unicode'n) (= c Unicode'N)
        (= c Unicode'o) (= c Unicode'O)
        (= c Unicode'p) (= c Unicode'P)
        (= c Unicode'q) (= c Unicode'Q)
        (= c Unicode'r) (= c Unicode'R)
        (= c Unicode's) (= c Unicode'S)
        (= c Unicode't) (= c Unicode'T)
        (= c Unicode'u) (= c Unicode'U)
        (= c Unicode'v) (= c Unicode'V)
        (= c Unicode'w) (= c Unicode'W)
        (= c Unicode'x) (= c Unicode'X)
        (= c Unicode'y) (= c Unicode'Y)
        (= c Unicode'z) (= c Unicode'Z)
    )
)

(defn Reader'isDigit [c]
    (or (= c Unicode'0) (= c Unicode'1) (= c Unicode'2) (= c Unicode'3) (= c Unicode'4) (= c Unicode'5) (= c Unicode'6) (= c Unicode'7) (= c Unicode'8) (= c Unicode'9))
)

(def Reader'naught (bits '11111))

(defn Reader'isWhitespace [c]
    (or (= c Unicode'space) (= c Unicode'comma) (= c Unicode'newline) (= c Reader'naught))
)

(defn Reader'read1 [r]
    (&read r)
)

(defn Reader'unread [r, c]
    (when (some? c)
        (&unread r c)
    )
    nil
)

(defn Reader'readToken [r, c]
    (loop [z (cons c nil)]
        (let [c (Reader'read1 r)]
            (if (or (nil? c) (Reader'isWhitespace c) (Reader'isTerminatingMacro c))
                (&do
                    (Reader'unread r, c)
                    (String'new (reverse z))
                )
                (recur (cons c z))
            )
        )
    )
)

#_"\n !\"#%&'()*,-./0123456789=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]_abcdefghijklmnopqrstuvwxyz"

(defn Reader'isSymbol [s]
    (let [s (if (and (= (first s) Unicode'minus) (= (second s) Unicode'slash)) (next (next s)) s)]
        (and
            (let [c (first s)]
                (or (= c Unicode'minus) (Reader'isLetter c) (= c Unicode'underscore) (Reader'isDigit c) (= c Unicode'question) (= c Unicode'exclamation) (= c Unicode'equals) (= c Unicode'ampersand) (= c Unicode'percent))
            )
            (loop [s (next s)]
                (or (nil? s)
                    (and
                        (let [c (first s)]
                            (or (= c Unicode'minus) (Reader'isLetter c) (= c Unicode'underscore) (Reader'isDigit c) (= c Unicode'apostrophe) (= c Unicode'asterisk) (= c Unicode'question) (= c Unicode'exclamation) (= c Unicode'equals) (= c Unicode'greater))
                        )
                        (recur (next s))
                    )
                )
            )
        )
    )
)

(defn Reader'read [r, delim, delim!]
    (loop []
        (let [c (loop [c (Reader'read1 r)] (if (and (some? c) (Reader'isWhitespace c)) (recur (Reader'read1 r)) c))]
            (cond
                (nil? c)
                    (&throw! "EOF while reading")
                (and (some? delim) (= delim c))
                    delim!
                'else
                    (let [f'macro (Reader'macro c)]
                        (if (some? f'macro)
                            (let [o (f'macro r c)]
                                (if (identical? o r) (recur) o)
                            )
                            (let [s (Reader'readToken r, c)]
                                (cond
                                    (= s "nil")          nil
                                    (= s "true")         true
                                    (= s "false")        false
                                    (Reader'isSymbol s) (Symbol'new s)
                                    'else               (&throw! "invalid token " s)
                                )
                            )
                        )
                    )
            )
        )
    )
)

(def Reader'READ_FINISHED (atom nil))

(defn Reader'readDelimitedForms [r, delim]
    (loop [z nil]
        (let [form (Reader'read r, delim, Reader'READ_FINISHED)]
            (if (identical? form Reader'READ_FINISHED)
                (reverse z)
                (recur (cons form z))
            )
        )
    )
)

(defn StringReader'escape [r]
    (let [c (Reader'read1 r)]
        (if (some? c)
            (cond
                (= c Unicode'n)         Unicode'newline
                (= c Unicode'backslash) c
                (= c Unicode'quotation) c
                'else                   (&throw! "unsupported escape character \\" c)
            )
            (&throw! "EOF while reading string")
        )
    )
)

(defn string-reader [r, _delim]
    (loop [z nil]
        (let [c (Reader'read1 r)]
            (if (some? c)
                (if (= c Unicode'quotation)
                    (String'new (reverse z))
                    (recur (cons (if (= c Unicode'backslash) (StringReader'escape r) c) z))
                )
                (&throw! "EOF while reading string")
            )
        )
    )
)

(defn discard-reader [r, _delim]
    (Reader'read r, nil, nil)
    r
)

(defn quote-reader [r, _delim]
    (list (! 'quote) (Reader'read r, nil, nil))
)

(declare Reader'dispatchMacro)

(defn dispatch-reader [r, _delim]
    (let [c (Reader'read1 r)]
        (if (some? c)
            (let [f'macro (Reader'dispatchMacro c)]
                (if (some? f'macro)
                    (f'macro r c)
                    (&do
                        (Reader'unread r, c)
                        (&throw! "no dispatch macro for " c)
                    )
                )
            )
            (&throw! "EOF while reading character")
        )
    )
)

(defn list-reader [r, _delim]
    (List'new (Reader'readDelimitedForms r, Unicode'rparen))
)

(defn vector-reader [r, _delim]
    (List'new (Reader'readDelimitedForms r, Unicode'rbracket))
)

(defn unmatched-delimiter-reader [_r, delim]
    (&throw! "unmatched delimiter " delim)
)

(defn Reader'macro [c]
    (cond
        (= c Unicode'quotation)  string-reader
        (= c Unicode'apostrophe) quote-reader       (= c Unicode'grave)    quote-reader
        (= c Unicode'lparen)     list-reader        (= c Unicode'rparen)   unmatched-delimiter-reader
        (= c Unicode'lbracket)   vector-reader      (= c Unicode'rbracket) unmatched-delimiter-reader
        (= c Unicode'hash)       dispatch-reader
    )
)

(defn Reader'dispatchMacro [c]
    (cond
        (= c Unicode'underscore) discard-reader
    )
)

(defn read [] (Reader'read Mandane'in, nil, nil))

(defn repl []
    (let [esc Unicode'escape] (print (str esc "[31mMandane " esc "[32m=> " esc "[0m")))
    (flush)
    (prn (eval (read)))
    (#_recur repl)
)

#_(repl)
