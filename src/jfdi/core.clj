(ns jfdi.core
  (:use clojure.pprint
        clojure.repl))

;; Used so error-throwing top-level forms don't block compilation. 
(defmacro box-error [& body]
  `(try
     ~@body
     (catch Throwable t# {:ERROR t#})))

;; This is a codewalk that assumes no prior knowledge of Clojure.  When printed
;; forms are hard to read, please use pprint to make them easier to read.

;; 1. The basics

;; prefix notation

(+ 3 4)
;; => 7

;; variarity

(+ 5 6 7)
;; => 18

(+)
;; => 0

;; nested forms OK (in fact, common)
(+ 137 (* 32 3 25 13))
;; => 31337

;; define a "variable" (in fact, it doesn't vary!)
(def six 6)

(+ six six)
;; => 12

;; 2. Using functions

((fn [x] (+ 1 x)) 7)
;; => 8

(#(+ 1 %) 7)
;; => 8

(def square (fn [x] (* x x)))

(square six)
;; => 36

(defn square [x] (* x x)) ;; more common / usable form.  

(square six)
;; => 36 (no change).

;; functions can be defined recursively.
(defn factorial [n]
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(range 10)
;; => (0 1 2 3 4 5 6 7 8 9)

(map factorial (range 10))
;; => (1 1 2 6 24 120 720 5040 40320 362880)

;; This factorial has two flaws. 
;; 1st issue: Integer overflow. 

(box-error
 (factorial 30)
)
;; =!! java.lang.ArithmeticException: integer overflow

(defn factorial-2 [n]
  (if (= n 0)
    1
    (*' n (factorial-2 (- n 1)))))

(factorial-2 30)
;; => 265252859812191058636308480000000N

;; 2nd issue: Stack overflow due to lack of tail recursion. 

(box-error
 (factorial-2 10000)
)
;; => {:ERROR #<java.lang.StackOverflowError ...>}

;; That's because Java does NOT natively support tail-call
;; optimization. Recursive functions are legal but won't be optimized.  If you
;; want TCO, you have to use loop and recur.

(defn factorial-3 [n]
  (loop [acc 1N, x n]
    (if (> x 0)
      (recur (*' acc x) (dec x))
      acc)))

(factorial-3 10000)
;; => <approximately 2.8462 * 10^35659>

;; or there is a more direct route, using reduce.

(defn factorial-4 [n]
  (reduce *' (range 1 (+ n 1))))

(factorial-4 5)
;; => 120

;; 3. Clojure's Data Types

;; You have all eight primitive types. 

(map class [false (byte 1) (short 2) (int 3) 4 (float 5) 6.0 \7])
;; (java.lang.Boolean java.lang.Byte java.lang.Short java.lang.Integer 
;;  java.lang.Long java.lang.Float java.lang.Double java.lang.Character)

;; Note that Clojure's natural tendency is to convert integers => Long
;; and decimals => Double.

;; Clojure also has rational numbersP:

(/ 5 7)
;; => 5/7

(class 5/7)
;; => clojure.lang.Ratio

;; ... plus arbitrary-precision integers.

(class 1N)
;; => clojure.lang.BigInt

;; You have a symbol type which becomes important when you're using Clojure's
;; code-as-data capabilities (e.g. macros).

(class '+)
;; => clojure.lang.Symbol

(list '+ 4 5)
;; (+ 4 5)

;; The list above looks like code-- because it is. You can evaluate it using
;; Clojure's eval.

(eval (list '+ 4 5))
;; => 9

;; eval has a bad reputation; it can produce *any* side effect. Don't use it in
;; production or high-security settings if you can help it. But it's important
;; to the theory of Lisp.

;; There's a keyword type. They're "string-like" but interned so comparison is
;; fast. They're used in many places, but one is as keys to a map, a data
;; structure covered later.

(class :red)
;; clojure.lang.Keyword

;; We've already seen one of Clojure's persistent data structures: the list. 

(cons 1 nil)
;; => (1)
(class (cons 1 nil))
;; => clojure.lang.PersistentList
(class '(1))  ; the same as the above.
;; => clojure.lang.PersistentList

;; The quoting policy might seem inconsistent. In inputs, lists get quoted. In
;; output, they aren't.

'(1 2 3)
;; => (1 2 3)

;; That's because the evaluator handles lists specially. Without the quote, it
;; will try to evaluate the list AS CODE, like so:

(box-error
 (1 2 3)
)
;; =!! ClassCastException java.lang.Long cannot be cast to clojure.lang.IFn

;; That's because the 1 is in function (first) position. The evaluator tries to
;; find an appropriate function, as it would for a symbol, but there is none. So
;; it's an error.

;; Because they are evaluated when read, and also because they're linked lists
;; (lacking random access) a more common data structure is the Vector.

[1 2 3]
;; => [1 2 3]
(vec (list 4 5 6))
;; => [4 5 6]

;; To pull apart lists and vectors, you have a variety of functions to do that.
(first [1 2 3 4])
;; => 1
(first '(1 2 3 4))
;; => 1
(rest [1 2 3 4])
;; => (2 3 4)
(rest '(1 2 3 4))
;; => (2 3 4)

;; The returns from rest look like lists, but they're actually something
;; different: seqs. 

(class (rest [1 2 3 4]))
;; => clojure.lang.PersistentVector$ChunkedSeq
(seq? (rest [1 2 3 4]))
;; => true

;; What are seqs about? The seq we get from calling rest on a Vector can be
;; thought-of as a seq "view". Higher-order functions like map and filter also
;; return seqs, regardless of the original type.

(map #(+ % 5) [1 2 3 4])
;; => (6 7 8 9)
(class (map #(+ % 5) [1 2 3 4]))
;; => clojure.lang.LazySeq
(class (filter even? [1 2 3 4]))
;; => clojure.lang.LazySeq

;; The seq is not a specific data type. It's an abstraction over a sequential,
;; linear collection. As an interface, seq is agnostic about whether it's lazy
;; or eager. map and filter return seqs for performance reasons. If the list
;; operators return lazy collections, then they can be stacked *without* the
;; (expensive) intermediate copying. That is, (map f (filter g (map h
;; big-list))) doesn't have to create an intermediate list for (map h big-list)
;; or (filter g (map h big-list)).

;; Back to vectors, we can "get" from them using get, nth, or the vector itself
;; as a function.

(nth [4 8 16 32 64] 3)
;; => 32
(get [4 8 16 32 64] 3)
;; => 32
([4 8 16 32 64] 3)
;; => 32

;; You can also immutably "update" vectors using assoc or conj. A new version is
;; returned; the original is unchanged.

(def v [4 8 16 32 64])

(conj v 128)
;; => [4 8 16 32 64 128]

v
;; => [4 8 16 32 64]

(assoc v 3 :orange)
;; => [4 8 16 :orange 64]

;; Maps (like Python dicts) are one of the most interesting data types; they
;; provide fast lookup by key.

(into {} [[2 "two"] [3 "three"] [5 "five"] [7 "seven"] [11 "eleven"]
          [13 "thirteen"]])
;; => {2 "two", 3 "three", 5 "five",
;;     7 "seven", 11 "eleven", 13 "thirteen"}

;; Commas are whitespace in Clojure. It's typical but not required to use them
;; for grouping (key, value) pairs in maps. Keys can be of any type (or mixed
;; types) but keywords are used often.

(def my-map {:name "Mike" :age 30 :zip "21231" :language :clojure})

(keys my-map)
;; => (:age :name :language :zip)
(vals my-map)
;; => (30 "Mike" :clojure "21231")

;; You can use assoc and get on maps, as with vectors. 
(assoc my-map :name "Michael")
;; => {:age 30, :name "Michael", :language :clojure, :zip "21231"}
(assoc my-map :new-key "31337")
;; => {:age 30, :name "Michael", :language :clojure, 
;;     :new-key "31337", :zip "21231"}

;; None of this work changes the original map, which is unchanged. 
my-map
;; => {:age 30, :name "Mike", :language :clojure, :zip "21231"}

(get my-map :language)
;; => :clojure

;; ... and you can use maps as functions.
(my-map :language)
;; => :clojure
(my-map :nonexistent-key)
;; => nil
(my-map :nonexistent-key :default)
;; => :default

;; You can remove keys using dissoc, like so:

(dissoc my-map :zip)
;; => {:age 30, :name "Michael", :language :clojure}

;; Finally, the last major data structure is the set:

(class #{2 3 5 7 11})
;; => clojure.lang.PersistentHashSet

(conj #{2 3 5 7 11} 13)
;; => #{2 3 5 7 11 13}

(disj #{2 3 5 7 11} 5)
;; => #{2 3 7 11}

;; Typically, maps and sets are unordered, using hash-based implementations that
;; are most performant. However, sorted versions are available.

(def my-unsorted-map {425 :violet, 470 :blue, 525 :green, 
                      580 :yellow, 610 :orange, 665 :red})
my-unsorted-map
;; => {610 :orange, 580 :yellow, 425 :violet, 
;;     525 :green,  470 :blue,   665 :red}

(def my-sorted-map (into (sorted-map) my-unsorted-map))

my-sorted-map
;; {425 :violet, 470 :blue, 525 :green, 580 :yellow, 610 :orange, 665 :red}

;; With sorted collections, functions like first, take, and subseq have meaning.
(first my-sorted-map)
;; => [425 :violet]
(take 2 my-sorted-map)
;; => ([425 :violet] [470 :blue])
(subseq my-sorted-map > 550)
;; => ([580 :yellow] [610 :orange] [665 :red])
(rsubseq my-sorted-map < 500)
;; => ([470 :blue] [425 :violet])

;; 4. Atoms-- Clojure's simplest mutable type.

;; This demo won't get into all of the options Clojure has for mutability, but
;; Atoms are one of the simplest. They're called atomic because they allow the
;; fundamental prerequisite for proper concurrency: atomic operations-- that is,
;; operations that can't be interfered with in-progress (producing inconsistent
;; states).

;; Create a mutable, atomic "place". 
(def a (atom 0))

;; You can read at atom using deref, or the equivalent short-hand @.

(deref a)
;; => 0
@a
;; => 0

;; You can write an atom using reset!
(reset! a 37)
;; => 37
@a
;; => 37

;; You can also use swap!, which atomically applies a function to the atom.
(swap! a #(* % 2))
;; => 74

;; The atomic swap! is very powerful because it allows proper
;; concurrency. Here's an example of what NOT to do with atoms.

(let [x @a] (reset! a (+ 1 x)))
;; => 75

;; Seems fine, in a single-threaded world. But let's see what happens when 10k
;; conceptual threads (which may not map to OS threads) do it.

(reset! a 0)
(dotimes [i 10000] (future (let [x @a] (reset! a (+ 1 x)))))
@a
;; => 8306
;;    (You won't usually get the result I did, but it'll generally be less
;;     than the expected 10000.)

;; What went wrong? If two threads read before either writes, then one will have
;; an out-of-date value for @a. That means that some of the updates (in this
;; case, 17% of them) get erased.

;; This is what proper concurrency looks like:

(reset! a 0)
(dotimes [i 10000] (future (swap! a #(+ 1 %))))
@a
;; => 10000

;; That's the right answer. The good news is that, while it's certainly possible
;; to do bad concurrency in Clojure, the language strives to make good
;; concurrency easier and prettier than the bad kind. 

;; 5. Data-pipelining with map, filter, reduce, et al. 

;; Clojure has a lot of great data-processing tools that operate on collections
;; and also on functions (higher-order functions).

(map #(+ % 5) [1 2 3 4])
;; => (6 7 8 9)
(filter even? [1 2 3 4])
;; => (2 4)
(reduce + [1 2 3 4])
;; => 10
(reduce + 10.0 [1 2 3 4])
;; => 20.0

;; Let's use this in-memory database:
(def db [{:name "Ayla"             :gender :female :level 74 :element :fire 
          :badassery [:over 9000]}
         {:name "Crono"            :gender :male   :level 53 :element :lightning 
          :zombie true}
         {:name "Frog"             :gender :male   :level 60 :element :water}
         {:name "Lucca"            :gender :female :level 61 :element :fire :glasses true}
         {:name "Marle"            :gender :female :level 57 :element :water}
         {:name "Magus"            :gender :male   :level 70 :element :shadow
          :hot-sister-issues true}
         {:name "Robo"             :gender :robot  :level 54 :element :shadow 
          :theme-song "rickroll"}])

;; Since keywords are functions on maps, we have an elegant representation for
;; the analogue of SQL "SELECT field FROM ..."

;; SELECT name FROM db;
(map :name db)
;; => ("Ayla", "Crono", "Frog", "Lucca", "Marle", "Magus", "Robo")

;; SELECT name FROM db WHERE gender = female;
(map :name (filter #(= (:gender %) :female) db))
;; => ("Ayla", "Lucca", "Marle")

;; Use the juxt function to select multiple fields.
;; SELECT name, level, element FROM db;
(map (juxt :name :level :element)
     (filter #(= (:gender %) :female) db))
;; => (["Ayla" 74 :fire] ["Lucca" 61 :fire] ["Marle" 57 :water])

;; SELECT name, level FROM db ORDER BY level;
(map (juxt :name :level) (sort-by :level db))
;; => (["Crono" 53] ["Robo" 54] ["Marle" 57] ["Frog" 60] 
;;     ["Lucca" 61] ["Magus" 70] ["Ayla" 74])

;; You can do also use group-by, reduce, and frequencies to do aggregations.

(group-by :gender db)
;; =>  {:female [{:gender :female, :name "Ayla", :level 74, ...} 
;;               {:gender :female, :name "Lucca", ...} 
;;               {:gender :female, :name "Marle", ...}], 
;;      :male   [{:gender :male,   :name "Crono", ...} 
;;               {:gender :male,   :name "Frog",  ...} 
;;               {:gender :male,   :name "Magus", ...}], 
;;      :robot  [{:gender :robot,  :name "Robo",  ...}]}

(frequencies (map :element db))
;; {:fire 2, :lightning 1, :water 2, :shadow 2}

;; Aggregation: summing all the levels.
(reduce + (map :level db))
;; 429

(defn average [coll]
  (/ (reduce + 0.0 coll) (count coll)))

(into {}
      (map (fn [[k v]] [k (average (map :level v))])
           (group-by :gender db)))

;; {:female 64.0 :male 61.0 :robot 54.0}

(into {}
      (map (fn [[k v]] [k (average (map :level v))])
           (group-by :element db)))
;; {:fire 67.5, :lightning 53.0, :water 58.5, :shadow 62.0}

(into {}
      (map (fn [[k v]] [k (average (map :level v))])
           (group-by #(.contains (:name %) "a") db)))
;; {true 65.5, false 55.666666666666664}

;; 6. Prime numbers! (Infinite data structures.)

;; Since the seqs returned by map, filter, et al are lazy, they might be
;; infinite. That's generally no issue, so long as only a finite number of
;; elements are demanded.

;; The 0-arity range function generates a lazy seq of all natural numbers. 

(def r (range))

;; Due to lazy evaluation, it's legal to have this data structure that
;; theoretically "contains" an infinite amount of stuff. Don't print it at the
;; REPL! You'll kill it, because printing an infinite data structure (which r
;; is) takes a while. But this is fine:

(take 10 r)
;; => (0 1 2 3 4 5 6 7 8 9)

(take 10 (map square r))
;; => (0 1 4 9 16 25 36 49 64 81)

(take 10 (filter #(= (mod % 10) 1) (map square r)))
;; => (1 81 121 361 441 841 961 1521 1681 2401)

(first (filter #(> % 1e9) (map square r)))
;; => 1000014129

;; Languages like Haskell (with saner recursion models) have prettier ways of
;; expressing the prime numbers using a lazy Sieve of Eratosthenes. I won't do
;; that here (at least not now). But it IS relatively straightforward to get the
;; entire set of primes into a data structure.

(defn prime? [n]
  (and (>= n 2)
       (every? #(not= (mod n %) 0) (range 2 (Math/sqrt (inc n))))))

(def primes (filter prime? (range)))

(take 10 primes)
;; => (2 3 5 7 11 13 17 19 23 29)

(nth primes 3378)
;; => 31337

(def twin-primes
  (filter (fn [[x y]] (= (- y x) 2))
          (map vector primes (rest primes))))

(take 8 twin-primes)
;; => ([3 5] [5 7] [11 13] [17 19] [29 31] [41 43] [59 61] [71 73])

(nth twin-primes 100)
;; => [3851 3853]

;; 7. Some uses of macros. 

;; We have the full power of Clojure in macros; reverse is just a normal Clojure
;; function, but we can use it in generating code.
(defmacro silly [& stuff] (reverse stuff))

(silly 8 9 +)
;; 17

(silly :a :b list)
;; => (:b :a)

;; Let's build a usable but utterly unnecessary macro. In practice, you'd never
;; do this. You'd either use a function or, if performance demands it, use
;; definline. General principle: never use a macro when a function will do.

(defmacro add-mul [x y z]
  (list '+ x (list '* y z)))

(add-mul 1 2 3)
;; => 7

;; To see audit generated code, you'll often use macro expansion functions like
;; macroexpand on the code-- as a list. 

(macroexpand '(add-mul 1 2 3))
;; => (+ 1 (* 2 3))

;; The add-mul macro above works, but it's not in a very readable form. For
;; complex code transformations, building up the nested lists of lists (of
;; lists) becomes unreadable. In practice, macro writers make heavy use of
;; syntax quoting.

;; When you quote a list, the quote is recursively applied to all symbols in the
;; list.

(= (list '+ 5 6) '(+ 5 6))
;; => true

;; What is quoting? It's something that stops evaluation for one conceptual
;; level (application of eval). When you give the REPL the form:
;; 
;;    '(+ 5 6)
;;
;; you are asking it not to evaluate that list (i.e. add 5 and 6) yet. 

;; You can, if you want, apply multiple levels of quoting, which means multiple
;; applications of the evaluator will be needed to run that code.

(def z ''(+ 5 (* 6 7)))
z
;; => (quote (+ 5 (* 6 7)))
(eval z)
;; => (+ 5 (* 6 7))
(eval (eval z))
;; => 47

;; Because lists have a "special relationship" with the evaluator, your go-to
;; collection type should generally be vectors. Lists are what you build code
;; in-- unless your problem specifically demands a linked list. This is unlike
;; Common Lisp where lists are also the go-to collection type.

;; Quotes pause evaluation, but sometimes one wants to *start*
;; evaluation again. That's what syntax-quoting is for. A
;; syntax-quoted form starts with a back-tick (`) and uses ~ and ~@ to
;; "resume" evaluation. One way to think of it is that the
;; syntax-quoted form lets you specify a static template, and ~ and ~@
;; allow the insertion of dynamic content. The ~ (unquote) lets you
;; insert one form, while ~@ (unquote-splicing) lets you insert a
;; sequence of forms.

(def x 5)

(list '+ x (+ x 1))
;; => (+ 3 5 6)

`(+ 3 ~x ~(+ x 1))
;; => (clojure.core/+ 3 5 6)

;; Syntax-quoting always namespace qualifies symbqols. Don't worry about that
;; for now. This is functionally equivalent to (+ 3 5 6)-- the unqualified '+ is
;; resolved to 'clojure.core/+ -- because that's the namespace in which the
;; function lives.

(eval `(+ 3 ~x ~(+ x 1)))
;; => 14

;; With syntax-quoting, we can build much more readable macros. Compare:

(defmacro ignore-errors-1 [& code]
  (let [t (gensym "t")]
    (concat (list 'try)
            code
            (list 
             (list 'catch 'Throwable t
                   (list 'println "Warning: Ignored error" 
                         (list '.toString t)))))))

;; The gensym function generates a new symbol that will typically never have
;; been seen before. It's used to avoid generating code that might interfere
;; with a free variable elsewhere in the code.

(gensym)
;; => G__10184        ;; you won't get the same symbol. 

;; That macro works...

(ignore-errors-1 (+ 4 5))
;; => 9
(ignore-errors-1 (/ 1 0))
;; <stdout> Warning: Ignored error java.lang.ArithmeticException: Divide by zero
;; => nil

;; ... but is really hard to read, much less maintain. Syntax quoting makes this
;; a lot better.

(defmacro ignore-errors-2 [& code]
  `(try
     ~@code
     (catch Throwable t#
       (println "Warning: Ignored error" (.toString t#)))))

;; The hash-marked t# will be resolved to a gensym (again, to avoid risk of
;; interfering with some free variable coincidentally named t).

(ignore-errors-1 (+ 4 5))
;; => 9
(ignore-errors-1 (/ 1 0))
;; <stdout> Warning: Ignored error java.lang.ArithmeticException: Divide by zero
;; => nil

;; 8. Final macro example: debugging using macros and with-redefs. 

;; Let's create a tracing macro that converts a form into a "vocal" form of
;; itself.

(defmacro tracing [form]
  `(do
     (println "The form was: " '~form)
     (let [ret# ~form]
       (println "Returning... " ret#)
       ret#)))

(tracing (+ 5 6))
;; <stdout> The form was:  (+ 5 6)
;; <stdout> Returning...  11
;; 11

;; This is more interesting in the context of tracing intermediate forms.
(vector (tracing (+ 3 4)) (tracing (* 3 4)))
;; <stdout> The form was:  (+ 3 4)
;; <stdout> Returning...  7
;; <stdout> The form was:  (* 3 4)
;; <stdout> Returning...  12
[7 12]

;; You can also use this to debug "foreign" code, like this. Here are two
;; functions of "legacy" code. 

(defn buggy-mult [x y]
  (+ 1 (* x y)))

(defn buggy-factorial [n]
  (reduce buggy-mult (range 1 (inc n))))

;; There's a bug, as you can see.

(buggy-factorial 4)
;; => 41

;; In fact, buggy-factorial is fine. The problem is deeper-- it's in
;; buggy-mult. If that were fixed, buggy-factorial would return the right
;; answer.

(with-redefs [buggy-mult *] (buggy-factorial 4))
;; => 24

;; ... but let's pretend we don't know that. However, we suspect buggy-mult
;; *might* be the source of our problem. We can using the tracing macro to trace
;; it. This requires a bit of care.

(def buggy-mult-root buggy-mult)
;; We have to do this, because we can't have buggy-mult redef'd to a function
;; that ends up calling buggy-mult. That creates an infinite loop!

(with-redefs [buggy-mult (fn [x y] (eval `(tracing (buggy-mult-root ~x ~y))))]
  (buggy-factorial 4))

;; <stdout> The form was:  (jfdi/buggy-mult-root 1 2)
;; <stdout> Returning...  3
;; <stdout> The form was:  (jfdi/buggy-mult-root 3 3)
;; <stdout> Returning...  10
;; <stdout> The form was:  (jfdi/buggy-mult-root 10 4)
;; <stdout> Returning...  41

;; We confirm our suspicion: the bug is in buggy-mult-root!

;; In practice, this is NOT the best way to trace a function. Throwing eval at
;; production systems is frowned upon, and for very good reasons. However, it
;; *was* quick. In a few seconds, we managed to do something that'd require a
;; bulky, reinvent-the-world framework in another language.
