(ns p99.core)

;;; All 99 problems are listed below. Each one indicates its level of
;;; difficulty as well as the text of the problem. There are just skeletons,
;;; feel free to re-write the code as you want, and most functions will need
;;; you to adjust their arguments to be meaningful.
;;;
;;; See src/p88/core_test for the tests.
;;;
;;; See also http://www.4clojure.com, where you can solve the problems online,
;;; and http://www.4clojure.com/problem/NUM, where NUM is a problem number, to
;;; see it online.

;; problem 1 (Elementary)
(def nothing-but-the-truth-solution
  ;; This is a clojure form. Enter a value which will make the form evaluate to
  ;; true. Don't over think it! If you are confused, see the getting started
  ;; page. Hint: true is equal to true.
  true)


;; problem 2 (Elementary)
(def simple-math-solution
  ;; If you are not familiar with polish notation, simple arithmetic might seem
  ;; confusing. Note: Enter only enough to fill in the blank (in this case, a
  ;; single number) - do not retype the whole problem.
  4)


;; problem 3 (Elementary)
(def intro-to-strings-solution
  ;; Clojure strings are Java strings. This means that you can use any of the
  ;; Java string methods on Clojure strings.
  "HELLO WORLD")


;; problem 4 (Elementary)
(def intro-to-lists-solution
  ;; Lists can be constructed with either a function or a quoted form.
  (list :a :b :c))


;; problem 5 (Elementary)
(def lists-conj-solution
  ;; When operating on a list, the conj function will return a new list with
  ;; one or more items "added" to the front.
  (list 1 2 3 4))


;; problem 6 (Elementary)
(def intro-to-vectors-solution
  ;; Vectors can be constructed several ways. You can compare them with lists.
  [:a :b :c])


;; problem 7 (Elementary)
(def vectors-conj-solution
  ;; When operating on a Vector, the conj function will return a new vector
  ;; with one or more items "added" to the end.
  [1 2 3 4])


;; problem 8 (Elementary)
(def intro-to-sets-solution
  ;; Sets are collections of unique values.
  #{:a :b :c :d})


;; problem 9 (Elementary)
(def sets-conj-solution
  ;; When operating on a set, the conj function returns a new set with one or
  ;; more keys "added".
  2)


;; problem 10 (Elementary)
(def intro-to-maps-solution
  ;; Maps store key-value pairs. Both maps and keywords can be used as lookup
  ;; functions. Commas can be used to make maps more readable, but they are not
  ;; required.
  20)


;; problem 11 (Elementary)
(def maps-conj-solution
  ;; When operating on a map, the conj function returns a new map with one or
  ;; more key-value pairs "added".
  [:b 2])


;; problem 12 (Elementary)
(def intro-to-sequences-solution
  ;; All Clojure collections support sequencing. You can operate on sequences
  ;; with functions like first, second, and last.
  3)


;; problem 13 (Elementary)
(def sequences-rest-solution
  ;; The rest function will return all the items of a sequence except the
  ;; first.
  [20 30 40])


;; problem 14 (Elementary)
(def intro-to-functions-solution
  ;; Clojure has many different ways to create functions.
  8)


;; problem 15 (Elementary)
(defn double-down-solution
  [x] ;; update args as needed
  ;; Write a function which doubles a number.
  (* 2 x))


;; problem 16 (Elementary)
(defn hello-world-solution
  [name] ;; update args as needed
  ;; Write a function which returns a personalized greeting.
  (str "Hello, " name "!"))


;; problem 17 (Elementary)
(def sequences-map-solution
  ;; The map function takes two arguments: a function (f) and a sequence (s).
  ;; Map returns a new sequence consisting of the result of applying f to each
  ;; item of s. Do not confuse the map function with the map data structure.
  [6 7 8])


;; problem 18 (Elementary)
(def sequences-filter-solution
  ;; The filter function takes two arguments: a predicate function (f) and a
  ;; sequence (s). Filter returns a new sequence consisting of all the items of
  ;; s for which (f item) returns true.
  [6 7])


;; problem 19 (Easy)
;; restrictions: last
(defn last-element-solution
  [es] ;; update args as needed
  ;; Write a function which returns the last element in a sequence.
  (loop [es es]
    (if (empty? (rest es))
      (first es)
      (recur (rest es)))))


;; problem 20 (Easy)
(defn penultimate-element-solution
  [es] ;; update args as needed
  ;; Write a function which returns the second to last element from a sequence.
  (loop [es es]
    (if (empty? (rest (rest es)))
      (first es)
      (recur (rest es)))))


;; problem 21 (Easy)
;; restrictions: nth
(defn nth-element-solution
  [es n] ;; update args as needed
  ;; Write a function which returns the Nth element from a sequence.
  (loop [es es n n]
    (if (= 0 n)
      (first es)
      (recur (rest es) (dec n)))))


;; problem 22 (Easy)
;; restrictions: count
(defn count-a-sequence-solution
  [es] ;; update args as needed
  ;; Write a function which returns the total number of elements in a sequence.
  (loop [es es ans 0]
    (if (empty? es)
      ans
      (recur (rest es) (inc ans)))))


;; problem 23 (Easy)
;; restrictions: reverse, rseq
(defn reverse-a-sequence-solution
  [es] ;; update args as needed
  ;; Write a function which reverses a sequence.
  (loop [es es acc []]
    (if (empty? es)
      acc
      (recur (rest es) (cons (first es) acc)))))


;; problem 24 (Easy)
(defn sum-it-all-up-solution
  [es] ;; update args as needed
  ;; Write a function which returns the sum of a sequence of numbers.
  (loop [es es s 0]
    (if (empty? es)
      s
      (recur (rest es) (+ s (first es))))))


;; problem 25 (Easy)
(defn find-the-odd-numbers-solution
  [es] ;; update args as needed
  ;; Write a function which returns only the odd numbers from a sequence.
  (loop [es es ans (seq [])]
    (if (empty? es)
      (reverse ans)
      (if (odd? (first es))
        (recur (rest es) (cons (first es) ans))
        (recur (rest es) ans)))))


;; problem 26 (Easy)
(defn fibonacci-sequence-solution
  [n] ;; update args as needed
  ;; Write a function which returns the first X fibonacci numbers.
  (cond (= n 0) []
        (= n 1) [1]
        (= n 2) [1 1]
        :else (loop [n (- n 2)
                     f0 1
                     f1 1
                     ans [1 1]]
                (if (= n 0)
                  (reverse ans)
                  (recur (dec n) f1 (+ f1 f0) (cons (+ f1 f0) ans))))))

;; problem 27 (Easy)
(defn palindrome-detector-solution
  [es] ;; update args as needed
  ;; Write a function which returns true if the given sequence is a palindrome.
  ;;
  ;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
  (= (reverse es) (seq es)))


;; problem 28 (Easy)
;; restrictions: flatten
(defn flatten-a-sequence-solution
  [es] ;; update args as needed
  ;; Write a function which flattens a sequence.
  (if (sequential? es)
    (mapcat flatten-a-sequence-solution es)
    (seq [es])))
;; muzui
;; ok?


;; problem 29 (Easy)
(defn get-the-caps-solution
  [s] ;; update args as needed
  ;; Write a function which takes a string and returns a new string containing
  ;; only the capital letters.
  (apply str (filter #(Character/isUpperCase %) (seq s))))

;; problem 30 (Easy)
(defn compress-a-sequence-solution
  [es] ;; update args as needed
  ;; Write a function which removes consecutive duplicates from a sequence.
  (if (empty? es)
    es
    (loop [prev (first es)
           es (seq (rest es))
           acc (seq [prev])]
      (if (empty? es)
        (reverse acc)
        (recur (first es) (rest es) (if (= prev (first es))
                                      acc
                                      (cons (first es) acc)))))))


;; problem 31 (Easy)
(defn pack-a-sequence-solution
  [es] ;; update args as needed
  ;; Write a function which packs consecutive duplicates into sub-lists.
  (if (empty? es)
    es
    (loop [prev (first es)
           es (seq (rest es))
           acc (seq [prev])
           acc-all (seq [])]
      (if (empty? es)
        (reverse (cons acc acc-all))
        (if (= (first es) prev)
          (recur
           (first es)
           (rest es)
           (cons (first es) acc)
           acc-all)
          (recur
           (first es)
           (rest es)
           (seq [(first es)])
           (cons acc acc-all)))))))

;; problem 32 (Easy)
(defn duplicate-a-sequence-solution
  [es] ;; update args as needed
  ;; Write a function which duplicates each element of a sequence.
  (mapcat #(seq [% %]) es))


;; problem 33 (Easy)
(defn replicate-a-sequence-solution
  [es n] ;; update args as needed
  ;; Write a function which replicates each element of a sequence a variable
  ;; number of times.
  (mapcat #(repeat n %) es))


;; problem 34 (Easy)
;; restrictions: range
(defn implement-range-solution
  [a b] ;; update args as needed
  ;; [a, b)
  ;; Write a function which creates a list of all integers in a given range.
  (loop [a a
         b b
         acc '()]
    (if (>= a b)
      (reverse acc)
      (recur (inc a) b (cons a acc)))))


;; problem 35 (Elementary)
(def local-bindings-solution
  7)


;; problem 36 (Elementary)



;; problem 37 (Elementary)
(defn regular-expressions-solution
  [& args] ;; update args as needed
  ;; Regex patterns are supported with a special reader macro.
  nil)


;; problem 38 (Easy)
;; restrictions: max, max-key
(defn maximum-value-solution
  [& ns] ;; update args as needed
  ;; Write a function which takes a variable number of parameters and returns
  ;; the maximum value.
  (loop [ns ns
         m (first ns)]
    (if (empty? ns)
      m
      (recur (rest ns) (if (< m (first ns))
                         (first ns)
                         m)))))


;; problem 39 (Easy)
;; restrictions: interleave
(defn interleave-two-seqs-solution
  [es1 es2] ;; update args as needed
  ;; Write a function which takes two sequences and returns the first item from
  ;; each, then the second item from each, then the third, etc.
  (loop [es1 es1
         es2 es2
         acc nil]
    (if (or (empty? es1) (empty? es2))
      (reverse acc)
      (recur (rest es1) (rest es2) (cons (first es2) (cons (first es1) acc))))))

;; problem 40 (Easy)
;; restrictions: interpose
(defn interpose-a-seq-solution
  [sep es] ;; update args as needed
  ;; Write a function which separates the items of a sequence by an arbitrary
  ;; value.
  (if (empty? es)
    es
    (loop [es es
           acc nil]
      (if (empty? (rest es))
        (reverse (cons (first es) acc))
        (recur (rest es) (cons sep (cons (first es) acc)))))))

;; problem 41 (Easy)
(defn drop-every-nth-item-solution
  [es m] ;; update args as needed
  ;; Write a function which drops every Nth item from a sequence.
  (loop [es es
         acc nil
         c 1]
    (if (empty? es)
      (reverse acc)
      (recur (rest es)
             (if (= (mod c m) 0)
               acc
               (cons (first es) acc))
             (inc c)))))


;; problem 42 (Easy)
(defn factorial-fun-solution
  [n] ;; update args as needed
  ;; Write a function which calculates factorials.
  (loop [n n
         f 1]
    (if (= n 0)
      f
      (recur (dec n) (* n f)))))


;; problem 43 (Medium)
(defn reverse-interleave-solution
  [es n] ;; update args as needed
  ;; Write a function which reverses the interleave process into x number of
  ;; subsequences.
  (loop [splitted (partition n es)
         acc nil]
    (if (empty? (first splitted))
      (reverse acc)
      (recur (map rest splitted) (cons (map first splitted) acc)))))


;; problem 44 (Medium)
(defn rotate-sequence-solution
  [n es] ;; update args as needed
  ;; Write a function which can rotate a sequence in either direction.
  (let [s (count es)
        k (max (quot (- (dec s) n) s) 0) ;; ceil
        n (+ n (* s k))
        es (cycle es)]
    (take s (drop n es))))


;; problem 45 (Easy)
(defn intro-to-iterate-solution
  [& args] ;; update args as needed
  ;; The iterate function can be used to produce an infinite lazy sequence.
  nil)


;; problem 46 (Medium)
(defn flipping-out-solution
  [f] ;; update args as needed
  ;; Write a higher-order function which flips the order of the arguments of an
  ;; input function.
  (fn [x y]
    (f y x)))


;; problem 47 (Easy)
(def contain-yourself-solution
  ;; The contains? function checks if a KEY is present in a given collection.
  ;; This often leads beginner clojurians to use it incorrectly with
  ;; numerically indexed collections like vectors and lists.
  4)


;; problem 48 (Easy)
(def intro-to-some-solution
  ;; The some function takes a predicate function and a collection. It returns
  ;; the first logical true value of (predicate x) where x is an item in the
  ;; collection.
  6)


;; problem 49 (Easy)
;; restrictions: split-at
(defn split-a-sequence-solution
  [n es] ;; update args as needed
  ;; Write a function which will split a sequence into two parts.
  (loop [es es
         c 0
         acc nil]
    (if (< c n)
      (recur (rest es) (inc c) (cons (first es) acc))
      (seq [(reverse acc) es]))))

;; problem 50 (Medium)
(defn split-by-type-solution
  [es] ;; update args as needed
  ;; Write a function which takes a sequence consisting of items with different
  ;; types and splits them up into a set of homogeneous sub-sequences. The
  ;; internal order of each sub-sequence should be maintained, but the
  ;; sub-sequences themselves can be returned in any order (this is why 'set'
  ;; is used in the test cases).
  (letfn [(build-type-map [es]
            (loop [es es
                   mp {}]
              (if (empty? es)
                mp
                (recur (rest es)
                       (assoc mp (type (first es)) (cons (first es) (mp (type (first es)))))))))]
    (let [mp (build-type-map es)]
      (for [k (keys mp)
            :let [v (mp k)]]
        (reverse v)))))

;; problem 51 (Easy)
(defn advanced-destructuring-solution
  [& args] ;; update args as needed
  ;; Here is an example of some more sophisticated destructuring.
  nil)


;; problem 52 (Easy)
(defn intro-to-destructuring-solution
  [& args] ;; update args as needed
  ;; Let bindings and function parameter lists support destructuring.
  nil)


;; problem 53 (Hard)
(defn longest-increasing-sub-seq-solution
  [ns] ;; update args as needed
  ;; Given a vector of integers, find the longest consecutive sub-sequence of
  ;; increasing numbers. If two sub-sequences have the same length, use the one
  ;; that occurs first. An increasing sub-sequence must have a length of 2 or
  ;; greater to qualify.
  (loop [prev (first ns)
         ns (rest ns)
         ans []
         acc [prev]]
    (if (empty? ns)
      (if (< (count ans) (count acc))
        (if (< 1 (count acc))
          (reverse acc)
          [])
        (if (< 1 (count ans))
          (reverse ans)
          []))
      (if (< prev (first ns))
        (recur (first ns) (rest ns) ans (cons (first ns) acc))
        (recur (first ns)
               (rest ns)
               (if (< (count ans) (count acc))
                 acc
                 ans)
               [(first ns)])))))


;; problem 54 (Medium)
;; restrictions: partition, partition-all
(defn partition-a-sequence-solution
  [n es] ;; update args as needed
  ;; Write a function which returns a sequence of lists of x items each. Lists
  ;; of less than x items should not be returned.
  (loop [es es
         acc nil]
    (if (> n (count es))
      (reverse acc)
      (recur (drop n es)
             (cons (take n es) acc)))))


;; problem 55 (Medium)
;; restrictions: frequencies
(defn count-occurrences-solution
  [es] ;; update args as needed
  ;; Write a function which returns a map containing the number of occurences
  ;; of each distinct item in a sequence.
  (loop [mp {}
         es es]
    (if (empty? es)
      mp
      (let [e (first es)]
        (if (contains? mp e)
          (recur (assoc mp e (inc (mp e))) (rest es))
          (recur (assoc mp e 1) (rest es)))))))


;; problem 56 (Medium)
;; restrictions: distinct
(defn find-distinct-items-solution
  [es] ;; update args as needed
  ;; Write a function which removes the duplicates from a sequence. Order of
  ;; the items must be maintained.
  (loop [es es
         st #{}
         acc nil]
    (if (empty? es)
      (reverse acc)
      (if (contains? st (first es))
        (recur (rest es) st acc)
        (recur (rest es) (conj st (first es)) (cons (first es) acc))))))
      


;; problem 57 (Elementary)
(def simple-recursion-solution
  ;; A recursive function is a function which calls itself. This is one of the
  ;; fundamental techniques used in functional programming.
  nil)


;; problem 58 (Medium)
;; restrictions: comp
(defn function-composition-solution
  [& fs] ;; update args as needed
  ;; Write a function which allows you to create function compositions. The
  ;; parameter list should take a variable number of functions, and create a
  ;; function applies them from right-to-left.
  (letfn [(comp1 [& fs]
            (loop [fs (reverse fs)
                   f (fn [x] x)]
              (if (empty? fs)
                f
                (recur (rest fs) (fn [x] ((first fs) (f x)))))))]
    (let [fL (last fs)
          fs (butlast fs)]
      (fn [& args] ((apply comp1 fs) (apply fL args))))))


;; problem 59 (Medium)
;; restrictions: juxt
(defn juxtaposition-solution
  [& fs] ;; update args as needed
  ;; Take a set of functions and return a new function that takes a variable
  ;; number of arguments and returns a sequence containing the result of
  ;; applying each function left-to-right to the argument list.
  (fn [& args]
    (loop [fs fs
           acc nil]
      (if (empty? fs)
        (reverse acc)
        (recur (rest fs) (cons (apply (first fs) args) acc))))))


;; problem 60 (Medium)
;; restrictions: reductions
(defn sequence-reductions-solution
  ;; Write a function which behaves like reduce, but returns each intermediate
  ;; value of the reduction. Your function must accept either two or three
  ;; arguments, and the return sequence must be lazy.
  ([f v es] (letfn [(iter [v es]
                      (if (empty? es)
                        (lazy-seq [v])
                        (let [nv (f v (first es))]
                          (lazy-seq
                           (cons v (iter nv (rest es)))))))]
              (iter v es)))
  ([f es] (sequence-reductions-solution f (first es) (rest es))))

;; restrictions: zipmap
(defn map-construction-solution
  [as bs] ;; update args as needed
  ;; Write a function which takes a vector of keys and a vector of values and
  ;; constructs a map from them.
  (loop [as as
         bs bs
         mp {}]
    (if (or (empty? as) (empty? bs))
      mp
      (recur (rest as) (rest bs) (assoc mp (first as) (first bs))))))


;; problem 62 (Easy)
;; restrictions: iterate
(defn re-implement-iterate-solution
  [f x] ;; update args as needed
  ;; Given a side-effect free function f and an initial value x write a
  ;; function which returns an infinite lazy sequence of x, (f x), (f (f x)),
  ;; (f (f (f x))), etc.
  (lazy-seq (cons x (re-implement-iterate-solution f (f x)))))


;; problem 63 (Easy)
;; restrictions: group-by
(defn group-a-sequence-solution
  [f es] ;; update args as needed
  ;; Given a function f and a sequence s, write a function which returns a map.
  ;; The keys should be the values of f applied to each item in s. The value at
  ;; each key should be a vector of corresponding items in the order they
  ;; appear in s.
  (let [mp
        (loop [es es
               mp {}]
          (if (empty? es)
            mp
            (let [k (f (first es))]
              (recur (rest es) (assoc mp k (conj (mp k) (first es)))))))]
    (into {} (for [[k v] mp]
               [k (reverse v)]))))


;; problem 64 (Elementary)
(def intro-to-reduce-solution
  ;; Reduce takes a 2 argument function and an optional starting value. It then
  ;; applies the function to the first 2 items in the sequence (or the starting
  ;; value and the first element of the sequence). In the next iteration the
  ;; function will be called on the previous return value and the next item
  ;; from the sequence, thus reducing the entire collection to one value. Don't
  ;; worry, it's not as complicated as it sounds.
  +)


;; problem 65 (Medium)
;; restrictions: class, type, Class, vector?, sequential?, list?, seq?, map?,
;;               set?, instance?, getClass
(defn black-box-testing-solution
  [seq] ;; update args as needed
  ;; Clojure has many sequence types, which act in subtly different ways. The
  ;; core functions typically convert them into a uniform "sequence" type and
  ;; work with them that way, but it can be important to understand the
  ;; behavioral and performance differences so that you know which kind is
  ;; appropriate for your application.
  ;;
  ;; Write a function which takes a collection and returns one of :map, :set,
  ;; :list, or :vector - describing the type of collection it was given. You
  ;; won't be allowed to inspect their class or use the built-in predicates
  ;; like list? - the point is to poke at them and understand their behavior.
  nil)


;; problem 66 (Easy)
(defn greatest-common-divisor-solution
  [a b] ;; update args as needed
  ;; Given two integers, write a function which returns the greatest common
  ;; divisor.
  (loop [a a
         b b]
    (if (= b 0)
      a
      (recur b (mod a b)))))


;; problem 67 (Medium)
(defn prime-numbers-solution
  [c] ;; update args as needed
  ;; Write a function which returns the first x number of prime numbers.
  (letfn [(prime? [n] (loop [k 2]
                        (if (< k n)
                          (if (= 0 (mod n k))
                            false
                            (recur (inc k)))
                          true)))]
    (take c (filter prime? (iterate inc 2)))))


;; problem 68 (Elementary)
(def recurring-theme-solution
  ;; Clojure only has one non-stack-consuming looping construct: recur. Either
  ;; a function or a loop can be used as the recursion point. Either way, recur
  ;; rebinds the bindings of the recursion point to the values it is passed.
  ;; Recur must be called from the tail-position, and calling it elsewhere will
  ;; result in an error.
  [7 6 5 4 3])

;; problem 69 (Medium)
;; restrictions: merge-with
(defn merge-with-a-function-solution
  [f & mps] ;; update args as needed
  ;; Write a function which takes a function f and a variable number of maps.
  ;; Your function should return a map that consists of the rest of the maps
  ;; conj-ed onto the first. If a key occurs in more than one map, the
  ;; mapping(s) from the latter (left-to-right) should be combined with the
  ;; mapping in the result by calling (f val-in-result val-in-latter)
  (letfn [(g [f m1 m2]
            (into {} (for [k (set (concat (keys m1) (keys m2)))]
                       (cond (and (m1 k) (m2 k)) [k (f (m1 k) (m2 k))]
                             (m1 k) [k (m1 k)]
                             (m2 k) [k (m2 k)]))))]
    (loop [res (first mps)
           mps (rest mps)]
      (if (empty? mps)
        res
        (recur (g f res (first mps)) (rest mps))))))

;; problem 70 (Medium)
(require '[clojure.string :as str])
(defn word-sorting-solution
  [s] ;; update args as needed
  ;; Write a function that splits a sentence up into a sorted list of words.
  ;; Capitalization should not affect sort order and punctuation should be
  ;; ignored.
  (let [words (re-seq #"[A-Za-z]+" s)]
    (sort-by str/lower-case words)))
        

;; problem 71 (Elementary)
(defn rearranging-code-solution
  [x] ;; update args as needed
  ;; The -> macro threads an expression x through a variable number of forms.
  ;; First, x is inserted as the second item in the first form, making a list
  ;; of it if it is not a list already. Then the first form is inserted as the
  ;; second item in the second form, making a list of that form if necessary.
  ;; This process continues for all the forms. Using -> can sometimes make your
  ;; code more readable.
  (last x))

;; problem 72: nothing here, just breath :)

;; problem 73 (Hard)
(defn analyze-a-tic-tac-toe-board-solution
  [board] ;; update args as needed
  ;; A tic-tac-toe board is represented by a two dimensional vector. X is
  ;; represented by :x, O is represented by :o, and empty is by :e.
  ;; A player wins by placing three Xs or three Os in a horizontal, vertical,
  ;; or diagonal row. Write a function which analyzes a tic-tac-toe board and
  ;; returns :x if X has won, :o if O has won, and nil if neither player has
  ;; won.
  (letfn [(third [coll] (nth coll 2))
          (check [board s] (or (check-row board s) (check-col board s) (check-diag board s)))
          (check-row [board s] (some #(= [s s s] %) board))
          (check-col [board s] (let [col1 (map first board)
                                     col2 (map second board)
                                     col3 (map third board)]
                                 (some #(= [s s s] %) [col1 col2 col3])))
          (check-diag [board s] (let [diag1 [(first (first board))
                                             (second (second board))
                                             (third (third board))]
                                      diag2 [(first (third board))
                                             (second (second board))
                                             (third (first board))]]
                                  (some #(= [s s s] %) [diag1 diag2])))]
    (cond (check board :o) :o
          (check board :x) :x
          :else nil)))


;; problem 74 (Medium)
(defn filter-perfect-squares-solution
  [s] ;; update args as needed
  ;; Given a string of comma separated integers, write a function which returns
  ;; a new comma separated string that only contains the numbers which are
  ;; perfect squares.
  (let [ns (map read-string (re-seq #"\d+" s))]
    (letfn [(perfect? [n]
              (loop [k 1]
                (cond (< n (* k k)) false
                      (= n (* k k)) true
                      :else (recur (inc k)))))]
      (reduce (fn [a b] (str a "," b)) (filter perfect? ns)))))


;; problem 75 (Medium)
(defn euler-s-totient-function-solution
  [x] ;; update args as needed
  ;; Two numbers are coprime if their greatest common divisor equals 1. Euler's
  ;; totient function f(x) is defined as the number of positive integers less
  ;; than x which are coprime to x. The special case f(1) equals 1. Write a
  ;; function which calculates Euler's totient function.
  (letfn [(gcd [a b]
            (loop [a a
                   b b]
              (if (= b 0)
                a
                (recur b (mod a b)))))]
    (count (filter #(= (gcd % x) 1) (range 0 x)))))


;; problem 76 (Medium)
(def intro-to-trampoline-solution
  ;; The trampoline function takes a function f and a variable number of
  ;; parameters. Trampoline calls f with any parameters that were supplied. If
  ;; f returns a function, trampoline calls that function with no arguments.
  ;; This is repeated, until the return value is not a function, and then
  ;; trampoline returns that non-function value. This is useful for
  ;; implementing mutually recursive algorithms in a way that won't consume the
  ;; stack.
  [1 3 5 7 9 11])


;; problem 77 (Medium)
(defn anagram-finder-solution
  [words] ;; update args as needed
  ;; Write a function which finds all the anagrams in a vector of words. A word
  ;; x is an anagram of word y if all the letters in x can be rearranged in a
  ;; different order to form y. Your function should return a set of sets,
  ;; where each sub-set is a group of words which are anagrams of each other.
  ;; Each sub-set should have at least two words. Words without any anagrams
  ;; should not be included in the result.
  (->> words
      (group-by #(apply str (sort %))) ;; kv
      (vals)
      (filter #(> (count %) 1))
      (map set)
      set))


;; problem 78 (Medium)
;; restrictions: trampoline
;;(def reimplement-trampoline-solution nil)
(defn reimplement-trampoline-solution
  [f & args] ;; update args as needed
  ;; Reimplement the function described in "Intro to Trampoline".
  (let [res (apply f args)]
    (loop [res res]
      (if (fn? res)
        (recur (res))
        res))))


;; ;; problem 79 (Hard)

(defn triangle-minimal-path-solution
  [triangle] ;; update args as needed
  ;; Write a function which calculates the sum of the minimal path through a
  ;; triangle. The triangle is represented as a collection of vectors. The path
  ;; should start at the top of the triangle and move to an adjacent number on
  ;; the next row until the bottom of the triangle is reached.
  (letfn [(next-solution [prev-ans next-row]
            (let [n (count next-row)]
              (letfn [(ok? [i]
                        (and (<= 0 i) (< i (dec n))))]
                (for [i (range n)]
                  (cond (and (ok? i) (ok? (dec i)))
                        (+ (nth next-row i) (min (nth prev-ans i)
                                                 (nth prev-ans (dec i))))
                        (ok? i) (+ (nth next-row i) (nth prev-ans i))
                        (ok? (dec i)) (+ (nth next-row i) (nth prev-ans (dec i))))))))]
    (loop [ans (first triangle)
           res (rest triangle)]
      (if (empty? res)
        (apply min ans)
        (recur (next-solution ans (first res)) (rest res))))))

;; problem 80 (Medium)
(defn perfect-numbers-solution
  [n] ;; update args as needed
  ;; A number is "perfect" if the sum of its divisors equal the number itself.
  ;; 6 is a perfect number because 1+2+3=6. Write a function which returns true
  ;; for perfect numbers and false otherwise.
  (letfn [(divisors [n]
            (filter #(= (mod n %) 0) (range 1 n)))]
    (= n (reduce + (divisors n)))))


;; problem 81 (Easy)
;; restrictions: intersection
(defn set-intersection-solution
  [s0 s1] ;; update args as needed
  ;; Write a function which returns the intersection of two sets. The
  ;; intersection is the sub-set of items that each set has in common.
  (loop [s0 (seq s0)
         acc #{}]
    (if (empty? s0)
      acc
      (recur (rest s0) (if (contains? s1 (first s0))
                         (conj acc (first s0))
                         acc)))))


;; problem 82 (Hard)
;; (defn word-chains-solution
;;   [& args] ;; update args as needed
;;   ;; A word chain consists of a set of words ordered so that each word differs
;;   ;; by only one letter from the words directly before and after it. The one
;;   ;; letter difference can be either an insertion, a deletion, or a
;;   ;; substitution. Here is an example word chain:
;;   ;;
;;   ;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;;   ;;
;;   ;; Write a function which takes a sequence of words, and returns true if they
;;   ;; can be arranged into one continous word chain, and false if they cannot.
;;   (letfn [(dfs [c rest visited]
;;             (if (empty? rest)
;;               true
              

;; problem 83 (Easy)
(defn a-half-truth-solution
  [& args] ;; update args as needed
  ;; Write a function which takes a variable number of booleans. Your function
  ;; should return true if some of the parameters are true, but not all of the
  ;; parameters are true. Otherwise your function should return false.
  (letfn [(not-nil? [x] (not (nil? x)))]
    (and (not-nil? (some true? args)) (not-nil? (some false? args)))))


;; problem 84 (Hard)
(defn transitive-closure-solution
  [rels] ;; update args as needed
  ;; Write a function which generates the transitive closure of a binary
  ;; relation. The relation will be represented as a set of 2 item vectors.
  (letfn [(next [rels]
            (set (concat rels (for [r1 (seq rels)
                                    r2 (seq rels)
                                    :when (= (second r1) (first r2))]
                                [(first r1) (second r2)]))))]
    (loop [rels rels]
      (let [new-rels (next rels)]
        (if (= (count rels) (count new-rels))
          rels
          (recur new-rels))))))


;; problem 85 (Medium)
(defn power-set-solution
  [st] ;; update args as needed
  ;; Write a function which generates the power set of a given set. The power
  ;; set of a set x is the set of all subsets of x, including the empty set and
  ;; x itself.
  (letfn [(power-set [es acc]
            (if (empty? es)
              (lazy-seq [acc])
              (lazy-seq (concat (power-set (rest es) (conj acc (first es)))
                                (power-set (rest es) acc)))))]
    (set (power-set (seq st) #{}))))


;; problem 86 (Medium)
(defn happy-numbers-solution
  [n] ;; update args as needed
  ;; Happy numbers are positive integers that follow a particular formula: take
  ;; each individual digit, square it, and then sum the squares to get a new
  ;; number. Repeat with the new number and eventually, you might get to a
  ;; number whose squared sum is 1. This is a happy number. An unhappy number
  ;; (or sad number) is one that loops endlessly. Write a function that
  ;; determines if a number is happy or not.
  (letfn [(square [n] (* n n))
          (step [n]
            (loop [n n
                   acc 0]
              (if (= n 0)
                acc
                (recur (quot n 10) (+ acc (square (mod n 10)))))))]
    (loop [n n]
      (cond (= n 1) true
            (= n 4) false
            :else (recur (step n))))))

(use 'clojure.set)
;; problem 88 (Easy)
(defn symmetric-difference-solution
  [sa sb] ;; update args as needed
  ;; Write a function which returns the symmetric difference of two sets. The
  ;; symmetric difference is the set of items belonging to one but not both of
  ;; the two sets.
  (into #{} (union (for [a sa
                        :when (not (contains? sb a))] a)
                  (for [b sb
                        :when (not (contains? sa b))] b))))

;; problem 89 (Hard)
;; simple graphだと思って良さそう？
;; connectedの判定をやる
;; (defn graph-tour-solution
;;   [g]
;;   ;; Starting with a graph you must write a function that returns true if it is
;;   ;; possible to make a tour of the graph in which every edge is visited
;;   ;; exactly once.
;;   ;;
;;   ;; The graph is represented by a vector of tuples, where each tuple
;;   ;; represents a single edge.
;;   ;;
;;   ;; The rules are:
;;   ;;
;;   ;; - You can start at any node.
;;   ;; - You must visit each edge exactly once. - All edges are undirected.
;;   (letfn [(vertex-num [g]
;;             (loop [es g
;;                    s #{}]
;;               (if (empty? es)
;;                 (count s)
;;                 (let [[a b] (first es)]
;;                   (recur (rest es) (into s [a b]))))))
;;           (get-vs [es] (reduce (fn [s e] (into s e)) #{} es))
;;           (uf [es]
;;             (let [vs (get-vs es)
;;                   init-uf (into {} (map #([% [%]])))]))
;;           (connected? [g]
;;             (loop [es g
;;                    group (map #(set [%]) get-vs)]))
;;           (get-deg-map [g]
;;             ;;{:a 2, :b 3, :c 4}
;;             (loop [es g
;;                    mp {}]
;;               (if (empty? es)
;;                 mp
;;                 (let [[a b] (first es)
;;                       next-mp (-> mp
;;                                   (assoc a (if (contains? mp a) (inc (mp a)) 1))
;;                                   (assoc b (if (contains? mp b) (inc (mp b)) 1)))]
;;                   (recur (rest es) next-mp)))))
;;           (deg-check [deg-map]
;;             (letfn [(check1
;;                       []
;;                       (let [ds (vals deg-map)
;;                             n (count ds)]
;;                         (and (= (->> ds (map #(mod % 2)) (filter #(= 1 %)) (count)) 2)
;;                              (= (->> ds (map #(mod % 2)) (filter #(= 0 %)) (count)) (- n 2)))))
;;                     (check2
;;                       []
;;                       (let [ds (vals deg-map)
;;                             n (count ds)]
;;                         (= (->> ds (map #(mod % 2)) (filter #(= 0 %)) (count)) n)))]
;;               (or (check1) (check2))))]
;;     (and (connected? g) (-> g (get-deg-map) (deg-check)))))

(defn get-deg-map [g]
  ;;{:a 2, :b 3, :c 4}
  (loop [es g
         mp {}]
    (if (empty? es)
      mp
      (let [[a b] (first es)
            next-mp (-> mp
                        (assoc a (if (contains? mp a) (inc (mp a)) 1))
                        (assoc b (if (contains? mp b) (inc (mp b)) 1)))]
        (recur (rest es) next-mp)))))


;; problem 90 (Easy)
(defn cartesian-product-solution
  [sa sb] ;; update args as needed
  ;; Write a function which calculates the Cartesian product of two sets.
  (into #{} (for [a sa
                  b sb] [a b])))

(defn get-vs
  ;; get vertexes
  [g]
  (into #{} (flatten (seq g))))

;; (defn reachable-nodes
;;   [s reachable-vs g]
;;   ;; g is like {:a [:b,:c], :b [:a], :c [:a]}
;;   (letfn [(iter [vs ans]
;;             (if (empty? vs)
;;               ans
;;               (let [v (first vs)]
;;                 (if (contains? reachable-vs v)))))]
;;     (iter (g s) #{})))

;; problem 91 (Hard)
(defn graph-connectivity-solution
  [g] ;; update args as needed
  ;; Given a graph, determine whether the graph is connected. A connected graph
  ;; is such that a path exists between any two given nodes.
  ;;
  ;; -Your function must return true if the graph is connected and false
  ;; otherwise.
  ;; -You will be given a set of tuples representing the edges of a graph. Each
  ;; member of a tuple being a vertex/node in the graph.
  ;; -Each edge is undirected (can be traversed either direction).
  ;;
  nil)

(require '[clojure.string :as str])
;; problem 92 (Hard)
(defn read-roman-numerals-solution
  [s] ;; update args as needed
  ;; Roman numerals are easy to recognize, but not everyone knows all the rules
  ;; necessary to work with them. Write a function to parse a Roman-numeral
  ;; string and return the number it represents.
  ;;
  ;; You can assume that the input will be well-formed, in upper-case, and
  ;; follow the subtractive principle. You don't need to handle any numbers
  ;; greater than MMMCMXCIX (3999), the largest number representable with
  ;; ordinary letters.
  (let [cs (str/split s "")
        tbl #{"I"}]
    nil))


;; problem 93 (Medium)
(defn partially-flatten-a-sequence-solution
  [es] ;; update args as needed
  ;; Write a function which flattens any nested combination of sequential
  ;; things (lists, vectors, etc.), but maintains the lowest level sequential
  ;; items. The result should be a sequence of sequences with only one level of
  ;; nesting.
  (let [es (seq es)]
    (if (and (sequential? es) (not (empty? es)) (not (sequential? (first es))))
      (lazy-seq [es])
      (lazy-seq (mapcat partially-flatten-a-sequence-solution es)))))


;; problem 94 (Hard)
(defn game-of-life-solution
  [& args] ;; update args as needed
  ;; The game of life is a cellular automaton devised by mathematician John
  ;; Conway.
  ;;
  ;; The 'board' consists of both live (#) and dead ( ) cells. Each cell
  ;; interacts with its eight neighbours (horizontal, vertical, diagonal), and
  ;; its next state is dependent on the following rules:
  ;;
  ;; 1) Any live cell with fewer than two live neighbours dies, as if caused by
  ;;    under-population.
  ;; 2) Any live cell with two or three live neighbours lives on to the next
  ;;    generation.
  ;; 3) Any live cell with more than three live neighbours dies, as if by
  ;;    overcrowding.
  ;; 4) Any dead cell with exactly three live neighbours becomes a live cell,
  ;;    as if by reproduction.
  ;;
  ;; Write a function that accepts a board, and returns a board representing
  ;; the next generation of cells.
  nil)


;; problem 95 (Easy)
(defn check-tree
  [x]
  (if (sequential? x)
    (if (not= (count x) 3)
      false
      (let [lf (check-tree (second x))
            rf (check-tree (second (rest x)))]
        (and lf rf)))
    (= x nil)))
(defn to-tree-or-not-to-tree-solution
  [x] ;; update args as needed
  ;; Write a predicate which checks whether or not a given sequence represents
  ;; a binary tree. Each node in the tree must have a value, a left child, and
  ;; a right child.
  (check-tree x))


(defn mirror-tree
  [t]
  (if (sequential? t)
    (let [lt (second t)
          rt (second (rest t))]
      (lazy-seq (seq [(first t) (mirror-tree rt) (mirror-tree lt)])))
    t))
;; problem 96 (Easy)
(defn beauty-is-symmetry-solution
  [t] ;; update args as needed
  ;; Let us define a binary tree as "symmetric" if the left half of the tree is
  ;; the mirror image of the right half of the tree. Write a predicate to
  ;; determine whether or not a given binary tree is symmetric. (see To Tree,
  ;; or not to Tree for a reminder on the tree representation we're using).
  (if (sequential? t)
    (let [lt (second t)
          rt (second (rest t))]
      (= lt (mirror-tree rt)))
    true))


;; problem 97 (Easy)
(defn pascal-s-triangle-solution
  [n] ;; update args as needed
  ;; Pascal's triangle is a triangle of numbers computed using the following
  ;; rules:
  ;;  - The first row is 1.
  ;;  - Each successive row is computed by adding together adjacent numbers in
  ;;  the row above, and adding a 1 to the beginning and end of the row.
  ;;
  ;; Write a function which returns the nth row of Pascal's Triangle.
  ;;
  (letfn [(next-row [prev]
            (loop [prev prev
                   next [1]]
              (if (= (count prev) 1)
                (conj next 1)
                (recur (rest prev) (conj next (+ (first prev) (second prev)))))))]
    (loop [n (dec n)
           ans [1]]
      (if (= n 0)
        ans
        (recur (dec n) (next-row ans))))))


;; problem 98 (Medium)
(defn equivalence-classes-solution
  [f es] ;; update args as needed
  ;; A function f defined on a domain D induces an equivalence relation on D,
  ;; as follows: a is equivalent to b with respect to f if and only if (f a) is
  ;; equal to (f b). Write a function with arguments f and D that computes the
  ;; equivalence classes of D with respect to f.
  (letfn [(gen-next-classes
            [e classes]
            (loop [classes classes
                   new-classes []
                   found false]
              (if (empty? classes)
                (if found
                  new-classes
                  (conj new-classes #{e}))
                (if found
                  (recur (rest classes) (conj new-classes (first classes)) found)
                  (let [target (first classes)]
                    (if (= (f e) (f (first target)))
                      (recur (rest classes) (conj new-classes (conj target e)) true)
                      (recur (rest classes) (conj new-classes target) false)))))))]
    (loop [es es
           classes (seq [])]
      (if (empty? es)
        (set classes)
        (recur (rest es) (gen-next-classes (first es) classes))))))

(defn to-digits
  [n]
  (loop [n n
         acc []]
    (if (= n 0)
      (vec (reverse acc))
      (recur (quot n 10) (conj acc (mod n 10))))))
;; problem 99 (Easy)
(defn product-digits-solution
  [x y] ;; update args as needed
  ;; Write a function which multiplies two numbers and returns the result as a
  ;; sequence of its digits.
  (to-digits (* x y)))
