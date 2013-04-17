(ns toychest.amb-test
  (:use clojure.test toychest.amb)
  (:require [clojure.set :as set])
  )

(deftest pythagorean-triple-test
  (testing "simple, multi-requirement problem"
    (is (= '([3 4 5])
           (amb-let [a (amb (range 1 10))
                     b (amb (range a 10))
                     c (amb (range b 10))]
                    [[#{a} (> a 2)]
                     [#{a b c} (= (+ (* a a) (* b b)) (* c c))]]
                    [a b c])))))

(deftest destructure-amb-test
  (testing "we can deal with destructured vars"
    (is (= '([1 2]) 
           (amb-let [[a b] (amb [[1 2] [3 4]])]
                    [[#{a b} (= 3 (+ a b))]]
                    [a b])))))

(deftest consistent-test
  (testing  "we can handle cross-cutting consistency checks"
    (is (= [[3 2 1] [1 3 2] [2 1 3]] 
           (first (amb-let [[a1 a2 a3] (amb (permutations #{ 1 2 3}))
                            [b1 b2 b3] (amb (permutations #{ 1 2 3}))
                            [c1 c2 c3] (amb (permutations #{ 1 2 3}))]
                           ;; each row and each col can have each val only once
                           [[#{a1 a2 a3} (= (set [ a1 a2 a3])  #{1 2 3})]
                            [#{b1 b2 b3} (= (set [ b1 b2 b3])  #{1 2 3})]
                            [#{c1 c2 c3} (= (set [ c1 c2 c3])  #{1 2 3})]
                            [#{a1 b1 c1} (= (set [ a1 b1 c1])  #{1 2 3})]
                            [#{a2 b2 c2} (= (set [ a2 b2 c2])  #{1 2 3})]
                            [#{a3 b3 c3} (= (set [ a3 b3 c3])  #{1 2 3})]]
                           [[a1 a2 a3]
                            [b1 b2 b3]
                            [c1 c2 c3]]))))))

(deftest multiple-dwelling-test
  (is (= "baker 3 cooper 2 fletcher 4 miller 5 smith 1" 
         (first (amb-let [baker (amb [1 2 3 4 5])
                          cooper (amb [1 2 3 4 5])
                          fletcher (amb [1 2 3 4 5])
                          miller (amb [1 2 3 4 5])
                          smith (amb [1 2 3 4 5])]
                         [[#{baker cooper fletcher miller smith}
                           (distinct?  baker cooper fletcher miller smith)]
                          [#{baker} (not (= baker 5))]
                          [#{cooper} (not (= cooper 1))]
                          [#{fletcher} (and (not (= fletcher 5))
                                            (not (= fletcher 1)))]
                          [#{miller cooper} (> miller cooper)]
                          [#{smith fletcher} (not (= (Math/abs (- smith fletcher)) 1))]
                          [#{fletcher cooper} (not (= (Math/abs (- fletcher cooper)) 1))]]
                         (format "baker %s cooper %s fletcher %s miller %s smith %s" baker cooper fletcher miller smith))))))

(deftest queens-test
  ;; count all the solutions to the 8-queens problem; confirm there are 92
  ;; each variable q_i is the row number of a queen in column i
  ;; using permutations speeds this up by preventing any row from being repeated
  ;; as such, we only need to check diagonals
  ;; to do this more efficiently, we could
  ;; (a) break assignments up and 
  ;; (b) separate out each of the edges
  (is (= 92 (count  (amb-let [[q1 q2 q3 q4 q5 q6 q7 q8 :as qs] 
                              (amb (permutations #{1 2 3 4 5 6 7 8}))]
                             [[#{qs} (every? identity (for [i (range 0 8)
                                                            j (range (inc i) 8)] ;; only check to the right
                                                        (not (or (= (- j i) (- (qs j) (qs i)))
                                                                 (= (- i j) (- (qs j) (qs i)))))))]]
                             [qs])))))
(def einstein-puzzle
  (let [nationalities  #{:english :spanish :ukranian :japanese :norwegian}
        color #{:red :green :ivory :yellow :blue}
        beverages   #{:coffee :tea :milk :juice :water}
        animals  #{:dog :snail :zebra :fox :horse}
        cigarettes  #{:old-golds :kools :chesterfields :lucky-strikes :parliaments}] 
    (amb-let [[nat1 nat2 nat3 nat4 nat5 :as nats] (amb (permutations nationalities))
              [col1 col2 col3 col4 col5 :as cols]  (amb (permutations color))
              [bev1 bev2 bev3 bev4 bev5 :as bevs] (amb (permutations beverages))
              [pet1 pet2 pet3 pet4 pet5 :as pets] (amb (permutations animals))
              [cig1 cig2 cig3 cig4 cig5 :as cigs] (amb (permutations cigarettes))]
             [;; The Englishman lives in the red house.
              [#{nats cols}
               (contains? (set (map vector nats cols))
                          [:english :red])]
              ;; The Spaniard owns the dog.
              [#{nats pets}
               (contains? (set (map vector nats pets))
                          [:spanish :dog])]              
              ;; Coffee is drunk in the green house.
              [#{bevs cols}
               (contains? (set (map vector bevs cols))
                          [:coffee :green])]
              ;; The Ukrainian drinks tea.
              [#{bevs nats}
               (contains? (set  (map vector bevs nats))
                          [:tea :ukranian])]
              ;; The Old Gold smoker owns snails.
              [#{cigs pets}
               (contains? (set (map vector cigs pets))
                          [:old-golds :snail])]
              ;; Kools are smoked in the yellow house.
              [#{cols cigs}
               (contains? (set (map vector cols cigs))
                          [:yellow :kools])]
              ;; Milk is drunk in the middle house.
              [#{bev3} (= bev3 :milk)] 
              ;; The Norwegian lives in the first house.
              [#{nat1}  (= :norwegian nat1)]
              ;; The Lucky Strike smoker drinks orange juice.
              [#{bevs cigs}
               (contains? (set (map vector cigs bevs))
                          [:lucky-strikes :juice])]
              ;; The Japanese smokes Parliaments.
              [#{nats cigs}
               (contains? (set (map vector cigs nats))
                          [:parliaments :japanese])]
              ;; The Norwegian lives next to the blue house.
              [#{nats cols}
               (let [i (.indexOf nats :norwegian)
                     j (.indexOf cols :blue)]
                 (or (= i (+ j 1))
                     (= i (- j 1))))]
              ;; Kools are smoked in the house next to the house where the horse is kept.
              ;; The man who smokes Chesterfields lives in the house next to the man with the fox.
              [#{cigs pets}
               (let [i (.indexOf pets :horse)
                     j (.indexOf cigs :kools)
                     n (.indexOf pets :fox)
                     m (.indexOf cigs :chesterfields)]
                 (and (or (= i (+ j 1))
                          (= i (- j 1)))
                      (or (= n (+ m 1))
                          (= n (- m 1)))))]
              ;; The green house is immediately to the right of the ivory house.
              [#{cols}
               (let [i (.indexOf cols :green)
                     j (.indexOf cols :ivory)]
                 (= i (+ j 1)))]]
             (map (fn [[n c b p ci]]
                    {:nationality n :color c
                     :beverage b :pet p
                     :cigarettes ci}) 
                  (map vector nats cols bevs pets cigs)))))

(deftest einstein-puzzle-test
  (let [correct '({:nationality :norwegian,
                  :color :yellow,
                  :beverage :water,
                  :pet :fox,
                  :cigarettes :kools}
                 {:nationality :ukranian,
                  :color :blue,
                  :beverage :tea,
                  :pet :horse,
                  :cigarettes :chesterfields}
                 {:nationality :english,
                  :color :red,
                  :beverage :milk,
                  :pet :snail,
                  :cigarettes :old-golds}
                 {:nationality :spanish,
                  :color :ivory,
                  :beverage :juice,
                  :pet :dog,
                  :cigarettes :lucky-strikes}
                 {:nationality :japanese,
                  :color :green,
                  :beverage :coffee,
                  :pet :zebra,
                  :cigarettes :parliaments})
        found einstein-puzzle]
    (is (= correct (first found)))))

(deftest infinite-domains-test
  (testing "can make progress even when the set of possible assignments is infinite"
    (is (= [7] (first (amb-let [a (amb (iterate inc 5))] [[#{a} (= 3 (rem a 4))]] [a]))))))



