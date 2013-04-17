(ns toychest.amb-test
  (:use clojure.test toychest.amb)
  (:require [clojure.set :as set]))

(deftest pytagorean-triple-test
  (testing "simple, multi-requirement problem"
           (is (= '([3 4 5])        
                  (amb-let [a (amb (range 1 10))
                           :where (> a 2)
                           b (amb (range a 10))
                           c (amb (range b 10))
                           :where (= (+ (* a a)
                                        (* b b))
                                     (* c c))]
                          [a b c])))))

(deftest destructure-amb-test
  (testing "we can deal with destructured vars"
    (is (= '([1 2]) 
           (amb-let [[a b] (amb [[1 2] [3 4]])
                    :where (= 3 (+ a b))]
                    [a b])))))

(deftest consistent-test
  (testing  "we can handle cross-cutting consistency checks"
    (is (= [[3 2 1] [1 3 2] [2 1 3]] 
           (first (amb-let [full-set #{ 1 2 3}                           
                           [a1 a2 a3] (amb (permutations full-set))
                           :where  (= (set [ a1 a2 a3]) full-set)
                           [b1 b2 b3] (amb (permutations full-set))
                           :where  (= (set [ b1 b2 b3]) full-set)
                           [c1 c2 c3] (amb (permutations full-set))
                           :where (and (= (set [ c1 c2 c3]) full-set)
                                       (= (set [ a1 b1 c1])  #{1 2 3})
                                       (= (set [ a2 b2 c2])  #{1 2 3})
                                       (= (set [ a3 b3 c3])  #{1 2 3}))]
                           [[a1 a2 a3]
                            [b1 b2 b3]
                            [c1 c2 c3]]))))))

(deftest multiple-dwelling-test
  (is (= "baker 3 cooper 2 fletcher 4 miller 5 smith 1" 
         (first (amb-let [baker (amb [1 2 3 4 5])
                         :where (not (= baker 5))
                         cooper (amb [1 2 3 4 5])
                         :where (not (= cooper 1))
                         fletcher (amb [1 2 3 4 5])
                         :where (and (not (= fletcher 5))
                                     (not (= fletcher 1)))
                          miller (amb [1 2 3 4 5])
                         :where (> miller cooper)
                          smith (amb [1 2 3 4 5])
                         :where (and (distinct?  baker cooper fletcher miller smith)
                                     (not (= (Math/abs (- smith fletcher)) 1))
                                     (not (= (Math/abs (- fletcher cooper)) 1)))]
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
                              (amb (permutations #{1 2 3 4 5 6 7 8}))
                              :where (every? identity (for [i (range 0 8)
                                                     j (range (inc i) 8)] ;; only check to the right?
                                                 (not (or (= (- j i) (- (qs j) (qs i)))
                                                          (= (- i j) (- (qs j) (qs i)))))))]
                             [qs])))))
(def einstein-puzzle
  (let [nationalities  #{:english :spanish :ukranian :japanese :norwegian}
        color #{:red :green :ivory :yellow :blue}
        beverages   #{:coffee :tea :milk :juice :water}
        animals  #{:dog :snail :zebra :fox :horse}
        cigarettes  #{:old-golds :kools :chesterfields :lucky-strikes :parliaments}] 
    (amb-let [[nat1 nat2 nat3 nat4 nat5 :as nats] (amb (permutations nationalities))
             ;; The Norwegian lives in the first house.
             :where  (= :norwegian nat1)
             [col1 col2 col3 col4 col5 :as cols] (amb (permutations color))
             ;; The Englishman lives in the red house.
             :where (and (contains? (set (map vector nats cols)) [:english :red])
                         ;; The green house is immediately to the right of the ivory house.
                         (let [i (.indexOf cols :green)
                               j (.indexOf cols :ivory)]
                           (= i (+ j 1)))
                         ;; The Norwegian lives next to the blue house.
                         (let [i (.indexOf nats :norwegian)
                               j (.indexOf cols :blue)]
                           (or (= i (+ j 1)) (= i (- j 1)))))
              [bev1 bev2 bev3 bev4 bev5 :as bevs] (amb (permutations beverages))
              :where (and (= bev3 :milk) ;; Milk is drunk in the middle house.
                          ;; Coffee is drunk in the green house.
                          (contains? (set (map vector bevs cols)) [:coffee :green])
                          ;; The Ukrainian drinks tea.
                          (contains? (set  (map vector bevs nats)) [:tea :ukranian])
                           )
              [pet1 pet2 pet3 pet4 pet5 :as pets] (amb (permutations animals))
              ;; The Spaniard owns the dog.
              :where (contains? (set (map vector nats pets)) [:spanish :dog])
              [cig1 cig2 cig3 cig4 cig5 :as cigs] (amb (permutations cigarettes))
              :where (and             ;; The Old Gold smoker owns snails.
                      (contains? (set (map vector cigs pets)) [:old-golds :snail])
                      ;; Kools are smoked in the yellow house.
                      (contains? (set (map vector cols cigs)) [:yellow :kools])
                      ;; The Lucky Strike smoker drinks orange juice.
                      (contains? (set (map vector cigs bevs)) [:lucky-strikes :juice])
                      ;; The Japanese smokes Parliaments.
                      (contains? (set (map vector cigs nats)) [:parliaments :japanese])
                      ;; Kools are smoked in the house next to the house where the horse is kept.
                      ;; The man who smokes Chesterfields lives in the house next to the man with the fox.
                      (let [i (.indexOf pets :horse) j (.indexOf cigs :kools)
                            n (.indexOf pets :fox) m (.indexOf cigs :chesterfields)]
                        (and (or (= i (+ j 1)) (= i (- j 1)))
                             (or (= n (+ m 1)) (= n (- m 1)))))) 
              ]
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
    (is (= 7 (first (amb-let [a (amb (iterate inc 5))
                               :where (= 3 (rem a 4))] 
                              a))))))



