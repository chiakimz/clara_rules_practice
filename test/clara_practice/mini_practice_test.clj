(ns clara-practice.mini-practice-test
  (:require [clojure.test :refer :all]
            [clara-practice.mini-practice :refer :all]
            [clara.rules :refer :all]
            [clara.tools.inspect :refer :all]
            [clojure.pprint :refer :all]))


(defn person-facts
  [person]
  (let [business-owner (make-business-owner person)
        docs (map #(->Document (:name person) %)
                  (:docs person))]
    (conj docs business-owner)))

(defn derive-facts-from [people]
  (mapcat person-facts people))

(defn create-session-with
  [facts]
  (let [session (mk-session 'clara-practice.mini-practice) with-facts (reduce insert session facts)]
    (fire-rules with-facts)))

(defn eligible [result]
  (map :?eligible result))

(deftest old-corner-shop-test
  (testing "that a dormant business is automatically not eligible"
    (let [facts (derive-facts-from [{:name "John Doe" :is-dormant true}])
          session (create-session-with facts)
          [{result :?not-eligible}] (query session not-eligible-query?)]
      (is (= result (->NotEligible "John Doe"))))))

