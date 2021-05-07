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
      (is (= result (->NotEligible "John Doe")))))

  (testing "submitting 4 documents makes you eligible"
    (let [facts (derive-facts-from [{:name "Adam Smith" :docs [:valid-loan-amount-requested :no-exposure :owner-id-provided :bank-statement]}])
          session (create-session-with facts)
          [{result :?eligible}] (query session eligible?)]
      (is (= result (->Eligible "Adam Smith")))))

  (testing "coming through a broker"
    (let [facts (derive-facts-from [{:name "Sigmund Freud" :through-broker true :docs [:no-exposure]}])
          session (create-session-with facts)
          [{result :?eligible}] (query session eligible?)]
      (is (= result (->Eligible "Sigmund Freud")))))

  (testing "the business hasn't been active for years"
    (let [facts (derive-facts-from [{:name "Soren Kierkegaard" :is-dormant true :docs [:turnover :bank-statement :valid-loan-amount-requested :no-exposure]}])
          session (create-session-with facts)
          result (query session eligible?)]
      (is (empty? result))))

  (testing "only done 3 chores is not good enough"))


