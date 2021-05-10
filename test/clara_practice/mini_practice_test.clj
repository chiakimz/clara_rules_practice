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

(defn eligibles [result]
  (map :?eligible result))

(deftest old-corner-shop-test
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

  (testing "coming through a broker but lacks docs"
    (let [facts (derive-facts-from [{:name "Sigmund Freud" :through-broker true}])
          session (create-session-with facts)
          [{result :?eligible}] (query session eligible?)]
      (is (empty? result))))

  (testing "the business hasn't been active for years"
    (let [facts (derive-facts-from [{:name "Soren Kierkegaard" :is-dormant true :docs [:turnover :bank-statement :valid-loan-amount-requested :no-exposure]}])
          session (create-session-with facts)
          result (query session eligible?)]
      (is (empty? result))))

  (testing "multiple business owners"
    (let [facts (derive-facts-from [{:name "Franz Ferdinand" :through-broker true :docs [:no-exposure]} {:name "Winston Churchill" :docs [:turnover :valid-loan-amount-requested :no-exposure :owner-id-provided]}])
          session (create-session-with facts)
          result (query session eligible?)]
      (is (= (eligibles result) (list (->Eligible "Franz Ferdinand") (->Eligible "Winston Churchill"))))))

  (testing "only submitting 3 docs is not good enough"
    (let [facts (derive-facts-from [{:name "Alexander the Great" :docs [:valid-loan-amount-requested :turnover :owner-id-provided]}])
          session (create-session-with facts)
          result (query session eligible?)]
      (is (empty? result)))))


