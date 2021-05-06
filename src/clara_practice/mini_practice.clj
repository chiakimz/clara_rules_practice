(ns clara-practice.mini-practice
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]))

(defrecord BusinessOwner [name is-dormant through-broker])

(defn make-business-owner [person]
  (let [default-values {:is-dormant false :through-broker false}]
    (map->BusinessOwner (merge default-values person))))

(defrecord NotEligible [name])

(defrecord Documents [name doc])

(defrecord SubmittedAllDocuments [name amount])

(defrecord Eligible [name])

(defquery not-eligible-query?
  []
  [?not-eligible <- NotEligible])

(defquery eligible?
  []
  [?eligible <- Eligible])

(defquery submitted-documents?
  []
  [?x <- SubmittedAllDocuments])

(defrule dormant-companies-are-not-eligible
  [BusinessOwner (= ?name name) (= is-dormant true)]
  =>
  (insert! (->NotEligible ?name)))

(defrule initial-details-provided
  "Here it's assumed that if someone comes through a broker,
  their initial details are automatically provided"
  [BusinessOwner (= ?name name) (= through-broker true)]
  =>
  (insert-all! [(->Documents ?name :turnover)
                (->Documents ?name :bank-statement)
                (->Documents ?name :valid-loan-amount-requested)]))

(defrule need-to-submit-four-documents
  [BusinessOwner (= ?name name)]
  [?c <- (acc/count) from (Documents (= ?name name))]
  [:test (> ?c 3)]
  =>
  (insert! (->SubmittedAllDocuments ?name ?c)))


(defrule completed-documents-grants-eligibility
  "Becomes eligible if you have submitted all documents"
  [SubmittedAllDocuments (= ?name name)]
  [:not [NotEligible (= ?name name)]]
  =>
  (insert! (->Eligible ?name)))
