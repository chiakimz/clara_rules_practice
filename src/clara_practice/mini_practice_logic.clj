(ns clara-practice.mini-practice-logic)

(def valid-docs
  {:turnover true
   :bank-statement true
   :valid-loan-amount-requested true
   :no-exposure true
   :owner-id-provided true})


(defn valid-docs?
  [doc]
  (doc valid-docs))

(defn docs-checker
  [report]
  (cond (and (= (count (:docs report)) 4) (map (:docs report) valid-docs?))
        (assoc report :status :submitted :eligible true)

        (= (:through-broker report) true)
        (assoc report :docs [:turnover :bank-statement :valid-loan-amount-requested]
                      :status :submitted :eligible true)

        :else (assoc report :status :incomplete :eligible false)))


