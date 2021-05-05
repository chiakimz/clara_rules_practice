(ns clara-practice.mini-practice
  (:require [clara.rules :refer :all]
            [clara.rules.accumulators :as acc]))

(defrecord BusinessOwner [name is-dormant through-broker])

(defn make-businessowner [person]
  (let [default-values {:is-dormant false :through-broker false}]
    (map->BusinessOwner (merge default-values person))))

(defrecord NotEligible [name])

(defrecord Documents [name doc])

(defrecord SubmittedDocuments [name amount])

(defrecord Eligible [name])



