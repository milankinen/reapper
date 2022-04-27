(ns reapper.hook
  (:require [shadow.build.data :refer [get-source-code]]
            [clojure.string :as string]))

(defn- intercepted-resource [rc]
  (println "INTERCEPT" (:ns rc))
  (assoc rc :source-fn (fn [state]
                         (println "GET SOURCE" (:ns rc))
                         (get-source-code state rc))))

(defn hook
  {:shadow.build/stage :compile-prepare}
  [{:keys [sources] :as build-state} prefix]
  (.printStackTrace (Exception.))
  (let [prefix-s (str prefix)
        sources' (->> (for [[k rc] sources]
                        (if (and (= (:type rc) :cljs)
                                 (string/starts-with? (str (:ns rc)) prefix-s))
                          [k (intercepted-resource rc)]
                          [k rc]))
                      (into {}))]
    (assoc build-state :sources sources')))