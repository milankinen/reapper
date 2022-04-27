(ns reapper.dev.main
  (:require [reapper.refresh :as refresh]
            [reapper.react :as react]))

(defn count-reducer [count {:keys [type]}]
  (case type
    :inc (inc count)
    :dec (dec count)
    :reset 0
    count))

(defn counter []
  (let [[count dispatch] (react/use-reducer count-reducer 0)]
    [:div
     [:h2 "Count is: " count]
     [:div
      [:button {:on-click #(dispatch {:type :inc})}
       "Increment"]
      [:button {:on-click #(dispatch {:type :dec})}
       "Decrement"]
      [:button {:on-click #(dispatch {:type :reset})}
       "Reset"]]]))

(defn app []
  [:<>
   [:div
    [:h1 "Tsers!!!"]
    ^:memo
    [counter]]])

(defn -main []
  (refresh/enable! 'reapper.dev)
  (let [el$ (js/document.getElementById "app")]
    (react/render [app] el$)))

(defn- ^:dev/before-load before-load []
  (refresh/before-load))

(defn- ^:dev/after-load after-load []
  (refresh/after-load))
