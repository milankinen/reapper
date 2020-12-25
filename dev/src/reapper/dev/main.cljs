(ns reapper.dev.main
  (:require [reapper.react :as react]))

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
    [counter]]])

(defn -main []
  (let [el$ (js/document.getElementById "app")]
    (react/render [app] el$)))