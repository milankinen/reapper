(ns reapper.dev.main
  (:require [reapper.react :as react]))

(defn app []
  [:h1 "Tsers!"])

(defn -main []
  (let [el$ (js/document.getElementById "app")]
    (react/render [app] el$)))