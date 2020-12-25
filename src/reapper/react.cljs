(ns reapper.react
  (:require [clojure.string :as string]
            ["react" :as reactjs]
            ["react-dom" :as react-dom]))

;; Cache for parsed hiccup tags and converted js props
(def ^:private tag-cache (js/Map.))
(def ^:private js-prop-cache
  (doto (js/Map.)
    (.set "class" "className")
    (.set "for" "htmlFor")
    (.set "charset" "charSet")))

;; Copied from reagent (originally copied from hiccup)
(def ^:private re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn- parse-tag [hiccup-tag]
  (let [tag-s (if (string? hiccup-tag) hiccup-tag (name hiccup-tag))]
    (or (.get tag-cache tag-s)
        (let [[tag id class] (->> tag-s (name) (re-matches re-tag) (next))
              _ (assert tag (str "Invalid tag: '" tag-s "'"))
              classes (some-> class (string/replace #"\." " "))
              result #js {:tag tag :id id :classes classes}]
          (.set tag-cache tag-s result)
          result))))

(defn- camelize-prop-key [s]
  (if-not (string/starts-with? s "data-")
    ;; JS interrop for perf
    (let [parts (.split s "-")
          n (alength parts)
          buf (js/Array. n)]
      (aset buf 0 (aget parts 0))
      (loop [i 1]
        (when (< i n)
          (as-> (aget parts i) p
                (str (.toUpperCase (.charAt p 0)) (subs p 1))
                (aset buf i p))
          (recur (inc i))))
      (.join buf ""))
    s))

(declare as-element jsfy-props)

(defn- primitive? [x]
  (or (boolean? x)
      (string? x)
      (number? x)
      (nil? x)
      (undefined? x)))

(defn- fq-name [kw]
  (str (if-let [ns (namespace kw)]
         (str ns "/"))
       (name kw)))

(defn- keyword->js-prop-name [k]
  (let [prop-s (name k)]
    (or (.get js-prop-cache prop-s)
        (let [res (camelize-prop-key prop-s)]
          (.set js-prop-cache prop-s res)
          res))))

(defn- jsfy-prop-key [k]
  (cond
    (keyword? k) (keyword->js-prop-name k)
    (string? k) k
    :else (throw (js/Error. (str "Invalid intrinsic property key" (pr-str k))))))

(defn- jsfy-prop-value [x]
  (cond
    (or (primitive? x)
        (fn? x)) x
    (keyword? x) (fq-name x)
    (symbol? x) (fq-name x)
    (map? x) (let [val #js {}]
               (doseq [[k v] x]
                 (unchecked-set val (jsfy-prop-key k) (jsfy-prop-value v)))
               val)
    (coll? x) (let [val #js []]
                (doseq [v x]
                  (.push val (jsfy-prop-value v)))
                val)
    (ifn? x) (fn [& args] (apply x args))
    (satisfies? IPrintWithWriter key) (pr-str key)
    :else x))

(defn- jsfy-class-name [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (symbol? x) (name x)
    (map? x) (->> (keep (fn [[k v]] (when v (jsfy-class-name k))) x)
                  (string/join " "))
    (coll? x) (->> (map jsfy-class-name x)
                   (string/join " "))
    :else (pr-str x)))

(defn- jsfy-element-props [props]
  (if (some? props)
    (let [js-props #js {}]
      (doseq [[k v] props]
        (case k
          :class (unchecked-set js-props "className" (jsfy-class-name v))
          :children nil
          (unchecked-set js-props (jsfy-prop-key k) (jsfy-prop-value v))))
      js-props)
    #js {}))

(defn- $ [type js-props cljs-children]
  (let [args #js [type js-props]]
    (doseq [child cljs-children]
      (.push args (as-element child)))
    (.apply reactjs/createElement nil args)))

(defn- create-fragment [props children]
  ($ reactjs/Fragment (jsfy-element-props props) children))

(defn- create-intrinsic-element [parsed-tag props children]
  (let [js-props (jsfy-element-props props)
        tag-name (unchecked-get parsed-tag "tag")
        id (unchecked-get parsed-tag "id")
        classes (unchecked-get parsed-tag "classes")]
    (when (some? id)
      (assert (nil? (unchecked-get js-props "id")) (str "Id defined twice for tag " tag-name))
      (unchecked-set js-props "id" id))
    (when (some? classes)
      (if-let [class-names-from-props (unchecked-get js-props "className")]
        (unchecked-set js-props "className" (str class-names-from-props " " classes))
        (unchecked-set js-props "className" classes)))
    ($ tag-name js-props children)))

(defn- unwrap-delegated-children [children]
  (when (seq children)
    (if (and (empty? (next children))
             (::children (meta (first children))))
      (first children)
      children)))

(defn- unwrap-props [wrapped-props]
  (let [children (some-> (unchecked-get wrapped-props "c")
                         (vary-meta assoc ::children true))
        props (or (unchecked-get wrapped-props "p") {})]
    (if children
      (assoc props :children children)
      props)))

(defn- wrap-props [props children]
  (if-some [key (:key props)]
    #js {:p props :c children :key (jsfy-prop-value key)}
    #js {:p props :c children}))

(def ^:private wrapper-key (js/Symbol "reapper$$wrapper"))
(def ^:private memo-wrapper-key (js/Symbol "reapper$$memo$$wrapper"))

(defn- display-name [comp]
  (or (.-displayName comp)
      (.-name comp)))

(defn- wrapper-component [component]
  (let [wrapper (fn [react-props]
                  (-> (unwrap-props react-props)
                      (component)
                      (as-element)))]
    (unchecked-set wrapper "displayName" (display-name component))
    wrapper))

(defn- memo-eq [prev-wrapped-props next-wrapped-props]
  (and (some? prev-wrapped-props)
       (some? next-wrapped-props)
       (= (unwrap-props prev-wrapped-props)
          (unwrap-props next-wrapped-props))))

(defn- create-component-element [component props children memo?]
  (let [type (if memo?
               (or (unchecked-get component memo-wrapper-key)
                   (let [wrapper (wrapper-component component)
                         memo (reactjs/memo wrapper memo-eq)]
                     (unchecked-set component memo-wrapper-key memo)
                     memo))
               (or (unchecked-get component wrapper-key)
                   (let [wrapper (wrapper-component component)]
                     (unchecked-set component wrapper-key wrapper)
                     wrapper)))]
    (reactjs/createElement type (wrap-props props children))))

(defn- hiccup->element [[type & [props & children :as props+children] :as hiccup]]
  (let [props (when (map? props) props)
        children (if props
                   (if (>= (count hiccup) 3)
                     (unwrap-delegated-children children)
                     (:children props))
                   (unwrap-delegated-children props+children))]
    (cond
      (= :<> type) (create-fragment props children)
      (or (keyword? type)
          (string? type)) (create-intrinsic-element (parse-tag type) props children)
      (or (fn? type)
          (ifn? type)) (create-component-element type props children (:memo (meta hiccup)))
      (some? (.-$$typeof type)) (create-component-element type props children false)
      :else (throw (js/Error. (str "Invalid hiccup tag type: " (type type)))))))

(defn- array-of-elements [xs]
  (let [elems #js []]
    (doseq [x xs] (.push elems (as-element x)))
    elems))

;;;;;

(defn as-element
  "Converts ClojureScript styled element (e.g. hiccup) to native
   React element. Normally you shouldn't use this from you CLJS
   codebase. Main use case is JavaScript library interoperability.
   ```clojure
   ;; the following lines are equivalent
   (as-element [:button {:disabled true} \"tsers\"])
   (create-element \"button\" #js {:disabled true} \"tsers\")
   ;; conversion is done for entire hiccup
   (def app-el
     (as-element [:div.app
                  [sidebar {:version 123 :title \"tsers\"]
                  [main {}]]))
   ```"
  [x]
  (cond
    (vector? x) (hiccup->element x)
    (primitive? x) x
    (seq? x) (array-of-elements x)
    (keyword? x) (fq-name x)
    (symbol? x) (fq-name x)
    (satisfies? IPrintWithWriter x) (pr-str x)
    :else x))

(def create-element
  "Native React.createElement. Does **not** perform any conversions,
   see [[as-element]] if you need to convert hiccup elements to
   native React elements."
  reactjs/createElement)

(defn render
  "Renders the given hiccup into the given container element"
  [hiccup container]
  {:pre [(instance? js/Node container)]}
  (react-dom/render (as-element hiccup) container))
