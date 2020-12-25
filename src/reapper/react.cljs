(ns reapper.react
  (:require [clojure.string :as string]
            ["react" :as reactjs]
            ["react/jsx-runtime" :refer [jsxs Fragment]]
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

(defn- parse-string-tag [tag-s]
  (or (.get tag-cache tag-s)
      (let [[tag id class] (->> tag-s (name) (re-matches re-tag) (next))
            _ (assert tag (str "Invalid tag: '" tag-s "'"))
            classes (some-> class (string/replace #"\." " "))
            result #js {:tag tag :id id :classes classes}]
        (.set tag-cache tag-s result)
        result)))

(defn- parse-keyword-tag [hiccup-tag]
  (parse-string-tag (name hiccup-tag)))

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

(defn- keyword->jsx-prop-name [k]
  (let [prop-s (name k)]
    (or (.get js-prop-cache prop-s)
        (let [res (camelize-prop-key prop-s)]
          (.set js-prop-cache prop-s res)
          res))))

(defn- ->jsx-prop-key [k]
  (cond
    (keyword? k) (keyword->jsx-prop-name k)
    (string? k) k
    :else (throw (js/Error. (str "Invalid intrinsic property key" (pr-str k))))))

(defn- ->jsx-prop-value [x]
  (cond
    (or (primitive? x)
        (fn? x)) x
    (keyword? x) (fq-name x)
    (symbol? x) (fq-name x)
    (map? x) (let [val #js {}]
               (doseq [[k v] x]
                 (unchecked-set val (->jsx-prop-key k) (->jsx-prop-value v)))
               val)
    (coll? x) (let [val #js []]
                (doseq [v x]
                  (.push val (->jsx-prop-value v)))
                val)
    (ifn? x) (fn [& args] (apply x args))
    (satisfies? IPrintWithWriter key) (pr-str key)
    :else x))

(defn- ->jsx-class-name [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (symbol? x) (name x)
    (map? x) (->> (keep (fn [[k v]] (when v (->jsx-class-name k))) x)
                  (string/join " "))
    (coll? x) (->> (map ->jsx-class-name x)
                   (string/join " "))
    :else (pr-str x)))

(defn- ->jsx-props [props children]
  (let [jsx-props #js {}]
    (when (some? props)
      (doseq [[k v] props]
        (case k
          (:key :children) nil
          :class (unchecked-set jsx-props "className" (->jsx-class-name v))
          (unchecked-set jsx-props (->jsx-prop-key k) (->jsx-prop-value v)))))
    (when (seq children)
      (let [js-children #js []]
        (doseq [child children]
          (.push js-children (as-element child)))
        (unchecked-set jsx-props "children" js-children)))
    jsx-props))

(defn- unwrap-delegated-children [children]
  (when (seq children)
    (if (and (empty? (next children))
             (::children (meta (first children))))
      (first children)
      children)))

(defn- create-fragment [props children]
  (let [jsx-props (->jsx-props props children)]
    (if-some [key (:key props)]
      (jsxs Fragment jsx-props (->jsx-prop-value key))
      (jsxs Fragment jsx-props))))

(defn- create-intrinsic-element [parsed-tag props children]
  (let [jsx-props (->jsx-props props children)
        tag-name (unchecked-get parsed-tag "tag")
        id (unchecked-get parsed-tag "id")
        classes (unchecked-get parsed-tag "classes")]
    (when (some? id)
      (assert (nil? (unchecked-get jsx-props "id")) (str "Id defined twice for tag " tag-name))
      (unchecked-set jsx-props "id" id))
    (when (some? classes)
      (if-let [class-names-from-props (unchecked-get jsx-props "className")]
        (unchecked-set jsx-props "className" (str class-names-from-props " " classes))
        (unchecked-set jsx-props "className" classes)))
    (if-some [key (:key props)]
      (jsxs tag-name jsx-props (->jsx-prop-value key))
      (jsxs tag-name jsx-props))))

(defn- create-native-component-element [type props children]
  (let [jsx-props (->jsx-props props children)]
    (if-some [key (:key props)]
      (jsxs type jsx-props (->jsx-prop-value key))
      (jsxs type jsx-props))))

(def ^:private wrapper-key
  (js/Symbol "reapper$$wrapper"))

(def ^:private memo-wrapper-key
  (js/Symbol "reapper$$memo$$wrapper"))

(defn- memo-eq [prev-js-props next-js-props]
  (and (some? prev-js-props)
       (some? next-js-props)
       (= (unchecked-get prev-js-props "p")
          (unchecked-get next-js-props "p"))
       (= (unchecked-get prev-js-props "c")
          (unchecked-get next-js-props "c"))))

(defn- cljs-component-wrapper [component]
  (let [wrapper (fn cljs-wrapper [js-props]
                  (let [children (some-> (unchecked-get js-props "c")
                                         (vary-meta assoc ::children true))
                        props' (or (unchecked-get js-props "p") {})
                        props (if children
                                (assoc props' :children children)
                                props')
                        result (component props)]
                    (as-element result)))
        display-name (when-let [s (or (.-displayName component)
                                      (.-name component))]
                       (if *assert*
                         (let [parts (string/split s #"\$")
                               ns-parts (pop parts)
                               name-part (peek parts)]
                           (-> (if (seq ns-parts)
                                 (str (string/join "." ns-parts) "/" name-part)
                                 name-part)
                               (string/replace #"_" "-")))
                         s))]
    (unchecked-set wrapper "displayName" display-name)
    wrapper))

(defn- create-cljs-component-element [component props children memo?]
  (let [type (if memo?
               (or (unchecked-get component memo-wrapper-key)
                   (let [wrapper (cljs-component-wrapper component)
                         memo (reactjs/memo wrapper memo-eq)]
                     (unchecked-set component memo-wrapper-key memo)
                     memo))
               (or (unchecked-get component wrapper-key)
                   (let [wrapper (cljs-component-wrapper component)]
                     (unchecked-set component wrapper-key wrapper)
                     wrapper)))]
    (if-some [key (:key props)]
      (jsxs type #js {:p (dissoc props :key) :c children} (->jsx-prop-value key))
      (jsxs type #js {:p props :c children}))))

(defn- hiccup->element [[type & [props & children :as props+children] :as hiccup]]
  (let [props (when (map? props) props)
        children (if props
                   (if (>= (count hiccup) 3)
                     (unwrap-delegated-children children)
                     (:children props))
                   (unwrap-delegated-children props+children))]
    (cond
      (= :<> type) (create-fragment props children)
      (keyword? type) (create-intrinsic-element (parse-keyword-tag type) props children)
      (string? type) (create-intrinsic-element (parse-string-tag type) props children)
      (or (fn? type)
          (ifn? type)) (create-cljs-component-element type props children (:memo (meta hiccup)))
      (some? (.-$$typeof type)) (create-native-component-element type props children)
      :else (throw (js/Error. (str "Invalid hiccup tag type: " (type type)))))))

;;;;;

(defn as-element
  "Converts a ClojureScript hiccup element to a native JavaScript
   React element. Normally you shouldn't need to use this from your
   CLJS codebase directly. Main use case is JS library interrop when
   you need to return JSX from your ClojureScript component(s) to a
   JavaScript React higher order component.

   ```clojure
   ;; TODO example here
   ```"
  [x]
  (cond
    (vector? x) (hiccup->element x)
    (primitive? x) x
    (seq? x) (let [elems #js []]
               (doseq [el x] (.push elems (as-element el)))
               elems)
    (keyword? x) (fq-name x)
    (symbol? x) (fq-name x)
    (satisfies? IPrintWithWriter x) (pr-str x)
    :else x))

(defn create-element
  "Equivalent to `React.createElement` but accepts only ClojureScript
   data structures as props and children. Normally you shouldn't need
   to use this from your CLJS codebase directly. Main use case is JS
   library interrop when you need to render a native JavaScript
   component in your CLJS component.

   ```clojure
   ;; TODO example here
   ```"
  [type props & children]
  {:pre [(map? props)]}
  (let [children (unwrap-delegated-children children)]
    (create-native-component-element type props children)))

(defn render
  "Renders the given hiccup into the given container element. See
   [React's documentation](https://reactjs.org/docs/rendering-elements.html)
   for more details.

   ```clojure
   (ns example
     (:require [reapper.react :refer [render]]))

   (def root
     (js/document.getElementById \"root\"))

   (render [:h1 \"Tsers!\"] root)
   ```"
  [hiccup container]
  {:pre [(instance? js/Node container)]}
  (react-dom/render (as-element hiccup) container))
