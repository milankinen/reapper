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

(defn- wrap-eff [eff]
  (fn effect-wrapper []
    (let [cancel (eff)]
      (when (fn? cancel)
        cancel))))

(def ^:private deps-interns
  (js/Map.))

(defn- intern-dependency [x]
  (let [s (str x)]
    (or (.get deps-interns s)
        (do (.set deps-interns s x)
            x))))

(defn- ->js-deps-array [deps]
  (when deps
    (let [js-deps #js []]
      (doseq [x deps]
        (->> (cond
               (keyword? x) (intern-dependency x)
               (symbol? x) (intern-dependency x)
               (uuid? x) (pr-str x)
               :else x)
             (.push js-deps)))
      js-deps)))

(defn- context? [x]
  (and (some? x)
       (some? (unchecked-get x "Provider"))))

(defn- debug-value-formatter [x]
  (if-not (primitive? x)
    (pr-str x)
    x))

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

(defn create-context
  "Wrapper for React [`createContext`](https://reactjs.org/docs/context.html#reactcreatecontext).
   For more details, see the official
   [React docs](https://reactjs.org/docs/context.html)."
  ([] (create-context nil))
  ([default-value]
   (reactjs/createContext default-value)))

(defn use-state
  "Wrapper for React [`useState`](https://reactjs.org/docs/hooks-reference.html#usestate) hook.
   Uses ClojureScript's value equality for state change detection and
   re-rendering. Otherwise behaves like native JavaScript version.

   ```clojure
   ;; Basic usage
   (let [[state set-state] (use-state 0)]
     ;; Accepts values
     (set-state 1)
     ;; Accepts functions
     (set-state inc)
     ...)

   ;; Lazy state initalization
   (let [[state set-state] (use-state (fn [] (get-expensive-state)))]
     ...)

   ;; Clojure value equality
   (let [[state set-state] (use-state {:foo 12})
     (set-state {:foo 12}) ;; wont trigger state change
     ...)
   ```

   For more details, see the official [React docs](https://reactjs.org/docs/hooks-state.html)."
  [initial]
  (let [xs (reactjs/useState initial)
        state (aget xs 0)
        js-set-state (aget xs 1)
        set-state (or (unchecked-get js-set-state wrapper-key)
                      (let [f (fn [next]
                                (js-set-state
                                  (fn [v']
                                    (let [v (if (fn? next) (next v') next)]
                                      (if (= v v') v' v)))))]
                        (unchecked-set js-set-state wrapper-key f)
                        f))]
    [state set-state]))

(defn use-reducer
  "Wrapper for React [`useReducer`](https://reactjs.org/docs/hooks-reference.html#usereducer) hook.
   Like [[use-state]], uses ClojureScript's value equality for state
   change detection and re-rendering. Otherwise behaves like native
   JavaScript version.

   ```clojure
   (defn reducer [count {:keys [type]}]
     (case type
       :inc (inc count)
       :dec (dec count)
       :reset 0))

   ;; Basic usage
   (let [[count dispatch] (use-reducer reducer 0)]
     (dispatch {:type :inc})
     ...)

   ;; Lazy state initalization
   (let [[count dispatch] (use-reducer reducer 0 (fn [n] (get-expensive-counter-initial n)))]
     ...)
   ```

   For more details, see the official [React docs](https://reactjs.org/docs/hooks-state.html)."
  ([reducer initial-arg]
   (use-reducer reducer initial-arg identity))
  ([reducer initial-arg init]
   {:pre [(fn? reducer)
          (fn? init)]}
   (let [cljs-reducer (reactjs/useCallback
                        (fn [state action]
                          (let [state' (reducer state action)]
                            (if (not= state' state)
                              state'
                              state)))
                        #js [reducer])
         xs (reactjs/useReducer cljs-reducer initial-arg init)
         state (aget xs 0)
         dispatch (aget xs 1)]
     [state dispatch])))

(defn use-ref
  "Wrapper for React [`useRef`](https://reactjs.org/docs/hooks-reference.html#useref) hook.
   Returns ClojureScript atom instead of plain mutable JS object. All
   Clojure's atom operations (like `reset!`, `swap!` and `add-watch`)
   are supported. Current value can be obtained by dereferencing the atom.
   The atom can also be used directly as `:ref` in intrinsic elements.

   ```clojure
   (let [input (use-ref nil)
         _     (use-effect (fn [] (js/console.log \"Input is:\" @node) [])]
     [input {:ref input
             ...}])
   ```"
  [initial]
  (let [js-ref (reactjs/useRef nil)]
    (or (unchecked-get js-ref "current")
        (let [a (atom initial)]
          (assert (identical? js/undefined (.-current a)))
          ;; For React ref interrop
          (js/Object.defineProperty
            a
            "current"
            #js {:get (fn [] @a)
                 :set (fn [val] (reset! a val))}
            a)))))

(defn use-effect
  "Wrapper for React [`useEffect`](https://reactjs.org/docs/hooks-reference.html#useeffect) hook.
   Like the JS version, uses reference equality for dependency change
   detection, treating the following types as 'value' types in addition
   to the JS primitives:

     * Keywords
     * Symbols
     * uuids

  Effect canceling function **must** satisfy `fn?` (in other words, it
  must be a function instead of `IFn` such as map or keyword). This
  enforces `use-effect` caller to explicitly define the cancellation
  and mitigates the risk of accidentally setting up the cancellation
  due to the implicit return value of the effect function.

  ```clojure
  (let [[status set-status] (use-state :initial)
         _ (use-effect
             (fn []
               (let [title (case status
                             :initial \"Initial\"
                             :loading \"Loading..\"
                             \"...\")
                 (set! js/document -title title)))
             [status])
     ...)
   ```

   For more details, see the official [React docs](https://reactjs.org/docs/hooks-effect.html).

   **Tip:** If you need \"deep\" comparison for other ClojureScript
   data types such as collections, use can use Clojure's `hash` function
   as a dependency. However, use this with caution because it may cause
   some unwanted performance issues.

   ```clojure
   (let [[nums set-nums] (use-state [1 2 3])
         _ (use-effect (fn [] ...) [(hash nums)])]
     ...)
   ```"
  [eff deps]
  {:pre [(fn? eff)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useEffect (wrap-eff eff) (->js-deps-array deps)))

(defn use-layout-effect
  "Wrapper for React [`useLayoutEffect`](https://reactjs.org/docs/hooks-reference.html#uselayouteffect) hook.
   Like the JS version, uses reference equality for dependency change
   detection, treating the following types as 'value' types in addition
   to the JS primitives:

     * Keywords
     * Symbols
     * uuids

  Effect canceling function **must** satisfy `fn?` (in other words, it
  must be a function instead of `IFn` such as map or keyword). This
  enforces `use-effect` caller to explicitly define the cancellation
  and mitigates the risk of accidentally setting up the cancellation
  due to the implicit return value of the effect function.

  ```clojure
  (let [[status set-status] (use-state :initial)
         _ (use-layout-effect
             (fn []
               (let [title (case status
                             :initial \"Initial\"
                             :loading \"Loading..\"
                             \"...\")
                 (set! js/document -title title)))
             [status])
     ...)
   ```

   For more details, see the official [React docs](https://reactjs.org/docs/hooks-effect.html).

   **Tip:** If you need \"deep\" comparison for other ClojureScript
   data types such as collections, use can use Clojure's `hash` function
   as a dependency. However, use this with caution because it may cause
   some unwanted performance issues.

   ```clojure
   (let [[nums set-nums] (use-state [1 2 3])
         _ (use-layout-effect (fn [] ...) [(hash nums)])]
     ...)
   ```"
  [eff deps]
  {:pre [(fn? eff)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useLayoutEffect (wrap-eff eff) (->js-deps-array deps)))

(defn use-memo
  "Wrapper for React [`useMemo`](https://reactjs.org/docs/hooks-reference.html#usememo) hook.
   Like the JS version, uses reference equality for dependency change
   detection, treating the following types as 'value' types in addition
   to the JS primitives:

     * Keywords
     * Symbols
     * uuids
  "
  [f deps]
  {:pre [(fn? f)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useMemo f (->js-deps-array deps)))

(defn use-callback
  "Wrapper for React [`useCallback`](https://reactjs.org/docs/hooks-reference.html#usecallback) hook.
   Like the JS version, uses reference equality for dependency change
   detection, treating the following types as 'value' types in addition
   to the JS primitives:

     * Keywords
     * Symbols
     * uuids
  "
  [cb deps]
  {:pre [(fn? cb)
         (or (vector? deps)
             (nil? deps))]}
  (reactjs/useCallback cb (->js-deps-array deps)))

(defn use-context
  "Wrapper for React [`useContext`](https://reactjs.org/docs/hooks-reference.html#usecontext) hook.
   The given context object must be obtained either from `React.createContext`
   or Reapper's [[create-context]].
  "
  [context]
  {:pre [(context? context)]}
  (reactjs/useContext context))

(defn use-debug-value
  "Wrapper for React [`useDebugValue`](https://reactjs.org/docs/hooks-reference.html#usedebugvalue) hook.
   Formats non-primitive values with `pr-str` but this behaviour
   can be changed by providing a custom formatter as a second
   parameter to the hook."
  ([value] (reactjs/useDebugValue value debug-value-formatter))
  ([value formatter]
   {:pre [(fn? formatter)]}
   (reactjs/useDebugValue value formatter)))
