(ns reapper.refresh
  (:require [clojure.string :as string]
            [reapper.react :as react]
            ["react-refresh/runtime" :as refresh-runtime]))

(defonce ^:private proxies
  (js/Map.))

(defonce ^:private stable-ids
  (js/Map.))

(defonce ^:private registrations
  (js/Map.))

(defonce ^:private dependencies
  (js/Map.))

(defonce ^:private id->type
  (js/Map.))

(defonce ^:private type->ids
  (js/Map.))

(def ^:private ^:dynamic *hooks-tracking* nil)
(def ^:private ^:dynamic *deps-tracking* nil)

(defn- mark-change! [ns-obj full-id type]
  #_(refresh-runtime/register type full-id))

(defn- track-usage! [ns-name prop-name]
  (when-some [t *deps-tracking*]
    (.add t (str ns-name "." prop-name))))

(defn- mark-dependency! [dependency depdendent]
  (if-let [deps (.get dependencies dependency)]
    (.add deps depdendent)
    (let [deps (js/Set.)]
      (.add deps depdendent)
      (.set dependencies dependency deps))))

(def ^:private ID (js/Symbol "reapper$$stable_id"))

(defn- make-ns-proxy [ns-name ns-obj]
  (let [handler #js {:set (fn [obj prop value]
                            (when (and (fn? value)
                                       (string? prop))
                              (let [full-id (str ns-name "." prop)]
                                ;; Remove old type -> id mapping
                                (when-let [prev-type (.get id->type full-id)]
                                  (when-let [ids (.get type->ids prev-type)]
                                    (.delete ids full-id)
                                    (when (zero? (.-size ids))
                                      (.delete type->ids prev-type))))
                                ;; Add new id -> type and type -> id mappings
                                (.set id->type full-id value)
                                (when-not (.get type->ids value)
                                  (.set type->ids value (js/Set.)))
                                (.add (.get type->ids value) full-id)))
                            ;; Add new type to ns
                            (unchecked-set obj prop value))
                     :get (fn [obj prop]
                            (track-usage! ns-name prop)
                            (unchecked-get obj prop))}]
    (js/Proxy. ns-obj handler)))

(defn- add-proxy [ns-name]
  (loop [parent js/goog.global
         [x & xs] (string/split ns-name #"\.")]
    (if (seq xs)
      (recur (unchecked-get parent x) xs)
      (let [ns-obj (unchecked-get parent x)
            proxy (make-ns-proxy ns-name ns-obj)]
        (.set proxies ns-name {:proxy proxy :ns-obj ns-obj})
        (unchecked-set parent x (make-ns-proxy ns-name ns-obj))))))

(defn- remove-proxy [ns-name]
  (when-let [{:keys [ns-obj]} (.get proxies ns-name)]
    (.delete proxies ns-name)
    (loop [parent js/goog.global
           [x & xs] (string/split ns-name #"\.")]
      (if (seq xs)
        (recur (unchecked-get parent x) xs)
        (unchecked-set parent x ns-obj)))))

(defn- intercept-provide! []
  (let [gprovide js/goog.provide]
    (when-not (unchecked-get gprovide "__reapper__")
      (let [provide (fn refresh-enabled-provide [ns-name]
                      (gprovide ns-name)
                      (add-proxy ns-name))]
        (unchecked-set provide "__reapper__" true)
        (unchecked-set js/goog "provide" provide)))))

(def ^:private -use-state react/use-state)
(def ^:private -use-reducer react/use-reducer)
(def ^:private -use-effect react/use-effect)
(def ^:private -use-layout-effect react/use-layout-effect)
(def ^:private -use-callback react/use-callback)
(def ^:private -use-memo react/use-memo)
(def ^:private -use-context react/use-context)
(def ^:private -use-debug-value react/use-debug-value)

(defn- track-hook! [hook-name deps]
  (when-some [t *hooks-tracking*]
    (doto t
      (.push hook-name)
      (.push (if deps (count deps) "-")))))

(defn- use-state [initial]
  (track-hook! "use-state" nil)
  (-use-state initial))

(defn- use-reducer
  ([reducer initial-arg init]
   (track-hook! "use-reducer" nil)
   (-use-reducer reducer initial-arg init))
  ([reducer initial-arg]
   (track-hook! "use-reducer" nil)
   (-use-reducer reducer initial-arg)))

(defn- use-effect [eff deps]
  (track-hook! "use-effect" deps)
  (-use-effect eff deps))

(defn- use-layout-effect [eff deps]
  (track-hook! "use-layout-effect" deps)
  (-use-layout-effect eff deps))

(defn- use-callback [cb deps]
  (track-hook! "use-callback" deps)
  (-use-callback cb deps))

(defn- use-memo [f deps]
  (track-hook! "use-memo" deps)
  (-use-memo f deps))

(defn- use-context [context]
  (track-hook! "use-context" nil)
  (-use-context context))

(defn- use-debug-value
  ([value]
   (track-hook! "use-debug-value" nil)
   (-use-debug-value value))
  ([value formatter]
   (track-hook! "use-debug-value" nil)
   (-use-debug-value value formatter)))

(defn- render-and-update-registration-props [cljs-component props registration]
  (let [result (cljs-component props)]
    (unchecked-set registration "latestProps" props)
    result))


(defn- render-and-register [native-component cljs-component props]
  (if-some [ids (.get type->ids cljs-component)]
    (binding [*hooks-tracking* #js []]
      (with-redefs [react/use-state use-state
                    react/use-reducer use-reducer
                    react/use-effect use-effect
                    react/use-layout-effect use-layout-effect
                    react/use-callback use-callback
                    react/use-memo use-memo
                    react/use-context use-context
                    react/use-debug-value use-debug-value]
        (doseq [id ids]
          (refresh-runtime/register native-component id))
        (let [result (cljs-component props)
              signature-key (str (.-name cljs-component) "$$" (.join *hooks-tracking* ";"))
              registration #js {:nativeComponent native-component
                                :cljsComponent   cljs-component
                                :signatureKey    signature-key
                                :latestProps     props}]
          (.set registrations native-component registration)
          (refresh-runtime/setSignature native-component signature-key false js/undefined)
          result)))
    (cljs-component props)))

(defn- refresh-renderer [cljs-component props wrapper-key]
  (let [native-component (unchecked-get cljs-component wrapper-key)]
    (if-let [registration (.get registrations native-component)]
      (render-and-update-registration-props cljs-component props registration)
      (render-and-register native-component cljs-component props))))

(defn enable! [pkg]
  {:pre [(simple-symbol? pkg)]}
  (when (and goog/DEBUG (not (unchecked-get js/window "__reapper_refresh_renderer__")))
    (unchecked-set js/window "__reapper_refresh_renderer__" refresh-renderer)
    (intercept-provide!)
    (doseq [ns-name (.keys proxies)
            :when (not (string/starts-with? ns-name (name pkg)))]
      (remove-proxy ns-name))))

(defn before-load [])

(defn after-load [])

(defonce ^:private __
  (when goog/DEBUG
    (refresh-runtime/injectIntoGlobalHook js/window)
    (intercept-provide!)))
