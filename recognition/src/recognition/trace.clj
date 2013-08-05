(ns recognition.trace)

(def ^:dynamic *handler* nil)

(defn call-handler [scope type]
  (when-not (nil? *handler*)
    (*handler* {:scope scope
                :type type})))

(defmacro with-scope [scope & body]
  `(do
     (call-handler ~scope :begin)
     (let [res# (do ~@body)]
       (call-handler ~scope :end)
       res#)))

(defmacro with-handler [handler & body]
  `(binding [*handler* ~handler]
     ~@body))

(defn nesting-time-logger []
  (let [level (atom [])
        start-time (atom {})]
    (fn [{:keys [scope type]}]
      (if (= type :begin)
        (do (println (apply str @level) (name type) (name scope))
            (swap! start-time assoc scope (System/currentTimeMillis))
            (swap! level conj \tab))
        (do (swap! level rest)
            (println (apply str @level) (name type) (name scope)
                     (- (System/currentTimeMillis) (@start-time scope)))
            (swap! start-time dissoc scope))))))
