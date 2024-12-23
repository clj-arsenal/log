(ns  clj-arsenal.log
  #?(:cljs (:require-macros clj-arsenal.log))
  (:require
   #?@(:cljd []
       :clj [[clojure.stacktrace :as stacktrace]])))

(def ^:private devtools? #?(:cljs (boolean (find-ns 'devtools.formatters.core)) :default false))

(defn default-logger
  [{:keys [level msg ex file line] :as event}]
  (let [data (cond-> event
               true
               (dissoc :level :msg :ex)

               (and file line)
               (->
                 (dissoc :file :line)
                 (assoc :loc (str file ":" line))))]
    #?(:cljs
       (let [log-fn (case level
                      :error (.bind js/console.error js/console)
                      :warn (.bind js/console.warn js/console)
                      :info (.bind js/console.info js/console)
                      :debug (.bind js/console.debug js/console))]
         (apply log-fn
           (cond-> [msg]
             (some? data)
             (conj data)

             (some? ex)
             (into ["\n" ex])

             (not devtools?)
             (->> (map pr-str)))))

        :cljd
        (do
          (println
            (case level
              :error "ERROR"
              :warn "WARN"
              :info "INFO"
              :debug "DEBUG")
            (cond-> msg (not (string? msg)) pr-str))
          (doseq [[k v] (cond-> (or data {}) true (dissoc :st) (and file line) (conj [:loc (str file ":" line)]))]
            (println k v))
          (when ex
            (println ex))
          (when-some [st (or (:st data)
                           (when (instance? Error ex)
                             (.-stackTrace ^Error ex))
                           (some-> (ex-data ex) :st))]
            (println st)))

        :clj
        (binding [*out* *err*]
          (printf "%s %s%n"
            (case level
              :error "ERROR"
              :warn "WARN"
              :info "INFO"
              :debug "DEBUG")
            (cond-> msg (not (string? msg)) pr-str))
          (doseq [[k v] (cond-> (or data {}) (and file line) (conj [:loc (str file ":" line)]))]
            (printf "  %s %s%n" k v))
          (when ex
            (stacktrace/print-stack-trace ex))
          (flush)))))

(defonce !loggers (atom #{default-logger}))

(defn add-logger!
  [logger]
  (swap! !loggers conj logger)
  nil)

(defn remove-logger!
  [logger]
  (swap! !loggers disj logger)
  nil)

(defn log*
  [event]
  (doseq [logger @!loggers]
    (logger event)))

(defmacro log
  [level & {:as data}]
  {:pre [(keyword? level)]}
  `(log*
     ~(merge data
        {#?@(:cljd/clj-host [:file *file*] :cljd [] :clj [:file *file*])
         :line (-> &form meta :line)
         #?@(:cljd/clj-host [:ns `(quote ~(ns-name *ns*))] :cljd [] :clj [:ns `(quote ~(ns-name *ns*))])
         :level level})))

(defmacro spy
  ([x]
   (let [value-sym (gensym)]
     `(let [~value-sym ~x]
        (log*
          ~{#?@(:cljd/clj-host [:file *file*] :cljd [] :clj [:file *file*])
            :line (-> &form meta :line)
            :spy `(quote ~x)
            :level :debug
            :msg value-sym
            #?@(:cljd/clj-host [:ns `(quote ~(ns-name *ns*))] :cljd [] :clj [:ns `(quote ~(ns-name *ns*))])})
        ~value-sym)))
  ([spy-name x]
   (let [value-sym (gensym)]
     `(let [~value-sym ~x]
        (log*
          ~{#?@(:cljd/clj-host [:file *file*] :cljd [] :clj [:file *file*])
            :line (-> &form meta :line)
            :spy `(quote ~spy-name)
            :level :debug
            :msg value-sym
            #?@(:cljd/clj-host [:ns `(quote ~(ns-name *ns*))] :cljd [] :clj [:ns `(quote ~(ns-name *ns*))])})
        ~value-sym))))
