(ns  clj-arsenal.log
  #?(:cljs (:require-macros clj-arsenal.log))
  (:require
   #?(:clj [clojure.stacktrace :as stacktrace])))

(def ^:private devtools? (boolean (find-ns 'devtools.formatters.core)))

(defn default-logger
  [{:keys [level msg ex file line] :as event}]
  (let [data (dissoc event :level :msg :ex :file :line)]
    #?(:cljs
       (let [log-fn (case level
                      :error (.bind js/console.error js/console)
                      :warn (.bind js/console.warn js/console)
                      :info (.bind js/console.info js/console)
                      :debug (.bind js/console.debug js/console))]
         (apply log-fn
           (cond-> [msg]
             (some? data)
             (conj (cond-> data (and file line) (conj [:loc (str file ":" line)])))

             (some? ex)
             (into ["\n" ex])

             (not devtools?)
             (->> (map pr-str)))))
        :clj
        (binding [*out* *err*]
          (printf "%s %s%n"
            (case level
              :error "ERROR"
              :warn "WARN"
              :info "INFO"
              :debug "DEBUG")
            (str msg))
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
  `(log* ~(merge data (-> &form meta (select-keys [:file :line])) {:ns `(quote ~(ns-name *ns*)) :level level})))

(defmacro spy
  ([x]
   (let [value-sym (gensym)]
     `(let [~value-sym ~x]
        (log*
          ~(merge
             {:spy `'~x :level :debug :msg ~value-sym :ns `(quote ~(ns-name *ns*))}
             (-> &form meta (select-keys [:file :line]))))
        ~value-sym)))
  ([spy-name x]
   (let [value-sym (gensym)]
     `(let [~value-sym ~x]
        (log*
          ~(merge
             {:spy spy-name :level :debug :msg ~value-sym :ns `(quote ~(ns-name *ns*))}
             (-> &form meta (select-keys [:file :line]))))
        ~value-sym))))
