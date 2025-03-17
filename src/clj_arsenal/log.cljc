(ns  clj-arsenal.log
  #?(:cljs (:require-macros clj-arsenal.log))
  (:require
   [clj-arsenal.basis :as b]
   [clojure.string :as str]
   #?@(:cljd
       [["dart:io" :as io]]

       :clj
       [[clojure.stacktrace :as stacktrace]
        [clojure.pprint :as pp]]
       
       :cljs
       [[clojure.pprint :as pp]])))

(def ^:private devtools? #?(:cljs (boolean (find-ns 'devtools.formatters.core)) :default false))

(defn- stringify
  [x]
  (if (string? x)
    x
    #?(:cljd (str x)
       :default (with-out-str (pp/pprint x)))))

(defn- stringify-log-args
  [args]
  (let [args-vec (vec args)]
    (str/join
      (map-indexed
        (fn [idx arg]
          (if
            (or (zero? idx)
              (and (string? arg) (re-matches #"\s.*" arg))
              (as-> (get args-vec (dec idx)) prev
                (and (string? prev) (re-matches #".*\s" prev))))
            (stringify arg)
            (str " " (stringify arg))))
        args-vec))))

(defn- default-logger-printer
  [level & args]
  (as-> args $
    (mapcat
      (fn flatten-args [x]
        (cond
          (nil? x)
          nil
          
          (seq? x)
          (mapcat flatten-args x)
          
          :else
          (vector x)))
      $)
    #?(:cljs
       (apply
         (case level
           :error (.bind js/console.error js/console)
           :warn (.bind js/console.warn js/console)
           :info (.bind js/console.info js/console)
           :debug (.bind js/console.debug js/console))
         (cond->> $ (not devtools?) (map stringify)))

       :cljd
       (.write io/stderr (stringify-log-args $))

       :clj
       (binding [*out* *err*]
         (apply print (stringify-log-args $))
         (flush))))
  nil)

(defn- default-logger-prefix
  [{:keys [level] :as _event}]
  (str/upper-case (name level)))

(defn- default-logger-src
  [{:keys [file line]}]
  (str file ":" line))

(defn- default-logger-stack-trace
  [st]
  (or
    (when (string? st)
      st)
    #?(:cljd nil
       :clj
       (when (or (sequential? st) (-> st class .isArray))
         (with-out-str
           (doseq [element st]
             (print "  ")
             (if (instance? StackTraceElement element)
               (stacktrace/print-trace-element element)
               (str element "\n"))))))
    (str st)))

(defn- default-logger-err
  [{:keys [msg err ex] :as event}]
  (let
    [err (or err ex)
     data (or (b/err-data err) (ex-data ex))
     cause (or (:cause data) #?(:cljd nil :clj (ex-cause err) :cljs (ex-cause err)))
     msg (or msg (str/join " " (filter some? [(:id data) (:msg data)])))
     skip-keys (set (::skip-keys event))

     extra-data
     (into {}
       (keep
         (fn [[k _ :as entry]]
           (when
             (and (not (contains? skip-keys k))
               (or (not (keyword? k)) (not= "clj-arsenal.log" (namespace k))))
             (case k
               (:id :msg :cause :st :ns :err :ex :line :file :level) nil
               entry))))
       (concat data event))

     st
     (or (:st data)
       #?(:cljs (.-stack ^js err)
          :cljd (when (instance? Error err) (.-stackTrace ^Error err))
          :clj (when (instance? Throwable err) (.getStackTrace ^Throwable err))))]
    (list
      (when-not (str/blank? msg)
        (list msg "\n\n"))
      (when (seq extra-data)
        (list
          :data "\n"
          extra-data
          "\n\n"))
      (when (some? st)
        (list
          :trace "\n"
          (default-logger-stack-trace st)
          "\n\n"))
      (when (some? cause)
        (list
          :cause "\n"
          cause
          "\n\n")))))

(defn default-logger
  [{:keys [level msg ex err] :as event}]
  (default-logger-printer level
    (list
      (default-logger-prefix event)
      (when-not (::skip-loc event)
        (default-logger-src event))
      "\n\n"
      (if (or (some? ex) (some? err))
        (default-logger-err event)
        (list
          (when (some? msg)
            (list msg "\n\n"))
          (let [extra-data (dissoc event :level :msg :ex :err :file :line)]
            (when (seq extra-data)
              (list
                :data "\n"
                extra-data
                "\n\n")))))
      "\n")))

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

(defmacro log "
Log something.
" [level & {:as data}]
  {:pre [(keyword? level)]}
  `(log*
     ~(merge
        {#?@(:cljd/clj-host [:file *file*] :cljd [] :clj [:file *file*])
         :line (-> &form meta :line)
         :level level

         #?@(:cljd [:ns `(quote ~(get-in &env [:nses :current-ns]))]
             :clj [:ns `(quote ~(ns-name *ns*))])}
        data)))

(defmacro spy "
Log and then return `x`.
" ([x]
   (let [value-sym (gensym)]
     `(let [~value-sym ~x]
        (log*
          ~{#?@(:cljd/clj-host [:file *file*] :cljd [] :clj [:file *file*])
            :line (-> &form meta :line)
            :spy `(quote ~x)
            :level :debug
            :msg value-sym

            #?@(:cljd [:ns `(quote ~(get-in &env [:nses :current-ns]))]
                :clj [:ns `(quote ~(ns-name *ns*))])})
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

            #?@(:cljd [:ns `(quote ~(get-in &env [:nses :current-ns]))]
                :clj [:ns `(quote ~(ns-name *ns*))])})
        ~value-sym))))
