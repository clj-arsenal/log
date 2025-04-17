(ns  clj-arsenal.log
  #?(:cljs (:require-macros clj-arsenal.log))
  (:require
   [clj-arsenal.basis :as b]
   [clojure.string :as str]
   #?@(:cljd
       [["dart:io" :as io]]

       :clj
       [[clojure.stacktrace :as st]]
       
       :cljs
       [])))

(def ^:private devtools? #?(:cljs (boolean (find-ns 'devtools.formatters.core)) :default false))
(def ^:private use-ansi-escape-codes? #?(:cljs (nil? js/globalThis.window) :default true))

(defn span
  [opts & children]
  (vary-meta children assoc ::span-opts (into {} opts)))

(defn span-positive
  [& children]
  (vary-meta children assoc ::span-opts {::style :positive}))

(defn span-negative
  [& children]
  (vary-meta children assoc ::span-opts {::style :negative}))

(defn span-accent
  [& children]
  (vary-meta children assoc ::span-opts {::style :accent}))

(def blank
  (vary-meta '() assoc ::span-opts {}))

(defn span?
  [x]
  (and (seq? x) (some? (::span-opts (meta x)))))

(defrecord ^:private AnsiEscape [code])

(defn- stringify-log-args
  [args]
  (loop
    [args-remaining args
     arg-strings []
     last-non-esc-str nil]
    (cond
      (empty? args-remaining)
      (str/join arg-strings)

      :else
      (let
        [[arg & rest-args] args-remaining]
        (cond
          (instance? AnsiEscape arg)
          (recur
            rest-args
            (cond-> arg-strings
              use-ansi-escape-codes?
              (conj (str "\u001B[" (:code arg) "m"))) 
            last-non-esc-str)

          (or (nil? last-non-esc-str)
            (and (string? last-non-esc-str)
              (case (last last-non-esc-str)
                (\space \newline) true
                false)))
          (as-> (str arg) arg-str
            (recur rest-args (conj arg-strings arg-str) arg-str))

          :else
          (as-> (str " " arg) arg-str
            (recur rest-args (conj arg-strings arg-str) arg-str)))))))

(def ^:private ansi-escapes
  {:reset (->AnsiEscape 0)
   :fg-red (->AnsiEscape 31)
   :fg-green (->AnsiEscape 32)
   :fg-accent (->AnsiEscape 33)})

(defn- default-logger-substitute-spans
  [coll reset-sequence]
  (mapcat
    (fn [x]
      (if-not (span? x)
        [x]
        (let
          [opts (::span-opts (meta x))

           style-sequence
           (case (::style opts)
             :positive [(:fg-green ansi-escapes)]
             :negative [(:fg-red ansi-escapes)]
             :accent [(:fg-accent ansi-escapes)]
             nil)]

          (concat
            style-sequence
            (default-logger-substitute-spans x reset-sequence)
            reset-sequence))))
    coll))

(defn- default-logger-printer
  [level & args]
  (let
    [args (default-logger-substitute-spans args [(:reset ansi-escapes)])]
    #?(:cljs
       (let
         [log-fn
          (case level
            :error (.bind js/console.error js/console)
            :warn (.bind js/console.warn js/console)
            :info (.bind js/console.info js/console)
            :debug (.bind js/console.debug js/console))]
         (if devtools?
           (apply log-fn args)
           (log-fn (stringify-log-args args))))

       :cljd
       (println (stringify-log-args args))

       :clj
       (binding [*out* *err*]
         (println (stringify-log-args args))
         (flush))))
  nil)

(defn- default-logger-prefix
  [{:keys [level] :as _event}]
  (as-> (str/upper-case (name level)) $
    (case $
      "ERROR"
      (span-negative $)
      $)))

(defn- default-logger-src
  [{:keys [file line]}]
  (str file ":" line))

(defn- default-logger-err-data
  [{:keys [err ex] :as event}]
  (let
    [err (or err ex)]
    (merge
      #?(:cljd nil :default (when-some [cause (ex-cause err)] {:cause cause}))
      #?(:cljs {:st (.-stack ^js err)}
         :cljd (when (instance? Error err) {:st (.-stackTrace ^Error err)})
         :clj (when (instance? Throwable err) {:st (.getStackTrace ^Throwable err)}))
      (ex-data err)
      (b/err-data err)
      (dissoc event :err :ex))))

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
             (if (instance? StackTraceElement element)
               (st/print-trace-element element)
               (print element))
             (println)))))
    (str st)))

(defn default-logger
  [{:keys [level ex err] :as event}]
  (let
    [data (if (or (some? ex) (some? err)) (default-logger-err-data event) event)
     skip-keys (set (::skip-keys data))

     prepared-data
     (->> data
       (filter
         (fn [[k _]]
           (not
             (or
               (contains? skip-keys k)
               (and (keyword? k) (= "clj-arsenal.log" (namespace k)))
               (= k :file)
               (= k :line)
               (= k :level)
               (= k :ns)))))
       (sort-by
         (fn [[k _]]
           (case k
             :msg [0 -1 -1]
             :st [3 0 0]
             :cause [3 0 1]
             (if (keyword? k)
               [1 (hash (namespace k)) (hash (name k))]
               [2 0 (hash k)])))))]
    (default-logger-printer level
      (span {}
        (default-logger-prefix event)
        (if-not (::skip-loc event)
          (default-logger-src event)
          blank)
        "\n\n"
        (apply span {}
          (map
            (fn [[k v]]
              (span {} (span-accent k) "\n"
                (case k
                  :st (default-logger-stack-trace v)
                  v)
                "\n\n"))
            prepared-data))))))

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
