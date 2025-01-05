A basic logging proxy for `clj-arsenal` libraries.

```clojure
(require `[clj-arsenal.log :refer [log spy] :as log])

(defn my-logger
 [event]
 (prn event))

 (log/remove-logger! log/default-logger)
 (log/add-logger! my-logger)

 (log :debug :msg "Something" :foo "blah" :bar "meh")
 ; >> {:level :debug :file "..." :line ... :ns ... :msg "Something" :foo "blah" :bar "meh"}

 (def something (spy (identity :something)))
 ; >> {:level :debug :file "..." :line ... :ns ... :msg :something :spy (identity :something)}
```
