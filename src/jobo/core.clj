(ns jobo.core
  (:require [clojure.java.io :as io]))

(defn write-edn-line [w obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false

            *out* w]
    (prn obj)))

(def default-init-state
  {:control :run})

(defn default-stats-fn [{:keys [last-touched items-done millis-per-item-avg millis-per-item-max size]
                         :or   {items-done          0
                                millis-per-item-avg 0
                                millis-per-item-max 0}
                         :as   state} x]
  (let [time     (System/currentTimeMillis)
        duration (- time last-touched)
        done     (inc items-done)]
    (merge
     state
     (when size {:percent-done (float (* 100 (/ done size)))})
     {:items-done          done
      :millis-per-item-avg (-> millis-per-item-avg
                               (* (dec done))
                               (+ duration)
                               (/ done)
                               double)
      :millis-per-item-max (max duration millis-per-item-max)
      :last-touched        time})))

(defn start! [& {:keys [fun input out-file stats-fn init-state name size resume]
                 :or   {init-state default-init-state
                        stats-fn   default-stats-fn}}]
  (let [time  (System/currentTimeMillis)
        state (atom (merge init-state
                           (when name {:name name})
                           (when size {:size size})
                           {:time-started time
                            :last-touched time}))]
    (when (and (not resume) (.exists (io/file out-file)))
      (io/delete-file out-file))
    (future
      (with-open [out (io/writer out-file :append true)]
        (loop [[fst & rst :as coll] input]
          (let [control (:control @state)]
            (cond
              (not (seq coll))  (swap! state assoc :control :done)
              (= :stop control)  nil
              (= :pause control) (do
                                   (Thread/sleep 3000)
                                   (recur coll))
              :else
              (let [result (try
                             (fun fst)
                             (catch Throwable e e))]
                (if (instance? Exception result)
                  (swap! state merge {:control :error :exception result})
                  (do
                    (write-edn-line out result)
                    (swap! state stats-fn result)
                    (recur rst)))))))))
    state))

(defn pause! [job]
  (swap! job assoc :control :pause))

(defn stop! [job]
  (swap! job assoc :control :stop))

(defn unpause! [job]
  (swap! job assoc :control :run))

(defn resume! [& {:keys [input out-file]
                  :as args}]
  (let [done-count (count (line-seq (io/reader (io/file out-file))))]
    (println "Skipping" done-count "items...")
    (start! (assoc args :input (drop done-count input) :resume true))))

(defn slurp
  "Read in an ednl file. Returns a lazy sequence of EDN values."
  [f]
  (->  f
       io/reader
       reader-types/push-back-reader
       reader-seq))
