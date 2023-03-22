(ns try
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [jobo.core :as jobo]
            [nextjournal.clerk :as clerk]))

(defn work [_]
  (let [pause (rand-int 5000)]
    (Thread/sleep (long pause))
    {:pause pause}))

(comment
  (def job
    (jobo/start! :fun work
                 :input (range 10)
                 :size 10
                 :out-file "out.ednl"))

  @job

  (jobo/pause! job)
  (jobo/unpause! job)
  (jobo/stop! job)


  (def job2
    (jobo/resume! :fun work
                  :input (range 10)
                  :size 10
                  :out-file "out.ednl"))

  @job2

  (clerk/serve! {:browse? true :watch-paths ["dev"]})
  )

(def transform-var
  (comp (clerk/update-val symbol)
        clerk/mark-presented))

(def counter-viewer
  {:transform-fn transform-var
   :render-fn '(fn [var-name]
                 (if-let [var (resolve var-name)]
                   (let [atom @var]
                     [:div
                      [:h2 "Counter Example"]
                      [:button.px-2.py-1.bg-blue-200.mr-1 {:on-click #(swap! atom update :counter inc)} "+"]
                      [:button.px-2.py-1.bg-blue-200.mr-1 {:on-click #(swap! atom update :counter dec)} "-"]
                      [:button.px-2.py-1.bg-blue-200.mr-1 {:on-click #(swap! atom (fn [_] {:counter 0}))} "reset"]
                      [nextjournal.clerk.render/inspect @atom]])
                   [:div "could not resolve" var-name]))})

(def slider-viewer
  {:render-fn '(fn [x] [:input {:type :range :value (:counter @@(resolve x)) :on-change #(swap! @(resolve x) assoc :counter (int (.. % -target -value)))}])
   :transform-fn transform-var})

{::clerk/visibility {:code :show :result :show}}

;; # 🧮 Counter in Cler
;; We `defonce` an atom and tag it with `^::clerk/sync`. This will create a corresponding (reagent) atom in the browser.
^::clerk/sync
(defonce my-state
  (atom {:counter 0}))

;; This is showing the state that the JVM has.
@my-state

^{::clerk/viewer counter-viewer}
#'my-state

^{::clerk/viewer slider-viewer}
`my-state

^::clerk/sync
(defonce anum
  (atom 1))

@anum

(defn make-atom [x]
  (atom x))

^::clerk/sync
(defonce aatom
  (make-atom 1))

@aatom

^::clerk/sync
(defonce job
  (jobo/start! :fun work
               :input (range 10)
               :size 10
               :out-file "out.ednl"))

;;State of the job:
@job

;; changing my-state on the JVM and running clerk/show! will update the slider
;; and counter accordingly:
(comment
  (swap! anum inc)
  (clerk/show! *ns*)
  (do
    (swap! my-state update :counter #(mod (+ % 33) 100))
    (clerk/recompute!)))
