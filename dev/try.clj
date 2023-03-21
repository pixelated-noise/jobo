(ns try
  (:require [jobo.core :as jobo]))

(defn work [_]
  (let [pause (rand-int 10000)]
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
  )
