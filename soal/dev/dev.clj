(ns dev
  (:require [clojure.tools.namespace.repl :refer [refresh refresh-all]]
            [soal.system :refer :all]))

(defn go []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'dev/go))
