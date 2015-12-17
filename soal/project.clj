(defproject soal "0.0.1-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.stuartsierra/component "0.3.0"]
                 [org.clojure/tools.nrepl "0.2.10"]
                 [org.clojure/tools.namespace "0.2.11"]
                         
                 [io.pedestal/pedestal.service "0.4.0"]
                 [io.pedestal/pedestal.jetty "0.4.0"]
                 ;; [ch.qos.logback/logback-classic "1.1.3" :exclusions [org.slf4j/slf4j-api]]
                 [org.slf4j/jul-to-slf4j "1.7.12"]
                 [org.slf4j/jcl-over-slf4j "1.7.12"]
                 ;;tools
                 [prismatic/schema "1.0.3"]
                 [com.taoensso/timbre "4.1.4"]
                 ;;html
                 [hiccup "1.0.5"]
                 [selmer "0.9.2"]
                 [hickory "0.5.4"]
                 ;;datomic
                 [com.datomic/datomic-free "0.9.5130"]]
  :profiles {:dev {:source-paths ["dev"]}}
  :resource-paths ["resources"]
  :main ^{:skip-aot true} soal.server)

