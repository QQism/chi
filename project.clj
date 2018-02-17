(defproject rst "0.1.0-SNAPSHOT"
  :description "This is a restructuredtext parser which output either HTML or hiccup data"
  :url "https://quang.be/rst"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.8"]]
  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.2.2"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   :plugins [[lein-cloverage "1.0.10"]
                             [lein-eftest "0.4.3"]]}}
  :cljsbuild {:builds [{:id "min"
                        :source-paths ["src"]
                        :compiler {:output-to  "resources/public/js/main.js"
                                   :output-dir "resources/public/js"
                                   :source-map "resources/public/js/main.js.map"
                                   :optimizations :advanced
                                   :pretty-print false
                                   }}
                       {:id "node-test"
                        :source-paths ["src" "test"]
                        :compiler {:output-to "target/cljsbuild/test.js"
                                   :output-dir "target/cljsbuild/out"
                                   :asset-path "target/cljsbuild/out"
                                   :source-map true
                                   :main rst.runner
                                   :optimizations :none
                                   :target :nodejs
                                   :pretty-print true}}]})
