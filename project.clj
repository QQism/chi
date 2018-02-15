(defproject rst "0.1.0-SNAPSHOT"
  :description "This is a restructuredtext parser which output either HTML or hiccup data"
  :url "https://quang.be/rst"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :profiles {:dev {:dependencies [[com.cemerick/piggieback "0.2.2"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   :plugins [[lein-cloverage "1.0.10"]
                             [lein-eftest "0.4.3"]]}
             }
  ;;:hooks [leiningen.cljsbuild]
  :cljsbuild {:builds {:min
                       {:source-paths ["src"]
                        :compiler {:output-to  "resources/public/js/main.js"
                                   :output-dir "resources/public/js"
                                   :source-map "resources/public/js/main.js.map"
                                   :optimizations :advanced
                                   :pretty-print false
                                   }
                        }
                       :test
                       {:source-paths ["src"]
                        :compiler {:output-to "target/test.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}}
              :test-commands {"unit" ["phantomjs"
                                      ;;"runners/speclj"
                                      "target/test.js"]}
              }
  )
