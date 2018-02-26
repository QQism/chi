(defproject chi "0.1.0-SNAPSHOT"
  :description "The restructuredtext compiler which generates HTML and more"
  :url "https://quang.be/chi"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]
  :jvm-opts ^:replace ["-Xmx1g" "-server"]
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
                                   :pretty-print false}}
                       {:id "release"
                        :source-paths ["src"]
                        :compiler {:output-to "release-js/chi.bare.js"
                                   :optimizations :advanced
                                   :pretty-print  false
                                   :elide-asserts true
                                   :optimize-constants true
                                   :output-wrapper false
                                   :parallel-build true}
                        :notify-command ["release-js/wrap_bare.sh"]}
                       {:id "node-test"
                        :source-paths ["src" "test"]
                        :compiler {:output-to "target/cljsbuild/test.js"
                                   :output-dir "target/cljsbuild/out"
                                   :asset-path "target/cljsbuild/out"
                                   :source-map true
                                   :main chi.runner
                                   :optimizations :none
                                   :target :nodejs
                                   :pretty-print true}}]}
  :clean-targets ^{:protect false} ["target"
                                    "release-js/chi.bare.js"
                                    "release-js/chi.js"])
