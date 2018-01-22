(defproject rst "0.1.0-SNAPSHOT"
  :description "This is a restructuredtext parser which output either HTML or hiccup data"
  :url "https://quang.be/rst"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [org.clojure/clojurescript "1.9.946" :scope "provided"]]
  :profiles {:dev {:dependencies [[midje "1.9.0" :exclusions [org.clojure/clojure]]]
                   :plugins [[lein-midje "3.2.1"]
                             [lein-cloverage "1.0.10"]]}})
