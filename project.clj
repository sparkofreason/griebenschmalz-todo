(defproject griebenschmalz-todo "0.3.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.189"]
                 [datascript "0.13.3"]
                 [datascript-transit "0.2.0"]
                 [rum "0.6.0"]
                 [griebenschmalz "0.3.0"]
                 [funcool/beicon "0.3.0"]]

  :plugins [[lein-cljsbuild "1.1.0"]]

  :cljsbuild {:builds [{:id           "advanced"
                        :source-paths ["src"]
                        :compiler     {:main          datascript-todo.core
                                       :output-to     "target/todo.js"
                                       :optimizations :advanced
                                       :pretty-print  false}}]}

  :profiles {:dev
             {:cljsbuild
              {:builds [{:id           "none"
                         :source-paths ["src"]
                         :compiler     {:main          datascript-todo.core
                                        :output-to     "target/todo.js"
                                        :output-dir    "target/none"
                                        :optimizations :none
                                        :source-map    true}}]}}})
