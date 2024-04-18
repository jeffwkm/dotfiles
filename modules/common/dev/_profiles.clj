{:user        {:plugins [[lein-ancient "0.7.0"]
                         [lein-marginalia "0.9.1"]
                         [jonase/eastwood "0.3.5" :exclusions [org.clojure/clojure]]
                         [lein-kibit "0.1.6"]
                         [refactor-nrepl "2.5.1"]
                         [cider/cider-nrepl "0.28.5"]]
               #_:dependencies #_[[acyclic/squiggly-clojure "0.1.9-SNAPSHOT"
                                   :exclusions [org.clojure/tools.reader]]]
               #_:env #_{:squiggly {:checkers [:eastwood]}}
               #_:jvm-opts #_["--add-opens"
                              "java.xml/com.sun.xml.internal.stream=ALL-UNNAMED"
                              "-XX:+UnlockExperimentalVMOptions"
                              #_"-XX:+UseZGC"]}
 :local-repl  {}}
