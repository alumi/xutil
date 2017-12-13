(defproject xutil "0.0.2-SNAPSHOT"
  :description "Tiny utility library for transducers."
  :url "http://github.com/alumi/xutil"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.3.465"]]
  :profiles {:test {:dependencies [[midje "1.9.0"]]
                    :plugins [[lein-midje "3.2.1"]]
                    :global-vars {*warn-on-reflection* true}
                    :jvm-opts ["-Xmx64m"]}}
  :aliases {"test" ["with-profile" "+test" "midje"]})
