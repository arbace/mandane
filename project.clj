(defproject mandane "x.y.z"
    :dependencies [[org.clojure/clojure "1.10.2"]]
    :plugins [[lein-try "0.4.3"]]
;   :global-vars {*warn-on-reflection* true}
    :jvm-opts ["-Xmx6g"] ; "-Xss16m"
    :javac-options ["-g"]
    :source-paths ["src"]
    :main mandane.core
    :aliases {"mandane" ["run" "-m" "mandane.core"]}
    :bootclasspath true)
