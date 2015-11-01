(defproject com.hypirion/subtex "0.1.0-SNAPSHOT"
  :description "Convert a small subset of LaTeX into Clojure data structures"
  :url "http://github.com/hyPiRion/subtex"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :prep-tasks [["compile" "com.hypirion.subtex.records"]
               "javac" "compile"]
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.8.0-beta1"]
                 [com.hypirion/rexf "0.1.0-SNAPSHOT"]]
  :profiles {:provided {:dependencies [[hiccup "1.0.5"]]}}
  )
