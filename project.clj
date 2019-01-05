(defproject alexis-texas "0.1.4-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/tools.logging "0.4.1"]
                 [com.rpl/specter "1.1.1"]
                 [org.suskalo/discljord "0.1.7-SNAPSHOT"]]
  :jvm-opts ["--add-modules" "java.xml.bind"]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.elide-meta=[:doc :added]"
                                  "-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :main alexis-texas.core)
