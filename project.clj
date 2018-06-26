(defproject razum2um/clj-soap "0.2.1"
  :description "SOAP Client and Server using Apache Axis2."
  :url "https://github.com/seancorfield/clj-soap"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.apache.axis2/axis2-adb "1.7.8"]
                 [org.apache.axis2/axis2-transport-http "1.7.8"]
                 [org.apache.axis2/axis2-transport-local "1.7.8"]]
  :source-paths ["src" "test"]
  :aot [clj-soap.test.core])

