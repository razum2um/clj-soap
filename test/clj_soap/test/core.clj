(ns clj-soap.test.core
  (:require [clojure.java.io :as io])
  (:use [clj-soap.core]
        [clojure.test]))

(def test-value (ref false))

(defservice jp.myclass.MyApp
  (changeval [^String string] (dosync (ref-set test-value string)))
  (hypotenuse ^Double [^Double x ^Double y] (Math/sqrt (+ (* x x) (* y y))))
  (doubl1 (^String [^String x] (str x x))
          (^Double [^Double x] (+ x x)))
  (doubl2 (^Double [^Double x] (+ x x))
          (^String [^String x] (str x x))))

(deftest test-my-app
  (serve "jp.myclass.MyApp")
  (let [cl (client-fn "http://localhost:6060/axis2/services/MyApp?wsdl")]
    (is (= 5.0 (cl :hypotenuse 3 4)) "SOAP call with return value")
    (cl :changeval "piyopiyo")
    (is (= "piyopiyo" @test-value) "SOAP call without return value")
    ; Axis2 does not support method overloading.
    ;(is (= 10.0 (cl :doubl1 5.0)))
    ;(is (= "abcabc" (cl :doubl1 "abc")))
    ;(is (= 10.0 (cl :doubl2 5.0)))
    ;(is (= "abcabc" (cl :doubl2 "abc")))
    ))

(defn axis-ops [wsdl-fname]
  (-> wsdl-fname
      io/resource
      make-client
      .getAxisService
      axis-service-operations))

(defn axis-op-by-name [wsdl-fname op-name]
  (->> wsdl-fname
       axis-ops
       (filter #(-> % axis-op-name (= op-name)))
       first))

;; wsdl service: http://www.dneonline.com/calculator.asmx
(deftest qualified-params
  (let [qualified-req (-> "fixtures/calculator.wsdl"
                          (axis-op-by-name "Add")
                          (make-request 1 2))]
    (is (re-find #"<\w+:intA>1</\w+:intA><\w+:intB>2</\w+:intB>" (.toString qualified-req)))))

(deftest unqualified-params
  (let [unqualified-req (-> "fixtures/calculator-unqualified.wsdl"
                            (axis-op-by-name "Add")
                            (make-request 3 4))]
    (is (re-find #"<intA>3</intA><intB>4</intB>" (.toString unqualified-req)))))

;;;; Test for exteral SOAP service
#_(let [client (client-fn "http://wsf.cdyne.com/WeatherWS/Weather.asmx?WSDL")]
    (client :GetCityWeatherByZIP "16001"))
