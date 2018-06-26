(ns clj-soap.core
  (:import [javax.wsdl.Definition]
           [javax.wsdl.factory.WSDLFactory]
           [org.apache.axis2.description AxisMessage AxisOperation]
           [org.apache.ws.commons.schema XmlSchemaForm XmlSchemaElement]))

;;; Defining SOAP Server

(defn flatten1 [coll] (mapcat identity coll))

(defn gen-class-method-decls [method-defs]
  (flatten1
    (letfn [(gen-decl [method-name arglist body]
              [method-name
               (vec (for [arg arglist] (or (:tag (meta arg)) String)))
               (or (:tag (meta arglist)) 'void)])]
      (for [method-def method-defs]
        (cond
          (vector? (second method-def))
          (list (let [[method-name arglist & body] method-def]
                  (gen-decl method-name arglist body)))
          (seq? (second method-def))
          (let [[method-name & deflist] method-def]
            (for [[arglist & body] deflist]
              (gen-decl method-name arglist body))))))))

(defn gen-method-defs [prefix method-defs]
  (flatten1
    (for [method-def method-defs]
      (cond
        (vector? (second method-def))
        (list (let [[method-name arglist & body] method-def]
                `(defn ~(symbol (str prefix method-name))
                   ~(vec (cons 'this arglist)) ~@body)))
        (seq? (second method-def))
        (let [[method-name & deflist] method-def]
          (cons
            `(defmulti ~(symbol (str prefix method-name))
               (fn [~'this & args#] (vec (map class args#))))
            (for [[arglist & body] deflist]
              `(defmethod ~(symbol (str prefix method-name))
                 ~(vec (map #(:tag (meta %)) arglist))
                 ~(vec (cons 'this arglist)) ~@body))))))))


(defmacro defservice
  "Define SOAP class.
  i.e. (defsoap some.package.KlassName (myfunc [String a int b] String (str a (* b b))))"
  [class-name & method-defs]
  (let [prefix (str (gensym "prefix"))]
    `(do
       (gen-class
         :name ~class-name
         :prefix ~prefix
         :methods ~(vec (gen-class-method-decls method-defs)))
       ~@(gen-method-defs prefix method-defs))))

(defn serve
  "Start SOAP server.
  argument classes is list of strings of classnames."
  [& classes]
  (let [server (org.apache.axis2.engine.AxisServer.)]
    (doseq [c classes]
      (.deployService server (str c)))))

;; Client call

(defn axis-service-namespace [axis-service]
  (.get (.getNamespaceMap axis-service) "ns"))

(defn axis-service-operations [axis-service]
  (iterator-seq (.getOperations axis-service)))

(defn axis-op-name [axis-op]
  (.getLocalPart (.getName axis-op)))

(defn axis-op-namespace [axis-op]
  (.getNamespaceURI (.getName axis-op)))

(defn axis-out-message? [^AxisMessage msg]
  (= "out" (.getDirection msg)))

(defn axis-in-message? [^AxisMessage msg]
  (= "in" (.getDirection msg)))

(defn axis-messages [^AxisOperation op]
  (iterator-seq (.getMessages op)))

(defn axis-op-args [axis-op]
  (when-let [^XmlSchemaElement schema (some-> (filter axis-out-message? (axis-messages axis-op))
                                              first .getSchemaElement)]
    (for [elem (-> schema .getSchemaType .getParticle .getItems)]
      {:name (.getName elem)
       :type (some-> elem .getSchemaType .getName keyword)
       :qualified? (-> schema .getForm (= XmlSchemaForm/QUALIFIED))})))

(defn axis-op-rettype [axis-op]
  (some-> (first (filter axis-in-message? (axis-messages axis-op)))
          .getSchemaElement .getSchemaType .getParticle .getItems first
          .getSchemaType .getName keyword))

(defmulti obj->soap-str (fn [obj argtype] argtype))

(defmethod obj->soap-str :integer [obj argtype] (str obj))
(defmethod obj->soap-str :double [obj argtype] (str obj))
(defmethod obj->soap-str :string [obj argtype] (str obj))
(defmethod obj->soap-str :boolean [obj argtype] (str obj))
(defmethod obj->soap-str :default [obj argtype] (str obj))

(defmulti soap-str->obj (fn [obj argtype] argtype))

(defmethod soap-str->obj :integer [soap-str argtype] (Integer/parseInt soap-str))
(defmethod soap-str->obj :double [soap-str argtype] (Double/parseDouble soap-str))
(defmethod soap-str->obj :string [soap-str argtype] soap-str)
(defmethod soap-str->obj :boolean [soap-str argtype] (Boolean/parseBoolean soap-str))
(defmethod soap-str->obj :default [soap-str argtype] soap-str)

(defprotocol ServiceClientFactory
  (make-client [x]))

(extend javax.wsdl.Definition
  ServiceClientFactory
  {:make-client (fn [x] (org.apache.axis2.client.ServiceClient. nil x nil nil))})

(extend java.net.URL
  ServiceClientFactory
  {:make-client (fn [x] (org.apache.axis2.client.ServiceClient. nil x nil nil))})

(extend java.net.URI
  ServiceClientFactory
  {:make-client (fn [^java.net.URI x]
                  (if (-> x .getScheme (.startsWith "http"))
                    (doto (make-client (java.net.URL. (str x)))
                      (.setOptions
                       (doto (org.apache.axis2.client.Options.)
                         (.setTo (org.apache.axis2.addressing.EndpointReference. (str x))))))
                    (make-client (-> (javax.wsdl.factory.WSDLFactory/newInstance)
                                     .newWSDLReader
                                     (.readWSDL (str x))))))})

(extend java.lang.String
  ServiceClientFactory
  {:make-client (fn [x] (make-client (java.net.URI. x)))})

(defn make-request [op & args]
  (let [factory (org.apache.axiom.om.OMAbstractFactory/getOMFactory)
        request (.createOMElement
                  factory (javax.xml.namespace.QName.
                            (axis-op-namespace op) (axis-op-name op)))
        op-args (axis-op-args op)]
    (doseq [[argval {:keys [name type qualified?]}] (map list args op-args)]
      (.addChild request
                 (doto (.createOMElement
                        factory (javax.xml.namespace.QName. (if qualified? (axis-op-namespace op) "") name))
                   (.setText (obj->soap-str argval type)))))
    request))

(defn get-result [op retelem]
  (let [ret-str (.getText (first (iterator-seq (.getChildElements retelem))))]
    (if (not (empty? ret-str))
      (soap-str->obj ret-str (axis-op-rettype op))
      (str retelem))))

(defn client-call [client op & args]
  (if (isa? (class op) org.apache.axis2.description.OutOnlyAxisOperation)
    (.sendRobust client (.getName op) (apply make-request op args))
    (get-result
      op (.sendReceive client (.getName op) (apply make-request op args)))))

(defn client-proxy [url]
  (let [client (make-client url)]
    (->> (for [op (axis-service-operations (.getAxisService client))]
               [(keyword (axis-op-name op))
                (fn soap-call [& args] (apply client-call client op args))])
      (into {}))))

(defn client-fn
  "Make SOAP client function, which is called as: (x :someMethod arg1 arg2 ...)"
  [url]
  (let [px (client-proxy url)]
    (fn [opname & args]
      (apply (px opname) args))))

