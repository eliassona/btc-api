(ns btc-api.market
  (:import [org.apache.commons.io FileUtils]
           [java.net URL]
           [java.io File]
           [javax.net.ssl SSLContext TrustManager X509TrustManager HttpsURLConnection]
           ))

(defn comma-split [text]
  (.split text ","))




(defn text->data [text]
  {:timestamp (-> text (nth 0) read-string)
   :open (-> text (nth 3) read-string)
   :high (-> text (nth 4) read-string)
   :low (-> text (nth 5) read-string)
   :close (-> text (nth 6) read-string)
   :volume (-> text (nth 7) read-string)
   }
  )

(def trust-all-certs 
  (into-array [(reify X509TrustManager
    (getAcceptedIssuers [this] nil)
    (checkClientTrusted [this certs authType])
    (checkServerTrusted [this certs authType]))]))

(let [sc (SSLContext/getInstance "SSL")]
  (.init sc nil trust-all-certs (java.security.SecureRandom.))   
  (HttpsURLConnection/setDefaultSSLSocketFactory (.getSocketFactory sc)))


(defn get-file []
  (let [url (URL. "https://www.cryptodatadownload.com/cdd/gemini_BTCUSD_day.csv")    
        connection (.openConnection url)
        is  (.getInputStream connection)]
    (slurp is)))

(def get-exchange-data 
  (map 
    text->data
    (rest 
      (rest 
        (map 
          comma-split 
          (.split  (get-file) "\n"))))))


(defn moving-average-of [start-ix days]
  (/ (reduce + (map :close (take days (nthrest get-exchange-data start-ix)))) days))


(defn mayer-multiple []
  (/ (-> get-exchange-data first :close) (moving-average-of 0 200)))

     



  
           