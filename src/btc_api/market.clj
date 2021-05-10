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

(def exchange-data 
  (map 
    text->data
    (rest 
      (rest 
        (map 
          comma-split 
          (.split  (get-file) "\n"))))))


(defn daily-gain-percent []
  (let [values (map :close exchange-data)]
    (loop [ix (dec (count values))
           res '()
           prev-value 0]
    (if (>= ix 0)
      (let [v (nth values ix)]
        (recur (dec ix) (cons (/ v (if (= prev-value 0) v prev-value)) res) v))
      res))))

(defn average [c]
  (* 365 (- (/ (reduce + c) (count c)) 1)))

(defn moving-average-of [start-ix days]
  (/ (reduce + (map :close (take days (nthrest exchange-data start-ix)))) days))




(defn moving-averages-of [days]
  (loop [ix (- (count exchange-data ) days)
         ma '()]
    (if (>= ix 0)
      (recur (dec ix) (cons (moving-average-of ix days) ma))
      ma)))
      

(defn moving-averages-delta-of [days]
  (let [mas (moving-averages-of days)]
    (loop [ix (dec (count mas))
           res '()
           prev-ma 0]
    (if (>= ix 0)
      (let [ma (moving-average-of ix days)]
        (recur (dec ix) (cons (- ma prev-ma) res) ma))
      res))))
      


(defn mayer-multiple []
  (/ (-> exchange-data first :close) (moving-average-of 0 200)))

     



  
           