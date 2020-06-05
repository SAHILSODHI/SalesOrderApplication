(ns sales)
(use '[clojure.string :only (split)])

(defn ReadData [fileName]
  (clojure.string/split-lines (slurp fileName)))

;Customer HashMap
(def customerMap
  (reduce
    (fn [customerVector [custID name address phoneNumber]]
      (merge customerVector (hash-map (Integer/parseInt custID) [name address phoneNumber])))
    {}
    (map #(clojure.string/split % #"\|") (ReadData "cust.txt"))))

;Product HashMap
(def productMap
  (reduce
    (fn [productVector [prodId itemDescription unitCost]]
      (merge productVector (hash-map (Integer/parseInt prodId) [itemDescription unitCost])))
    {}
    (map #(clojure.string/split % #"\|") (ReadData "prod.txt"))))

;Sales HashMap
(def salesMap
  (reduce
    (fn [salesVector [salesID custID prodID itemCount]]
      (merge salesVector (hash-map (Integer/parseInt salesID) [custID prodID itemCount])))
    {}
    (map #(clojure.string/split % #"\|") (ReadData "sales.txt"))))

(defn sortMap [args]
  (into (sorted-map) args)
  )

(defn getCustomerName [k customerId productID itemCounts]
  (print k ": [")
  (print (get (get customerMap (Integer/parseInt customerId)) 0) " ")
  (print (get (get productMap (Integer/parseInt productID)) 0) " ")
  (println itemCounts "]")
  )

(defn refactor [k v]
  (doall (getCustomerName k (get v 0) (get v 1) (get v 2)))
  )

(defn printSalesData [args]
  (doall (map (fn [[k v]] (refactor k v)) args))
  )

(defn printData [args]
  (doall (map (fn [[k v]] (println k ":" v)) args))
  )

(defn customerProducts [customerID]
  (doall(println (reduce + (map (fn [[keySale val]]
                                   (if (= (Integer/parseInt (get val 0)) customerID)
                                     (do (* (Double/parseDouble (get (get productMap (Integer/parseInt (get val 1))) 1))
                                            (Integer/parseInt (get val 2))))
                                     (do 0.0)
                                     )
                                   ) salesMap))))
  )

(defn customerSales []
  (println "Enter customer name")
  (def userName (read-line))
  (print userName " :$")
  (def filteredCustomers (filter (fn [[k v]] (= (get v 0) userName)) customerMap))
  (if (= nil (seq filteredCustomers))
    (do (print "0.0"))
    (do (map (fn [[k v]] (if (= (get v 0) userName) (do (customerProducts k))
                                                (do (print "")))) customerMap))
    )
  )

(defn getProductCount [productIdentification]
  (doall (println (reduce + (map (fn [[keyS valS]]
                                   (if (= (Integer/parseInt (get valS 1)) productIdentification)
                                     (do (Integer/parseInt (get valS 2)))
                                     (do 0)
                                     )
                                   ) salesMap))))
  )

(defn productCount []
  (println "Enter product name")
  (def itemName (read-line))
  (print itemName ":")
  (def filteredProducts (filter (fn [[prodKey prodVal]] (= (get prodVal 0) itemName)) productMap))
  (if (= nil (seq filteredProducts))
    (do (print "0"))
    (do (map (fn [[prodKey prodVal]]
               (if (= (get prodVal 0) itemName)
                 (do (getProductCount prodKey))
                 (do (print ""))
                 )
               ) productMap))
    )
  )

(defn exit [] (doall (println "Good Bye")))
(defn ShowMenu []
  (println "\n***Sales Menu***\n\n1. Display Customer Table\n2. Display Product Table\n3. Display Sales Table\n4. Total Sales for Customer\n5. Total Count for Product\n6. Exit\n\nEnter an option?")
  (def userChoice (read-line))
  (case userChoice
    "1" (printData (sortMap customerMap))
    "2" (printData (sortMap productMap))
    "3" (printSalesData (sortMap salesMap))
    "4" (doall (customerSales))
    "5" (doall (productCount))
    "6" (exit))
  (if (not= "6" userChoice)
    (recur)))
(ShowMenu)
