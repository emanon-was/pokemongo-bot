(ns pokemongo-bot.item
  (:import
   [com.pokegoapi.api.inventory Item]
   [POGOProtos.Inventory.Item ItemIdOuterClass$ItemId]))

(defprotocol IItemId
  (item-id [_]))

(extend-protocol IItemId
  ItemIdOuterClass$ItemId
  (item-id [i]
    (let [descriptor (.getValueDescriptor i)]
      {:name   (.getName descriptor)
       :number (.getNumber descriptor)}))
  Item
  (item-id [i]
    (item-id (.getItemId i))))

(defprotocol ItemStatus
  (status [_]))

(extend-protocol ItemStatus
  Item
  (status [item]
    {:item-id (item-id item)
     :count (.getCount item)}))



