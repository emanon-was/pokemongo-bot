(ns pokemongo-bot.authenticate
  (:import
   [com.pokegoapi.auth GoogleUserCredentialProvider]
   [com.pokegoapi.auth GoogleAutoCredentialProvider]))

(defprotocol IAccount
  (login [_ http-client]))

(defrecord GoogleAccount [user password]
  IAccount
  (login [account http-client]
    (GoogleAutoCredentialProvider. http-client (:user account) (:password account))))

(defrecord GoogleToken [token]
  IAccount
  (login [account http-client]
    (GoogleUserCredentialProvider. http-client (:token account))))


