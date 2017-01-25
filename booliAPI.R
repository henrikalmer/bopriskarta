library(digest)
library(httr)
library(jsonlite)
library(stringi)

url <- "https://api.booli.se/"
auth <- data.frame(callerId="bopriskarta", privateKey="1KpJKCMVFzZ2s2YqEv3CPqTMUmzl76V5mwNiQw1a", stringsAsFactors=FALSE)

callApi <- function(endpoint, data, auth) {
	auth$time <- as.character(as.integer(Sys.time()))
	auth$unique <- stri_rand_strings(1, 16)
	auth$hash <- digest(paste0(auth$callerId, auth$time, auth$privateKey, auth$unique), algo="sha1", serialize=FALSE)
	params <- cbind(data, auth[ , names(auth) != "privateKey"])
	res <- GET(paste0(url, endpoint), query=params, add_headers=(Accept="application/vnd.booli-v2+json"))
	fromJSON(content(res, "text", encoding="UTF-8"), flatten=TRUE)
}

areasFromPoint <- function(lat, lng) {
	res <- callApi("areas", data.frame(lat=lat, lng=lng), auth)
	res$areas
}
