setwd("~/Uczelnia/R i Big Data/Projekt2/")

library(httr)
library(dplyr)
library(stringi)


options(stringsAsFactors = FALSE)

dane <- read.csv2("applica_data_sample.csv")

dane <- filter(dane, source == "https://www.facebook.com/AliorBankSA")

ortografia <- function(body) {
    
    set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    
    korekta <- POST("https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/spell/single",
                    body = list(message=list(body=body), token="2$zgITnb02!lV"),
                    add_headers("Content-Type" = "application/json"), encode = "json")
    
    
    if (!is.null(content(korekta, "parsed")$error)) {
        warning("Korekta nie powiodła się", immediate. = TRUE)
        return(body)
    } else {
        return(content(korekta, "parsed")$output)
    }
}




for (i in 1:nrow(dane)) {
    dane[i, "body"] <- ortografia(dane$body[i]) 
    cat("\r", i, "/", nrow(dane))
}
cat("\n")


rzeczowniki <- function(body) {
    set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    nlp <- POST("https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/nlp/single",
                body = list(message=list(body=body),
                            token="2$zgITnb02!lV"),
                add_headers("Content-Type" = "application/json"), 
                encode = "json")
    
    tmp <- content(nlp, "parsed")
    
    rzeczowniki <- sapply(tmp$elements, function(elem) { 
        ifelse(!is.null(elem$cTag) && elem$cTag == "Noun", 
               elem$base, 
               "")
    })

    # usunięcie ciapków
    rzeczowniki <- stri_replace_all_fixed(rzeczowniki, "'", "")
    rzeczowniki <- stri_replace_all_fixed(rzeczowniki, '"', "")
    
    rzeczowniki <- rzeczowniki[stri_length(rzeczowniki) > 0]
    tolower(paste0(rzeczowniki, collapse = "|"))
}



dane <- cbind(dane, rzeczowniki = rep("", nrow(dane)))

for (i in 1:nrow(dane)) {
    dane[i, "rzeczowniki"] <- rzeczowniki(dane[i, "body"])
    cat("\r", i, "/", nrow(dane))
}
cat("\n")

write.csv(dane, file = "dane.csv")

#head(dane[,c("body", "rzeczowniki")], 1)

#sort(table(unlist(stri_split_fixed(dane$rzeczowniki, "|"))))










