library(httr)
library(dplyr)
library(stringi)


options(stringsAsFactors = FALSE)

sciezka_do_zapisu <- "C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt2/"

dane <- read.csv("C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt2/dane.csv", header = T, sep=";", encoding = "UTF-8", )

dane <- filter(dane, source == "https://www.facebook.com/AliorBankSA")

# wywalam \v
ajajaj <- stri_detect_fixed(dane[,9], pattern = "\v")
dane[ajajaj, 9] <- stri_replace_all_fixed(dane[ajajaj, 9], pattern = "\v", replacement = " ")


# puszczam ortografie


ortografia <- function(body) {
   
   require(httr)
   set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
   
   korekta <- POST("https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/spell/single",
                   body = list(message=list(body=body), token="2$zgITnb02!lV"),
                   add_headers("Content-Type" = "application/json"), encode = "json")
   
   
   if (!is.null(content(korekta, "parsed")$error)) {
      warning("Korekta nie powiodła się", immediate. = TRUE)
      return(body)
   } else {
      if(is.null(content(korekta, "parsed")$output)){
         return("BŁĄD ORTOGRAFII")
      }else{
         return(content(korekta, "parsed")$output)
      }
   }
}





ile <- nrow(dane)

library(doParallel)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


r <- foreach(i = 1:ile, .combine=rbind) %dopar% {
   ortografia(dane$body[i]) 
}

stopCluster(cl)
Sys.time()

# sprawdzam czy jakis blad wyskoczyl z tym nullem

#which(r == "BŁĄD ORTOGRAFII")
#dane[which(r == "BŁĄD ORTOGRAFII"), ] -> zle

#zle

# zapisuje to na wszelki wypadek
#write.csv(r, file = "r.csv")



# robimy te rzeczowniki (dodalam w obu funkcjach require(), w ortografia dalam linijke BLAD ORTOGRAFII)


rzeczowniki <- function(body) {
   require(httr)
   require(stringi)
   set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
   nlp <- POST("https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/nlp/single",
               body = list(message=list(body=body),
                           token="2$zgITnb02!lV"),
               add_headers("Content-Type" = "application/json"), 
               encode = "json")
   
   
   tmp <- content(nlp, "parsed")
   
   if (!is.list(tmp)) return("")
   
   rzeczowniki <- sapply(tmp$elements, function(elem) { 
      ifelse(!is.null(elem$cTag) && elem$cTag == "Noun", 
             elem$base, 
             "")
   })
   
   rzeczowniki <- rzeczowniki[stri_length(rzeczowniki) > 0]
   
   # usunięcie ciapków
   rzeczowniki <- stri_replace_all_fixed(rzeczowniki, "'", "")
   rzeczowniki <- stri_replace_all_fixed(rzeczowniki, '"', "")
   
   tolower(paste0(rzeczowniki, collapse = "|"))
}



ile1 <- length(r) 
Sys.time()
library(doParallel)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

r1 <- foreach(i = 1:ile1, .combine=rbind) %dopar% {
   rzeczowniki(r[i])
}

stopCluster(cl)
Sys.time()



#write.csv(r1, file = "r1.csv")

dane1 <- cbind(dane, r1)

colnames(dane1)[colnames(dane1) == "r1"] <- "rzeczowniki"

write.csv(dane1, file = file.path(sciezka_do_zapisu,"dane_ost.csv"))








