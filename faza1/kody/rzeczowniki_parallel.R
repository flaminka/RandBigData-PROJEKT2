library(httr)
library(dplyr)
library(stringi)


options(stringsAsFactors = FALSE)

###### DANE DO FUNKCJI GŁÓWNEJ ##########


sciezka_do_zapisu <- "C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt2/"

sciezka_do_danych <-"C:/Users/E540/Desktop/SMAD/R i Big Data/Projekt2/dane.csv"

dane1 <- read.csv(sciezka_do_danych, header = T, sep=";", encoding = "UTF-8")

dane1 <- filter(dane1, source == "https://www.facebook.com/AliorBankSA")


#####  GŁÓWNA FUNKCJA #######

wydobywanie_rzeczownikow <- function(sciezka_dane, sciezka_zapis, dane){
   
   
   
   #### FUNKCJE POMOCNICZE ####
   
   ortografia <- function(body) {
      
      require(httr)
      set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
      
      korekta <- POST("https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/spell/single",
                      body = list(message=list(body=body), token="2$zgITnb02!lV"),
                      add_headers("Content-Type" = "application/json"), encode = "json")
      
      
      if (!is.null(content(korekta, "parsed")$error) |
          is.null(content(korekta, "parsed")$output)) {
         warning("Korekta nie powiodła się", immediate. = TRUE)
         return(body)
      } else {
         return(content(korekta, "parsed")$output)
      }
   }
   
   
   
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
   
   
   # wywalam \v
   czy_V <- stri_detect_fixed(dane[, "body"], pattern = "\v")
   dane[czy_V, "body"] <- stri_replace_all_fixed(dane[czy_V, "body"], pattern = "\v", replacement = " ")
   
   ile <- nrow(dane)
   
   require(doParallel)
   cl <- makeCluster(detectCores() - 1)
   registerDoParallel(cl)
   
   
   r <- foreach(i = 1:ile, .combine=rbind) %dopar% {
      
       poprawione <- ortografia(dane$body[i]) 
       rzeczowniki(poprawione)

   }
   
   stopCluster(cl)

   dane <- cbind(dane, rzeczowniki = r)
   
   write.csv(dane, file = file.path(sciezka_do_zapisu,"dane_ost.csv"))
   
}


# UZYCIE FUNKCJI

#Sys.time()
#wydobywanie_rzeczownikow(sciezka_dane = sciezka_do_danych, sciezka_zapis =  sciezka_do_zapisu, dane = dane1)
#Sys.time()
