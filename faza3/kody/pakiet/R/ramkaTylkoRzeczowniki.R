#' Funkcja służy do wyodrębnienia z danych rzeczownikow 
#' 
#' Funkcja wyodrębnia z danych rzeczowniki i zapisuje je jako
#' dodatkową kolumnę w formie rzeczownik1|rzeczownik2, po czym tak zmienioną
#' ramkę danych zapisuje w formie pliku .rda
#' 
#' @param sciezka_dane ścieżka dostępu do danych typu .csv, rozdzielone ";", dane muszą być 
#' ramką danych z kolumnami body (tekst do poprawy), source (napis informujący, 
#' skąd pochodzi dany tekst)
#' @param sciezka_zapis sciezka dostepu do miejsca zapisu danych
#' @param nazwa_wynikowa nazwa wynikowego pliku .rda
#' @param ktore_zrodlo napis, mówiący, dla ktorego zrodla poprawic teksty 
#' (tu: dla jakiego banku), domyślnie ustawiony na NULL - poprawia całą ramkę
#' danych
#' @import httr
#' @import dplyr
#' @import stringi
#' @import doParallel
#' @export

ramkaTylkoRzeczowniki <- function(sciezka_dane, sciezka_zapis, nazwa_wynikowa,
                                  ktore_zrodlo=NULL){
   
   options(stringsAsFactors = FALSE)
   
   stopifnot(is.character(sciezka_dane), is.character(sciezka_zapis),
             is.character(nazwa_wynikowa))
   stopifnot(is.character(ktore_zrodlo) | is.null(ktore_zrodlo))
   
   # wczytywanie danych
   dane <- read.csv(sciezka_dane, header = T, sep=";", encoding = "UTF-8")
   
   if(!is.null(ktore_zrodlo)){
      dane <- filter(dane, source == ktore_zrodlo)
   }
   
   
   # usuwanie elementów: '\v'
   czy_V <- stri_detect_fixed(dane[, "body"], pattern = "\v")
   dane[czy_V, "body"] <- stri_replace_all_fixed(dane[czy_V, "body"], pattern = "\v", replacement = " ")
      
   # przerabianie danych paralelnie
   ile <- nrow(dane)
   
   cl <- makeCluster(detectCores() - 1)
   registerDoParallel(cl)
      
   r <- foreach(i = 1:ile, .combine=rbind) %dopar% {
         
         poprawione <- poprawaOrtografii(dane$body[i]) 
         wydobywanieRzeczownikow(poprawione)
         
         }
      
   stopCluster(cl)
      
   dane <- cbind(dane, rzeczowniki = r)
   
   save(dane, file = file.path(sciezka_zapis, paste0(nazwa_wynikowa,".rda")))
      
}