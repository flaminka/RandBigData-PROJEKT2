#' Funkcja rozk³adu s³owa w postach
#' 
#' Funkcja zwraca iloœci wyst¹pieñ danego s³owa w rozbiciu na kolejne dni
#' 
#' @param dane - ramka danych zawieraj¹ca kolumny rzeczownik i created_at
#' @param slowo - s³owo którego rozk³ad chcemy znaleŸæ
#' @return zwraca ramkê danych z kolumn¹ date - kolejne dni oraz ile - liczba wyst¹pieñ s³owa w odpowiadaj¹cym mu dniu
#' 
#' @import dplyr
#' @export

rozklad_slowa<-function(dane, slowo){
  require(dplyr)
  date=as.Date(dane$created_a)
  dane$created_at=date
  date=as.Date(min(date):max(date), origin = "1970-01-01")
  
  ile=sapply(klaster, date, function(i, dane, slowo){
    require(dplyr)
    nrow(filter(dane, created_at == i, rzeczownik==slowo))
  }, dane=dane, slowo=slowo)
  cbind(date, ile)
}