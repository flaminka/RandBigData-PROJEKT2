#' Funkcja rozk³adu s³owa w postach
#' 
#' Funkcja zwraca iloœci wyst¹pieñ danego s³owa w rozbiciu na kolejne dni
#' 
#' @param dane - ramka danych zawieraj¹ca kolumny rzeczownik i created_at
#' @param slowo - s³owo którego rozk³ad chcemy znaleŸæ
#' @return zwraca ramkê danych z kolumn¹ date - kolejne dni oraz ile - liczba wyst¹pieñ s³owa w odpowiadaj¹cym mu dniu
#' 
#' @import dplyr
#' @import parallel
#' @export

rozklad_slowa<-function(dane, slowo){
  require(parallel)
  require(dplyr)
  date=as.Date(dane$created_at)
  dane$created_at=date
  date=as.Date(min(date):max(date), origin = "1970-01-01")
  
  c=makeCluster(detectCores()-1)
  ile=parSapply(c, date, function(i, dane, slowo){
    require(dplyr)
    nrow(filter(dane, created_at == i, rzeczownik==slowo))
  }, dane=dane, slowo=slowo)
  stopCluster(c)
  cbind(date, ile)
}