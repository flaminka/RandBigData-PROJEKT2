#' Funkcja rozk�adu s�owa w postach
#' 
#' Funkcja zwraca ilo�ci wyst�pie� danego s�owa w rozbiciu na kolejne dni
#' 
#' @param dane - ramka danych zawieraj�ca kolumny rzeczownik i created_at
#' @param slowo - s�owo kt�rego rozk�ad chcemy znale��
#' @return zwraca ramk� danych z kolumn� date - kolejne dni oraz ile - liczba wyst�pie� s�owa w odpowiadaj�cym mu dniu
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