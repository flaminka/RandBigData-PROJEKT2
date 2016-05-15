#' Funkcja zwracaj¹ca najpopularniejsze s³owo
#' 
#' Funkcja na ka¿dy okres k dni zwraca najpopularniejsze s³owo (s³owa)
#' 
#' @param k - d³ugoœæ okresu (w dniach)
#' @param dane - ramka danych zawieraj¹ca kolumny created_at - daty postóW, rzeczownik - rzeczownik wystêpuj¹cy w poœcie w danym czasie 
#' @return Zwraca ramkê danych z kolumnami rzecz - s³ow wystêpuj¹ce najczêœciej, ile - liczba jego wyst¹pienia, date - data pocz¹tku okresu
#' 
#' @import dplyr
#' @import parallel
#' @export

theMostFrequently<-function(k, dane){
  require(dplyr)
  require(parallel)
  date=as.Date(dane$created_at)
  dane$created_at=date
  date=unique(date)
  
  klaster=makeCluster(detectCores()-1)
  rozklad=parLapply(klaster, 1:length(date), function(i, k, date, dane){
    require(dplyr)
    wybr=dane%>%filter(created_at%in%as.Date(date[i]:(date[i]+k-1), origin = "1970-01-01"))
    rzecz=wybr$rzeczownik
    tab=table(rzecz)
    rzecz=names(tab)[tab==max(tab)]
    ile=tab[tab==max(tab)]
    data.frame(rzecz, ile, date=rep(date[i],length(rzecz)))
  }, k=k, date=date, dane=dane)
  stopCluster(klaster)
  do.call("rbind", rozklad)
}