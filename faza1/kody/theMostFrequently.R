#' Funkcja zwracaj�ca najpopularniejsze s�owo
#' 
#' Funkcja na ka�dy okres k dni zwraca najpopularniejsze s�owo (s�owa)
#' 
#' @param k - d�ugo�� okresu (w dniach)
#' @param dane - ramka danych zawieraj�ca kolumny created_at - daty post�W, rzeczownik - rzeczownik wyst�puj�cy w po�cie w danym czasie 
#' @return Zwraca ramk� danych z kolumnami rzecz - s�ow wyst�puj�ce najcz�ciej, ile - liczba jego wyst�pienia, date - data pocz�tku okresu
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