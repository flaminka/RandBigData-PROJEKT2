#' Funkcja znajduj¹ca jednotematyczne w¹tki
#' 
#' Funkcja znajduje w¹tki w których czêœciej ni¿ f wystêpuj¹ s³owa na post.
#' 
#' @param dateStart data pocz¹tku okresu w formacie "yyyy-mm-dd" jako character
#' @param dateStop data koñca okresu w formacie "yyyy-mm-dd" jako character
#' @param dane ramka danych z kolumnami: created_at, id, thread_id, rzeczownik
#' @param npost minimalna liczba postów w w¹tku które s¹ uwzglêdniane
#' @param freq minimalna czêstotliwoœc wytstêpowania s³owa na post
#' 
#' @return zwraca listê ramek danych, ka¿dy element listy jest nazwany numerem w¹tku (thread_id)
#' @import dplyr
#' @export

filterByPlotList <- function(dateStart, dateStop, dane, npost=5, freq=0.5){
  okres=dane%>%filter(as.character(dane$created_at)>=dateStart, as.character(dane$created_at)<=dateStop)
  watki=okres%>%select(thread_id, id)%>%distinct(id)%>%group_by(thread_id)%>%summarise(ile=n())
  rzeczowniki=okres%>%select(rzeczownik, thread_id)%>%group_by(rzeczownik, thread_id)%>%summarise(ile_rzecz=n())
  razem=rzeczowniki%>%left_join(watki, by="thread_id")%>%mutate(f=ile_rzecz/ile)%>%ungroup()%>%filter(ile>2)%>%arrange(desc(f))
  wazne=razem%>%filter(ile>=npost, f>=freq)%>%arrange(thread_id, desc(f))
  wazne=wazne%>%left_join(okres%>%select(thread_id, created_at)%>%group_by(thread_id)%>%summarise(date=substr(min(as.character(created_at)), 1, 10)),by="thread_id")
  l=lapply(unique(wazne$thread_id), function(id){
    wazne%>%filter(thread_id==id)%>%select(rzeczownik, ile, f, date)
  })
  names(l)<-unique(wazne$thread_id)
  l
}