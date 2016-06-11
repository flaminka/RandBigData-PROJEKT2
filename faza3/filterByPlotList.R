#' Funkcja znajduj¹ca jednotematyczne w¹tki
#' 
#' Funkcja znajduje w¹tki w których czêœciej ni¿ f wystêpuj¹ s³owa na post.
#' 
#' @param dateStart data pocz¹tku okresu w formacie "yyyy-mm-dd" jako character
#' @param dateStop data koñca okresu w formacie "yyyy-mm-dd" jako character
#' @param dane ramka danych z kolumnami: created_at, id, tread, rzeczownik
#' @param npost minimalna liczba postów w w¹tku które s¹ uwzglêdniane
#' @param freq minimalna czêstotliwoœc wytstêpowania s³owa na post
#' 
#' @return zwraca listê ramek danych, ka¿dy element listy jest nazwany numerem w¹tku (tread)
#' 

filterByPlotList <- function(dateStart, dateStop, dane, npost=5, freq=0.5){
  okres=dane%>%filter(as.character(dane$created_at)>=dateStart, as.character(dane$created_at)<=dateStop)
  watki=okres%>%select(tread, id)%>%distinct(id)%>%group_by(tread)%>%summarise(ile=n())
  rzeczowniki=okres%>%select(rzeczownik, tread)%>%group_by(rzeczownik, tread)%>%summarise(ile_rzecz=n())
  razem=rzeczowniki%>%left_join(watki, by="tread")%>%mutate(f=ile_rzecz/ile)%>%ungroup()%>%filter(ile>2)%>%arrange(desc(f))
  wazne=razem%>%filter(ile>=npost, f>=freq)%>%arrange(tread, desc(f))
  wazne=wazne%>%left_join(okres%>%select(tread, created_at)%>%group_by(tread)%>%summarise(date=substr(min(as.character(created_at)), 1, 10)),by="tread")
  l=lapply(unique(wazne$tread), function(id){
    wazne%>%filter(tread==id)%>%select(rzeczownik, ile, f, date)
  })
  names(l)<-unique(wazne$tread)
  l
}