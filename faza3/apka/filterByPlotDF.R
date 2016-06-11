#' Funkcja znajduj?ca jednotematyczne w?tki
#' 
#' Funkcja znajduje w?tki w kt?rych cz??ciej ni? f wyst?puj? s?owa na post.
#' 
#' @param dateStart data pocz?tku okresu w formacie "yyyy-mm-dd" jako character
#' @param dateStop data ko?ca okresu w formacie "yyyy-mm-dd" jako character
#' @param dane ramka danych z kolumnami: created_at, id, tread, rzeczownik
#' @param npost minimalna liczba post?w w w?tku kt?re s? uwzgl?dniane
#' @param freq minimalna cz?stotliwo?c wytst?powania s?owa na post
#' 
#' @return zwraca ramek danych wszystkich wybranych s??w z ich cz?sto?ciami 
#' 

filterByPlotDF <- function(dateStart, dateStop, dane, npost=5, freq=0.5){
  require(dplyr)
    
  okres=dane%>%filter(as.character(dane$created_at)>=dateStart, as.character(dane$created_at)<=dateStop)
  watki=okres%>%select(tread, id)%>%distinct(id)%>%group_by(tread)%>%summarise(ile=n())
  rzeczowniki=okres%>%select(rzeczownik, tread)%>%group_by(rzeczownik, tread)%>%summarise(ile_rzecz=n())
  razem=rzeczowniki%>%left_join(watki, by="tread")%>%mutate(f=ile_rzecz/ile)%>%ungroup()%>%filter(ile>2)%>%arrange(desc(f))
  wazne=razem%>%filter(ile>=npost, f>=freq)%>%arrange(tread, desc(f))
  wazne%>%left_join(okres%>%select(tread, created_at)%>%group_by(tread)%>%summarise(date=substr(min(as.character(created_at)), 1, 10)),by="tread")
}