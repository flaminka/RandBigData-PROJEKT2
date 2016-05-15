#'
#'
#'
#'
#'
#'

analiza_zdarzenia<- function(zdarzenie, dane, mostf, k){
  require(dplyr)
  date=as.Date(dane$created_a)
  dane$created_at=date
  date=date%>%unique()%>%sort()
  dats=filter(mostf, rzecz==zdarzenie)%>%select(date)%>%arrange(date)%>%t()%>%as.Date
  d=as.Date(dats[1]:(dats[1]+k-1), origin = "1970-01-01")
  take=c(TRUE,diff(dats)>=k)
  take=(1:length(dats))[take]
  lapply(1:length(take), function(i, t, d, k, dane, z){
    if(i==length(t)){
      dd=as.Date(dats[t[i]]:(dats[length(dats)]+k-1), origin="1970-01-01")
    }else{
      dd=as.Date(dats[t[i]]:(dats[t[i+1]-1]+k-1), origin="1970-01-01")
    }
    posty=dane%>%select(created_at, body, rzeczownik)%>%filter(rzeczownik==z, created_at %in% dd)
    dat=sort(unique(posty$created_at))
    zd=lapply(dat, function(d, posty){
      posty%>%filter(created_at==d)%>%select(body)%>%t()%>%as.vector()
    }, posty=posty)
    names(zd)=dat
    zd
  }, t=take, d=dats, k=k, dane=dane, z=zdarzenie)
}