#' Funkcja służy do rysowania wykresu szeregu czasowego wystąpień podanego słowa per tydzień
#' 
#' @param slowo słowo, dla którego ma być narysowany wykres
#' 
#' @param data dane z kolumną created_at z czasami utworzenia postów
#' 
#' @param korpus korpus utworzony na podstawie danych
#' 
#' @return zwraca listę z tyloma elementami, dla ilu funkcja uznała wartość wystąpienia analizowanego słowa jest zbyt duża (większa od 3 odchyleń stand.), 
#' w każdym elemencie listy znajdują się daty z podanego zbioru danych, które odpowiadają danemu tygodniowi
#' 
#' @export


wykres_ts <- function(slowo, data = dane, korpus=korpus_mac){
   require(xts)
   czasy <- data$created_at
   czasy <- as.POSIXct(czasy)
   ile <- table(czasy)
   wystapienia1<- korpus[,slowo]
   razem <- data.frame( czas = czasy, wystapienia = wystapienia1)
   razem.xts <- xts(razem$wystapienia,as.POSIXct(razem$czas))
   ends <- endpoints(razem.xts,on='weeks') 
   skumulowane <- period.apply(razem.xts,ends,sum)
   plot.xts(skumulowane, main = paste0("Wystąpienia słowa ",slowo," w czasie (per week)"), auto.grid = F, type="l")
   lines(skumulowane, col = "blue")
   which <- as.numeric(skumulowane) > sqrt(var(as.numeric(skumulowane)))*3 #IQR(skumulowane)*1.34  
   daty <- skumulowane[which]
   tydzien <- format(x=as.Date(as.character(index(daty))), format="%W"  )
   rok <- format(x=as.Date(as.character(index(daty))), format="%Y"  )
   kiedy <- lapply(as.list(1:length(rok)), function(i){
      
      czas <- format(czasy[format(czasy, "%W") == tydzien[i] & format(czasy, "%Y") == rok[i]], "%Y-%m-%d %H:%M:%S")
      
   })
   
   kiedy <- kiedy[order(as.numeric(daty), decreasing = T)] # zwraca w kolejnosci malejacej
}
