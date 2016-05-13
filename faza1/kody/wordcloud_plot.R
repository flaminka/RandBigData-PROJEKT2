#' Funkcja służy do rysowania chmury słów
#' 
#' @param wektor wektor napisów postaci wyraz1|wyraz2|wyraz3
#' @param slowa wektor słów do wyrysowania
#' @param licznosci wektor liczby wystąpień słów z wektora slowa
#' @param ile_slow liczba słów do wyrysowania na wykresie
#' @param od_srodka wartość logiczna, czy umieszczać na środku najpopularniejsze słowa
#' @param kolory kolory słów
#' @param kolory_wg_liczn czy kolory przyporządkować wg liczności czy przypadkowo słowom
#' @param tytul tytuł wykresu
#' @example wordcloud_plot(dane$rzeczowniki, tytul = "Moja pierwsza chmurka")
#' @example wordcloud_plot(slowa=ranking$word, licznosci = ranking$count, tytul = "Moja pierwsza chmurka")
#' @export


wordcloud_plot <- function(wektor=NULL, slowa=NULL, licznosci=NULL, ile_slow=10, od_srodka = F, 
                           kolory = c("#1B676B", "#519548", "#88C425", "#BEF202"), kolory_wg_liczn = F, tytul=""){
   
   require(wordcloud)
   require(RColorBrewer) #mozna dodac palete typu  brewer.pal(8,"Set1")
   
   # gdy podajemy slowa i licznosci
   if(is.null(wektor) & !is.null(slowa) & !is.null(licznosci)){
      
      #sortuje malejaco po liczebnosciach
      order_slow <- order(licznosci, decreasing = T)
      licznosci <- licznosci[order_slow]
      slowa <- slowa[order_slow]
      wordcloud(words = slowa, freq = licznosci, max.words = ile_slow,  random.order = !od_srodka, 
                colors = kolory, random.color = !kolory_wg_liczn)
      text(0.5,1,tytul, cex = 2)
      
   }else if(!is.null(wektor)){
      
      wektor <- unlist(strsplit(unlist(wektor), split = "|", fixed = T))
      wektor <- sort(table(wektor), decreasing = T)
      slowa <- names(wektor)
      licznosci <- wektor
      
      wordcloud(words = slowa, freq = licznosci, max.words = ile_slow,  random.order = !od_srodka, 
                colors = kolory, random.color = !kolory_wg_liczn)
      
      text(0.5,1,tytul, cex = 2)
   }
   
   
}