#' Funkcja wybierraj�ca rzadko wyst�puj�ce s�owa
#' 
#' Funkcja wybiera s�owakt�re wyst�puj� rzadziej ni� u�amek ist w cza�ym zbiorze
#' 
#' @param mostf - ramka danych zawieraj�ca kolumny rzecz - s�owo, ile - liczno�� wyst�powania s�owa, date - data pocz�tku okresu wyst�powania s�owa
#' @param ist - poziom maksymalnej cz�sto�ci wyst�powania danego s�owa w kolumnie rzecz aby wybra� dla niego rekordy
#' @return Funckcja zwraca dane mostf aletylko dla s��w z rzecz kt�re wyst�puj� rzadziej ni� ist
#' 
#' @import dplyr
#' @export

wybor_zdarzen <- function(mostf, ist){
  require(dplyr)
  x=mostf %>%
    select(rzecz, ile) %>%
    filter(ile>=10) %>%
    group_by(rzecz) %>%
    summarise(ile_wyst=sum(ile), ile=n()) %>%
    ungroup() %>%
    arrange(ile, ile_wyst)
  n=sum(x$ile)*ist
  slowa = (x %>%
             filter(ile<n) %>%
             select(rzecz))$rzecz
  mostf %>%
    filter(rzecz %in% slowa)
}