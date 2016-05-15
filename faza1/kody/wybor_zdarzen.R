#' Funkcja wybierraj¹ca rzadko wystêpuj¹ce s³owa
#' 
#' Funkcja wybiera s³owaktóre wystêpuj¹ rzadziej ni¿ u³amek ist w cza³ym zbiorze
#' 
#' @param mostf - ramka danych zawieraj¹ca kolumny rzecz - s³owo, ile - licznoœæ wystêpowania s³owa, date - data pocz¹tku okresu wystêpowania s³owa
#' @param ist - poziom maksymalnej czêstoœci wystêpowania danego s³owa w kolumnie rzecz aby wybraæ dla niego rekordy
#' @return Funckcja zwraca dane mostf aletylko dla s³ów z rzecz które wystêpuj¹ rzadziej ni¿ ist
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