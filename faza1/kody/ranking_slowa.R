#' Funkcja służy do tworzenia rankingu slow wystepujacych w calym wektorze 
#' 
#' @param wektor wektor napisow postaci wyraz1|wyraz2|wyraz3
#' @return zwraca ramke danych z posortowanymi (malejaco wg licznosci) slowami, procentem wystapien slowa w wektorze, pozycja w rankingu
#' @example ranking <- ranking_slowa(dane$rzeczowniki)
#' @export


ranking_slowa <- function(wektor){
   
   
   rzeczowniki <- unlist(strsplit(unlist(wektor), split = "|", fixed = T))
   wszystkie <- sort(table(rzeczowniki), decreasing = T)
   
   ranking_wszyscy <- data.frame(word = names(wszystkie), count = wszystkie )
   rownames(ranking_wszyscy) <- NULL
   ranking_wszyscy$rank <- rownames(ranking_wszyscy)
   ranking_wszyscy$procent <- ranking_wszyscy$count/sum(ranking_wszyscy$count)*100
   
   return(ranking_wszyscy)
}