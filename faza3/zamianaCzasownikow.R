#' Funkcja służy do zamiany czasowników w bezokoliczniku na odpowiadające im
#' rzeczowniki odczasownikowe (czasowniki typu: pływać -> pływanie)
#' 
#' @param wektor wektor napisow postaci wyraz1|wyraz2|wyraz3
#' @return zwraca zmieniony wektor w tej samej formie (zwroty wielowyrazowe laczy znakiem _ zamiast spacji)
#' @example dane$rzeczowniki <- zamiana_czasownikow(dane$rzeczowniki)
#' @import stringi
#' @export

zamianaCzasownikow <- function(wektor){
   
   wektor <- stri_replace_all_regex(wektor, pattern="\\s", replacement = "\\_")
   
   #zamiana czasowniki typu pływać, skakać na pływanie, skakanie
   po_zmianie_wektor  <- lapply(as.list(1:length(wektor)), function(x){
      
      cos <- wektor[x]
      cos <- unlist(strsplit(unlist(cos), split="|", fixed = T))
      cos <- stri_replace_all_regex(cos, pattern="ać$", replacement = "anie")
      cos <- paste0(cos, collapse = "|")
      
   })
   
   po_zmianie_wektor <- unlist(po_zmianie_wektor)
   
   return(po_zmianie_wektor)
   
}



