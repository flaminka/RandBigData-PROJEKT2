#' Funkcja służy do zamiany czasownikow w bezokoliczniku na rzeczownik odczasownikowy (pływać -> pływanie)
#' 
#' @param wektor wektor napisow postaci wyraz1|wyraz2|wyraz3
#' @return zwraca zmieniony wektor w tej samej formie (zwroty wielowyrazowe laczy znakiem _ zamiast spacji)
#' @example dane$rzeczowniki <- zamianaCzasownikow(dane$rzeczowniki)
#' @import stringi
#' @export

zamianaCzasownikow <- function(wektor){
   
   wektor <- stri_replace_all_regex(wektor, pattern="\\s", replacement = "\\_")
   
   po_zmianie_wektor  <- lapply(as.list(1:length(wektor)), function(x){
      
      cos <- wektor[x]
      cos <- unlist(strsplit(unlist(cos), split="|", fixed = T))
      cos <- stri_replace_all_regex(cos, pattern="ać$", replacement = "anie")
      cos <- paste0(cos, collapse = "|")
   })
}



