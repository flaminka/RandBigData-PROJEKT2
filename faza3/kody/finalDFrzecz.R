#' Funkcja oddzielaj¹ca s³owa i czyszcz¹ca dane
#' 
#' Funkcja przekszta³ca wejœciow¹ ramkê danych na dane z oddzielonymi poszczegulnymi s³owami z kolumny rzeczowniki w kolumnie o tej samej nazwie. 
#' 
#' 
#' @param dane - ramka danych do przekszta³cenia
#' @return ramka danych ze zmienion¹ kolumn¹ rzeczowniki
#' @import dplyr
#' @export
#' 

finalDFRzecz<- function(dane){
  dane$rzeczowniki <- usuwanieStopwordsow(dane$rzeczowniki)
  dane$rzeczowniki <- zamianaCzasownikow(dane$rzeczowniki)
  separateNouns(dane)
}