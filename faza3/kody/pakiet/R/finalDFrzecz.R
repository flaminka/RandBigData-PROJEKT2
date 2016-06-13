#' Funkcja oddzielająca słowa 
#' 
#' Funkcja przekształca wejściową ramkę danych na dane z oddzielonymi poszczegulnymi słowami z kolumny rzeczowniki w kolumnie o nazwie rzeczownik. Ponadto zachowuje tylko kolumny o nazwach: id, parent_id, tread_id, created_at, user_name, body. Ponadto funkcja usówa stopwordsy i zamienia czasowniki na rzeczowniki odczasownikowe.
#' 
#' 
#' @param dane - ramka danych do przekształcenia, musi zwierać kolumny o nazwie: id, parent_id, tread_id, created_at, user_name, body, rzeczowniki.
#' @return ramkę danych z kolumnami: id, parent_id, tread_id, created_at, user_name, body, rzeczownik przy czym w kolumnie rzeczownik występująpojedyńcze rzeczowniki bez powtórzeń w ramch jednego postu.
#' @import dplyr
#' @import parallel
#' @export
#' 

finalDFRzecz<- function(dane){
  dane$rzeczowniki <- usuwanieStopwordsow(dane$rzeczowniki)
  dane$rzeczowniki <- zamianaCzasownikow(dane$rzeczowniki)
  
  separateNouns(dane)
}