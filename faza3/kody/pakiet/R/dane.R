#' Dane, na których pracuje aplikacja
#' 
#' Dane te są 3-elementową listą, na której są 3 ramki danych o nazwie: ALIOR, BZW, ING, które zawierają następujące kolumny:
#' \itemize{
#'  \item id. id posta
#'  \item parent_id. id posta nadrzędnego
#'  \item created_at. data utworzenia posta 
#'  \item tread numer wątku
#'  \item user_name. nazwa użytkownika piszącego post
#'  \item body. treść postu
#'  \item rzeczowniki. rzeczownik występujące w poście oddzielone "|"
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name dane
#' @usage data(dane)
#' @format lista z 3 elementami będacymi ramkami danych