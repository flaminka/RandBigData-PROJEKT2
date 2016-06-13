#' Przykładowe posty z facebookowego profilu Alior banku z rozbiciem na poszczególne rzeczowniki
#' 
#' Zbiór zwiera następujące kolumny:
#' \itemize{
#'  \item id. id posta
#'  \item parent_id. id posta nadrzędnego
#'  \item created_at. data utworzenia posta 
#'  \item threat_id. numer wątku
#'  \item user_name. nazwa użytkownika piszącego post
#'  \item body. treść postu
#'  \item rzeczownik. pojedyńczy rzeczownik występujący w poście
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name AliorNouns
#' @usage data(AliorNouns)
#' @format ramka danych z 300 wierszami i 7 kolumnami.