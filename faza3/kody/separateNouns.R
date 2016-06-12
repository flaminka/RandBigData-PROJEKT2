#' Nowa ramka danych
#' 
#' Funkcja tworz¹ca ramke danych z pojedyñczymi rzeczownikami
#'
#' @param dane - ramka danych o kolumnach X, id, parent_id, created_at, thread_id, subject, user_name, user_link, source, body, rzeczowniki, gdzie rzeczowniki s¹ podane po kolei oddzielone |
#' @return zwraca podobn¹ danych w której zamiast rzeczowników jest rzeczownik - pojedyñcze s³owo oraz nie ma kolumny X
#' @import parallel
#' @export 
#' 

separateNouns <- function(dane){
  klaster=makeCluster(detectCores()-1)
  lista=parLapply(klaster, 1:nrow(dane), function(i, dane){
    tmp=unlist(strsplit(as.character(dane$rzeczowniki[i]), split="|", fixed = T))
    ile=length(tmp)
    data.frame(id=as.character(rep(dane$id[i], ile)), parent_id=as.character(rep(dane$parent_id[i], ile)),
                          created_at=as.character(rep(dane$created_at[i], ile)), thread_id=as.character(rep(dane$thread_id[i], ile)),    
                          user_name=as.character(rep(dane$user_name[i], ile)), body=as.character(rep(dane$body[i], ile)), rzeczownik=tmp)
  }, dane=dane)
  stopCluster(klaster)
  do.call("rbind", lista)
}