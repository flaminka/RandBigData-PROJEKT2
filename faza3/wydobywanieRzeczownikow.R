#' Funkcja służy do wybierania rzeczowników z podanego napisu
#' 
#' @param body napis, który ma zostać poprawiony
#' @return zwraca poprawiony napis (wektor 1-elem. typu character)
#' @example poprawione <- wydobywanieRzeczownikow(dane$body[i]) 
#' @import httr, stringi
#' @export

wydobywanieRzeczownikow <- function(body) {
   
   set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
   
   nlp <- POST("https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/nlp/single",
               body = list(message=list(body=body),
                           token="2$zgITnb02!lV"),
               add_headers("Content-Type" = "application/json"), 
               encode = "json")
   
   tmp <- content(nlp, "parsed")
   
   if (!is.list(tmp)) return("")
   
   rzeczowniki <- sapply(tmp$elements, function(elem) { 
      ifelse(!is.null(elem$cTag) && elem$cTag == "Noun", 
             elem$base, 
             "")
   })
   
   rzeczowniki <- rzeczowniki[stri_length(rzeczowniki) > 0]
   
   # usunięcie ciapków
   rzeczowniki <- stri_replace_all_fixed(rzeczowniki, "'", "")
   rzeczowniki <- stri_replace_all_fixed(rzeczowniki, '"', "")
   
   # łączenie rzeczowników w jeden napis, rozdzielonych znakiem '|'
   rzeczowniki <- tolower(paste0(rzeczowniki, collapse = "|"))
   
   return(rzeczowniki)
}