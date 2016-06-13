#' Poprawianiwe Ortografii
#' 
#' Funkcja służy do poprawy ortografii w podanych tekstach
#' 
#' @param body napis, który ma zostać poprawiony
#' @return zwraca poprawiony napis (wektor 1-elem. typu character)
#' @import httr
#' @export

poprawaOrtografii <- function(body) {
   
   stopifnot(is.character(body))
   set_config( config( ssl_verifypeer = 0L, ssl_verifyhost = 0L))
   
   korekta <- POST("https://ec2-54-194-28-130.eu-west-1.compute.amazonaws.com/ams-ws-nlp/rest/spell/single",
                   body = list(message=list(body=body), token="2$zgITnb02!lV"),
                   add_headers("Content-Type" = "application/json"), encode = "json")
   
   
   if (!is.null(content(korekta, "parsed")$error) |
       is.null(content(korekta, "parsed")$output)) {
      warning("Korekta nie powiodła się", immediate. = TRUE)
      return(body)
   } else {
      return(content(korekta, "parsed")$output)
   }
   
}