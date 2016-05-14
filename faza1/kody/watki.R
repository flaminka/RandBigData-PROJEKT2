library(dplyr)
library(stringi)
library(tm)
library(googleVis)


#' Funkcja wybierająca rzeczowniki dla zadanego watku
#' 
#' @param watek Id wątku
#' @param dane Ramka danych zawierająca kolumny thread_id i rzeczowniki
#' @return Wektor rzeczowników występujących w danym wątku.
#' @import magrittr, dplyr
#' @export
#' 
slowa_w_watku <- function(watek, dane) {
    
    dane %>% 
        filter(thread_id == watek) %>%
        .$rzeczowniki -> 
        rzeczowniki
    
    unlist(strsplit(rzeczowniki, "|", fixed = TRUE))
}




#' Funkcja wybierająca charakterystyczne słowa
#' 
#'  Charakterystyczne słowa są wybierane na podstawie wag tfidf.
#'  Szczegóły: https://pl.wikipedia.org/wiki/TFIDF
#'  
#' @param slowa Lista wektorów ze słowami. Każdy wektor zawiera słowa z
#'  jednego dokumentu.
#' @param ile Liczba zwracanych charakterystycznych słów dla każdego wektora.
#' @param min_tfidf Minimalna waga tfidf, dla której słowo uznawane jest za
#' charakterystyczne. Im większy parametr min_tfidf, tym mniej słów jest zwracanych.
#' @value Lista charakterystycznych słów
#' @import tm
#' @export
#' 
charakterystyczne <- function(slowa, ile = NULL, min_tfidf = NULL) {
    
    # łaczenie słów wewnątrz jednego wątku w napis
    napis <- sapply(slowa, paste, collapse = " ")
    
    korpus <- VCorpus(VectorSource(slowa))
    
    # macierz liczby wystąpień słów
    macierz <- DocumentTermMatrix(korpus)
    
    # tfidf
    tfidf <- weightTfIdf(macierz)
    
    # zamiana macierzy rzadkiej na gęstą
    M <- as.matrix(tfidf)
    
    if (is.null(ile)) {
        # z każdego dokumentu wybieramy te słowa, które mają tfidf >= min_tfidf
        apply(M, 1, function(row) names(row[row >= min_tfidf]))
        
    } else {
        char <- apply(M, 1, function(row) names(sort(row, decreasing = TRUE)[1:ile]))
        as.list(data.frame(char))
    }
}



#' TODO: ZROBIĆ DOKUMENTACJĘ
#' 
#' @import dplyr, tm, googleVis
#'
wykres_watki_keywords <- function(dane, keywords, min_postow = 2, 
                                  ile_char = 5, zerowe = FALSE) {
    
    
    liczby_postow <- table(dane$thread_id)
    
    watki <- names(liczby_postow)[liczby_postow >= min_postow]
    
    slowa <- lapply(watki, slowa_w_watku, dane)
    
    # zlicza słowa klucze w watkach
    ile_keywords <- sapply(slowa, function(x) sum(x %in% keywords))
    ile_keywords <- data.frame(thread_id = as.numeric(watki), 
                               ile_keywords = ile_keywords)
    
    
    # ramka z charakterystycznymi słowami odzielonymi '<br>'
    char <- charakterystyczne(slowa, ile = 5)
    char <- sapply(char, paste, collapse = "<br>")
    char <- data.frame(thread_id = as.integer(watki), char = char)
    
    # daty założenia wątków
    dane %>% group_by(thread_id) %>%
        arrange(created_at) %>%
        slice(1) %>%     # pierwszy wiersz w każdej grupie
        ungroup() %>%
        select(thread_id, created_at) ->
        daty
    
    # łączenie 3 ramek
    ramka <- inner_join(daty, ile_keywords,  by = "thread_id")
    ramka <- inner_join(ramka, char, by = "thread_id")
    
    # przygotowanie danych do wyświetlenia w "chmurce"
    ramka$ile_keywords.html.tooltip <- paste(ramka$thread_id, ramka$char, sep = "<br>")
    ramka$char <- NULL
    
    if (!zerowe) ramka <- filter(ramka, ile_keywords != 0)
    
    gvisLineChart(data=ramka, xvar = "created_at", 
                  yvar = c("ile_keywords", "ile_keywords.html.tooltip"), 
                  options=list(width=1000, height=500, 
                               lineWidth = 0,
                               pointSize = 5,
                               legend='none',
                               tooltip="{isHtml:'true'}",
                               hAxis="{title:'Data utworzenia'}",
                               vAxis="{title:'Liczba słow kluczowych'}"))
    
}











