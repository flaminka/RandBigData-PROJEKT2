library(dplyr)
library(stringi)

options(stringsAsFactors = FALSE)
dane <- read.csv("../dane/dane_ost.csv")

# Konwersja na utf-8, na Windowsie trzeba zakomentować
dane <- lapply(dane, function(x) 
    if(is.character(x)) stri_encode(x, from = "cp-1250", to = "utf-8")
    else x)
dane <- data.frame(dane)


liczby_postow <- table(dane$thread_id)

# liczby watków o zadanej liczbie postów
table(liczby_postow)

# na razie ograniczę się do wątków niekrótszych niż 10 postów
watki <- names(liczby_postow)[liczby_postow >= 10]

# wybrano
length(watki)
# wątków z
length(unique(dane$thread_id))


# watek <- watki[1]
#sapply(dane, class)

#' Funkcja wybierająca rzeczowniki dla zadanego watku
#' 
#' @param watek Id wątku
#' @param dane Ramka danych zawierająca kolumny thread_id i rzeczowniki
#' @return Wektor rzeczowników występujących w danym wątku.
#' @import magrittr, dplyr
#' @export
#' 
#'
slowa_w_watku <- function(watek, dane) {
    
    dane %>% 
        filter(thread_id == watek) %>%
        .$rzeczowniki -> 
        rzeczowniki
    
    
    unlist(strsplit(rzeczowniki, "|", fixed = TRUE))
}


slowa <- lapply(watki, slowa_w_watku, dane)


source("ranking_slowa.R")


rankingi <- sapply(slowa, function(sl) ranking_slowa(sl)$word[1:5])

View(rankingi)



library(tm)

#' Funkcja wybierająca charakterystyczne słowa
#' 
#'  Charakterystyczne słowa są wybierane na podstawie wag tfidf.
#'  Szczegóły: https://pl.wikipedia.org/wiki/TFIDF
#'  
#' @param slowa Lista wektorów ze słowami. Każdy wektor zawiera słowa z
#'  jednego dokumentu.
#' @param min_tfidf Minimalna waga tfidf, dla której słowo uznawane jest za
#' charakterystyczne. Im większy parametr min_tfidf, tym mniej słów jest zwracana.
#' @value Lista charakterystycznych słów
#' @import tm
#' @export
#' 
charakterystyczne <- function(slowa, min_tfidf) {
    
    
    # łaczenie słów wewnątrz jednego wątku w napis
    napis <- sapply(slowa, paste, collapse = " ")
   
    korpus <- VCorpus(VectorSource(slowa))
    
    # macierz liczby wystąpień słów
    macierz <- DocumentTermMatrix(korpus)
    
    # tfidf
    tfidf <- weightTfIdf(macierz)
    
    # zamiana macierzy rzadkiej na gęstą
    M <- as.matrix(tfidf)
    
    # z każdego dokumentu wybieramy te słowa, które mają tfidf >= min_tfidf
    char <- apply(M, 1, function(row) names(row[row >= min_tfidf]))
    
    char[sapply(char, length) > 0]
    
}

char <- charakterystyczne(slowa, 0.3)

char

dane[dane$thread_id == watki[280], "body"]
