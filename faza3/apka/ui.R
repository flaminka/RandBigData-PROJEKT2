library(shinydashboard)
library(shiny)




header <- dashboardHeader(title = "Wykrywanie charakterystycznych słów na fanpage'ach banków",
                          titleWidth = 650)

header$children[[3]]$children[[3]] <- 
    tags$div(class = "autorzy",
             "Ewa Baranowska, Dorota Łępicka, Michał Mück, Michał Stolarczyk")



sidebar <- dashboardSidebar(
    dateRangeInput("daty", "Wybierz zakres dat", 
                   start = "2013-06-20", end = "2016-03-01"),
    
    selectInput("slowoKlucz", "Wybierz słowo kluczowe",
                choices = c("problem", "awaria", "reklamacja"), 
                selected = "problem")
    
    
)




body <- dashboardBody(
    tags$head(tags$style(HTML('
      .autorzy {
        color: #FFFFFF;
        font-size: 16px;
        margin-left: 120px;
        margin-top: 12px;
      }
      * {
        font-family: serif;
      }
    '))),
    
    
    box(title = "Liczba słów kluczowych", width = 8,
        plotOutput("wykres",
                   click = "wykresClick",
                   width = 600, height = 400)),
    box(title = "Wybrany wątek", width = 4,
        htmlOutput("watek"))
   
)




dashboardPage(
    header,
    sidebar,
    body
)
