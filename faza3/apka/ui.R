library(shinydashboard)
library(shiny)


style <- tags$head(tags$style(HTML('
      .autorzy {
        color: #FFFFFF;
        font-size: 16px;
        margin-left: 120px;
        margin-top: 12px;
      }
      * {
        font-family: serif;
      }
      .box-header {
        text-align: center;
      }
      p { padding-left: 0.2cm;
          padding-right: 0.2cm  }
    ')))
####################

header <- dashboardHeader(title = "Wykrywanie charakterystycznych słów na fanpage'ach banków",
                          titleWidth = 650)
header$children[[3]]$children[[3]] <- 
    tags$div(class = "autorzy",
             "Ewa Baranowska, Dorota Łępicka, Michał Mück, Michał Stolarczyk")
####################

sidebar <- dashboardSidebar(
    selectInput("slowoKlucz", "Wybierz słowo kluczowe",
                choices = c("problem", "awaria", "reklamacja", "błąd"), 
                selected = "problem"),
    
    selectInput("bank", "Wybierz bank",
                choices = c("ALIOR", "BZW", "ING"),
                selected = "ALIOR"),
    
    dateRangeInput("daty", "Wybierz zakres dat", 
                   start = "2013-06-20", end = "2016-03-01",
                   min = "2013-06-20", max = "2016-03-01"),
    br(), br(), br(), br(),
    p("Aplikacja wykrywa wątki na fanpage'u wybranego banku, w których często 
      wystęuje wybrane słowo kluczowe. Na wykresie każdy punkt oznacza jeden wątek.
      Po kliknięciu w punkt wyświetlane są podstawowe informacje na temat wątku 
      oraz jego trzy pierwsze posty. ")
)
###################

body <- dashboardBody(
    style,
    box(title = "Liczba słów kluczowych w wątkach", width = 8, status = "primary",
        plotOutput("wykres",
                   click = "wykresClick")),
    box(title = "Wybrany wątek", width = 4, status = "primary",
        htmlOutput("watek"))
)


dashboardPage(
    header,
    sidebar,
    body
)
