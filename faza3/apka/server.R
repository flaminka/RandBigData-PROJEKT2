library(ggplot2)
source("filterByPlotDF.R", encoding = "cp1250")
source("filterByPlotList.R", encoding = "cp1250")

dane <- read.csv("dane_rzecz.csv", stringsAsFactors = FALSE)

czeste <- filterByPlotDF("2013-06-20", "2016-02-28", dane)

czeste$date <- as.POSIXct(czeste$date)







shinyServer(function(input, output, session) {

    wybierzRzeczownik <- reactive({
        filter(czeste, rzeczownik == input$slowoKlucz)
    })
    
    bliskieKlikniecia <- reactive({
        nearPoints(wybierzRzeczownik(), input$wykresClick, threshold = 8)
    })
    
    
    output$wykres <- renderPlot({
        ramka <- wybierzRzeczownik()[,c("date", "ile")]
        
        ggplot(ramka, aes(x = date, y = ile)) + 
            geom_point() + 
            coord_cartesian(ylim = c(0, max(ramka$ile)+1),
                            xlim = as.POSIXct(input$daty) )
    })
    
    output$watek <- renderUI({
        watek <- bliskieKlikniecia()$tread
        if (length(watek) > 0) {
            watek <- watek[1]
            HTML(paste(filter(dane, tread == watek)$body[1:3], collapse = "<br/>"))
            
        } else {
            "Kliknij na punkt na wykresie, aby wybrać wątek"
        }
    })
    
    
})