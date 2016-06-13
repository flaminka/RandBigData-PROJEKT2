library(ggplot2)
library(stringi)
library(dplyr)
library(ClockworkOranges)

czeste <- list(ALIOR = filterByPlotDF("2013-06-20", "2016-02-28", dane$ALIOR),
               BZW = filterByPlotDF("2013-06-20", "2016-02-28", dane$BZW),
               ING = filterByPlotDF("2013-06-20", "2016-02-28", dane$ING))

czeste <- lapply(czeste, function(x) {x$date <- as.POSIXct(x$date); x} )




shinyServer(function(input, output, session) {
    
    
    wybierzRzeczownik <- reactive({
        filter(czeste[[input$bank]], rzeczownik == input$slowoKlucz)
    })

    watek <- reactiveValues(
        watek = NULL
    )
    
    output$wykres <- renderPlot({
        watek <- watek$watek
        ramka <- wybierzRzeczownik()[,c("tread", "date", "ile")]
        if (length(watek) > 0) {
            #watek <- watek[1]
            ramka$zaznaczone <- as.factor(0 + (ramka$tread == watek))
         } else {
            ramka$zaznaczone <- as.factor(rep(0, nrow(ramka)))
           }
        
        cbPalette <- c("#000000", "#56B4E9", "#E69F00", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        ggplot(ramka, aes(x = date, y = ile)) + 
            geom_point(size = 4, aes(colour = zaznaczone)) + 
            coord_cartesian(ylim = c(0, max(ramka$ile)+1),
                            xlim = as.POSIXct(input$daty) ) +
            xlab("Data utworzenia") +
            ylab("Liczba słów kluczowych") + 
            theme(legend.position="none") + 
            scale_colour_manual(values=cbPalette)
    })
    
    output$watek <- renderUI({
        watek <-  watek$watek
        if (length(watek) > 0) {
            watek <- watek[1]
            
            filter(dane[[input$bank]], tread == watek) %>% slice(1) ->
                utworzony
            utworzony <- utworzony$created_at
            
            typowe <- filter(czeste[[input$bank]], tread == watek)$rzeczownik
            typowe <- paste(typowe, collapse = ", ")
            
            filter(dane[[input$bank]], tread == watek) %>%
                group_by(id) %>%
                slice(1) %>% ungroup() -> posty
            
            pierwsze3 <- slice(posty, 1:3)
            ile_postow <- nrow(posty)
            
            HTML(paste0("Data utworzenia: ", utworzony, "<br/>",
                        "Liczba postow: ", ile_postow, "<br/>",
                        "Częste rzeczowniki: ", typowe, "<br/> <br/>",
                        paste(pierwsze3$body, collapse = "<br/><br/>")))
            
        } else {
            "Kliknij na punkt na wykresie, aby wybrać wątek"
        }
        
    })
    
    observeEvent(input$wykresClick, {
        bliskie <- nearPoints(wybierzRzeczownik(), input$wykresClick, 
                              threshold = 10)
        if (nrow(bliskie) > 0) {
            watek$watek <- bliskie$tread[1]
        }
    })
    observeEvent(input$slowoKlucz,  {
        watek$watek <- NULL
    })
    observeEvent(input$bank,  {
        watek$watek <- NULL
    })
})
