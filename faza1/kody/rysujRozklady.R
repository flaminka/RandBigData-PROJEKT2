#' Funkcja służy do rysowania histogramu i boxplota na jednym wykresie
#' 
#' @param wektor wektor z rzeczownikami
#' 
#' @param tytulgl tytuł główny wykresu
#' 
#' @param tytulboxp tytuł boxplota
#' 
#' @param tytulhist tytuł histogramu
#' 
#' @param max_hist_x maksymalna liczba na osi x
#' 
#' @param krok długość słupka w histogramie
#' 
#' @export



rysujRozklady <- function(wektor, tytulgl="", tytulboxp="", tytulhist="", max_hist_x, krok=10){
   
   require(ggplot2)
   
   staty <- summary(wektor)
   
   DF <- data.frame(x=factor(c(""),
                             levels= c("")), 
                    min=staty[1], low=staty[2], mid=staty[3], top=staty[5], 
                    max=staty[6])
   
   p_d <- ggplot(DF, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
      geom_boxplot(stat = "identity", colour="#556270",fill="#4ECDC4") + ggtitle(tytulboxp) +
      xlab("") + ylab("Ilość rzeczowników")+ coord_cartesian(ylim = c(0, staty[5] + 10)) +
      theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14))
   
   y_r <- ggplot_build(p_d)$panel$ranges[[1]]$y.minor_source
   y_r <- y_r[length(y_r)]
   x_r <- ggplot_build(p_d)$panel$ranges[[1]]$x.major_source
   x_r <- x_r[length(x_r)]
   
   p_d <- p_d +  annotate("text", label = paste0("I kwartyl: ", staty[2]), x = 0.7*x_r, y =y_r, size = 4, colour = "black")+
      annotate("text", label = paste0("III kwartyl: ", staty[5]), x = 0.7*x_r, y =0.9*y_r, size = 4, colour = "black")
   
   
   DF <- as.data.frame(wektor)
   
   p_d1 <- ggplot(data=DF, aes(DF$wektor)) + 
      geom_histogram(breaks=seq(0, staty[6], by = krok), 
                     col="#DB0A5B", 
                     fill="#EBC2C2") + 
      labs(title=tytulhist)+
      theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14))+
      xlab("") +  ylab("Liczność") + coord_cartesian(xlim = c(0, max_hist_x))
   
   y_r <- ggplot_build(p_d1)$panel$ranges[[1]]$y.minor_source
   y_r <- y_r[length(y_r)]
   
   p_d1 <- p_d1 +  annotate("text", label = paste0("max: ", staty[6]), x = 0.7*max_hist_x, y =y_r, size = 4, colour = "black")+
      annotate("text", label = paste0("mediana: ", staty[3]), x = 0.7*max_hist_x, y =0.9*y_r, size = 4, colour = "black")+
      annotate("text", label = paste0("średnia: ", staty[4]), x = 0.7*max_hist_x, y =0.8*y_r , size = 4, colour = "black")
   
   multiplot(p_d1,p_d, cols=2, title=tytulgl)
}
