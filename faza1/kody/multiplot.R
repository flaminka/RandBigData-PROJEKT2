#' Funkcja służy do łączenia wykresów w jeden wykres
#' 
#' @param ... wykresy do złączenia
#' 
#' @param cols liczba kolumn z wykresami
#' 
#' @param titlesize wielkość czcionki tytułu
#' 
#' @param title tytuł wykresu
#' 
#' @export

multiplot <- function(..., cols=1, titlesize = 18, title="") {
   
   require(grid)
   
   plots <- c(list(...))
   
   numPlots = length(plots)

   layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
   
   if (numPlots==1) {
      print(plots[[1]])
      
   } else {
      
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout)+1, ncol(layout), heights = unit(c(0.7, 4), "null"))))
      grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2), gp = gpar(fontsize = titlesize))
      
      for (i in 1:numPlots) {
         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row+1,
                                         layout.pos.col = matchidx$col))
      }
   }
}
