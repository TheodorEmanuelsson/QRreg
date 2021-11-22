#' Plots for a linreg object
#'
#' @param x An object of class linreg
#' @param ... Additional arguments that we don't use
#' @examples
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab ggtitle coord_cartesian theme theme_void annotation_custom theme_classic element_rect element_text stat_summary_bin
#' @importFrom stats median
#' @importFrom grid roundrectGrob unit
#' @importFrom here here
#' @export 

plot.linreg <- function(x, ...){
  plotdata <- data.frame(x[["Residuals"]],
                         x[["Fitted values"]], 
                         x[["Square root of standardized residuals"]])
  p1 <- ggplot2::ggplot(plotdata,
               aes(y = plotdata[,1], x = plotdata[,2])) +
    geom_point(shape = 1) +
    stat_summary_bin(fun = median, geom = "line", color = "red") + 
    stat_summary_bin(fun = mean, geom = "line", color = "gray", linetype = 2) +
    #geom_line(aes(y = mean(residu), x = fitval), colour = "red") +
    #geom_line(aes(y = median(residu), x = fitval), linetype = 2, colour = "grey") +
    xlab(paste0("Fitted values \n", "linreg(",deparse(x[["formula"]]),")" )) + ylab("Residuals") +
    ggtitle("Residuals vs Fitted") + liu_theme()
  
  t <- grid::roundrectGrob()
  
  p2 <- ggplot2::ggplot(plotdata,
               aes(y = plotdata[,3], x = plotdata[,2])) +
    geom_point(shape = 1) +
    stat_summary_bin(fun = mean, geom = "line", color = "red") +
    #geom_line(aes(y = mean(stdresid), x = fitval), colour = "red") +
    #geom_line(aes(y = median(stdresid), x = fitval), linetype = 2, colour = "grey") +
    xlab(paste0("Fitted values \n", "linreg(",deparse(x[["formula"]]),")" )) + ylab(expression(sqrt("Standardized residuals"))) +
    ggtitle("Scale-Location") + annotation_custom(t, xmin = 200, xmax = 275, ymin = -.005, ymax = -.008) +
    coord_cartesian(clip = "off") +
    theme(plot.margin = grid::unit(c(1, 1, 3, 1), "lines")) + liu_theme()
  
  #logofilename <- here::here("inst/liu_logo-1000-300x81.jpg")
  logofilename <- "./inst/liu_logo-1000-300x81.jpg"
  #logofilename <- system.file("inst", "liu_logo-1000-300x81.jpg", package="curlylinreg")
  l <- get_jpg(logofilename)
  
  logo_plot <- ggplot(mapping = aes( x = 0:1, y = 1)) +
    theme_void() +  annotation_custom(l, xmin = 0.8, xmax = 1)
  
  gridExtra::grid.arrange(p1, p2, logo_plot, heights = c(0.55, 0.55, 0.06))
}
      
