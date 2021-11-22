#' A custom theme for Linkoping University used in plot() of a linreg object
#' 
#' @description Internal use in the plot.linreg() function
#' @keywords internal
#' @export 

liu_theme <- function(){
  theme_classic() + theme(
    # Title color and dize
    plot.title = element_text(color="white", size = 20, hjust = 0.5),
    # Axis color
    axis.title = element_text(color = "black", size = 15),
    # Background
    rect = element_rect(fill = "#66d5ed")
    
  )
}