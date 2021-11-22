#' A function for getting a jpg file for adding the logo to plot() of a linreg object
#' 
#' @description Internal use in the plot.linreg() function
#' @param filename The name of a png file
#' 
#' @keywords internal
#' @importFrom jpeg readJPEG
#' @importFrom grid rasterGrob
#' @export 

get_jpg <- function(filename){
  grid::rasterGrob(jpeg::readJPEG(filename))
}
