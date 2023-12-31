#' png64 Function
#' 
#' This function creates a plot object that can be used in a moodle quiz
#' @param plt some graph object
#' @return a character vector 
#' @export

png64 = function(plt) {
  pngfile = tempfile()
  grDevices::png(pngfile, width = 400, height = 400)
  print(plt)
  grDevices::dev.off()
  pltout = base64::img(pngfile, Rd = TRUE, alt = "a")
  m = nchar(pltout)
  pltout = substring(pltout, 6, m-1)
  pltout
}
