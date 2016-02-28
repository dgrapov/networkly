#'Start GUI
#'@description start GUI as shiny app.
#'@usage networklyShiny()
#'@return shiny app
#'@import shiny
#'@export
networklyShiny<-function(){
  shiny::runApp(system.file("", package = "networkly"),
                display.mode = "normal",
                launch.browser = TRUE)
}
