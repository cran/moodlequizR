#' shinymoodlequizR 
#' 
#' This function runs the moodlequizR shiny app
#' @return None
#' @export

shinymoodlequizR = function () 
{
    shiny::runApp(system.file('shinymoodlequizR', package='moodlequizR'))
    
}
