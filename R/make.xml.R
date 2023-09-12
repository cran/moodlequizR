#' make.xml
#' 
#' This function is a simple wrapper for genquiz. It reads file from folder and runs genquiz. The default is to then remove the quiz.
#' @param fun (unquoted) name of function that makes a quiz, or number of a quiz
#' @param k how many quizzes?
#' @param folder folder were fun.R is located
#' @param ... further arguments passed to fun
#' @return None
#' @export

make.xml = function (fun, k=1, folder, ...) 
{
    if(missing(folder)) {
       stop("The folder where the R scipt can be found and the file newquiz.xml will be stored is required")
       
    }  
    funname = deparse(substitute(fun))  
    if(suppressWarnings(!is.na(as.numeric(funname)))) {
       j=seq_along(dir())[startsWith(dir(), paste0("quiz", fun))]
       funname=substring(dir()[j], 1, nchar(dir()[j])-2)
       source(paste0(folder, "/", funname, ".R"))
       fun=function(...) eval(parse(text=funname))(...)
    } 
    else source(paste0(folder, "/", funname, ".R"))
    message(funname)    
    suppressWarnings(genquiz(fun, k, folder=folder, ...))
    
}
