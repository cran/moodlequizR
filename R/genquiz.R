#' genquiz
#' 
#' This function generates an xml file for import into moodle.
#' @param k =1, how many quizzes?
#' @param fun name of the R routine that makes a quiz
#' @param folder where is the .R located?
#' @param Show =FALSE (optional) want to see what it looks like?
#' @param problem (optional) which problem should be done?
#' @param funname name of quiz
#' @param ... further arguments passed to fun
#' @return None 
#' @export

genquiz = function(k=1, fun, folder, problem=0, funname, Show = FALSE, ...) {
    if(missing(folder)) {
        stop("The folder where the R scipt can be found and the file XML will be stored is required")
    }  
    outfile = c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
             "<quiz>")

    for(j in problem) {
      for(i in 1:k) {
        if(problem[1]==0) info = fun(...)
        else info = fun(problem=j, ...)
        if(Show) message(info)
        lns = c("<!-- question: 0   -->", 
          "<question type=\"category\">",
              "<category>", 
                 paste0("<text>$course$/", info$category, "</text>"),
              "</category>", 
            "</question>", " ",           
            "<!-- question: ", sample(1:1e6,1), "  -->",
              "<question type=\"cloze\">", 
              "<name>",
                   paste0("<text>", info$quizname,  i, "</text>"),
              "</name>",  
              "<questiontext format=\"html\">",
                   paste0("<text><![CDATA[", info$qtxt, "]]></text>"),
               "</questiontext>",  
               "<generalfeedback format=\"html\">",
                   paste0("<text><![CDATA[", info$atxt, "]]></text>"),
               "</generalfeedback>",
               "<penalty>0.0000000</penalty>",
               "<hidden>0</hidden>", 
               "<hint format=\"html\">",
                   paste0("<text><![CDATA[", info$htxt, "]]></text>"),
               "</hint>",
          "</question>")
        outfile = c(outfile, "", lns)      
      }  
    }        
    outfile = c(outfile, "</quiz>") 
    if(missing(funname)) funname=deparse(substitute(fun))
    write(outfile, paste0(folder, "/", funname, ".xml") )  
    
}
