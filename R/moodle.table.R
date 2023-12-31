#' moodle.table
#'
#' This function takes a data frame or vector and generates the html code to display it in a
#' moodle quiz 
#' @param x df or vector
#' @param DoRowNames print row names?
#' @param DoBorder print border?
#' @param ncols for vectors, how many items per row?
#' @return A character vector with html code
#' @export
#' @examples
#' moodle.table(round(rnorm(50), 1))
#' moodle.table(mtcars)

moodle.table = function (x, DoRowNames=FALSE, DoBorder=FALSE, ncols=10) 
{
    if(is.vector(x)) {
        n = length(x)
        xlengths = rep(max(nchar(as.character(x))), ncols)
        k = n %% ncols
        if(k==0)
            x = matrix(x, ncol=ncols)
        else {
            add = rep("&nbsp;", floor(n/ncols)*ncols+ncols-n)
            x = matrix(c(x, add), ncol=ncols,byrow = TRUE)
        }
        padding = 2
        DoColnames=FALSE
    }
    else {
        padding = 8
        DoColnames = TRUE
        xlengths = rep(0, dim(x)[2])
        for(i in 1:length(xlengths))
           xlengths[i] = max(nchar(colnames(x)[i]), nchar(as.character(x[, i])))
    }   
    n = dim(x)[1]
    m = dim(x)[2]    
    lns = ifelse(DoBorder, "<TABLE BORDER>", "<TABLE>")
    nxtline = "<tr>"
    for(i in 1:m)
       nxtline = paste0(nxtline, 
          "<td>", paste0(rep("-", xlengths[i]+padding), collapse=""), "</td>")
    nxtline = paste0(nxtline, "</tr>", collapse="")
    lns = c(lns, nxtline)    
    if(DoColnames) {
       nxtline = "<tr>"
       if(DoRowNames) 
           nxtline = paste0(nxtline, "<td> &nbsp;</td>")
       for (i in 1:m) 
          nxtline = paste0(nxtline, paste("<th>", 
          colnames(x)[i], "</th>"), collapse = "")
       nxtline = paste0(nxtline, "</tr>")
       lns = c(lns, nxtline)
    }    
    for(j in 1:n) {
        nxtline = "<tr>"
        if(DoRowNames) 
           nxtline = paste0(nxtline, "<th>", rownames(x)[j], "</th>")
        for (i in 1:m) nxtline = paste0(nxtline, "<td>", x[j, i], "</td>")
        nxtline = paste0(nxtline, "</tr>", collapse="")
        lns = c(lns, nxtline)
     }
     lns = c(lns,"</Table>")
     paste0(lns, collapse="") 
}
