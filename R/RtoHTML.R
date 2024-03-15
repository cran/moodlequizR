#' RtoHTML
#' 
#' This function creates the code needed to make the output of selected R function appear correctly in moodle quizzes.
#' @param method name of the R routine
#' @param x data passed to all functions
#' @param y data passed to functions t.test (two-sample) and lm
#' @param n data passed to function binom.test
#' @param varnames names of variables as they are shown in quiz
#' @param ... additional arguments passed to method 
#' @return a string
#' @export
RtoHTML = function(method, x, y, n, varnames, ...) {
   sg =function(x, n=5) NMcalc::signif2(x, n)
   sp=function(k) paste0(rep("&nbsp;", k), collapse="")
   otherargs = as.list(substitute(list(...)))[-1L]
   methodname=deparse(substitute(method))
   if(methodname=="t.test" & missing(y)) {
      info=stats::t.test(x,  ...)
      out=paste0(c("<p>", sp(3), "One Sample t-test"),collapse="")
      out[2]=""
      if(missing(varnames)) varnames = "x"
      out[3]= paste("data: ", varnames[1])
      out[4] = paste0("t = ", sg(info$statistic,5),
                     ", df = ", length(x)-1, 
                     ", p-value = ", round(info$p.value, 4))
      alttext = "not equal to "
      if(info$alternative=="less") alttext="less than "
      if(info$alternative=="greater") alttext="greater than "
      out[5] = paste0("alternative hypothesis: true mean is ", alttext, info$null.value)
      cl = 0.95
      if("conf.level" %in% names(otherargs)) cl=otherargs$conf.level
      out[6] = paste0(100*cl, " percent confidence interval:")
      out[7] = paste0(sg(info$conf.int[1], 5),"  ",sg(info$conf.int[2], 5))
      out[8:10] = c("sample estimates:", "mean of x", sg(info$estimate, 5))
   }
   if(methodname=="t.test" & !missing(y)) {
     info=stats::t.test(x, y,  ...)
     out=paste0(c("<p>", sp(3), "Welch Two Sample t-test"), collapse="")
     out[2]=""
     if(missing(varnames)) varnames = "x"
     out[3]= paste("data: ", varnames[1])
     out[4] = paste0("t = ", sg(info$statistic,5),
                     ", df = ", length(x)+length(y)-2, 
                     ", p-value = ", round(info$p.value, 4))
     alttext = "not equal to "
     if(info$alternative=="less") alttext="less than "
     if(info$alternative=="greater") alttext="greater than "
     out[5] = paste0("alternative hypothesis: true difference in means is ", alttext, info$null.value)
     cl = 0.95
     if("conf.level" %in% names(otherargs)) cl=otherargs$conf.level
     out[6] = paste0(100*cl, " percent confidence interval:")
     out[7] = paste0(sg(info$conf.int[1], 5),"  ",sg(info$conf.int[2], 5))
     out[8] = "sample estimates:"
     out[9] = paste0(sp(2), "mean of x", sp(2),"mean of y", collapse = "")
     out[10] = paste(sg(info$estimate[1], 5), sp(2), sg(info$estimate[2], 5))
   }   
   if(methodname=="binom.test") {
     info=stats::binom.test(x=x, n=n,  ...)
     out=paste0(c("<p>", sp(3), "Exact binomial test"), collapse = "")
     out[2]=""
     out[3]= paste("data: ", x, " and ",  n)
     out[4] = paste0("number of successes = ", x,
                     ", number of trials = ", n, 
                     ", p-value = ", round(info$p.value, 4))
     alttext = "not equal to "
     if(info$alternative=="less") alttext="less than "
     if(info$alternative=="greater") alttext="greater than "
     out[5] = paste0("alternative hypothesis: true probability of success is ", alttext, info$null.value)
     cl = 0.95
     if("conf.level" %in% names(otherargs)) cl=otherargs$conf.level
     out[6] = paste0(100*cl, " percent confidence interval:")
     out[7] = paste0(sg(info$conf.int[1], 5),"  ",sg(info$conf.int[2], 5))
     out[8:10] = c("sample estimates:", "probability of success", sg(info$estimate, 5))
   }
   if(methodname=="lm") {
     info=stats::summary.lm(stats::lm(y~x,  ...))
     if(missing(varnames)) varnames=c("x","y")
     fit=stats::summary.lm(stats::lm(x~y, ...))
     out=paste0("<p>Call:<br> lm(formula=", varnames[2],"~",varnames[1],")")
     out[2]= "<p>Residuals:"
     z=info$residuals
     out[3] = paste(sp(6), "Min", sp(7), "1Q", sp(5), "Median", sp(7), "3Q", sp(5), "Max")
     out[4] = paste(sg(c(min(z), stats::quantile(z, 0.25), 
                         stats::median(z), stats::quantile(z, 0.75), 
                         max(z)), 5), collapse=" ")
     out[5] ="<p>Coefficients:"
     out[6] = paste0(sp(20), "Estimate", sp(3), "Std. Error", sp(3), " t value", sp(3), " Pr(>|t|)")
     out[7] = paste0("(Intercept)",sp(1), 
                     paste(sg(fit$coefficients[1, ], 4), collapse="&nbsp;")
                     , collapse="")
     out[8] = paste0("y", sp(17), 
                     paste0(sg(fit$coefficients[2, ], 4), sp(2), collapse = " "),
                     collapse="")
     out[9] = paste0("Residual standard error: ", sg(info$sigma, 4), " on ", 
                     info$df[2], " degrees of freedom", collapse="")
     out[10] = paste0("Multiple R-squared:  ", sg(info$r.squared, 4), sp(2), 
                      "Adjusted R-squared: ", sg(info$adj.r.squared, 4))
     p=1-stats::pf(info$fstatistic[1], info$fstatistic[2], info$fstatistic[3])
     if(p<1e-4) p = format(p)
     else p= sg(p, 5)
     out[11] = paste0("F-statistic: ", sg(info$fstatistic[1], 4),
                      " on ", round(info$fstatistic[2]),
                      " and ", round(info$fstatistic[3]), " DF, p-value: ", 
                      p, collapse="")
   }   

   paste(out, collapse="<br>")
}
