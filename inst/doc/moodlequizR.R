## ----setup, include = FALSE---------------------------------------------------
require(knitr)
require(grid)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  make.xml(quizxyz, 20)

## ----eval=FALSE---------------------------------------------------------------
#  moodlequizR::shinymoodlequizR()

## -----------------------------------------------------------------------------
example0=function() {
   category="Examples / 1"  # moodle category where quiz will be stored
   quizname="problem -" # running counter for problems
   n=50+1*sample(0:50, 1)  # randomized sample size
   m=round(runif(1, 90, 110), 1) # randomized mean
   s=runif(1, 8, 12) # randomized standard deviation
   d=sample(1:3) # randomized number of digits
   x=rnorm(n, m, s) # generate data
   x=round(x, d) # round it to d digits
   res=as.list(1:1)
   res[[1]]= round(mean(x), d+1) # calculate correct answer, to one digit more than the data.
   qtxt =  paste0( "<p>The mean of the data is  ",
        moodlequizR::nm(res[[1]], w = c(100,80), eps = c(0, 0.01))," </p>" )
# create question text with correct answer   
   atxt =  paste0( "<p>The mean of the data is  ", res[[1]]," </p>" )
# create answer text with correct answer      
   htxt = "Use the mean command"
# create hint   
   list(qtxt = paste0("<h5>", qtxt, "</h5>", moodle.table(x)),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}

## ----eval=FALSE---------------------------------------------------------------
#  example1 <- function() {
#     category <- "Examples / Percentage 1"
#     quizname <- "problem - "
#  #----------------------
#  # Question 1
#  #----------------------
#     n <- sample(200:500, 1)
#     p <- runif(1, 0.5, 0.6)
#     x <- rbinom(1, n, p)
#     per <- round(x/n*100, 1)
#     qtxt <- paste0("Question 1: In a survey of ", n, "
#        people ", x, " said that they prefer Coke
#        over Pepsi. So the percentage of people who prefer
#        Coke over Pepsi is
#        {:NM:%100%", per,":0.1~%80%", per, ":0.5}%")
#     htxt <- "Don't forget to multiply by 100, don'tinlcude % sign in answer"
#     atxt <- paste0("Question 1: x/n*100 = ", x, "/", n,
#            "*100 = ", per, " (rounded to 1 digit)")
#  #----------------------
#  # Question 2
#  #----------------------
#     p1 <- round(runif(1, 50, 60), 1)
#     if(per<p1) mc <- c("{:MC:~%100%lower~%0%the same~%0%higher}", " < ")	
#     if(p1==per) mc <- c("{:MC:~%0%lower~%100%the same~%0%higher}", " = ")
#     if(per>p1) mc <- c("{:MC:~%0%lower~%0%the same~%100%higher}", " > ")
#     qtxt <- paste0(qtxt, "<p>Question 2: In a survey some
#       years ago the percentage was ", p1, "%.
#      So the percentage now is ", mc[1])
#     htxt <- ""
#     atxt <- paste0(atxt, "<p>Question 2: ", per , mc[2], p1)
#  
#     list(qtxt = paste0("<h5>",qtxt,"</h5>"),
#          htxt = paste0("<h5>", htxt,"</h5>"),
#          atxt = paste0("<h5>", atxt,"</h5>"),
#          category = category, quizname = quizname)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  moodlequizR::mc(options, w)

## -----------------------------------------------------------------------------
moodlequizR::mc(1, c(0,0, 100))

## ----eval=FALSE---------------------------------------------------------------
#  nm(x, w, eps, ndigits, pts = 1)

## -----------------------------------------------------------------------------
moodlequizR::nm(c(54.7, 54.7), c(100, 80), c(0.1, 0.5), pts=2)

## ----eval=FALSE---------------------------------------------------------------
#  sa(txt, w=100, caps=TRUE, pts=1)

## -----------------------------------------------------------------------------
moodlequizR::sa("correlation coefficient")

## -----------------------------------------------------------------------------
example2   <- function() {
   category <- "moodlequizR / Percentage 2"
   quizname <- "problem - "
#---------------------- 
# Question 1
#----------------------
   n <- sample(200:500, 1)
   p <- runif(1, 0.5, 0.6)
   x <- rbinom(1, n, p)
   per <- round(x/n*100, 1)
   qtxt <- paste0("Question 1: In a survey of ", n, "
       people ", x, " said that they prefer Coke over
       Pepsi. So the percentage of people who prefer
     	Coke over Pepsi is ", 
      nm(c(per, per), c(100, 80), c(0.1, 0.5)), "%")
   htxt <- "Don't forget to multiply by 100, don'tinlcude % sign in answer"
   atxt <- paste0("Question 1: x/n*100 = ", x, "/", n, "*100 = ", per, " (rounded to 1 digit)")
#---------------------- 
# Question 2
#----------------------
   p1 <- round(runif(1, 0.5, 0.6)*100, 1)
   if(per<p1) {w <- c(100, 0, 0); amc <-"<"}
   if(per==p1) {w <- c(0, 100,  0); amc <-"="}
   if(per>p1) {w <- c(0, 0, 100); amc <-">"}
   opts <- c("lower", "the same", "higher")	
   
   qtxt <- paste0(qtxt, "<p>Question 2: In a survey some
        years ago the percentage was ", p1, "%.
        So the percentage now is ", mc(opts, w)) 
   htxt <- ""
   atxt <- paste0(atxt, "<p>Question 2: ", per , amc, p1)

   list(qtxt = paste0("<h5>",qtxt,"</h5>"), 
        htxt = paste0("<h5>", htxt,"</h5>"), 
        atxt = paste0("<h5>", atxt,"</h5>"), 
        category = category, quizname = quizname) 
}

## ----eval=FALSE---------------------------------------------------------------
#  dta=paste.data()

## -----------------------------------------------------------------------------
x=1:20
y=2*x+round(rnorm(20, 0, 3),2)
summary(lm(y~x))

## ----eval=FALSE---------------------------------------------------------------
#  make.xml(quizxyz, k=20, folder=getwd())

## -----------------------------------------------------------------------------
example3 <- function(whichstory) {
   category <- paste0("moodlequizR / Percentage - Story - ", whichstory)
   quizname <- "problem -"
    
   if(whichstory==1) {  
      n <- sample(200:500, 1)
      p <- runif(1, 0.5, 0.6)
      x <- rbinom(1, n, p)
      per <- round(x/n*100, 1)
      qtxt <- paste0("In a survey of ", n, " people ", x, "
        said that they prefer Coke over Pepsi. So the 
        percentage of people who prefer Coke over 
        Pepsi is ", 
        nm(c(per, per), c(100, 80), c(0.1, 0.5)), "%")
   } 
   if(whichstory==2) {  
      n <- sample(1000:1200, 1)
      p <- runif(1, 0.45, 0.55)
      x <- rbinom(1, n, p)
      per <- round(x/n*100, 1)
      qtxt <- paste0("In a survey of ", n, " likely 
        voters ", x, " said that they would vote for
        candidate A. So the percentage of people who 
        will vote for candidate A is ", 
        nm(c(per, per), c(100, 80), c(0.1, 0.5)), "%")      
   }
   if(whichstory==3) {  
      n <- sample(100:200, 1)
      p <- runif(1, 0.75, 0.95)
      x <- rbinom(1, n, p)
      per <- round(x/n*100, 1)
      qtxt <- paste0("In a survey of ", n, " people ", 
        x, " said that they are planning a vacation 
        this summer. So the percentage of people who 
        are planning a vacation is ", nm(c(per, per),
        c(100, 80), c(0.1, 0.5)), "%")      
   }
   htxt <- ""
   atxt <- paste0("x/n*100 = ", x, "/", n, "*100 = ", 
          per, " (rounded to 1 digit)")

   list(qtxt = paste0("<h5>",qtxt,"</h5>"), 
        htxt = paste0("<h5>", htxt,"</h5>"), 
        atxt = paste0("<h5>", atxt,"</h5>"), 
        category = category, quizname = quizname) 
}

## ----eval=FALSE---------------------------------------------------------------
#  make.xml(example3, 20, whichstory=2)

## ----eval=FALSE---------------------------------------------------------------
#  moodledata = paste.data()

## -----------------------------------------------------------------------------
example4 <- 
function() {
   category <- "moodlequizR / Percentage from Raw Data" 
   quizname <- "problem - "

   n <- sample(200:500, 1)
   p <- runif(1, 0.5, 0.6)
   x <- sample(c("Coke", "Pepsi"), size=n, 
               replace=TRUE, prob=c(p,1-p))
   per <- round(table(x)[1]/n*100, 1)

   qtxt <- paste0("In a survey people were asked whether 
    they prefer Coke over Pepsi. Their answers
    are below. So the percentage of people who 
    prefer Coke over Pepsi is ", 
    nm(c(per, per), c(100, 80), c(0.1, 0.5)), "%<hr>",
    moodle.table(x))
   htxt <- "" 
   atxt <- paste0("x/n*100 = ", table(x)[1], "/", n,
            "*100 = ", per, " (rounded to 1 digit)")
   
   list(qtxt = paste0("<h5>",qtxt,"</h5>"), 
        htxt = paste0("<h5>", htxt,"</h5>"),
        atxt = paste0("<h5>", atxt,"</h5>"), 
        category = category, quizname = quizname) 
}

## -----------------------------------------------------------------------------
example5 <- function(bell=TRUE) {
  require(base64)
   category <- paste0("moodlequizR / 
      bell-shaped? - ", ifelse(bell, "Yes", "No")) 
   quizname <- "problem - "

   n <- 1000
   if(bell) x <- rnorm(n, 10, 3)
   else x<- rchisq(n, 2) + 8
   
   plt <- hplot(x, n=50, returnGraph=TRUE) 
   plt64 <- moodlequizR::png64(plt)
   if(bell) mmc <- mc(5, c(100, 0))
   else mmc <- mc(5, c(0, 100))
   qtxt <- paste0("The data shown in this histogram ", 
              mmc, " bell-shaped<hr>", plt64)
   
   htxt <- "" 
   if(bell) atxt <- "It is bell-shaped"
   else atxt <- "It is not bell-shaped"
   
   list(qtxt = paste0("<h5>", qtxt, "</h5>"), 
        htxt = paste0("<h5>", htxt, "</h5>"),         
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
}

## -----------------------------------------------------------------------------
example6 <- function() {
   category <- "moodlequizR / Integral"
   quizname <- "problem - "

   A <- round(runif(1), 1)
   B <- round(runif(1, 1, 2), 1)
   fun <- function(x) {x*exp(x)}
   I <- round(integrate(fun, A, B)$value, 2)
   qtxt <- paste0("\\(\\int_{", A, "}^{", B, "} xe^x dx = \\)", 
   nm(I, eps=0.1))
   htxt <- ""
   atxt <- paste0("\\(\\int_{", A, "}^{", B, "} xe^x dx = \\)", I)
   list(qtxt = paste0("<h5>", qtxt, "</h5>"), 
        htxt = paste0("<h5>", htxt, "</h5>"),         
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
}

