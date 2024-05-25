shinyWidgets::setBackgroundColor(
  color = "ghostwhite",
  gradient = c("linear", "radial"),
  direction = c("bottom", "top", "right", "left"),
  shinydashboard = FALSE
)

mcchoices=c("yes , no", 
            "lower ,not equal to , higher , can not say", 
            "lower , not equal to , higher", 
            "is statistically significant , is not statistically significant", 
            "is statistically significant , is not statistically significant , can not say", 
            "is , is not", 
            "Male , Female",
            "true , false",
            "has , does not have",
            "!= , < , >",
            "<,>", "+,-", 
            "&mu; , &pi; , &sigma; , &lambda; , &rho; , other")
names(mcchoices)=mcchoices
names(mcchoices)[11]="\u03BC / \u03C0 / \u03C3 / \u03BB / \u03C1 / other"
Distributions=c("Normal", "Uniform", "Beta", "Gamma", "Categorical Variable", "Bivariate Normal", "R Code", "No Data")
moodleRquizzes=c(" ", paste0("moodleRexample",1:15), "Refresh Page for New Selection")
names(moodleRquizzes)=c("0: None", 
                        "1: Find the Mean", 
                        "2: Find the Mean and Median", 
                        "3: Find the Five Number Summary", 
                        "4: One Categorical Variable", 
                        "5: Two Categorical Variables",   
                        "6: Confidence Interval for Mean", 
                        "7: Confidence Interval for Percentage",  
                        "8: Hypothesis Testing for Mean", 
                        "9: Sample Size for Proportion", 
                        "10: Correlation and Regression", 
                        "11: Example for Multiple Stories",
                        "12: Example for a Quiz with Data from Internet",
                        "13: Example on how to Display R Output in a Moodle Quiz",
                        "14: Precalculus: Solve A Linear System", 
                        "15: Calculus: Find Derivatives and Integral", 
                        "Refresh Page for New Selection")  
               
shinyUI(fluidPage(
  titlePanel("moodlequizR"),
  shinyWidgets::setBackgroundColor("ghostwhite"),
  radioButtons("dtl", "Detailed Explanations", choices=c("No", "Yes"), inline = TRUE), 
  fluidRow(  
    column(4, fileInput("filefromfolder", "Choose dta File of Existing Quiz", accept = ".dta")),  
    column(4, selectInput("moodleRquizzes", "Choose Included MoodlequizR Quiz", choices=moodleRquizzes)),
    column(4, uiOutput("readold"))
  ),
  fluidRow(
    column(3, style = "background-color:#E3FEF7;", textInput("quizname", "Name of Quiz / File ",  value="")),
    column(6, style = "background-color:#F2613F;", textInput("folder","Folder for Files", value="c:\\"))
  ), 
  uiOutput("nofolder"), 
  
  fluidRow(
    column(6, textInput("category","Category", placeholder=" top /  middle / bottom")),
    column(6, textInput("numquiz","Number of Quizzes", value="25", width="35%")),  
  ),  
  fluidRow( 
    conditionalPanel( condition = "input.dtl == 'Yes'",
      HTML("<h5>&nbsp;&nbsp;Choose the name of the quiz as it will appear in the questions bank. This will also be the name of the .R file</h5>"),                    
      HTML("<h5>&nbsp;&nbsp;If you want to start with one of the built-in quizzes, choose it here and then click the button twice</h5>"),
      HTML("<h5>&nbsp;&nbsp;Fields in red are required, fields in blue recommended</h5>")
    )
  ),
  fluidRow(
    conditionalPanel( condition = "input.dtl == 'Yes'",
      HTML("<h5>&nbsp;&nbsp;Choose the Category / Subcategory where the quizzes will be stored in moodle</h5>"),
      HTML("<h5>&nbsp;&nbsp;Choose how many quizzes to generate</h5>"),
      HTML("<h5>&nbsp;&nbsp;Choose where the R script and the XML file are saved</h5>")
    )
  ),  
  fluidRow(
    column(12, textAreaInput("comments", "Comments (Optional)"))
  ),
  fluidRow(
    conditionalPanel( condition = "input.dtl == 'Yes'",
        HTML("<h5>&nbsp;&nbsp;How many questions will the quiz have?</h5>"),                    
    )
  ),
  fluidRow(
    column(3, textInput("n", "Sample Size", value="50,100,1", width="50%")),
    column(5, selectInput("distribution", "Type of Data", choices = Distributions)),
    column(4, radioButtons("showdata", "Show Data in Moodle", choices=c("yes","no"), inline=TRUE))
  ),
  fluidRow(
    conditionalPanel( condition = "input.dtl == 'Yes'",
      HTML("<h5>&nbsp;&nbsp;Choose the desired sample size. For fixed sample size just enter one number</h5>"),                    
      HTML("<h5>&nbsp;&nbsp;To get a randomly chosen sample size enter three number separated by a comma. Default is 50 to 100 in steps of 1.</h5>")
    )
  ),  
  fluidRow(
     conditionalPanel( condition = "input.distribution == 'Uniform'",     
        column(2, textInput("Ufrom", "From", value="1", width = "100%")),
        column(2, textInput("Uto", "To", value="10", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Normal'",     
        column(2, textInput("Nmean", "Mean", value="90,110,1", width = "100%")),
        column(2, textInput("Nstd", "Std", value="1", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Beta'",     
        column(2, textInput("Balpha", "shape1", value="1", width = "100%")),
        column(2, textInput("Bbeta", "shape2", value="1", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Gamma'",     
        column(2, textInput("Galpha", "shape", value="1", width = "100%")),
        column(2, textInput("Gbeta", "rate", value="1", width = "100%"))
     ),
     conditionalPanel( condition = "input.distribution == 'Categorical Variable'",
        column(4, textInput("pc","Values First Variable", value = "Male, Female")),
        column(4, textInput("pr","Values Second Variable (Optional)", placeholder = "Young, Middle Age, Old")),
        conditionalPanel( condition = "input.pr!=''",
            column(4, textInput("cat2varnames", "Names of Variables", placeholder="X,Y"))
       )                  
     ),
     conditionalPanel( condition = "input.distribution == 'R Code'",     
        column(12, textAreaInput("RCode", "Code to Generate Data", placeholder="Write your R code, data should be called x", height="200px", width="600px"))
     ),
     conditionalPanel( condition = "input.distribution == 'Bivariate Normal'",    
        column(2, textInput("BNmeans", "Means", value="0, 0", width = "50%")),
        column(2, textInput("BNstds", "Stds", value="1, 1", width = "50%")),
        column(2, textInput("BNcor", "Correlation", value="0.5", width = "50%")),
        column(4, textInput("BNnames", "Variable Names", value="X,Y"))
     ),
     column(6,uiOutput("catvarinfo"))
   ),
   conditionalPanel( condition = "input.dtl == 'Yes'",
     HTML("<h5>Choose the desired parameters</h5>"),                    
     HTML("<h5>To get randomly chosen values enter three numbers From/To/Step separated by a comma</h5>")
   ),   
   conditionalPanel( condition = "input.distribution != 'Categorical Variable'", 
     conditionalPanel( condition = "input.distribution != 'Bivariate Normal'",           
       conditionalPanel( condition = "input.distribution != 'R Code'",           
         fluidRow(          
           column(4, selectInput("ndigit", "Round data to ... digits behind decimal", choices=c(-7:7), selected="1")),
           column(3, selectInput("srt", "Sort data?", choices=c("Yes","No"), selected="No"))
         )  
       )  
     )
  ),
  conditionalPanel( condition = "input.dtl == 'Yes'",
     HTML("<h5>Choose the type  and distribution for the data</h5>"),                    
     HTML("<h5>The option R Code allows entering the code directly, with</h5>"),
     HTML("<h5>different lines separated by a semicolon</h5>")
  ),  
  HTML("<hr>"),
  fluidRow(
     column(12, style = "background-color:#E3FEF7;", textAreaInput("gencalc","General Calculations", value="")),
     conditionalPanel( condition = "input.dtl == 'Yes'",
        HTML("<h5>&nbsp;&nbsp;Here you can write any R code that needs to be executed.</h5>")
     )
  ),
  uiOutput("defineInputs"),
  fluidRow(column(6, actionButton("addbutton", HTML("<font color=\"blue\">Add another Question / Part of a Question<font color=\"black\">")))),
  HTML("<hr>"),
  radioButtons("addgraph", "Add a Graph?", choices=c("No", "Yes"), inline=TRUE),
  conditionalPanel( condition = "input.addgraph=='Yes'",
    textAreaInput("graphcommand", "Graph Commands", value="plt=")
  ),
  textAreaInput("htxt", "Any hints after first try?", ""),
  fluidRow(column(3, radioButtons("doquiz", "Generate xml file", choices = c("Yes", "No"), inline = TRUE))),
  fluidRow(column(6, style = "background-color:#F2613F;",actionButton("xmlbutton",HTML("<font color=\"red\">Create the Files!<font color=\"black\">")))  ),
  textOutput("messages"),
  fluidRow(verbatimTextOutput("text"))
))