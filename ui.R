library(shiny)


shinyUI(pageWithSidebar(  
  
   
  headerPanel("Course10_Capstone_NextWordPrediction"),
  
  sidebarPanel(
      helpText("Enter a few words then hit Go! 
               button for next word prediction."),
      
      textInput("inputTxt", 
                  label = "Input Word(s)",
                  value = "Enter a few words"),
      
      br(),
      
      actionButton("goButton", label = "Go!"),
      
      p("Click the button to update the values displayed in the main panel.")
    ),
    
    mainPanel(
      h2("Suggested Next Word"),
      helpText("Based on prebuilt ngram dictionaries"),
      verbatimTextOutput("nextWord")
    )
  )
  
)

# shinyUI(pageWithSidebar(  
#   titlePanel("Moving Average Plot of S&P500 (6/22/2006-6/22/2016)"),  
#   sidebarPanel(    
#     sliderInput('n', 'Window Size (Days)', value = 60, min = 1, max = 360, step = 1),
#     br(),
#     p(strong(em("Documentation:",a("Moving Average of S&P500",href="https://github.com/suilou/DataScience9_DevelopingDataProducts/tree/master/app/Readme.md")))),
#     p(strong(em("Github repository:",a("Developing Data Products - Peer Assessment Project; Shiny App",href="https://github.com/suilou/DataScience9_DevelopingDataProducts/tree/master/app"))))
#   
#   ), 
#   mainPanel(    
#     plotOutput('newMv')  
#   )
# ))