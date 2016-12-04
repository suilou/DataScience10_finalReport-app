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
