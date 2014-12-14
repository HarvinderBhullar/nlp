library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Coursera Data Science Capstone Project"),
    h4("Predict Text by: Harvinder"),
    
    # Sidebar with controls to provide a caption, select a dataset,
    # and specify the number of observations to view. Note that
    # changes made to the caption in the textInput control are
    # updated in the output area immediately as you type
    sidebarLayout(
        sidebarPanel(
            textInput("caption", "Type text here:", " "),
            p(" ")
        ),
        
        
        # Show the caption, a summary of the dataset and an HTML 
        # table with the requested number of observations
        mainPanel(
            h3("Predicted Current Word:"),
            h3(textOutput("caption")),
            h3("Predicted Next Word:"),
            h3(textOutput("caption2"))
        )
    )
))