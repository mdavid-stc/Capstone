#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that predicts text
shinyUI(fluidPage(

  # Application title
  titlePanel("Predicting the next desired word"),


  # Sidebar with a slider input for choosing the discontinuity year
  sidebarLayout(
    sidebarPanel(
        helpText("Please enter some text, and the result will be predictions of the probable following word"),
        textInput("inText",  "Text input:", value=""),
        p(" "),
        helpText("Note that if you are the first person to use this app in a while, you may have to wait 20 seconds for the first suggestions to show. But it should be quick after that."),
        p("Documentation:"), a("https://github.com/mdavid-stc/Capstone"),
        p("Presentation:"), a("http://rpubs.com/MarkDavid/395750")
    ),

    # Show a plot of the generated distribution
    mainPanel(
        h4("Predicted next words (in probability order):"),
        h3(textOutput("predictions"))
    )
  )
))
