# Capstone Project application delivery
Next word predictor, meant for cellphone use

## Instructions

The goal of this exercise is to create a product to highlight the prediction algorithm that [I] have built and to provide an interface that can be accessed by others via a Shiny app.


Create a Shiny application and deploy it on Rstudio's servers.

### Your Shiny Application

1. Write a shiny application with associated supporting documentation. The documentation should be thought of as whatever a user will need to get started using your application.
2. Setup locally for Shiny
    a. library(rsconnect)
    b. If you haven't already set the account info up on this computer: rsconnect::setAccountInfo(name='mdavid-stc', token='E231D39D4EB3CB439BD79E13D8D5398A', secret='q044Dw/CvXWt6dQCIVKDdZKInJQ5RTYhnSUXoETI')
3. Check dependencies
    a. setwd("/Training/DataScience/R/Capstone/shiny")
    b. rsconnect::appDependencies()
    c. They should all be from CRAN
4. Deploy the application on Rstudio's shiny server (https://shiny.rstudio.com/articles/shinyapps.html)
    a. Test by clicking the Run App button at the top right on either the server.R or ui.R view
    b. Click Publish button while viewing app
5. Share the application link
    https://mdavid-stc.shinyapps.io/CapstoneProject/
6. Share your server.R and ui.R code on github

The application must include the following:

1. Some form of input (widget: textbox)
2. Some operation on the ui input in server.R
3. Some reactive output displayed as a result of server calculations
4. You must also include enough documentation so that a novice user could use your application.






You can also publish using both formats to github manually using gh-pages, though your github branch must have a .nojekyll fle and be on a branch names gh-pages. There's more on gh-pages here https://pages.github.com/ and there is a video lecture outlining how to do this.
