#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Setup definitions
phrases_fn <- "data/phrases.rda"
pIDs_fn <- "data/pIDs.rda"
predmatrix_fn <- "data/fullPhraseMatrix.rda"

server <- TRUE
numReplacement <- "xxnumxx"


# Load encoding/decoding hashmaps
library(hashmap)
phrases <- load_hashmap(file=phrases_fn)
pIDs <- load_hashmap(file=pIDs_fn)

# Load predictions (encoded) into array
load(file=predmatrix_fn)


library(dplyr)
library(tidytext)

encode <- function(term) {
    phrases[[term]]
}

decode <- function(ID) {
    pIDs[[ID]]
}

predict <- function(input, predNum = 3) {
    # Tokenize the same way that the n-gram lists were built.
    toks <- tidy(input) %>% unnest_tokens(word, x, token="words", to_lower=TRUE) %>% na.omit()
    # Get simpler format
    toks <- toks[[1]]

    out <- character(predNum)   # pre-allocate
    ansNum <- 1

    while (length(toks) >= 1) {
        while (TRUE) {
            j <- 1
            term <- paste(toks, collapse=" ")
            eanswer <- encode(term)
            if (!is.na(eanswer)) {
                break   # We found a phrase we know
            }
            # If we don't know this phrase, drop the first term and try again.
            if (length(toks) <= 1) {
                break   # We tried everything we could
            }
            toks <- toks[-1]
        }
        if (is.na(eanswer)) {
            if (!server) {
                print("Used model with length 0")
            }
            out <- c("I", "the", "of")
        } else {
            if (!server) {
                print(paste("Used model with length", length(toks)))
            }
            while (ansNum <= predNum) {
                if (eanswer > nrow(pw)) {   # We know this word, but not what comes after
                    break
                }
                ans <- decode(pw[eanswer, j])
                if (is.na(ans)) {
                    break
                }
                if (ansNum > 1) {   # Prevent duplicates
                    if (ans == out[1]) {
                        j <- j + 1
                        next
                    }
                    if (ansNum > 2 && ans == out[2]) {
                        j <- j + 1
                        next
                    }
                }
                if (!server) {
                    print(paste(term, ans))
                }
                if (ans == numReplacement) {
                    out[ansNum] <- "1"
                } else {
                    out[ansNum] <- ans
                }
                ansNum <- ansNum + 1
                j <- j + 1
            }
        }
        if (ansNum <= predNum) {   # We could find more predictions with shorter models
            toks <- toks[-1]   # so drop the first term and try again
            next
        } else {   # Found 3 suggestions, so we're done
            break
        }
    }
    out
}


library(shiny)

# Define server logic required to predict text
shinyServer(function(input, output) {

    output$predictions <- renderText({
        out <- predict(input$inText)
        out[1] <- paste(out[1], ",")
        out[2] <- paste(out[2], ",")
        out
    })

})
