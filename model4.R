# Johns Hopkins Data Science Specialization
# Coursera Capstone Project

# Fourth attempt at a prediction model

# How many predicted terms to output, up to 4
predNum <- 3
server <- FALSE
# Size of model (how long are the phrases?), up to 5
size <- 4   # This includes the prediction word, so must be > 1

predictions_fn <- "data/predictions.txt"
words_fn <- "data/words.rda"
wIDs_fn <- "data/wIDs.rda"
phrases_fn <- "data/phrases.rda"
pIDs_fn <- "data/pIDs.rda"
predmatrix_fn <- "data/fullPhraseMatrix.rda"
test_sent_fn <- "data/sentences_test.txt"

numReplacement <- "xxnumxx"

# Get encoding/decoding hashmaps
library(hashmap)
phrases <- load_hashmap(file=phrases_fn)
pIDs <- load_hashmap(file=pIDs_fn)
if (phrases$size() != pIDs$size())
    stop("Phrases map is different size than phrase IDs map")



### Build model ###

# Columns are: Given term/phrase (rowkey), Prediction #1, #2, #3
Cp1 <- 1
Cp2 <- 2
Cp3 <- 3
Cp4 <- 4
#    where the 2nd and higher predictions may be empty
#Cterm <- 4   # decoded term for this (row) key

#library(Matrix)
#mw <- Matrix(0, nrow=words2$size(), ncol=3, sparse=TRUE)   # mw = word matrix
#pw <- Matrix(0, nrow=nrow(preds), ncol=3, sparse=TRUE)   # pw = phrase matrix
# 1528 bytes empty for size=3


encode <- function(term) {
    phrases[[term]]
}

decode <- function(ID) {
    pIDs[[ID]]
}

# Load predictions (encoded) into array

if (file.exists(predmatrix_fn)) {
    load(file=predmatrix_fn)   # sets pw array
} else {
    # Load all predictions
    cc <- c("character", "character", "integer", "character", "integer", "character", "integer", "character", "integer")
    preds <- read.table(file=predictions_fn, sep="\t", header=FALSE, quote="", colClasses=cc)
    colnames(preds) <- c("phrase", "next1", "freq1", "next2", "freq2", "next3", "freq3", "next4", "freq4")

    # Non-sparse (full array) method:
    pw <- array(dim=c(nrow(preds), predNum))
    # 21183160 bytes (20 MB)

    len <- nrow(preds)
    for (i in 1:len) {
        pw[i, Cp1] <- encode(preds$next1[i])   # must exist or wouldn't have written the row to preds
        if (predNum > 1) {
            term <- preds$next2[i]
            if (!is.null(term) && term != "")
                pw[i, Cp2] <- encode(term)
        }
        if (predNum > 2) {
            term <- preds$next3[i]
            if (!is.null(term) && term != "")
                pw[i, Cp3] <- encode(term)
        }
        if (predNum > 3) {
            term <- preds$next4[i]
            if (!is.null(term) && term != "")
                pw[i, Cp4] <- encode(term)
        }

        if (i %% 10000 == 0)
            print(paste(i, "of", len))
    }
    save(pw, file=predmatrix_fn)
}
object.size(pw)   # 53,232,808 bytes
# 400 MB including Cterm









### Setup ###
library(dplyr)
library(tidytext)

### Use model ###

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


### Testing ###

# model length 1
predict("junk junk junk")

# model length 2
predict("junk junk food")

# model length 3
predict("whom I love")

# mixed length models (3 & 2)
predict("home is so")

# mixed length models (2 & 1)
predict("i just bought new")

predict("I needsome")


foundIt <- function(expected, actual) {
    out <- FALSE
    for (k in 1:predNum) {
        if (actual[k] == expected) {
            out <- TRUE
            break
        }
    }
    out
}

runTest <- function(model_size = size) {
    attempts <- 0
    correct <- 0
    test_df <- data.frame(x=character(),
                          stringsAsFactors=FALSE)
    con <- file(test_sent_fn, open="rb")
    while (TRUE) {
        line <- readLines(con, encoding="lat", skipNul=TRUE, n=1)
        if (length(line) == 0) {
            break
        }
        adj_size <- model_size
        test_df[1,] <- line
        # Split into tokens
        toks <- test_df %>% unnest_tokens(ngram, x, token="words", to_lower=TRUE) %>% ungroup() %>% na.omit()
        if (nrow(toks) < 2) { next }
        len <- nrow(toks) - model_size + 1
        if (model_size == 1) {   # Just do single tokens
            for (i in 1:len-1) {
                attempts <- attempts + 1
                result <- predict(toks$ngram[i])
                expected <- toks$ngram[i+1]
                print(paste(phrase, ":", expected, "[", paste(result, collapse = ", "), "]"))
                if (foundIt(expected, result)) { correct <- correct + 1 }
            }
        } else {
            if (len < 1) {   # Happens if sentence is shorter than size
                # Just try once with shorter model
                adj_size <- nrow(toks)
                len <- 1
            }
            # Sliding window of phrases to predict next from, from i to j; j+1 is tok we're trying to predict
            for (i in 1:len) {
                j <- i + adj_size - 2
                if (j > nrow(toks)) { break }
                if (j < i) { j <- i }
                for (k in i:j) {
                    if (k == i) { phrase <- toks$ngram[k] }
                    else { phrase <- paste(phrase, toks$ngram[k]) }
                }
                attempts <- attempts + 1
                result <- predict(phrase)
                expected <- toks$ngram[j+1]
                print(paste(phrase, ":", expected, "[", paste(result, collapse = ", "), "]"))
                if (foundIt(expected, result)) { correct <- correct + 1 }
            }
        }
    }
    close(con)

    score <- correct / attempts * 100   # percentage
    print(paste("Percent guessed correctly:", score, "%   (", correct, "out of", attempts, ")"))
}

runTest()   # default to 4

runTest(2)   # 22.07%

runTest(3)   # 28.08%

runTest(4)   # 29.6%
