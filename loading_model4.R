# Johns Hopkins Data Science Specialization
# Coursera Capstone Project
setwd("/Training/DataScience/R/Capstone")
set.seed(6844)
library(plyr)   # for ddply
library(dplyr)   # for %>% and unnesting
library(tidytext)   # for making tidy tables

# Size of model (how long are the phrases?), up to 5
size <- 4   # This includes the prediction word

# How many predicted terms to output, up to 4
predNum <- 3

# Minimum occurrence of a term to be considered
min_occ <- 2

### Sampling
# Use rbinom below to create a random subsample of the data.
pct <- 1   # percent to choose (0-1)
# This was used extensively during exploration and initial model formation.

### Data sources, intermediate files, and sizes ###

# Check for the presence of the data, possibly zipped.
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zipfn <- "Coursera-SwiftKey.zip"

stopfile <- "final/en_US/stopwords.txt"
blogfn <- "final/en_US/en_US.blogs.txt"
newsfn <- "final/en_US/en_US.news.txt"
twitfn <- "final/en_US/en_US.twitter.txt"
phrasefn <- "final/en_US/common_phrases.txt"

tidyblogfn <- "data/tidy_blogs.rda"
tidynewsfn <- "data/tidy_news.rda"
tidytwitfn <- "data/tidy_twitter.rda"
tidyphrasefn <- "data/tidy_phrase.rda"

sentblogfn <- "data/sentblog.txt"
sentnewsfn <- "data/sentnews.txt"
senttwitfn <- "data/senttwit.txt"
sentphrfn <- "data/sentphr.txt"
sent_fn <- "data/sentences.txt"
test_sent_fn <- "data/sentences_test.txt"

allwords_fn <- "data/allwords.txt"
allphrases_fn <- "data/allphrases.txt"

uniqwords_fn <- "data/alluniqwords.txt"
uniqphrases_fn <- "data/alluniqphrases.txt"

predictions_fn <- "data/predictions.txt"

words_fn <- "data/words.rda"
wIDs_fn <- "data/wIDs.rda"
phrases_fn <- "data/phrases.rda"
pIDs_fn <- "data/pIDs.rda"

# Sizes are from "wc -l"
blogsz <-  898288
blogsent <- 2206868   # was 2370408
newssz <- 1010242
newssent <- 1902598   # was 2025757
twitsz <- 2360148
twitsent <- 3476228   # was 3771608
phrasesz <- 1938
phrasesent <- 1942
# wc -l sent_fn => 7822784

numReplacement <- "xxnumxx"


# Make sure the data is available in the current directory.
if (!file.exists(blogfn) || !file.exists(newsfn) || !file.exists(twitfn)) {
    if (!file.exists(zipfn)) {
        # Download the data file
        download.file(fileUrl, zipfn, "curl")
    }
    # Unzip the data file
    unzip(zipfn)
}


# Read stopwords file
#   Could use data("stop_words") {tidytext}
con <- file(stopfile, open="r")
shitlist <- readLines(con)
close(con)



### Function Definitions ###

# Fix some punctuation that messes up the tokenization
cleanText <- function(vec) {
    for (i in 1:length(vec)) {
        line <- iconv(vec[i], "UTF-8", "UTF-8")
        line <-  gsub("[\u201C\u201D\u201E\u201F\u2033\u2036]", '"', line)   # "smart" quotes
        line <-  gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", line)   # "smart" quotes
        line <-  gsub(" [#@]([A-Za-z0-9])", " ", line)   # remove Twitter hashtags, references
        line <-  gsub("__+", " ", line)   # rows of underscores

        line <-  gsub("\\b\\d{1,3}(,\\d{3})*(\\.\\d+)?\\b", numReplacement, line)   # make all numbers the same
        line <-  gsub("\\b[0-9]+([/.][0-9]+)?\\b", numReplacement, line)   # make all numbers the same
        # The slash above is for ratings like "6.5/10" or fractions like "2/3"

        # Anything left that starts with a number, but has letters after
        # TODO: 8-char hexadecimal codes, "1800s", ordinals
        line <-  gsub("\\b[0-9]+(.*)\\b", paste(numReplacement,"\\1"), line)

        # TODO: Could also fix bad sentence endings by removing periods after abbreviations, like "Mr." -> "Mr"

        vec[i] <- line
    }
    out <- tidy(vec)
    out$docId <- as.numeric(rownames(out))
    out
}


# Split by sentences (also lowercases)
sentenceBreak <- function(tbl, sent_con, src_name, debugging=FALSE) {
    len <- nrow(tbl)
    numsent <- 0
    for (i in 1:len) {
        tryCatch({
            # TODO: Maybe break on some profanity, numbers

            # This NLP could be done better with Aspire (https://wiki.searchtechnologies.com/)
            sents <- unnest_tokens(tbl[i,], x, x, token="sentences", to_lower=TRUE) %>% ungroup()
            # Ignore sentences of one word, or sentences with non-printing chars (foreign language chars).
            for (j in nrow(sents):1) {
                if (!grepl(" ", sents$x[j]) || grepl('[^[:print:]]', sents$x[j])) {
                    sents <- sents[-j,]
                }
            }
            numsent <- numsent + nrow(sents)
            # Write all sentences out to file to skip this loop later
            write.table(cbind(source=src_name, sents), sent_con, sep="\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
        },
        # Skip lines with non-UTF-8 chars
        warning = function(w) {},
        error = function(e) {}
        )
        if (i %% 10000 == 0)
            print(paste(format(i, big.mark=","), "of", format(len, big.mark=",")))
    }
    print(paste("Sentence count: ", numsent))
    numsent
}


rawToSentences <- function(sent_con, src_name, base_fn, base_sz, tidy_fn, binary=FALSE) {
    if (file.exists(tidy_fn)) {
        load(file=tidy_fn)
        if (pct < 1.0) {   # sample for speed
            len <- nrow(tidy_set)
            tidy_set <- tidy_set[rbinom(len*pct, len, .5), ]
        }
    } else {
        # Read text lines from plain text file.
        if (binary) {
            con <- file(base_fn, open="rb")   # Must read as binary to avoid "incomplete final line" due to SUB
        } else {
            con <- file(base_fn, open="r")
        }
        # Encoding as UTF-8 leads to tolower errors.
        lines <- readLines(con, base_sz, encoding="lat", skipNul=TRUE)   # character vector
        close(con)   ## Close the connection when we are done

        # Check that full file was read, then reduce if desired.
        len <- length(lines)
        if (len != base_sz)
            print(paste("ERROR!", base_fn, "was read improperly!"))
        if (pct < 1.0)   # sample for speed
            lines <- lines[rbinom(len*pct, len, .5)]

        # Get the token list for the data.
        tidy_set <- cleanText(lines)
        rm(lines)
        save(tidy_set, file=tidy_fn)   # store for later quick loading
    }
    sentences <- sentenceBreak(tidy_set, sent_con, src_name)
    rm(tidy_set)
    sentences
}






### Load All Data Sources ###

# Here are the main calls to
# 1. Read in the various data files
# 2. Normalize numbers
# 3. Split into sentences


# Check if sentence-breaking has already been done.
if (file.exists(sent_fn)) {
    #sentences <- read.table(file=sent_fn, header=TRUE, sep="\t")
} else {
    ## BLOG DATA
    if (!file.exists(sentblogfn)) {
        print("Raw blog data to sentences")
        sent_con <- file(sentblogfn, open="w")
        sentb <- rawToSentences(sent_con, "blog", blogfn, blogsz, tidyblogfn)
        close(sent_con)
    }
    # NEWS DATA
    if (!file.exists(sentnewsfn)) {
        print("Raw news data to sentences")
        sent_con <- file(sentnewsfn, open="w")
        sentn <- rawToSentences(sent_con, "news", newsfn, newssz, tidynewsfn, TRUE)
        close(sent_con)
    }
    # TWITTER DATA
    if (!file.exists(senttwitfn)) {
        print("Raw twitter data to sentences")
        sent_con <- file(senttwitfn, open="w")
        sentt <- rawToSentences(sent_con, "twitter", twitfn, twitsz, tidytwitfn)
        close(sent_con)
    }
    # COMMON PHRASE DATA
    # These are added phrases of common English cliches, found around the Web (see file for sources).
    if (!file.exists(sentphrfn)) {
        print("Raw phrase data to sentences")
        sent_con <- file(sentphrfn, open="w")
        sentp <- rawToSentences(sent_con, "phrases", phrasefn, phrasesz, tidyphrasefn)
        close(sent_con)
    }

    # Re-write individual sentence files into one.
    # OR cat sentblog.txt sentnews.txt senttwit.txt sentphr.txt > sentences.txt
    sent_con <- file(sent_fn, open="w")
    # Sentences file header
    write.table("source\tdocId\tx", sent_con, sep="\t", col.names=FALSE, row.names=FALSE, quote=FALSE)
    con <- file(sentblogfn, open="r")
    lines <- readLines(con, blogsz, encoding="lat", skipNul=TRUE)
    writeLines(lines, sent_con)
    close(con)

    con <- file(sentnewsfn, open="rb")
    lines <- readLines(con, newssz, encoding="lat", skipNul=TRUE)
    writeLines(lines, sent_con)
    close(con)

    con <- file(senttwitfn, open="r")
    lines <- readLines(con, twitsz, encoding="lat", skipNul=TRUE)
    writeLines(lines, sent_con)
    close(con)

    con <- file(sentphrfn, open="r")
    lines <- readLines(con, phrasesz, encoding="lat", skipNul=TRUE)
    writeLines(lines, sent_con)
    close(con)
    close(sent_con)
    rm(lines)
}


# 4. Split into n-grams (up to 'size')
# 5. Remove profanity
# 6. Save to files

# Given a list of terms, cull them and write n-grams to files.
writeTerms <- function(terms, N) {
    # Write all words & phrases into the files
    for (i in 1:nrow(terms)) {
        term <- terms$ngram[i]   # pick out the word/bigram/trigram
        # Remove stopwords/profanity; avoid foreign language characters
        if ((N > 1 || !(term %in% shitlist)) && !grepl('[^[:print:]]', term)) {
            if (N == 1) {
                write(term, words_con, append=TRUE)
                # TODO: Could write just first word here, or save first words to their own file.
            }
            else {
                write(term, phrase_con, append=TRUE)
            }
        }
    }
}


addAllNgrams <- function(s, N) {
    if (N == 1) {
        # Split all text into unigrams, remove punctuation
        cnts <- s %>% unnest_tokens(ngram, x, token="words", to_lower=TRUE) %>% na.omit()
    } else {
        # Find all word pairs/triples
        cnts <- s %>% unnest_tokens(ngram, x, token="ngrams", to_lower=TRUE, collapse=FALSE, n=N) %>% na.omit()
    }

    # Save all words/phrases to files
    if (nrow(cnts) > 0) {
        writeTerms(cnts, N)
        if (N > 1 && s$source == "phrase") {
            # Write twice to give these phrases extra weight
            writeTerms(cnts, N)
        }
    }
}


df <- data.frame(source=character(),
                 docId=integer(),
                 x=character(),
                 stringsAsFactors=FALSE)
sent_con = file(sent_fn, open="rb")   # contains some SUB chars from news
line = readLines(sent_con, n=1)   # header
header <- strsplit(line, split='\t')[[1]]
i <- 0
tcnt <- 0
prev_source <- "none"
print("Breaking sentences into n-grams")
words_con <- file(allwords_fn, open="w")
phrase_con <- file(allphrases_fn, open="w")
testing_con <- file(test_sent_fn, open="w")
while (TRUE) {
    # Lines are: source, doc Id, n-gram (term or phrase)
    line = readLines(sent_con, n=1)   # TODO: Could read in chunks larger than 1
    if (length(line) == 0) {
        break
    }
    i <- i + 1
    df[1,] <- strsplit(line, split='\t')[[1]]
    # Save the first few sentences from each original source for testing
    if (df$source[1] != "phrases" && df$source[1] != prev_source) {
        write(df$x[1], testing_con, append=TRUE)
        tcnt <- tcnt + 1
        if (tcnt == 1000) {
            # Reset for next group
            prev_source <- df$source[1]
            tcnt <- 0
        }
    } else {
        # Do the main model building process
        for (j in 1:size) {
            addAllNgrams(df[1,], j)
        }
    }
    if (i %% 10000 == 0)
        print(format(i, big.mark=","))   # of 7.5M
}
close(sent_con)
close(testing_con)
close(words_con)
close(phrase_con)


# 7. Sort, count, and unique

# Cut down size of single words set
if (file.exists(uniqwords_fn)) {
    eword_set <- read.table(file=uniqwords_fn, header=FALSE)
    colnames(eword_set) <- c("freq", "ew")
    eword_set <- eword_set[eword_set$freq > min_occ, ]   # Remove rare terms
} else {
    # Same as: export LC_ALL=C; sort allwords.txt | uniq -c > alluniqwords.txt
    main_words <- read.delim(file=allwords_fn, sep='\t', header=FALSE)
    sort_words <- as.data.frame(main_words[order(main_words$V1),])
    colnames(sort_words) <- "ew"
    rm(main_words)
    # Get counts along with uniques.
    uniqmains <- ddply(sort_words, .(ew), nrow)
    colnames(uniqmains) <- c("ew", "freq")
    rm(sort_words)
    # Cut down size to items found more than threshold
    eword_set <- uniqmains[uniqmains$freq > (min_occ-1), ]   # -1 because file contained only terms found more than once
    rm(uniqmains)
    # Swap column order to match Unix version
    eword_set <- eword_set[c("freq", "ew")]
    save(eword_set, file=uniqwords_fn)   # store for later quick loading
}


# Cut down size of phrases set
if (file.exists(uniqphrases_fn)) {
    ephrase_set <- read.table(file=uniqphrases_fn, header=FALSE)
    colnames(ephrase_set) <- c("freq", "ew")
    ephrase_set <- ephrase_set[ephrase_set$freq > min_occ, ]   # Remove rare phrases
} else {
    # Except we run out of memory trying this
    #   so do: export LC_ALL=C; sort allphrases.txt | uniq -c > alluniqphrases.txt
    main_phrases <- read.delim(file=allphrases_fn, sep='\t', header=FALSE)
    sort_phrases <- as.data.frame(main_phrases[order(main_phrases$V1),])
    colnames(sort_phrases) <- "ew"
    rm(main_phrases)
    # Get counts along with uniques.
    uniqphrases <- ddply(sort_phrases, .(ew), nrow)
    colnames(uniqphrases) <- c("ew", "freq")
    rm(sort_phrases)
    # Cut down size to items found more than threshold
    ephrase_set <- uniqphrases[uniqphrases$freq > (min_occ-1), ]   # -1 because file contained only terms found more than once
    rm(uniqphrases)
    # Swap column order to match Unix version
    ephrase_set <- ephrase_set[c("freq", "ew")]
    save(ephrase_set, file=uniqphrases_fn)   # store for later quick loading
}


# 8. Find most common next terms

# Keep track of best following terms for a particular phrase.
# Since the sorting might end up with "in the dark", "in the dark alley", "in the jar", we track the different lengths with different data frames.

# Total of 2 terms
follow2 <- data.frame(phrase=character(),
                      next1=character(),   # most common next term
                      freq1=integer(),     # frequency of most common term
                      next2=character(),   # 2nd most common term
                      freq2=integer(),
                      next3=character(),   # 3rd most common term
                      freq3=integer(),
                      next4=character(),   # 4th most common term
                      freq4=integer(),
                      stringsAsFactors=FALSE)

# Total of 3 terms
follow3 <- data.frame(phrase=character(),
                      next1=character(),   # most common next term
                      freq1=integer(),     # frequency of most common term
                      next2=character(),   # 2nd most common term
                      freq2=integer(),
                      next3=character(),   # 3rd most common term
                      freq3=integer(),
                      next4=character(),   # 4th most common term
                      freq4=integer(),
                      stringsAsFactors=FALSE)

# Total of 4 terms
follow4 <- data.frame(phrase=character(),
                      next1=character(),   # most common next term
                      freq1=integer(),     # frequency of most common term
                      next2=character(),   # 2nd most common term
                      freq2=integer(),
                      next3=character(),   # 3rd most common term
                      freq3=integer(),
                      next4=character(),   # 4th most common term
                      freq4=integer(),
                      stringsAsFactors=FALSE)


# Total of 5 terms
follow5 <- data.frame(phrase=character(),
                      next1=character(),   # most common next term
                      freq1=integer(),     # frequency of most common term
                      next2=character(),   # 2nd most common term
                      freq2=integer(),
                      next3=character(),   # 3rd most common term
                      freq3=integer(),
                      next4=character(),   # 4th most common term
                      freq4=integer(),
                      stringsAsFactors=FALSE)

# Shift results down appropriately to make room for the new term
insertAt <- function(N, follow, nextTerm, freq) {
    if (N == 1) {
        follow$next4 <- follow$next3
        follow$freq4 <- follow$freq3
        follow$next3 <- follow$next2
        follow$freq3 <- follow$freq2
        follow$next2 <- follow$next1
        follow$freq2 <- follow$freq1
        follow$next1 <- nextTerm
        follow$freq1 <- freq
    } else if (N == 2) {
        follow$next4 <- follow$next3
        follow$freq4 <- follow$freq3
        follow$next3 <- follow$next2
        follow$freq3 <- follow$freq2
        follow$next2 <- nextTerm
        follow$freq2 <- freq
    } else if (N == 3) {
        follow$next4 <- follow$next3
        follow$freq4 <- follow$freq3
        follow$next3 <- nextTerm
        follow$freq3 <- freq
    } else if (N == 4) {
        follow$next4 <- nextTerm
        follow$freq4 <- freq
    }
    follow
}

# Key is phrase , keep track of top 3 most common following terms
insertPrediction <- function(follow, phrase, nextTerm, freq, pred_con) {
    if (nrow(follow) == 0) {   # Starting anew
        follow <- data.frame(phrase, next1=nextTerm, freq1=freq, next2="", freq2=0, next3="", freq3=0, next4="", freq4=0)
    } else if (follow$phrase != phrase) {   # Done with this phrase
        # Write out
        write.table(follow, file=pred_con, sep="\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
        # Start over with new phrase
        follow <- data.frame(phrase, next1=nextTerm, freq1=freq, next2="", freq2=0, next3="", freq3=0, next4="", freq4=0)
    } else if (freq > follow$freq1) {   # new most common
        follow <- insertAt(1, follow, nextTerm, freq)
    } else if (freq > follow$freq2) {   # new 2nd most common
        follow <- insertAt(2, follow, nextTerm, freq)
    } else if (freq > follow$freq3) {   # new 3rd most common
        follow <- insertAt(3, follow, nextTerm, freq)
    } else if (freq > follow$freq4) {   # new 4th most common
        follow <- insertAt(4, follow, nextTerm, freq)
    }
    # else this is not common enough, so ignore
    follow
}

# Read file of phrases with frequencies, line-by-line
upcon = file(uniqphrases_fn, open="rb")   # contains some SUB chars from news
pred_con = file(predictions_fn, open="w")   # main output file
i <- 0
while (TRUE) {
    line = readLines(upcon, n=1)   # TODO: Could read in chunks larger than 1
    if (length(line) == 0) {
        break
    }
    i <- i + 1
    # Lines are: count w1 w2 [w3 [w4]]
    toks <- strsplit(line, split="\\s+")[[1]]
    if (toks[1] == "")
        toks <- toks[-1]

    len <- length(toks)
    # Form frequency (first column), following term (last column), and phrase (concatenation of rest of columns)
    freq <- strtoi(toks[1])
    # Drop line if freq is too low
    if (freq < min_occ || len < 3)
        next
    nextTerm <- toks[len]
    phrase <- toks[2]
    phraselen <- 2
    if (len > 3) {
        phrase <- paste(phrase, toks[3])
        phraselen <- phraselen + 1
    }
    if (len > 4) {
        phrase <- paste(phrase, toks[4])
        phraselen <- phraselen + 1
    }
    if (len > 5) {
        phrase <- paste(phrase, toks[5])
        phraselen <- phraselen + 1
    }

    if (phraselen == 2) {
        follow2 <- insertPrediction(follow2, phrase, nextTerm, freq, pred_con)
    } else if (phraselen == 3) {
        follow3 <- insertPrediction(follow3, phrase, nextTerm, freq, pred_con)
    } else if (phraselen == 4) {
        follow4 <- insertPrediction(follow4, phrase, nextTerm, freq, pred_con)
    } else if (phraselen == 5) {
        follow5 <- insertPrediction(follow5, phrase, nextTerm, freq, pred_con)
    }

    if (i %% 10000 == 0)   # size 3: of 53,117,576 ; size 4: of 108,853,848
        print(format(i, big.mark=","))
}
# Write the last phrases
if (nrow(follow2) != 0) {
    write.table(follow2, file=pred_con, sep="\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
}
if (nrow(follow3) != 0) {
    write.table(follow3, file=pred_con, sep="\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
}
if (nrow(follow4) != 0) {
    write.table(follow4, file=pred_con, sep="\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
}
if (nrow(follow5) != 0) {
    write.table(follow5, file=pred_con, sep="\t", append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
}

close(upcon)
close(pred_con)


# TODO: use uniqfirstwords.txt to predict in unknown cases




# 9. Make encoding maps to switch from strings to integers

# Load all predictions
cc <- c("character", "character", "integer", "character", "integer", "character", "integer", "character", "integer")
preds <- read.table(file=predictions_fn, sep="\t", header=FALSE, quote="", colClasses=cc)
colnames(preds) <- c("phrase", "next1", "freq1", "next2", "freq2", "next3", "freq3", "next4", "freq4")
object.size(preds)
# size 198736856 bytes = 189 MB
# size 536518912 bytes = 511 MB

# Form encoding hashmaps for phrases and terms
phraseID <- as.integer(1)
library(hashmap)
# Allocate and seed the phrases-to-ints hash table
term <- preds$phrase[1]
phrases <- hashmap(term, phraseID)
# And the reverse lookup tables.
pIDs <- hashmap(phraseID, term)

# Encode all words as integers
# This function gives the encoding regardless of whether the term has been seen before.
# N == 1 means a single word; N > 1 means a phrase.
# If the building argument is TRUE, it builds the hash table as it is used, if necessary.
encodeWord <- function(term, building = FALSE) {
    out <- phrases[[term]]
    if (is.na(out) && building) {
        # Add term to hashmap
        phraseID <<- phraseID + 1
        phrases[[term]] <- phraseID
        pIDs[[phraseID]] <- term
        out <- phraseID
    }
    out
}

# This loop just builds the hashmap so that we can get the size.
# Note that the number of the row must be the encoding of the term/phrase for that row.
# This loop must start the encoding to make that true.
len <- nrow(preds)
for (i in 1:len) {
    term <- preds$phrase[i]
    encodeWord(term, TRUE)
}
for (i in 1:len) {
    term <- preds[i,]$next1
    encodeWord(term, TRUE)
    term <- preds[i,]$next2
    if (!is.null(term) && term != "")
        encodeWord(term, TRUE)
    term <- preds[i,]$next3
    if (!is.null(term) && term != "")
        encodeWord(term, TRUE)
    term <- preds[i,]$next4
    if (!is.null(term) && term != "")
        encodeWord(term, TRUE)
    if (i %% 10000 == 0)   # size 4: of 4,436,049
        print(paste(format(i, big.mark=","), "of", format(len, big.mark=",")))
}
# These 3 should all be the same:
phraseID
phrases$size()
pIDs$size()

save_hashmap(phrases, file=phrases_fn)   # store for later quick loading
save_hashmap(pIDs, file=pIDs_fn)   # store for later quick loading
