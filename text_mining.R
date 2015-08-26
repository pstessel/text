# https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html

setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/text_mining")

rm(list=ls(all = TRUE))

library(tm)
library(wordcloud)

cname <- file.path(".", "corpus", "txt")
cname
length(dir(cname))
dir(cname)

# load texts into R; uses tm
docs <- Corpus(DirSource(cname))
summary(docs)

inspect(docs[2])

### PRE-PROCESSING ###

# remove punctuation
docs <- tm_map(docs, removePunctuation)
inspect(docs[3])

# remove special characters
for(j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
}
inspect(docs)

# remove numbers
docs <- tm_map(docs, removeNumbers)

# convert to lowercase
docs <- tm_map(docs, tolower)
inspect(docs)

# remove stopwords
length(stopwords("english"))
stopwords("english")

# remove particular words

docs <- tm_map(docs, removeWords, c("the", "and", "that", "for", "intermountain", "data"))
# Just replace "department" and "email" with words that you would like to
# remove.

# Combining words that should stay together If you wish to preserve a concept is
# only apparent as a collection of two or more words, then you can combine them
# or reduce them to a meaningful acronym before you begin the analysis. Here, I
# am using examples that are particular to qualitative data analysis.
#
# for (j in seq(docs)) { docs[[j]] <- gsub("qualitative research", "QDA",
# docs[[j]]) docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
# docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]]) docs[[j]] <-
# gsub("research methods", "research_methods", docs[[j]])
}

# Removing common word endings (e.g., “ing”, “es”, “s”) This is referred to as
# “stemming” documents. We stem the documents so that a word will be
# recognizable to the computer, despite whether or not it may have a variety of
# possible endings in the original text.

library(SnowballC)
docs <- tm_map(docs, stemDocument)
inspect(docs[3]) # Check to see if it worked.

# Stripping unnecesary whitespace from your documents: The above preprocessing
# will leave the documents with a lot of “white space”. White space is the
# result of all the left over spaces that were not removed along with the words
# that were deleted. The white space can, and should, be removed.

docs <- tm_map(docs, stripWhitespace)
inspect(docs[3]) # Check to see if it worked.

# To Finish Be sure to use the following script once you have completed
# preprocessing. This tells R to treat your preprocessed documents as text
# documents.

docs <- tm_map(docs, PlainTextDocument)

### END OF PREPROCESSING ###


# STAGE THE DATA ----------------------------------------------------------

# To proceed, create a document term matrix. This is what you will be using from this point on.

dtm <- DocumentTermMatrix(docs)
inspect(dtm)

inspect(dtm[1:5, 1:20]) # view first 5 docs & first 20 terms - modify as you like
dim(dtm) # This will display the number of documents & terms (in that order)

# Create transpose of matrix
tdm <- TermDocumentMatrix(docs)
tdm


### EXPLORE DATA ------------------------------------------------------------

# Organize terms by their frequency

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)

# If you prefer to export the matrix to Excel:
m <- as.matrix(dtm)
dim(m)
write.csv(m, file="dtm.csv")

## Focus

# Remove sparse terms
dtms <- removeSparseTerms(dtm, 0.1) # Creates a matrix with max. 10% empty space
inspect(dtms)

# Word frequency
freq[head(ord)]
freq[tail(ord)]

# Frequency of frequencies
head(table(freq), 20)

# 20 lowest word frequencies
tail(table(freq), 20)

# For a less, fine-grained look at term freqency we can view a table of the
# terms we selected when we removed sparse terms, above. (Look just under the
# word “Focus”.)

freq <- colSums(as.matrix(dtms))
freq

# The above matrix was created using a data transformation we made earlier. What
# follows is an alternative that will accomplish essentially the same thing.

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)

head(freq, 14)

# This will identify all terms that appear frequently (in this case, 50 or more
# times).

findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your text data.

wf <- data.frame(word=names(freq), freq=freq)
head(wf)

## Plot Word Frequencies

# Plot words that appear at least 50 times

library(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

## Relationships Between Terms

# If you have a term in mind that you have found to be particularly meaningful
# to your analysis, then you may find it helpful to identify the words that most
# highly correlate with that term.
#
# If words always appear together, then correlation=1.0.

findAssocs(dtm, c("health" , "intermountain"), corlimit=0.85) # specifying a correlation limit of 0.98

## Word Clouds
library(wordcloud)
set.seed(142)
wordcloud(names(freq), freq, min.freq = 25)

# Plot the 100 most frequently used words.
set.seed(142)
wordcloud(names(freq), freq, max.words = 100)

# Add color and plot words occuring at least 20 times
set.seed(142)
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

