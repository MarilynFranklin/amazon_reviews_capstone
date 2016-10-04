library(reshape2)
library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(tm)
library(qdap)
library(wordcloud)

qdap_clean <- function(x) {
  x <- replace_abbreviation(x)
  x <- replace_contraction(x)
  x <- replace_ordinal(x)
  x <- replace_symbol(x)
  x <- tolower(x)
  return(x)
}

reviews <- fread("ScoredReviews.csv", header = TRUE, stringsAsFactors = FALSE)

reviews_with_sentiment <- reviews %>% select(anger, anticipation, disgust, fear,
                                             joy, sadness, surprise, trust,
                                             negative, positive, is_helpful, Id)
melted <- melt(reviews_with_sentiment, id.vars=c("Id", "is_helpful"))
means <- ddply(melted, c("is_helpful", "variable"), summarise, mean=mean(value))

ggplot(data = means, aes(x = factor(is_helpful), y = mean)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = variable))

helpful_reviews <- reviews %>% filter(is_helpful == 1)
unhelpful_reviews <- reviews %>% filter(is_helpful == 0)

# Clean text
helpful   <- qdap_clean(helpful_reviews$Text)
unhelpful <- qdap_clean(unhelpful_reviews$Text)

my_stopwords <- c(stopwords('english'))

helpful.corpus <- Corpus(VectorSource(helpful))
helpful.corpus <- tm_map(helpful.corpus, removePunctuation)
helpful.corpus <- tm_map(helpful.corpus, removeWords, my_stopwords)
helpful.dtm <- TermDocumentMatrix(helpful.corpus)
helpful.dtm2 <- removeSparseTerms(helpful.dtm, sparse=0.95)
helpful.df <- as.data.frame(inspect(helpful.dtm2))
helpful.df.scale <- scale(helpful.df)
helpful_d <- dist(helpful.df.scale, method = "euclidean")
helpful_fit <- hclust(helpful_d, method="ward.D")

unhelpful.corpus <- Corpus(VectorSource(unhelpful))
unhelpful.corpus <- tm_map(unhelpful.corpus, removePunctuation)
unhelpful.corpus <- tm_map(unhelpful.corpus, removeWords, my_stopwords)
unhelpful.dtm <- TermDocumentMatrix(unhelpful.corpus)
unhelpful.dtm2 <- removeSparseTerms(unhelpful.dtm, sparse=0.95)
unhelpful.df <- as.data.frame(inspect(unhelpful.dtm2))
unhelpful.df.scale <- scale(unhelpful.df)
unhelpful_d <- dist(unhelpful.df.scale, method = "euclidean")
unhelpful_fit <- hclust(unhelpful_d, method="ward.D")


# plot reviews
plot(unhelpful_fit)
groups <- cutree(unhelpful_fit, k=5)
rect.hclust(unhelpful_fit, k=5, border="red")

plot(helpful_fit)
groups <- cutree(helpful_fit, k=5)
rect.hclust(helpful_fit, k=5, border="red")

# Remove words
my_stopwords <- c(stopwords('english'), 'amazon', 'like', 'coffee', 'taste')
helpful.corpus <- tm_map(helpful.corpus, removeWords, my_stopwords)
unhelpful.corpus <- tm_map(unhelpful.corpus, removeWords, my_stopwords)

# Create TDM
helpful.dtm <- TermDocumentMatrix(helpful.corpus)
unhelpful.dtm <- TermDocumentMatrix(unhelpful.corpus)

# Create matrix
helpful_matrix   <- as.matrix(helpful.dtm)
unhelpful_matrix <- as.matrix(unhelpful.dtm)

# Calculate frequency
helpful_frequency   <- rowSums(helpful_matrix)
unhelpful_frequency <- rowSums(unhelpful_matrix)

# Plot wordcloud
wordcloud(names(helpful_frequency),
          helpful_frequency,
          max.words = 25,
          color = "blue")

wordcloud(names(unhelpful_frequency),
          unhelpful_frequency,
          max.words = 25,
          color = "blue")
