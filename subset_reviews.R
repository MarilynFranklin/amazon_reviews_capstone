library(dplyr)
library(data.table)
library(syuzhet)
library(qdap)

reviews <- fread("Reviews.csv", header = TRUE, stringsAsFactors=FALSE)
reviews <- data.frame(reviews)

categorize_review_helpfulness <- function(helpfulness_ratio) {
  if  (helpfulness_ratio > .5) {
    return ("helpful")
  } else {
    return ("unhelpful")
  }
}

categorize_review <- function(score) {
  if  (score > 3) {
    return ("positive")
  } else if (score < 3) {
    return ("negative")
  } else {
    return ("neutral")
  }
}

# Remove unrated reviews
reviews <- reviews %>% filter(HelpfulnessDenominator > 0)

# Add helpfulness_ratio
reviews <- reviews %>%
  mutate(helpfulness_ratio = HelpfulnessNumerator/HelpfulnessDenominator)

helpful_reviews <- reviews %>%
  filter(helpfulness_ratio > .5) %>%
  sample_n(10000)

unhelpful_reviews <- reviews %>%
  filter(helpfulness_ratio < .5) %>%
  sample_n(10000)

reviews <- rbind(helpful_reviews, unhelpful_reviews)

# Add unhelpful_count
reviews <- reviews %>%
  mutate(unhelpful_count = HelpfulnessDenominator - HelpfulnessNumerator)

# Add is_helpful and is_unhelpful
reviews <- reviews %>%
  mutate(is_helpful = as.numeric(helpfulness_ratio > .5)) %>%
  mutate(is_unhelpful = as.numeric(helpfulness_ratio < .5))

# Add is_high_review, is_low_review, and is_neutral_review
reviews <- reviews %>%
  mutate(is_high_review = as.numeric(Score > 3)) %>%
  mutate(is_low_review = as.numeric(Score < 3)) %>%
  mutate(is_neutral_review = as.numeric(Score == 3))

# Add review length
reviews <- reviews %>%
  mutate(review_length = nchar(Text))

reviews <- reviews %>%
  mutate(helpful_category = sapply(helpfulness_ratio, categorize_review_helpfulness))

reviews <- reviews %>%
  mutate(review_category = sapply(Score, categorize_review))

# Add Automated readability index
# https://en.wikipedia.org/wiki/Automated_readability_index

# Add person
reviews <- reviews %>% mutate(person = Id)

# Split sentences
sentences <- data.table(sentSplit(reviews, "Text"))

# Calculate readability_index
readability <- automated_readability_index(sentences$Text,sentences$person)
readability <- readability$Readability %>%
  select(person, Automated_Readability_Index)

reviews <- left_join(reviews, readability, by = "person")

with_sentences <- lapply(reviews$Text, get_sentences)

# Calculate syuzhet sentiment
with_sentences <- lapply(reviews$Text, get_sentences)
with_sentiment <- lapply(with_sentences, get_sentiment, method="syuzhet")
sum_sentiment <- lapply(with_sentiment, function(x) sum(x))
reviews$sentiment_score <- unlist(sum_sentiment)

# Calculate NRC sentiment
with_sentences <- lapply(reviews$Text, get_sentences)
with_nrc_sentiment <- lapply(with_sentences, get_nrc_sentiment)
sums <- lapply(with_nrc_sentiment, colSums)
sums_list_df <- lapply(sums, data.frame)
sums_list_t_m <- lapply(sums_list_df, t)
sums_list_t_df <- lapply(sums_list_t_m, data.frame)
nrc_df <- do.call(rbind.data.frame, sums_list_t_df)
nrc_df$Id <- reviews$Id
reviews <- left_join(reviews, nrc_df, by = "Id")

write.csv(reviews, "ScoredReviews.csv")
