library(dplyr)
library(data.table)
library(textcat)

reviews <- fread("Reviews.csv", header = TRUE, stringsAsFactors = FALSE)
reviews <- data.frame(reviews)

# Remove unrated reviews
reviews <- reviews %>% filter(HelpfulnessDenominator > 0)

# Add helpfulness_ratio
reviews <- reviews %>%
  mutate(helpfulness_ratio = HelpfulnessNumerator/HelpfulnessDenominator)

helpful_reviews <- reviews %>%
  filter(helpfulness_ratio > .5) %>%
  sample_n(30000)

# Remove reviews that aren't in English
helpful_reviews$language <- as.factor(textcat(helpful_reviews$Text))
helpful_reviews <- helpful_reviews %>%
  filter(language == "english") %>%
  sample_n(10000)


unhelpful_reviews <- reviews %>%
  filter(helpfulness_ratio < .5) %>%
  sample_n(30000)
# Remove reviews that aren't in English
unhelpful_reviews$language <- as.factor(textcat(unhelpful_reviews$Text))
unhelpful_reviews <- unhelpful_reviews %>%
  filter(language == "english") %>%
  sample_n(10000)

reviews <- rbind(helpful_reviews, unhelpful_reviews)

write.csv(reviews, "ReviewSubset.csv")
