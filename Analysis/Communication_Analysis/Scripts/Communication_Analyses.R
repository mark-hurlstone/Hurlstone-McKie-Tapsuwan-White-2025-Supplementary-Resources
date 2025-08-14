# ------------------------------------------------------------------------------
# Text Analysis of Communication Data 
# - Preprocess text (lowercase, remove stopwords, custom stopwords, stemming)
# - Word frequency analysis and wordcloud
# - Word associations
# - Sentiment analysis
# - Emotion proportion plot 
#
# Input: AllCommunication.rtf 
# Output: Top word frequencies, wordcloud, associations, sentiment summaries, 
# emotion plot
# ------------------------------------------------------------------------------

# Load libraries
library(tidytext)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. Load and Preprocess Text
# ------------------------------------------------------------------------------

# Specify path to the communication text file
text_path <- "AllCommunication.rtf" 
text <- readLines(text_path, warn = FALSE)

# Create corpus
TextDoc <- Corpus(VectorSource(text))

# Replace certain characters with spaces
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

# Convert to lowercase
TextDoc <- tm_map(TextDoc, content_transformer(tolower))

# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)

# Remove English stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))

# Remove custom domain-specific stopwords
TextDoc <- tm_map(TextDoc, removeWords, c("portia","triton","sinope","leda", "yeah"))

# Remove punctuation
TextDoc <- tm_map(TextDoc, removePunctuation)

# Remove extra whitespace
TextDoc <- tm_map(TextDoc, stripWhitespace)

# Apply stemming
TextDoc <- tm_map(TextDoc, stemDocument)

# ------------------------------------------------------------------------------
# 2. Word Frequency Analysis
# ------------------------------------------------------------------------------

# Build term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)

# Convert to matrix and sort by frequency
dtm_m <- as.matrix(TextDoc_dtm)
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)

# Display top 5 words
head(dtm_d, 5)

# Plot top 5 most frequent words
barplot(dtm_d[1:5, ]$freq, las = 2, names.arg = dtm_d[1:5, ]$word,
        col = "lightgreen", main = "Top 5 Most Frequent Words",
        ylab = "Word Frequencies")

# Wordcloud (top 100 words)
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words = 100, random.order = FALSE, rot.per = 0.40,
          colors = brewer.pal(8, "Dark2"))

# ------------------------------------------------------------------------------
# 3. Word Associations
# ------------------------------------------------------------------------------

# Find associations for specific key words
findAssocs(TextDoc_dtm, terms = c("protect","risk","round","good","yes"), corlimit = 0.2)

# Find associations for frequently occurring words (≥50 occurrences)
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 50), corlimit = 0.25)

# ------------------------------------------------------------------------------
# 4. Sentiment Analysis
# ------------------------------------------------------------------------------

# Sentiment scores using different methods
syuzhet_vector <- get_sentiment(text, method = "syuzhet")
bing_vector    <- get_sentiment(text, method = "bing")
afinn_vector   <- get_sentiment(text, method = "afinn")

# Summaries
summary(syuzhet_vector)
summary(bing_vector)
summary(afinn_vector)

# Compare sentiment sign (first few entries)
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)

# NRC sentiment classification (categorical emotions)
d <- get_nrc_sentiment(text)
head(d, 10)

# ------------------------------------------------------------------------------
# 5. Emotion Proportion Plot (ggplot2)
# ------------------------------------------------------------------------------

# Aggregate NRC results
td <- data.frame(t(d))
td_new <- data.frame(rowSums(td[2:ncol(td)]))
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL

# Filter to first 8 emotions
emotion_df <- td_new[1:8, ]
emotion_df$sentiment <- factor(
  emotion_df$sentiment,
  levels = c("disgust", "sadness", "anger", "surprise", "fear", "joy", "trust", "anticipation")
)

# Convert counts to proportions
emotion_df <- emotion_df %>%
  mutate(Proportion = count / sum(count),
         Header = "Emotions in Communication")

# Plot proportions
ggplot(emotion_df, aes(x = sentiment, y = Proportion, fill = sentiment)) +
  geom_col(width = 0.55, position = position_dodge(0.55)) +
  scale_y_continuous(limits = c(0, 0.3), expand = c(0, 0), breaks = seq(0, 0.3, by = 0.1)) +
  scale_fill_manual(values = rep("#355C7D", 8)) +
  facet_wrap(~Header) +
  geom_hline(yintercept = 0) +
  labs(x = "Emotion", y = "Proportion") +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12, color = "black"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.position = "none")

# Save plot
ggsave("Emotions.pdf", width = 8, height = 5)
