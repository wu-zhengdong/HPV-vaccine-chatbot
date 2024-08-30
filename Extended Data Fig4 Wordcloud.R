library(jsonlite)
library(wordcloud2)
library(wordcloud)
library(RColorBrewer)
library(readxl)

word_freq_CN <- read_excel("./CN_word_cloud_freq_df_20240709.xlsx")
word_freq_EN <- read_excel("./EN_word_cloud_freq_df_20240709.xlsx")

# Chinese version
set.seed(0)
png(filename = "./wordcloud_CN_0709.png", width = 1150, height = 1150, res=300)
par(mar = c(0, 0, 0, 0))
wordcloud(
  words = word_freq_CN$word, 
  freq = word_freq_CN$freq, 
  min.freq = 1, 
  max.words = 200,
  random.order = FALSE, 
  rot.per = 0.35, 
  scale = c(4, 0.5),
  colors = brewer.pal(8, "Dark2")
)
dev.off()

# English version
set.seed(0)
png(filename = "./wordcloud_EN_0709.png", width = 1500, height = 1500, res=300)
par(mar = c(0, 0, 0, 0))
wordcloud(
  words = word_freq_EN$word, 
  freq = word_freq_EN$freq, 
  min.freq = 1, 
  max.words = 200,
  random.order = FALSE, 
  rot.per = 0.35, 
  scale = c(4, 0.5),
  colors = brewer.pal(8, "Dark2")
)
dev.off()
