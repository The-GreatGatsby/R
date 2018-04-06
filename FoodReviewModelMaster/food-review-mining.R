
# install.packages("tidytext")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("ggplot2")

library(tidytext)
library(dplyr)
library(readr)
library(ggplot2)

#Setting project directory
getwd
setwd("~/food-review-model")

#Loading dataset
reviews <- read_csv("food.csv")
reviews <- reviews %>%
  mutate(nr = row_number())

head(reviews)

#Splitting data into training and testing sets
half <- nrow(reviews) %/% 2
trainSet <- reviews %>% slice(1:half)
testSet <- reviews %>% slice((half + 1):n())

#Analizing words in Summary
# selectは列を抽出するだけ。
# unnest_tokensは、「token」引数を指定しなければ、デフォルトで、一行一単語に分解する。
# Summaryに入っている文章を一行単語に分解して、列名をwordに変更。
descWords <- trainSet %>%
  select(nr, Summary) %>%
  unnest_tokens(word, Summary)

#Structure
descWords %>%
  count(word, sort = T) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() + 
  xlab(NULL) +
  ylab("liczba wystﾄ?pieﾅ?") +
  coord_flip()



# descWordsには、単語に分解されたデータが入っている。
# count関数で、descWordのword列のそれぞれの単語頻度を集計。
# 「sort=T」は頻度高い順に並び替える。
aaa <- descWords %>% count(word, sort = T) 
# countで、頻度の列名はnになってる。
# n（頻度）が201以上のみ選択。
aaa %<>% filter(n > 200)

##############################
# reorderで、wordを頻度順にしてる？
# 並び替えたデータを、word列に代入？
aaa %<>% mutate(word = reorder(word, n))

# x軸を単語、y軸を頻度
# xラベルなし
# yラベルは「Frequency」
aaa %>% ggplot(aes(x = word, y = n)) +
  geom_col() +   #これないと、グラフの棒の部分が出力されへん。
  xlab(NULL) +
  ylab("Frequency") +
  coord_flip()  #これないと、縦棒グラフになるし、単語見えへん。

















#Sentiment analysis

#lexicon
#(-5 - very negative, 5 - very positive)
afinn <- get_sentiments("afinn")
head(afinn)

#Including the impact of word "not"
descWords <- descWords %>%
  mutate(not = ifelse(word == "not", -1, 1))

for(i in rev(2:nrow(descWords))){
  descWords$not[i] = descWords$not[i-1]
}

descSentiment <- descWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment*not))

descSentiment2 <- descWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment))

descSentiment %>% left_join(reviews) %>%
  ggplot(aes(x=Score, y=sum)) +
  geom_point(alpha = .1) +
  geom_smooth(method="lm")

descSentiment %>% left_join(reviews) %>%
  group_by(Score) %>%
  summarise(mean = mean(sum)) %>%
  ggplot(aes(x=Score, y=mean)) +
  geom_point()





#Same for full text of review  
textWords <- trainSet %>%
  select(nr, Text) %>%
  unnest_tokens(word, Text)

textWords <- textWords %>%
  mutate(not = ifelse(word == "not", -1, 1))

for(i in rev(2:nrow(textWords))){
  textWords$not[i] = textWords$not[i-1]
}

textSentiment <- textWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment*not))

textSentiment %>% left_join(reviews) %>%
  ggplot(aes(x=Score, y=sum)) +
  geom_point(alpha = .1) +
  geom_smooth(method="lm")

textSentiment %>% left_join(reviews) %>%
  group_by(Score) %>%
  summarise(mean = mean(sum)) %>%
  ggplot(aes(x=Score, y=mean)) +
  geom_point()


#Punctuation
?regex
descPunct <- reviews %>%
  mutate(excl = str_detect(Summary, pattern="[!]"),
  quest = str_detect(Summary, pattern="[?]"),
  ellip = str_detect(Summary, pattern="[.]{3}"))


descPunct %>%
  ggplot(aes(x=excl, y=Score)) +
  geom_boxplot()
  
