
# install.packages("tidytext")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("ggplot2")

library(tidytext)
library(dplyr)
library(readr)
library(ggplot2)
library(magrittr)
library(stringr)

#Setting project directory
setwd("C:/Users/rstud/Documents/GitHub/R/FoodReviewModelMaster")

#Loading dataset
reviews <- read_csv("food.csv")
# nrという列名を作って、そこに列番号を代入する。
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
############### lexicon（語彙目録）
# afinnという語彙目録では、様々な感情的単語に‐5から5までで得点付け。計量化。
# 語彙目録はafinn以外に、"bing”, “nrc”, "loughran"がある。
#(-5 - very negative, 5 - very positive)
afinn <- get_sentiments("afinn")
#具体的にどんな単語が何点を付けられているかをチェック。
head(afinn)

# nrcは感情を簡単な単語に一般化。
nnn <- get_sentiments("nrc")
head(nnn,20)

# bingは、positiveとnegativeの二択化。
bbb <- get_sentiments("bing")
head(bbb,20)

#loughran語彙目録は、
# negative, positive, uncertainty, litigious, constraining, superfluousの5つに分類された
lll <- get_sentiments("loughran")
head(lll,20)

#Including the impact of word "not"
# notという列名を作り、wordがnotなら‐1、それ以外は1を入れる。
descWords <- descWords %>%
  mutate(not = ifelse(word == "not", -1, 1))

# not列の得点を1行上にずらす。
for(i in rev(2:nrow(descWords))){
  descWords$not[i] = descWords$not[i-1]
}

# descWordsにある全ての単語を‐5から5で評価。
# inner_joinを使ってるので、descWordsの単語の内、afinnと被ってる単語のみ残る。
descSentiment <- descWords %>% inner_join(afinn)
# score列名をsentimentに変更
descSentiment %<>% rename(sentiment = score)
# nrは、その単語が最初のデータ（trainSet）の何行目にあるか。だからtrainSetと行数が一行違い。
# sentimentとnotをかけた数値を代入し、nrが同じならそれらの数値を合算し、sum列に代入。
# これで、何行目のレビューは何点かを知れる。
descSentiment %<>% group_by(nr) %>% summarise(sum = sum(sentiment*not))

# sentimentとnotをかけない版
descSentiment2 <- descWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment))

# descWordsのデータはそのままで、descWords(left側)と一致するreviewsのみ追加。
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



#Punctuationがあれば、TRUE・FALSE付ける。
descPunct <- reviews %>%
  mutate(excl = str_detect(Summary, pattern="[!]"),
  quest = str_detect(Summary, pattern="[?]"),
  ellip = str_detect(Summary, pattern="[.]{3}"))

descPunct %>%
  ggplot(aes(x=excl, y=Score)) +
  geom_boxplot()

