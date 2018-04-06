library(dplyr)
library(janeaustenr)

d <- data_frame(txt = prideprejudice)
d

# dには、各行に文章が入っているし、空白の行もある。
# unnest_tokensを使うと、文章を単語に分けて、一行一単語にする。しかも空白行は削除。
# 今回はtxtという列名の文章を、一行一単語に分解している。且、wordという列名に変更。
d %>%
  unnest_tokens(word, txt)

d2<- d %>% 
  unnest_tokens(word, txt)

# dは結果を出力しているだけだが、d2にはunnest_tokensの結果を代入している。
# 行数が10倍になっているのでわかるかも。
nrow(d)
nrow(d2)
View(d)
View(d2)

# token="sentence"にすることで、文章を「ピリオド・！・？」区切りにする。そして、列名をtxtからsentenceに変更。
# だから「mr.」とか変な感じになってる。
d %>%
  unnest_tokens(sentence, txt, token = "sentences")
d3 <- d %>%
  unnest_tokens(sentence, txt, token = "sentences")

# バイグラム。
d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)
d4 <- d %>%
  unnest_tokens(ngram, txt, token = "ngrams", n = 2)


# 1グラム毎に、2単語スキップする4グラム
# 例：「pride and prejudice by jane austen chapter 1 it is a truth」なら、
# まず1つ目のグラムは一単語目の「pride」。そのあと2単語（andとprejudice）スキップして、
# 2つ目のグラムは「by」。そして、2単語（janeとausten）をスキップして、
# 3つ目のグラムは「chapter」。そして、2単語（1とit）をスキップして、
# 4つ目のグラムは「is」。
# 「pride and prejudice by jane austen chapter 1 it is a truth」の、2skip_4gramは、「pride by chapter is」。
d %>%
  unnest_tokens(ngram, txt, token = "skip_ngrams", n = 4, k = 2)
d5 <- d %>%
  unnest_tokens(ngram, txt, token = "skip_ngrams", n = 4, k = 2)


############################# regexはよー分からん。
# [\\d]は「1|2|3|4|5|6|7|8|9|0」の略。今回屋と、「1チャプター1行」
# だから1行目は、「chapter 1」以前の文章が入ってる。
# 2行目は、「chapter 1」と「chapter 2」の間の文章が入ってる。
# 3行目は、「chapter 2」と「chapter 3」の間の文章が入ってる。4行目以降も同じ感じで続く。
# でも「chapter [\\d]」の部分は削除される。だから、11行目以降は、数字が1つ入ってるけど、
# これは、チャプター数が二桁になってて一桁目だけ削除されずに残ったから
d %>%
  unnest_tokens(chapter, txt, token = "regex", pattern = "Chapter [\\d]")
d6 <- d %>%
  unnest_tokens(chapter, txt, token = "regex", pattern = "Chapter [\\d]")


# 引数の詳しいことは良く分からんかったけど、
# とりあえず、「一行一単語」、且、「空白の行もそのまま残す」ってこと。
d %>%
  unnest_tokens(word, txt, token = stringr::str_split, pattern = " ")
d7 <- d %>%
  unnest_tokens(word, txt, token = stringr::str_split, pattern = " ")
# d2は「一行一単語、且、空白削除」
# d7は、空白残してる。d2より行数が多いことからも確認できる。
nrow(d2)
nrow(d7)





#######################################################################
############################ワケワカメ###################
# tokenize HTML
h <- data_frame(row = 1:2,
                text = c("<h1>Text <b>is</b>", "<a href='example.com'>here</a>"))

aaa <- h %>%
  unnest_tokens(word, text, format = "html")
######################################################################
