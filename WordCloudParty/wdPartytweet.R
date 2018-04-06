#あいうえお
# 各政党のツイッターアカウント。
# 各政党の発言内容の違いを、ワードクラウドで可視化できるか？


library(rtweet)
library(tm)
library(RMeCab)
library(dplyr)
library(purrr)
library(magrittr)
library(randomForest)
library(rpart)
library(kernlab)
library(stringr)
library(rvest)
library("rpart.plot")
library("wordcloud")
library("twitteR")
library(ROAuth)

consumerKey <- "hQQ8mlkFhG4UoUV67xTwIbQTu"
consumerSecret <- "dd2ldZBbaU6wJS3ElV5xQozZg1SjuVJUgITlA9Cc8K6cub59Xo"
accessToken <- "898910528730406912-zKbDnTeda1pHuQvXgokDmQOzxnB3XAh"
accessSecret <- "lxpkojqoTPABQT2g3F5SZvI9CNPTmwyDpiIg7TTAn8tlb"

options(httr_oauth_cache = TRUE)
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)



# 立憲民主党
rikken <- userTimeline("CDP2017",       # ユーザ名を@無しで入れる
                         n=3200,                  # 取得件数を指定（上限は3200?）
                         maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                         sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                         includeRts=FALSE,      # RTを含むかどうか
                         excludeReplies=FALSE  # リプライを含むかどうか
)
# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(rikken[[1]])
head(rikken)
# まずはツイート以外の邪魔な情報を削除
rikken_text <- sapply(rikken, statusText)
head(rikken_text)
str(rikken_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
########################しかし、数字も削除されるから微妙
rikken_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
rikken_text <- rikken_text[!is.na(rikken_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
rikken_text2 <- paste(rikken_text, collapse ="")
rikken_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(rikken_text2, xfile)

# 形態素解析
rikken_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
rikken_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(rikken_df,15)
# なぜかローマ字入ってるから、直接指定して削除。
# 代名詞・接尾も邪魔やから削除。
rikken_df %<>% filter(!TERM %in% c("A","F","FE","FFFD","U","%","％"))
rikken_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(rikken_df)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
rikken_df %<>% select(everything(), FREQ = starts_with("file"))
head(rikken_df)
unlink(xfile) #一時ファイルを削除
rikken_df %>% arrange(FREQ) %>% tail(50)


# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(rikken_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
rikken_df2 <- rikken_df%>% select(TERM, POS1, FREQ) %>%
              group_by(TERM, POS1) %>%
              summarize(FREQ = sum(FREQ))
nrow(rikken_df2) # 数行少なくなったことを確認
wordcloud (rikken_df2$TERM, rikken_df2$FREQ, min.freq = 3, family = "JP1")




########################### 日本維新
ishin <- userTimeline("osaka_ishin",       # ユーザ名を@無しで入れる
                         n=3200,                  # 取得件数を指定（上限は3200?）
                         maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                         sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                         includeRts=FALSE,      # RTを含むかどうか
                         excludeReplies=FALSE  # リプライを含むかどうか
)
# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(ishin[[1]])
head(ishin)
# まずはツイート以外の邪魔な情報を削除
ishin_text <- sapply(ishin, statusText)
head(ishin_text)
str(ishin_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
ishin_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
ishin_text <- ishin_text[!is.na(ishin_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
ishin_text2 <- paste(ishin_text, collapse ="")
ishin_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(ishin_text2, xfile)

# 形態素解析
ishin_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
ishin_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(ishin_df,15)

# 代名詞・接尾も邪魔やから削除。
ishin_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(ishin_df)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
ishin_df %<>% select(everything(), FREQ = starts_with("file"))
head(ishin_df)
unlink(xfile) #一時ファイルを削除
ishin_df %>% arrange(FREQ) %>% tail(50)


# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(ishin_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
ishin_df2 <- ishin_df %>% select(TERM, POS1, FREQ) %>%
  group_by(TERM, POS1) %>%
  summarize(FREQ = sum(FREQ))
nrow(ishin_df2) # 数行少なくなったことを確認
wordcloud (ishin_df2$TERM, ishin_df2$FREQ, min.freq = 4, family = "JP1")




########################## 自由民主党
jimin <- userTimeline("jimin_koho",       # ユーザ名を@無しで入れる
                         n=3200,                  # 取得件数を指定（上限は3200?）
                         maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                         sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                         includeRts=FALSE,      # RTを含むかどうか
                         excludeReplies=FALSE  # リプライを含むかどうか
)

# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(jimin[[1]])
head(jimin)
# まずはツイート以外の邪魔な情報を削除
jimin_text <- sapply(jimin, statusText)
head(jimin_text)
str(jimin_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
########################しかし、数字も削除されるから微妙
jimin_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
jimin_text <- jimin_text[!is.na(jimin_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
jimin_text2 <- paste(jimin_text, collapse ="")
jimin_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(jimin_text2, xfile)

# 形態素解析
jimin_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
jimin_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(jimin_df,15)
# なぜかローマ字入ってるから、直接指定して削除。
# 代名詞・接尾も邪魔やから削除。
jimin_df %<>% filter(!TERM %in% c("A","U","FFFC","FFFD","ー"))
jimin_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(jimin_df,15)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
jimin_df %<>% select(everything(), FREQ = starts_with("file"))
head(jimin_df)
unlink(xfile) #一時ファイルを削除
jimin_df %>% arrange(FREQ) %>% tail(50)
#訳分らん1文字漢字も削除
jimin_df %<>% filter(!TERM %in% c("党","木","こ","平","月","将","明","年","日","回"))

# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(jimin_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
jimin_df2 <- jimin_df%>% select(TERM, POS1, FREQ) %>%
  group_by(TERM, POS1) %>%
  summarize(FREQ = sum(FREQ))
nrow(jimin_df2) # 数行少なくなったことを確認
wordcloud (jimin_df2$TERM, jimin_df2$FREQ, min.freq = 5, family = "JP1")





######################### 民進党
minshin <- userTimeline("MinshintoNews",       # ユーザ名を@無しで入れる
                         n=3200,                  # 取得件数を指定（上限は3200?）
                         maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                         sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                         includeRts=FALSE,      # RTを含むかどうか
                         excludeReplies=FALSE  # リプライを含むかどうか
)
# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(minshin[[1]])
head(minshin)
# まずはツイート以外の邪魔な情報を削除
minshin_text <- sapply(minshin, statusText)
head(minshin_text)
str(minshin_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
########################しかし、数字も削除されるから微妙
minshin_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
minshintext <- minshin_text[!is.na(minshin_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
minshin_text2 <- paste(minshin_text, collapse ="")
minshin_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(minshin_text2, xfile)

# 形態素解析
minshin_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
minshin_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(minshin_df,15)
# 代名詞・接尾も邪魔やから削除。
minshin_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(minshin_df)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
minshin_df %<>% select(everything(), FREQ = starts_with("file"))
head(minshin_df)
unlink(xfile) #一時ファイルを削除
minshin_df %>% arrange(FREQ) %>% tail(50) #パイプ処理やから並び替えたデータは代入されへん。

# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(minshin_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
minshin_df2 <- minshin_df %>% select(TERM, POS1, FREQ) %>%
  group_by(TERM, POS1) %>%
  summarize(FREQ = sum(FREQ))
nrow(minshin_df2) # 数行少なくなったことを確認
minshin_df2 %>% arrange(FREQ) %>% tail(50)
wordcloud (minshin_df2$TERM, minshin_df2$FREQ, min.freq = 5, family = "JP1")






########################### 公明党
komei <- userTimeline("komei_koho",       # ユーザ名を@無しで入れる
                         n=3200,                  # 取得件数を指定（上限は3200?）
                         maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                         sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                         includeRts=FALSE,      # RTを含むかどうか
                         excludeReplies=FALSE  # リプライを含むかどうか
)
# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(komei[[1]])
head(komei)
# まずはツイート以外の邪魔な情報を削除
komei_text <- sapply(komei, statusText)
head(komei_text)
str(komei_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
########################しかし、数字も削除されるから微妙
komei_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
komei_text <- komei_text[!is.na(komei_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
komei_text2 <- paste(komei_text, collapse ="")
komei_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(komei_text2, xfile)

# 形態素解析
komei_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
komei_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(komei_df,15)
# なぜかローマ字入ってるから、直接指定して削除。
# 代名詞・接尾も邪魔やから削除。
komei_df %<>% filter(!TERM %in% c("FFFD","U"))
komei_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(komei_df)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
komei_df %<>% select(everything(), FREQ = starts_with("file"))
head(komei_df)
unlink(xfile) #一時ファイルを削除
komei_df %>% arrange(FREQ) %>% tail(50)
#「日」や「月」が入ってるから削除。たぶん「〇月〇日」がASCII文字削除で、消えた。
komei_df %<>% filter(!TERM %in% c("日","月","党"))

# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(komei_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
komei_df2 <- komei_df%>% select(TERM, POS1, FREQ) %>%
  group_by(TERM, POS1) %>%
  summarize(FREQ = sum(FREQ))
nrow(komei_df2) # 数行少なくなったことを確認
wordcloud (komei_df2$TERM, komei_df2$FREQ, min.freq = 5, family = "JP1")







########################## 日本共産党
kyosan <- userTimeline("jcp_cc",       # ユーザ名を@無しで入れる
                         n=3200,                  # 取得件数を指定（上限は3200?）
                         maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                         sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                         includeRts=FALSE,      # RTを含むかどうか
                         excludeReplies=FALSE  # リプライを含むかどうか
)
# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(kyosan[[1]])
head(kyosan)
# まずはツイート以外の邪魔な情報を削除
kyosan_text <- sapply(kyosan, statusText)
head(kyosan_text)
str(kyosan_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
########################しかし、数字も削除されるから微妙
kyosan_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
kyosan_text <- kyosan_text[!is.na(kyosan_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
kyosan_text2 <- paste(kyosan_text, collapse ="")
kyosan_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(kyosan_text2, xfile)

# 形態素解析
kyosan_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
kyosan_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(kyosan_df,15)
# 代名詞・接尾も邪魔やから削除。
kyosan_df %<>% filter(!TERM %in% c("％"))
kyosan_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(kyosan_df)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
kyosan_df %<>% select(everything(), FREQ = starts_with("file"))
head(kyosan_df)
unlink(xfile) #一時ファイルを削除
kyosan_df %>% arrange(FREQ) %>% tail(50)

# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(kyosan_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
kyosan_df2 <- kyosan_df%>% select(TERM, POS1, FREQ) %>%
  group_by(TERM, POS1) %>%
  summarize(FREQ = sum(FREQ))
nrow(kyosan_df2) # 数行少なくなったことを確認
wordcloud (kyosan_df2$TERM, kyosan_df2$FREQ, min.freq = 3, family = "JP1")




########################### 自由党
jiyu <- userTimeline("seikatsu1pr",       # ユーザ名を@無しで入れる
                     n=3200,                  # 取得件数を指定（上限は3200?）
                     maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                     sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                     includeRts=FALSE,      # RTを含むかどうか
                     excludeReplies=FALSE  # リプライを含むかどうか
)
# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(jiyu[[1]])
head(jiyu)
# まずはツイート以外の邪魔な情報を削除
jiyu_text <- sapply(jiyu, statusText)
head(jiyu_text)
str(jiyu_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
########################しかし、数字も削除されるから微妙
jiyu_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
jiyu_text <- jiyu_text[!is.na(jiyu_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
jiyu_text2 <- paste(jiyu_text, collapse ="")
jiyu_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(jiyu_text2, xfile)

# 形態素解析
jiyu_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
jiyu_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(jiyu_df,15)
# なぜかローマ字入ってるから、直接指定して削除。
# 代名詞・接尾も邪魔やから削除。
jiyu_df %<>% filter(!TERM %in% c("A","U","ー"))
jiyu_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(jiyu_df)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
jiyu_df %<>% select(everything(), FREQ = starts_with("file"))
head(jiyu_df)
unlink(xfile) #一時ファイルを削除
jiyu_df %>% arrange(FREQ) %>% tail(50)
#「日」や「月」が入ってるから削除。たぶん「〇月〇日」がASCII文字削除で、消えた。
jiyu_df %<>% filter(!TERM %in% c("日","月","党","前"))

# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(jiyu_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
jiyu_df2 <- jiyu_df%>% select(TERM, POS1, FREQ) %>%
  group_by(TERM, POS1) %>%
  summarize(FREQ = sum(FREQ))
nrow(jiyu_df2) # 数行少なくなったことを確認
wordcloud (jiyu_df2$TERM, jiyu_df2$FREQ, min.freq = 25, family = "JP1")





########################### 社会民主党
shamin <- userTimeline("SDPJapan",       # ユーザ名を@無しで入れる
                       n=3200,                  # 取得件数を指定（上限は3200?）
                       maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
                       sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
                       includeRts=FALSE,      # RTを含むかどうか
                       excludeReplies=FALSE  # リプライを含むかどうか
)
# userTimeline関数は、リスト型で、ツイート以外にも様々な情報を取得してくれる。試しに見てみる。
str(shamin[[1]])
head(shamin)
# まずはツイート以外の邪魔な情報を削除
shamin_text <- sapply(shamin, statusText)
head(shamin_text)
str(shamin_text[[1]])

#ツイートの中にも、IDやURLなど邪魔な情報が含まれているので削除。
########################しかし、数字も削除されるから微妙
shamin_text %<>% str_replace_all("\\p{ASCII}","")
# 欠損値があれば、削除。
shamin_text <- shamin_text[!is.na(shamin_text)]
# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
shamin_text2 <- paste(shamin_text, collapse ="")
shamin_text2 %>% head()

# そのデータを一時アイルに保存
xfile <- tempfile()
write(shamin_text2, xfile)

# 形態素解析
shamin_df <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
shamin_df %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))
head(shamin_df,15)
# なぜかローマ字入ってるから、直接指定して削除。
# 代名詞・接尾も邪魔やから削除。
shamin_df %<>% filter(!TERM %in% c("F","U","FE","ーー"))
shamin_df %<>% filter(!POS2 %in% c("代名詞","接尾"))
head(shamin_df,15)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
shamin_df %<>% select(everything(), FREQ = starts_with("file"))
head(shamin_df)
unlink(xfile) #一時ファイルを削除
shamin_df %>% arrange(FREQ) %>% tail(50)
#「日」や「月」が入ってるから削除。たぶん「〇月〇日」がASCII文字削除で、消えた。
shamin_df %<>% filter(!TERM %in% c("日","月","党","籠","木","寛","徳"))

# termとpos1が同じでもpos2が違うくて同じ語が2個以上出現している。
# 一つにまとめる。
nrow(shamin_df) 
#ツイート取得数が1000個やった時と行数が変わってない。そんな呟いてない？
shamin_df2 <- shamin_df%>% select(TERM, POS1, FREQ) %>%
  group_by(TERM, POS1) %>%
  summarize(FREQ = sum(FREQ))
nrow(shamin_df2) # 数行少なくなったことを確認
wordcloud (shamin_df2$TERM, shamin_df2$FREQ, min.freq = 4, family = "JP1")


