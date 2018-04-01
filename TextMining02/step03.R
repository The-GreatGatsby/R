

# 複数ファイルの解析
# 文字単位の解析
library(RMeCab)
docDF.result <- docDF("speech", type = 0)
head(docDF.result, 10)

# 複数ファイルの解析
# 形態素単位の解析
docDF.result.2 <- docDF("speech", type = 1)
head(docDF.result.2, 10)


# 品詞を限定した形態素解析
docDF.result.3 <- docDF("speech", type = 1, pos = c("名詞", "形容詞"))
head(docDF.result.3, 10)

# 文字2-gramの集計
docDF.result.4 <- docDF("speech", type = 0, N = 2)
head(docDF.result.4, 10)

# 形態素2-gramの集計
docDF.result.5 <- docDF("speech", type = 1, N = 2)
head(docDF.result.5, 10)

# 単語2-gramの集計（名詞，動詞，形容詞，副詞のみ）
docDF.result.6 <- docDF("speech", type = 1, N = 2, pos = c("名詞", "動詞", "形容詞", "副詞"))
head(docDF.result.6, 10)

# 品詞の情報を削除
docDF.result.7 <- docDF.result.6[, -2] #2列目を削除
docDF.result.7 <- docDF.result.7[, -2] #3列目が2列目にきているため、2列目削除でOK
head(docDF.result.7, 10)





# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(textometry)
# データセットの準備
data(robespierre)
# データセットの確認
robespierre

# 1列目（D1）の総語数
sum(robespierre[, 1])
# 1～10列目（D1～D10）の総語数
colSums(robespierre)

# 100語あたりの相対頻度を計算
relative.freq <- robespierre / apply(robespierre, 2, sum) * 100
# 小数点以下2位までを表示
round(relative.freq,2)

# scale関数で、標準化頻度を計算
scale.result <- scale(robespierre)
# 小数点以下2位までを表示
round(scale.result, 2)

# D1におけるdeの観測頻度（1行目，1列目）
robespierre[1, 1]
# 列ごとの平均値
apply(robespierre, 2, mean)
# 列ごとの標準偏差
apply(robespierre, 2, sd)
# 標準化頻度の検算
(464 - 1399.1667) / 3147.9423
# 小数点以下2位までを表示
round((464 - 1399.1667) / 3147.9423, 2)





# TF-IDFの計算
tf.idf <- docDF("speech", type = 1, weight = "tf*idf")
head(tf.idf, 5)

# 観測頻度の集計
speech.result <- docDF("speech", type = 1)
head(speech.result)

# Abe.txt における「.」（1行目，4列目）のTF-IDFを計算
TF <- 1
IDF <- log2(3 / 1)
TF * (IDF + 1)










##### 第10章 #####

##### 10.1 #####

# 追加パッケージのインストール（初回のみ）
install.packages("languageR", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(languageR)
# データセットの準備
data(alice)
# データセットの冒頭20語の確認
head(alice, 20)

# テキストファイルからのデータ読み込み（Obama.txtを選択）
text.data <- scan(file.choose(), what = "char", sep = "\n", quiet = TRUE)
# 単語ベクトルの作成
word.vector <- unlist(strsplit(text.data, "\\W"))
# スペースを削除
not.blank <- which(word.vector != "")
obama <- word.vector[not.blank]
# データの確認
head(obama, 20)

# インターネット上のデータの読み込み
text.data <- scan("http://www.xxx/yyy.txt", what = "char", sep = "\n", quiet = TRUE)

# 分析テキストの指定
word.vector <- alice
# 大文字を小文字に変換
word.vector.lower <- tolower(word.vector)
# 検索語の生起位置を取得（ここでは，"rabbit"）
word.positions <- which(word.vector.lower == "rabbit")
# 検索語の前後何語まで表示するかを指定（ここでは，5語）
context <- 5
# KWICコンコーダンスの作成
for(i in seq(word.positions)) {
  if(word.positions[i] == 1) {
    before <- NULL
  } else {
    start <- word.positions[i] - context
    start <- max(start, 1)
    before <- word.vector.lower[start : (word.positions[i] - 1)]
  }
  end <- word.positions[i] + context
  after <- word.vector.lower[(word.positions[i] + 1) : end]
  after[is.na(after)] <- ""
  keyword <- word.vector.lower[word.positions[i]]
  cat("--------------------", i, "--------------------", "\n")
  cat(before, "[", keyword, "]", after, "\n")
}

# 検索語の生起位置を視覚化
plot(word.vector.lower == "rabbit", type = "h", yaxt = "n", main = "rabbit")

##### 10.2 #####

# 追加パッケージのインストール（初回のみ）
install.packages("tm", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(tm)
# 数字と句読点の削除
corpus.cleaned <- removeNumbers(word.vector.lower)
corpus.cleaned <- removePunctuation(corpus.cleaned)
# スペースを削除
not.blank <- which(corpus.cleaned != "")
corpus.cleaned <- corpus.cleaned [not.blank]
# 頻度表の作成
freq.list <- table(corpus.cleaned)
sorted.freq.list <- sort(freq.list, decreasing = TRUE)
sorted.table <- paste(names(sorted.freq.list), sorted.freq.list, sep = ": ")
# 頻度表（頻度上位20位まで）の確認
head(sorted.table, 20)

# ストップワードを個別に設定（ここでは，"the"と"and"を除外）
corpus.cleaned.2 <- removeWords(corpus.cleaned, c("the", "and"))
# スペースを削除
not.blank <- which(corpus.cleaned.2 != "")
corpus.cleaned.2 <- corpus.cleaned.2[not.blank]
# 頻度表の作成
freq.list.2 <- table(corpus.cleaned.2)
sorted.freq.list.2 <- sort(freq.list.2, decreasing = TRUE)
sorted.table.2 <- paste(names(sorted.freq.list.2), sorted.freq.list.2, sep = ": ")
# 頻度表（頻度上位20位まで）の確認
head(sorted.table.2, 20)

# 語幹処理
corpus.cleaned.3 <- stemDocument(corpus.cleaned)
# 頻度表の作成
freq.list.3 <- table(corpus.cleaned.3)
sorted.freq.list.3 <- sort(freq.list.3, decreasing = TRUE)
sorted.table.3 <- paste(names(sorted.freq.list.3), sorted.freq.list.3, sep = ": ")
# 頻度表（頻度上位20位まで）の確認
head(sorted.table.3, 20)

# 追加パッケージのインストール（初回のみ）
install.packages("wordcloud", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(wordcloud)
wordcloud(corpus.cleaned, min.freq = 5, random.order = FALSE)

# 2-gramsの抽出
ngrams <- paste(corpus.cleaned[1 : (length(corpus.cleaned) - 1)], corpus.cleaned[2 : length(corpus.cleaned)])
# 頻度集計
ngram.freq <- table(ngrams)
sorted.ngram.freq <- sort(ngram.freq, decreasing = TRUE)
sorted.ngram.table <- paste(names(sorted.ngram.freq), sorted.ngram.freq, sep = ": ")
# 頻度上位20位までを表示
head(sorted.ngram.table, 20)

##### 10.3 #####

# 検索語の指定（ここでは，"rabbit"）
search.word <- "\\brabbit\\b"
# スパンの指定（ここでは，前後2語まで）
span <- 2
span <- (-span : span)
# 出力ファイル名の指定（ここでは，output.txt）
output.file <- "output.txt"
# 検索語の出現する位置を特定
positions.of.matches <- grep(search.word, corpus.cleaned, perl = TRUE)
# 共起語の集計
results <- list()
for(i in 1 : length(span)) { 
  collocate.positions <- positions.of.matches + span[i]
  collocates <- corpus.cleaned[collocate.positions]
  sorted.collocates <- sort(table(collocates), decreasing = TRUE)
  results[[i]] <- sorted.collocates
}
# 集計表のヘッダーを出力
cat(paste(rep(c("W_", "F_"), length(span)), rep(span, each = 2), sep = ""), "\n", sep = "\t", file = output.file)
# 集計データを出力
lengths <- sapply(results, length)
for(k in 1 : max(lengths)) {
  output.string <- paste(names(sapply(results, "[", k)), sapply(results, "[", k), sep = "\t")
  output.string.2 <- gsub("NA\tNA", "\t", output.string, perl = TRUE)
  cat(output.string.2, "\n", sep = "\t", file = output.file, append = TRUE)
}

##### 10.4 #####

# 追加パッケージのインストール（初回のみ）
install.packages("koRpus", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(koRpus)
# テキストの読み込み（Obama.txtを選択）
tok <- tokenize(file.choose(), lang = "en")

# 異語率の計算
TTR(tok)

# ギロー指数の計算
R.ld(tok)

# MATTRの計算
MATTR(tok)
# MTLDの計算
MTLD(tok)

# Flesch-Kincaid Grade Levelの計算
flesch.kincaid(tok)

# Coleman-Liau Indexの計算
coleman.liau(tok)
# Automated Readability Indexの計算
ARI(tok)
