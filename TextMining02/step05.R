


# 追加パッケージのインストール（初回のみ）
# install.packages("ca", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(ca)
# 6人の作家の12種類のテキストから、アルファベット26文字の頻度を集計したデータセット。
data(author)
head(author, 5)

# 対応分析（ca関数）
ca.result <- ca(author)
plot(ca.result)

# 行データ（テキスト）のみを表示
plot(ca.result, what = c("all", "none"))
# 列データ（変数）のみを表示
plot(ca.result, what = c("none", "all"))

# 対応分析から得られた詳しい結果の確認
ca.result
# 行データの表示（第1～2次元のみ）
ca.result$rowcoord[, 1 : 2]
# 行データの第1次元の得点を並び換え
sort(ca.result$rowcoord[, 1], decreasing = TRUE)
# 行データの第2次元の得点を並び換え
sort(ca.result$rowcoord[, 2], decreasing = TRUE)
# 列データの表示（第1～2次元のみ）
ca.result$colcoord[, 1 : 2]
# 列データの第1次元の得点を並び換え
sort(ca.result$colcoord[, 1], decreasing = TRUE)
# 列データの第2次元の得点を並び換え
sort(ca.result$colcoord[, 2], decreasing = TRUE)

# 相対頻度の計算
author.2 <- author / apply(author, 1, sum)
# ユークリッド距離の計算
dist.result <- dist(author.2, method = "euclidean")
# 階層型クラスター分析（ウォード法）
hclust.result <- hclust(dist.result, method = "ward.D2")
# 結果の可視化
plot(hclust.result)


# データセットの転置
# さっきは作品（テキスト）をグループ化したが、今回はアルファベット26文字を変数とするため、転置している。
author.3 <- t(author.2)
# ユークリッド距離の計算
dist.result.2 <- dist(author.3, method = "euclidean")
# 階層型クラスター分析（ウォード法）
hclust.result.2 <- hclust(dist.result.2, method = "ward.D2")
# 結果の可視化
plot(hclust.result.2)

# 階層型クラスターつきのヒートマップ
heatmap(author.2)





##### 9.2 #####

# 追加パッケージのインストール（初回のみ）
# install.packages("kernlab", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(kernlab)
# データセットの準備
data(spam)
# データの冒頭の5行のみを表示
head(spam, 5)

# CSVファイルからのデータ読み込み（spam.csvを選択）
spam <- read.csv(file.choose(), header = TRUE)

# 訓練データと評価データに分割
# 奇数のベクトルを生成
n <- seq(1, nrow(spam), by = 2)
# 奇数行のデータを抽出
spam.train <- spam[n, ]
# 偶数行のデータを抽出
spam.test <- spam[-n, ]
# 奇数行データの冒頭5行の確認
head(spam.train, 5)
# 偶数行データの冒頭5行の確認
head(spam.test, 5)

# 線形判別分析LDAは、MASSライブラリにある
library(MASS)
# 線形判別分析
# 判別式の作成
lda.result <- lda(type ~ ., data = spam.train)
# 結果の確認
lda.result
# 「prior probabilies of groups」は、訓練データ全体におけるnonspamとspamの割合
# 「group means」は、各説明変数に関する群ごとの平均値。
# 「coefficients of linear discriminants」は、線形判別関数の係数。
# 線形判別関数の係数の絶対値（正負関係なく）が大きいほど、nonspamとspamの分類に役立つ



# 判別式に基づく自動分類
lda.predict.result <- predict(lda.result, spam.test)
# 自動分類結果の正誤を確認
lda.tab <- table(spam.test$type, lda.predict.result$class)
# 正誤をまとめた表を表示
lda.tab
# 分類精度の確認（表の対角要素の総数を全要素数で割る）
sum(diag(lda.tab)) / sum(lda.tab)

# rpartパッケージの読み込み（Rを起動するごとに毎回）
library(rpart)
# 決定木による判別モデルの構築
rpart.result <- rpart(type ~ ., data = spam.train)
# 判別モデルの確認
rpart.result



# install.packages("partykit", dependencies = TRUE)
# 決定木の結果を可視化するには、partykitパッケージのplot関数
library(partykit)
# 決定木の判別モデルの可視化
plot(as.party(rpart.result))

# 決定木分析で、過学習を避けるために「枝の剪定」を行う。
# 枝の剪定(plotcp関数)基準の決定
# 出力された図の点線と交差する値（今回は0.036）が
plotcp(rpart.result)

# 剪定基準を指定して判別モデルを構築
rpart.result.2 <- rpart(type ~ ., data = spam.train, cp = 0.036)
# 決定木の判別モデルの可視化
plot(as.party(rpart.result.2))

# 決定木による自動分類
rpart.predict.result <- predict(rpart.result, spam.test, type = "class")
# 自動分類結果の正誤を確認
rpart.tab <- table(spam.test$type, rpart.predict.result)
# 正誤をまとめた表を表示
rpart.tab
# 分類精度の確認（表の対角要素の総数を全要素数で割る）
sum(diag(rpart.tab)) / sum(rpart.tab)




# install.packages("randomForest", dependencies = TRUE)
# ランダムフォレストとは、アンサンブル学習の１つ。
# 大量の決定木を生成し、全ての決定機から得られる結果を多数決して、最終的な分類をする。
library(randomForest)
# 乱数を固定
set.seed(1)
# ランダムフォレスト
randomForest.result <- randomForest(type ~ ., data = spam.train)
# ランダムフォレストによる自動分類
randomForest.predict.result <- predict(randomForest.result, spam.test)
# 自動分類結果の正誤を確認
randomForest.tab <- table(spam.test$type, randomForest.predict.result)
# 正誤をまとめた表を表示
randomForest.tab
# 分類精度の確認（表の対角要素の総数を全要素数で割る）
sum(diag(randomForest.tab)) / sum(randomForest.tab)

# 変数重要度の可視化
varImpPlot(randomForest.result)





# install.packages("languageR", dependencies = TRUE)
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
# ある語が全体を通して使われているのか、一部だけに登場するのかが分かる。
# 今回のコンコーダンスプロットでは、「rabbit」は前半と後半で多く使われているとわかった。
plot(word.vector.lower == "rabbit", type = "h", yaxt = "n", main = "rabbit")






# install.packages("tm", dependencies = TRUE)
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




# 共起語の頻度分析
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




# install.packages("koRpus", dependencies = TRUE)
library(koRpus)
# テキストの読み込み（Obama.txtを選択）
tok <- tokenize(file.choose(), lang = "en")

# 異語率の計算
# 異語率は、テキストの総語数に影響される。だから、総語数の異なるテキストを比較するのはアカン。
TTR(tok)

# ギロー指数の計算
# ギロー指数は、総語数の影響を緩和できる。
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

