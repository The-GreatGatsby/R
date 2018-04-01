


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
install.packages("kernlab", dependencies = TRUE)
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

# MASSパッケージの読み込み（Rを起動するごとに毎回）
library(MASS)
# 線形判別分析
# 判別式の作成
lda.result <- lda(type ~ ., data = spam.train)
# 結果の確認
lda.result

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

# 追加パッケージのインストール（初回のみ）
install.packages("partykit", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(partykit)
# 決定木の判別モデルの可視化
plot(as.party(rpart.result))

# 枝の剪定基準の決定
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

# 追加パッケージのインストール（初回のみ）
install.packages("randomForest", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
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