# とんかつ食べたい



# アルファベット大文字
LETTERS
# アルファベット小文字
letters
# 全て小文字に変換
tolower(LETTERS)
# 全て大文字に変換
toupper(letters)

# pasteは、文字列の結合（デフォルトでは，スペースが挟まれる）
paste("William", "Shakespeare")
paste("夏目", "漱石")

# スペースなしで結合
paste("夏目", "漱石", sep = "")
# 文字列の入った変数を結合
Kawabata <- "川端"
Yasunari <- "康成"
paste(Kawabata, Yasunari)

# 連番のついた変数を生成
# No.という文字列に、１から５までの数字をそれぞれ結合。
paste("No.", 1 : 5, sep = "")

# 文字列の文字数を出力
nchar("cat")
nchar("猫")

# 各文字列の文字数を計算
nchar(c("I", "love", "cats"))
nchar(c("私", "は", "猫", "が", "好き", "です"))


word.length <- nchar(c("I", "love", "cats"))
# 1単語あたりの平均文字数
mean(word.length)


# ワードスペクトル（1単語あたりの文字数の頻度表）
# この例では，1文字の単語が1つで，4文字の単語が2つ
table(word.length)

# 1文字目から3文字目までを取り出す　※Aは０番目ではない！！！！！！
substr("ABCDE", start = 1, stop = 3)
# 2文字目から4文字目までを取り出す
substr("あいうえお", start = 2, stop = 4)

# 動詞の過去形のベクトル
verbs <- c("asked", "had", "looked", "took")
# edという文字列を含む要素の番号を抽出
verbs.n <- grep("ed", verbs)
# 抽出した要素の番号が入っている。
verbs.n
# 抽出した番号を手がかりとして，検索条件に合致した要素を表示。添字を使う。
verbs[verbs.n]


words <- c("asked", "edited", "edition", "education", "looked")
# 単純にedという文字列を含む単語を検索
words.n <- grep("ed", words)
words[words.n]

# edという文字列で終わる単語のみを検索。「＄とperl=TRUE」
words.n.2 <- grep("ed$", words, perl = TRUE)
words[words.n.2]
# edという文字列で始まる単語のみを検索。「＾とperl=TRUE」
words.n.3 <- grep("^ed", words, perl = TRUE)
words[words.n.3]

######################　重要！！！！！！！！！
verbs.2 <- c("asked", "looked", "walked")
# edで終わる英単語をsに置換
gsub("ed$", "s", verbs.2, perl = TRUE)
#############################################


####################### 重要！！！！！！！
# 日本語の語末の「く」を「い」に置換
adverbs <- c("美しく", "高く", "速く")
gsub("く$", "い", adverbs, perl = TRUE)
############################################


################### 重要！！！！！！！！！！！
# 置換機能を用いた文字列の削除
nouns <- c("birds", "cats", "dogs")
gsub("s$", "", nouns, perl = TRUE)
##############################################


##################　重要！！！！！！！！！！！
# A and Bの形式の文字列のベクトル
and <- c("black and white", "bread and butter", "cats and dogs")
# "and"を区切りとして，文字列を分割
strsplit(and, split = " and ")
# 出力をリスト形式からベクトル形式に変換
unlist(strsplit(and, split = " and "))
###############################################

#####################　重要！！！！！！！！！！！！
# ひとかたまりの英文
ulysses <- "Stately, plump Buck Mulligan came from the stairhead, bearing a bowl of lather on which a mirror and a razor lay crossed."
# スペースを区切りとした分割
unlist(strsplit(ulysses, split = " "))
#################################################


#####################　重要！！！！！！！！！！！！
yukiguni <- "国境の長いトンネルを抜けると雪国であった。"
# 一文字ずつ、分割
unlist(strsplit(yukiguni, split = ""))
###################################################

setwd("C:/Users/rstud/Documents/GitHub/R/TextMining02")
# ファイルが作業ディレクトリにある場合
data01 <- read.csv("data/data01.csv" , header = FALSE)
data01

# マウス操作でdata01.csvを選択する場合
data01 <- read.csv(file.choose(), header = FALSE)

# マウス操作でdata02.csvを選択する場合
data02 <- read.csv(file.choose(), header = TRUE)
data02

# マウス操作でdata03.csvを選択する場合
# 冒頭の2行を読みとばす
data03 <- read.csv(file.choose(), header = TRUE, skip = 2)
data03

# マウス操作でdata04.csvを選択する場合
# 行ラベル・列ラベルがある場合は「header=FALSE」と「row.names=1」
data04 <- read.csv(file.choose(), header = TRUE, row.names = 1)
data04

# txtを読み込むには、scan
# 文単位で読み込む場合（data05.txtを選択）
# quiet=TRUEで、データの個数を非表示。
# sep="\n"で文節単位に分ける。指定しないと単語単位になる。
# what=charは数字ではなく文字をスキャンするという意味。
data05 <- scan(file.choose(), what = "char", sep = "\n", quiet = TRUE)
data05

#単語単位で読み込む場合（data05.txtを選択）
data05 <- scan(file.choose(), what = "char", quiet = TRUE)
data05

##### 第5章 #####

##### 5.1 #####

# 追加パッケージのインストール（初回のみ）
install.packages("corpora", dependencies = TRUE)

# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(corpora)

# データセットの準備
data(BNCbiber)
# データの冒頭の5行のみを表示
head(BNCbiber, 5)

# CSVファイルからのデータ読み込み（BNCbiber.csvを選択）
BNCbiber <- read.csv(file.choose(), header = TRUE, row.names = 1)

# ヒストグラムの描画
hist(BNCbiber[, 2])

# データのクラスの確認
class(BNCbiber)
# ヒストグラムの描画
hist(BNCbiber$f_01_past_tense)

# ヒストグラムのタイトルと軸ラベルを変更
hist(BNCbiber[, 2], main = "past tense", xlab = "frequency", ylab = "number of texts")

# ヒストグラムの色を変更
hist(BNCbiber[, 2], main = "past tense", xlab = "frequency", ylab = "number of texts", col = "grey")
# Rで使える色の確認
colors()

##### 5.2 #####

# 箱ひげ図の描画
boxplot(BNCbiber[, 2], range = 0)

# 箱ひげ図の作成に用いられている要約統計量の確認
boxplot.stats(BNCbiber[, 2])

# 箱ひげ図のタイトルと色を変更
boxplot(BNCbiber[, 2], range = 0, main = "past tense", col = "grey")

# 箱ひげ図の外れ値を表示
boxplot(BNCbiber[, 2], main = "past tense", col = "grey")
# CSVファイルからのデータ読み込み（pym.csvを選択）
pym <- read.csv(file.choose(), header = TRUE, row.names = 1)

# データの冒頭の5行のみを表示
head(pym, 5)

# グループ別の箱ひげ図の描画
boxplot(pym[, 2] ~ pym[, 6], names = c("high", "low"), col = "grey")

# ノッチのある箱ひげ図の描画
boxplot(pym[, 2] ~ pym[, 6], names = c("high", "low"), col = "grey", notch = TRUE)

# 追加パッケージのインストール（初回のみ）
install.packages("beeswarm", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(beeswarm)
# 箱ひげ図の上に個々のデータの分布を重ねて描画
boxplot(pym[, 2] ~ pym[, 6], names = c("high", "low"), col = "grey")
beeswarm(pym[, 2] ~ pym[, 6], col = "black", pch = 16, add = TRUE)

# 追加パッケージのインストール（初回のみ）
install.packages("vioplot", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(vioplot)
# ヴァイオリンプロットを描画
vioplot(pym[1 : 50, 2], pym[51 : 101, 2], names = c("high", "low"), col = "grey")

##### 5.3 #####

# 追加パッケージのインストール（初回のみ）
install.packages("textometry", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(textometry)
# データセットの準備
data(robespierre)
# データセットの確認
robespierre
# データ最終行の削除
robespierre.2 <- robespierre[-6, ]
# 修正したデータセットの確認
robespierre.2
# モザイクプロットを描画
mosaicplot(robespierre.2)

# CSVファイルからのデータ読み込み（robespierre.csvを選択）
robespierre <- read.csv(file.choose(), header = TRUE, row.names = 1)
# データ最終行の削除
robespierre.2 <- robespierre[-6, ]
# モザイクプロットを描画
mosaicplot(robespierre.2)

ラベルの向きを変更
mosaicplot(robespierre.2, las = 2)

##### 5.4 #####

# CSVファイルからのデータ読み込み（FPP.csvを選択）
FPP <- read.csv(file(file.choose(), encoding = "cp932"), header = TRUE, row.names = 1)

# データの冒頭の5行のみを表示
head(FPP)

# 散布図を描画
plot(FPP[, 1], FPP[, 4])

# 散布図の引数を指定
plot(FPP[, 1], FPP[, 4], main = "FPP", xlab = "Google", ylab = "BCCWJ")

# 点の大きさとタイプと色を指定
plot(FPP[, 1], FPP[, 4], main = "FPP", xlab = "Google", ylab = "BCCWJ", cex = 1.2, pch = 16, col = "grey")

# 追加パッケージのインストール（初回のみ）
install.packages("car", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(car)
# 散布図と箱ひげ図を同時に描画
scatterplot(FPP[, 1], FPP[, 4], xlab = "Google", ylab = "BCCWJ", smoother = FALSE, reg.line = FALSE)

# 散布図行列を描画
pairs(FPP)

##### 第6章 #####

##### 6.1 #####

# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(RMeCab)
# 短い文章の形態素解析
RMeCabC("すもももももももものうち")

RMeCabC.result <- RMeCabC("すもももももももものうち")
# データ形式の確認
class(RMeCabC.result)
# データ形式の変換
RMeCabC.result.2 <- unlist(RMeCabC.result)
RMeCabC.result.2
# データのクラスの確認
class(RMeCabC.result.2)

# 解析結果の一部のみを表示
RMeCabC.result.2[1]
RMeCabC.result.2[2]
RMeCabC.result.2[1 : 3]

# 品詞情報のみを表示
names(RMeCabC.result.2)

# 単語の原形を復元
RMeCabC.result.3 <- RMeCabC("オーム社は1914（大正3）年、電気雑誌「OHM」誌の創刊とともに創業いたしました。以来、科学技術分野の雑誌、専門書、実務書、教科書の発行を中心に出版活動を行ってまいりました。2014（平成26）年には電気雑誌「OHM」が創刊100周年の節目を迎え、会社も新たな時代へと新しい一歩を踏み出しました。現在は専門書、実務書などに加えて一般書、実用書、資格試験参考書など、幅広い分野での出版事業を展開しております。それらを通じて、読者の皆様に喜んでいただくことはもちろんのこと、社会に貢献することを目標にしております。", 1)
RMeCabC.result.4 <- unlist(RMeCabC.result.3)
RMeCabC.result.4

# 追加パッケージのインストール（初回のみ）
install.packages("wordcloud", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(wordcloud)

# RMeCabText関数で形態素解析（wagahaiwa_nekodearu.txtを選択）
RMeCabText.result <- RMeCabText(file.choose())
# RMeCabText関数の結果の確認
head(RMeCabText.result, 5)
# 単語ベクトルの作成
RMeCabText.result.2 <- unlist(sapply(RMeCabText.result, "[[", 1))
# 単語ベクトルの確認
head(RMeCabText.result.2, 5)
# ワードクラウドを描画
wordcloud(RMeCabText.result.2, min.freq = 2, random.order = FALSE)

# Mac版Rのグラフで日本語が文字化けするのを防止
par(family = "HiraKakuProN-W3")
# ワードクラウドを描画
wordcloud(RMeCabText.result.2, min.freq = 2, random.order = FALSE)

##### 6.2 #####

# 形態素解析結果から単語の頻度表を作成
# table関数で頻度集計
RMeCabC.result.table <- table(RMeCabC.result.4)
# sort関数で頻度が高い順に並び替え
RMeCabC.result.table.2 <- sort(RMeCabC.result.table, decreasing = TRUE)
# 集計結果の確認
head(RMeCabC.result.table.2, 10)

# 形態素解析結果から品詞の頻度表を作成
# names関数で品詞の情報を抽出
RMeCabC.result.table.3 <- table(names(RMeCabC.result.4))
# これ以降は，単語の頻度表を作成する場合と同じ
RMeCabC.result.table.4 <- sort(RMeCabC.result.table.3, decreasing = TRUE)
# 集計結果の確認
head(RMeCabC.result.table.4, 10)

# RMeCabFreq関数による頻度表の作成
# ファイルの読み込み（wagahaiwa_nekodearu.txtを選択）
RMeCabFreq.result <- RMeCabFreq(file.choose())
# 集計結果の確認
head(RMeCabFreq.result, 5)

# RMeCabFreq関数の結果を頻度順に並び替え
RMeCabFreq.result.2 <- RMeCabFreq.result[order(RMeCabFreq.result$Freq, decreasing = TRUE), ]
# 並び替えた結果の確認
head(RMeCabFreq.result.2, 5)

# 総語数の計算
# 以下の2種類の書き方が可能
sum(RMeCabFreq.result.2[, 4])
sum(RMeCabFreq.result.2$Freq)

# 異語率の計算
# 異語数は，nrow(RMeCabFreq.result.2)で計算
nrow(RMeCabFreq.result.2) / sum(RMeCabFreq.result.2$Freq)

# 頻度表の書き出し
write.table(RMeCabFreq.result.2, file = "wordlist.csv", sep = ",", row.names = TRUE, col.names = NA)
# Macなどで出力したファイルが文字化けした場合
write.table(RMeCabFreq.result.2, file = "wordlist.csv", sep = ",", row.names = TRUE, col.names = NA, fileEncoding = "UTF-8")
# 保存したファイルがどこにあるか分からなくなった場合
getwd()

# 特定の条件に合致する単語のみを抽出
# 「猫」という文字列を含む単語のみを表示
RMeCabFreq.result.2[grep("猫", RMeCabFreq.result.2$Term), ]
# 「犬」という文字列を含む単語のみを表示
RMeCabFreq.result.2[grep("犬", RMeCabFreq.result.2$Term), ]
# 「猫」もしくは「犬」という文字列を含む単語のみを表示
RMeCabFreq.result.2[grep("猫|犬", RMeCabFreq.result.2$Term), ]

# 「猫」という単語のみを表示
RMeCabFreq.result.2[grep("^猫$", RMeCabFreq.result.2$Term), ]

##### 6.3 #####

# n-gramの抽出
# 文字2-gram（wagahaiwa_nekodearu.txtを選択）
ngram.result.1 <- Ngram(file.choose(), type = 0)
# 集計結果の確認
head(ngram.result.1, 5)
# 単語2-gram（wagahaiwa_nekodearu.txtを選択）
ngram.result.2 <- Ngram(file.choose(), type = 1)
# 集計結果の確認
head(ngram.result.2, 5)
# 品詞2-gram（wagahaiwa_nekodearu.txtを選択）
ngram.result.3 <- Ngram(file.choose(), type = 2)
# 集計結果の確認
head(ngram.result.3, 5)

# 単語n-gramの抽出における品詞の指定（wagahaiwa_nekodearu.txtを選択）
ngram.result.4 <- Ngram(file.choose(), type = 1, pos = c("名詞", "動詞", "形容詞", "副詞"))
# 集計結果の確認
head(ngram.result.4, 5)

# n-gramの長さを変更（wagahaiwa_nekodearu.txtを選択）
ngram.result.5 <- Ngram(file.choose(), type = 1, N = 3)
# 集計結果の確認
head(ngram.result.5, 5)

# Ngram関数の解析結果を頻度順に並び替え
ngram.result.6 <- ngram.result.2[order(ngram.result.2$Freq, decreasing = TRUE), ]
# 並び替えた結果の確認
head(ngram.result.6, 5)

# docDF関数によるn-gramの抽出（wagahaiwa_nekodearu.txtを選択）
docDF.result <- docDF(file.choose(), type = 1, N = 2)
# 集計結果の確認
head(docDF.result, 5)

##### 6.4 #####

# 共起語を集計（wagahaiwa_nekodearu.txtを選択）
collocate.result <- collocate(file.choose(), node = "吾輩", span = 5)
# 集計結果の確認
head(collocate.result, 5)

# TとMIを計算
collScores.result <- collScores(collocate.result, node = "吾輩", span = 5)
# 計算結果の確認
head(collScores.result, 5)

# 共起強度の計算結果を並び替え
# Tで並び替え
collScores.result.2 <- collScores.result[order(collScores.result$T, decreasing = TRUE), ]
# 並び替えた結果の確認
head(collScores.result.2, 5)

# MIで並び替え
collScores.result.3 <- collScores.result[order(collScores.result$MI, decreasing = TRUE), ]
# 並び替えた結果の確認
head(collScores.result.3, 5)

# 追加パッケージのインストール（初回のみ）
install.packages("igraph", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(igraph)
# NgramDFによる共起語の集計（wagahaiwa_nekodearu.txtを選択）
NgramDF.result <- NgramDF(file.choose(), type = 1, N = 2, pos = "名詞")
# 共起頻度2以上のペアのみを抽出
NgramDF.result.2 <- subset(NgramDF.result, Freq > 1)
# ネットワークの描画
g <- graph.data.frame(NgramDF.result.2, directed = FALSE)
plot(g, vertex.label = V(g)$name, vertex.color = "grey")

# 共起頻度3以上のペアのみを抽出
NgramDF.result.3 <- subset(NgramDF.result, Freq > 2)
# ネットワークの描画
g.2 <- graph.data.frame(NgramDF.result.3, directed = FALSE)
plot(g.2, vertex.label = V(g.2)$name, vertex.color = "grey")

##### 第7章 #####

##### 7.1 #####

# 複数ファイルの解析
# 文字頻度の集計
library(RMeCab)
docDF.result <- docDF("speech", type = 0)
# 解析結果の確認
head(docDF.result, 10)

# 複数ファイルの解析
# 単語頻度の集計
docDF.result.2 <- docDF("speech", type = 1)
# 解析結果の確認
head(docDF.result.2, 10)

# 品詞を限定した集計
docDF.result.3 <- docDF("speech", type = 1, pos = c("名詞", "形容詞"))
# 解析結果の確認
head(docDF.result.3, 10)

# 文字2-gramの集計
docDF.result.4 <- docDF("speech", type = 0, N = 2)
# 集計結果の確認
head(docDF.result.4, 10)
# 単語2-gramの集計
docDF.result.5 <- docDF("speech", type = 1, N = 2)
# 集計結果の確認
head(docDF.result.5, 10)
# 単語2-gramの集計（名詞，動詞，形容詞，副詞のみ）
docDF.result.6 <- docDF("speech", type = 1, N = 2, pos = c("名詞", "動詞", "形容詞", "副詞"))
# 集計結果の確認
head(docDF.result.6, 10)

# 品詞の情報を削除
docDF.result.7 <- docDF.result.6[, -2]
docDF.result.7 <- docDF.result.7[, -2]
# 削除した結果の確認
head(docDF.result.7, 10)

##### 7.2 #####

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
relative.freq <- t(t(robespierre) / apply(robespierre, 2, sum) * 100)
# 小数点以下2位までを表示
relative.freq

# 標準化頻度を計算
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
# 計算結果の確認
head(tf.idf, 5)

# 観測頻度の集計
speech.result <- docDF("speech", type = 1)
# 計算結果の確認
head(speech.result)
# Abe.txt における「.」（1行目，4列目）のTF-IDFを計算
TF <- 1
IDF <- log2(3 / 1)
TF * (IDF + 1)

##### 第8章 #####

##### 8.1 #####

# クロス集計表の準備
cross.tab <- matrix(c(96, 54, 52, 48), nrow = 2, ncol = 2, byrow = TRUE)
rownames(cross.tab) <- c("Male", "Female")
colnames(cross.tab) <- c("Jotai", "Keitai")
# クロス集計表の確認
cross.tab

# フィッシャーの正確確率検定
fisher.test(cross.tab)

# カイ自乗検定
chisq.test(cross.tab, correct = FALSE)

# 2×3のクロス集計表の準備
cross.tab.2 <- matrix(c(805, 414, 226, 99, 38, 12), nrow = 2, ncol = 3, byrow = TRUE)
rownames(cross.tab.2) <- c("Correct", "Error")
colnames(cross.tab.2) <- c("Level 1", "Level 2", "Level 3")
# クロス集計表の確認
cross.tab.2
# 2×3のクロス集計表にフィッシャーの正確確率検定を実行
fisher.test(cross.tab.2)

# 1列目と2列目を検定
fisher.test(cross.tab.2[, c(1, 2)])
# 1列目と3列目を検定
fisher.test(cross.tab.2[, c(1, 3)])
# 2列目と3列目を検定
fisher.test(cross.tab.2[, c(2, 3)])

# 表中の数値を全て10倍
cross.tab.3 <- cross.tab * 10
# 10倍したデータの確認
cross.tab.3
# フィッシャーの正確確率検定
fisher.test(cross.tab.3)

# オッズ比の計算
(cross.tab[1, 1] / cross.tab[2, 1]) / (cross.tab[1, 2] / cross.tab[2, 2])

# 10倍したデータでオッズ比を計算
(cross.tab.3[1, 1] / cross.tab.3[2, 1]) / (cross.tab.3[1, 2] / cross.tab.3[2, 2])

# 追加パッケージのインストール（初回のみ）
install.packages("vcd", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(vcd)
# オッズ比の計算
oddsratio(cross.tab, log = FALSE)
# オッズ比の信頼区間（下限値，上限値）の計算
confint(oddsratio(cross.tab, log = FALSE))

# クラメールのVの計算
V <- assocstats(cross.tab.3)
V

# 追加パッケージのインストール（初回のみ）
install.packages("RVAideMemoire", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(RVAideMemoire)
# クラメールのVの信頼区間（下限値，上限値）の計算
cramer.test(cross.tab.3)

##### 8.2 #####

# 追加パッケージのインストール（初回のみ）
install.packages("corpora", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(corpora)
# データセットの準備
data(BNCbiber)
# データの冒頭の5行のみを表示
head(BNCbiber, 5)
# 相関係数の計算
cor(BNCbiber[, 2], BNCbiber[, 4])

# 散布図の描画
plot(BNCbiber[, 2], BNCbiber[, 4], xlab = "past tense", ylab = "present tense")

# 無相関検定
cor.test(BNCbiber[, 2], BNCbiber[, 4])

# スピアマンの順位相関係数の計算
cor(BNCbiber[, 2], BNCbiber[, 4], method = "spearman")

# ピアソンの積率相関係数
cor(BNCbiber[, 2 : 4])
# スピアマンの順位相関係数の計算
cor(BNCbiber[, 2 : 4], method = "spearman")

# 追加パッケージのインストール（初回のみ）
install.packages("psych", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(psych)
# 相関係数が表示された散布図行列の作成
pairs.panels(BNCbiber[, 2 : 4])

# 単回帰分析
lm.result <- lm(BNCbiber[, 2] ~ BNCbiber[, 4])
# 結果の確認
lm.result

# 回帰式の可視化
plot(BNCbiber[, 4], BNCbiber[, 2], xlab = "present tense", ylab = "past tense", pch = 16, col = "grey")
abline(lm.result)

# 重回帰分析
lm.result.2 <- lm(BNCbiber[, 2] ~ BNCbiber[, 3] + BNCbiber[, 4])
# 結果の確認
lm.result.2

##### 第9章 #####

##### 9.1 #####

# 追加パッケージのインストール（初回のみ）
install.packages("ca", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(ca)
# データセットの準備
data(author)
# データの冒頭の5行のみを表示
head(author, 5)

# 対応分析
ca.result <- ca(author)
# 結果の可視化
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
