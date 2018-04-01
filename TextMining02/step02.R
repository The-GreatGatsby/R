

# install.packages("corpora", dependencies = TRUE)
# dependencies = TRUEは、そのパッケージを動かすたっめに必要な他ののパッケージも、一緒にインストールしてくれる。


library(corpora)

# corporaパッケージに付随しているデータセット
data(BNCbiber)
# データの冒頭の5行のみを表示
head(BNCbiber, 5)

# CSVファイルからのデータ読み込み（BNCbiber.csvを選択）
BNCbiber <- read.csv(file.choose(), header = TRUE, row.names = 1)



###################　エラー　###################################
# ヒストグラムの描画
hist(BNCbiber[, 2])
# データのクラスの確認
class(BNCbiber)
# $添字を使って、特定の列のデータをヒストグラム。
hist(BNCbiber$f_01_past_tense)

# ヒストグラムのタイトルと軸ラベルを変更
# mainはヒストグラムの題名。
# xlabはx軸の名前、ylabはy軸の名前、設定。
hist(BNCbiber[, 2], main = "past tense", xlab = "frequency", ylab = "number of texts")

# ヒストグラムの色を変更
hist(BNCbiber[, 2], main = "past tense", xlab = "frequency", ylab = "number of texts", col = "grey")
# Rで使える色の確認
colors()
###############################################################


# 箱ひげ図の描画
boxplot(BNCbiber[, 2], range = 0)

# 箱ひげ図の作成に用いられている要約統計量の確認
# $statsは、最小・第一四分位置・中央値・第三四分位置・最大
boxplot.stats(BNCbiber[, 2])

# 箱ひげ図のタイトルと色を変更
boxplot(BNCbiber[, 2], range = 0, main = "past tense", col = "grey")

# 箱ひげ図の外れ値を表示
boxplot(BNCbiber[, 2], main = "past tense", col = "grey")
# CSVファイルからのデータ読み込み（pym.csvを選択）
pym <- read.csv(file.choose(), header = TRUE, row.names = 1)

# データの冒頭の5行のみを表示
# 2列目のletは、各行の名詞の文字数
head(pym, 5)

# 出現頻度が高い・低いを、文字数で分けた箱ひげ図。
# 高頻度単語は文字数が少ない。
# 低頻度はばらつきが大きいとわかる。
boxplot(pym[, 2] ~ pym[, 6], names = c("high", "low"), col = "grey")

# ノッチ（くびれ）のある箱ひげ図の描画
boxplot(pym[, 2] ~ pym[, 6], names = c("high", "low"), col = "grey", notch = TRUE)




# 追加パッケージのインストール（初回のみ）
# install.packages("beeswarm", dependencies = TRUE)
library(beeswarm)
# beeswarm関数は、箱ひげ図の上に個々のデータの分布を重ねて描画
boxplot(pym[, 2] ~ pym[, 6], names = c("high", "low"), col = "grey")
beeswarm(pym[, 2] ~ pym[, 6], col = "black", pch = 16, add = TRUE)



# 追加パッケージのインストール（初回のみ）
# install.packages("vioplot", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(vioplot)
# ヴァイオリンプロットを描画
vioplot(pym[1 : 50, 2], pym[51 : 101, 2], names = c("high", "low"), col = "grey")



# 追加パッケージのインストール（初回のみ）
# install.packages("textometry", dependencies = TRUE)
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
# ラベルの向きを変更
mosaicplot(robespierre.2, las = 2)

# FPP.csvは日本語なので、文字コード選択）
FPP <- read.csv(file(file.choose(), encoding = "cp932"), header = TRUE, row.names = 1)
# 日本語の２１種類の１人称代名詞を集計したもの。
head(FPP)

# 一列目がGoogle、４列目がBBCWJ。この二つの散布図。
# Googleで頻度が高い語がBBCWJでも高いことが分かる。
plot(FPP[, 1], FPP[, 4])
# ラベル設定。
plot(FPP[, 1], FPP[, 4], main = "FPP", xlab = "Google", ylab = "BCCWJ")

# 散布図の点をデザインできる。pch引数で指定。p103
# 点の大きさとタイプと色を指定
# cex=1.2は、点の大きさを1.2倍
plot(FPP[, 1], FPP[, 4], main = "FPP", xlab = "Google", ylab = "BCCWJ", cex = 1.2, pch = 16, col = "red")
colors()

# 追加パッケージのインストール（初回のみ）
# install.packages("car", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(car)
# scatterplot関数は、箱ひげ図の付随したグラフ。
scatterplot(FPP[, 1], FPP[, 4], xlab = "Google", ylab = "BCCWJ", smoother = FALSE, reg.line = FALSE)

# 散布図行列を描画
pairs(FPP)











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

# 品詞情報のみを表示。names関数。
names(RMeCabC.result.2)

############################ 重要！！！！！！！！！
## 単語の原形を復元
RMeCabC.result.3 <- RMeCabC("オーム社は1914（大正3）年、電気雑誌「OHM」誌の創刊とともに創業いたしました。以来、科学技術分野の雑誌、専門書、実務書、教科書の発行を中心に出版活動を行ってまいりました。2014（平成26）年には電気雑誌「OHM」が創刊100周年の節目を迎え、会社も新たな時代へと新しい一歩を踏み出しました。現在は専門書、実務書などに加えて一般書、実用書、資格試験参考書など、幅広い分野での出版事業を展開しております。それらを通じて、読者の皆様に喜んでいただくことはもちろんのこと、社会に貢献することを目標にしております。", 1)
RMeCabC.result.4 <- unlist(RMeCabC.result.3)
RMeCabC.result.4

# 追加パッケージのインストール（初回のみ）
# install.packages("wordcloud", dependencies = TRUE)
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

# ワードクラウドをするには、単語ベクトルが必要。
# random.order=FALSEで、単語の配置を固定
wordcloud(RMeCabText.result.2, min.freq = 2, random.order = FALSE)




# 形態素解析結果から単語の頻度表を作成
# table関数で頻度集計
RMeCabC.result.table <- table(RMeCabC.result.4)
# sort関数で頻度が高い順に並び替え
RMeCabC.result.table.2 <- sort(RMeCabC.result.table, decreasing = TRUE)
# 集計結果の確認
head(RMeCabC.result.table.2, 10)

# 形態素解析結果から品詞の頻度表を作成
# names関数で、品詞の情報を抽出
RMeCabC.result.table.3 <- table(names(RMeCabC.result.4))
# これ以降は，単語の頻度表を作成する場合と同じ
RMeCabC.result.table.4 <- sort(RMeCabC.result.table.3, decreasing = TRUE)
head(RMeCabC.result.table.4, 10)


# RMeCabFreq関数による頻度表の作成
# wagahaiwa_nekodearu.txtを選択
RMeCabFreq.result <- RMeCabFreq(file.choose())
head(RMeCabFreq.result, 5)

# RMeCabFreq関数の結果を頻度順に並び替え
RMeCabFreq.result.2 <- RMeCabFreq.result[order(RMeCabFreq.result$Freq, decreasing = TRUE), ]
head(RMeCabFreq.result.2, 5)

# 総語数の計算
# 以下の2種類の書き方が可能
sum(RMeCabFreq.result.2[, 4])
sum(RMeCabFreq.result.2$Freq)

# 異語率の計算
# 異語数は，nrow(RMeCabFreq.result.2)で計算
# 異語数はどれだけ異なる語が多いかを0から1で表す。1に近い程、語彙が豊富。
nrow(RMeCabFreq.result.2) / sum(RMeCabFreq.result.2$Freq)

# write.table関数は、作成した頻度表をcsvファイルに書き出す
# fileは、ファイル名
write.table(RMeCabFreq.result.2, file = "wordlist.csv", sep = ",", row.names = TRUE, col.names = NA)
# Macなどで出力したファイルが文字化けした場合
# write.table(RMeCabFreq.result.2, file = "wordlist.csv", sep = ",", row.names = TRUE, col.names = NA, fileEncoding = "UTF-8")
# 保存したファイルがどこにあるか分からなくなった場合
getwd()

########################## 重要！！！！！！！！
# 「猫」という文字列を含む単語のみを表示
RMeCabFreq.result.2[grep("猫", RMeCabFreq.result.2$Term), ]
# 「犬」という文字列を含む単語のみを表示
RMeCabFreq.result.2[grep("犬", RMeCabFreq.result.2$Term), ]
# 「猫」もしくは「犬」という文字列を含む単語のみを表示
RMeCabFreq.result.2[grep("猫|犬", RMeCabFreq.result.2$Term), ]
# 「^猫$」で"猫"という単語のみを表示
RMeCabFreq.result.2[grep("^猫$", RMeCabFreq.result.2$Term), ]

###########################################################



# n-gramの抽出
# 文字2-gram（wagahaiwa_nekodearu.txtを選択）
ngram.result.1 <- Ngram(file.choose(), type = 0)
head(ngram.result.1, 5)

# 形態素2-gram（wagahaiwa_nekodearu.txtを選択）
ngram.result.2 <- Ngram(file.choose(), type = 1)
head(ngram.result.2, 5)

# 品詞2-gram（wagahaiwa_nekodearu.txtを選択）
ngram.result.3 <- Ngram(file.choose(), type = 2)
head(ngram.result.3, 5)

# 形態素n-gramの抽出における品詞の指定（wagahaiwa_nekodearu.txtを選択）
ngram.result.4 <- Ngram(file.choose(), type = 1, pos = c("名詞", "動詞", "形容詞", "副詞"))
head(ngram.result.4, 5)

# n-gramの長さを変更（wagahaiwa_nekodearu.txtを選択）
ngram.result.5 <- Ngram(file.choose(), type = 1, N = 3)
head(ngram.result.5, 5)

# Ngram関数の解析結果を頻度順に並び替え
ngram.result.6 <- ngram.result.2[order(ngram.result.2$Freq, decreasing = TRUE), ]
head(ngram.result.6, 5)

# docDF関数によるn-gramの抽出（wagahaiwa_nekodearu.txtを選択）
docDF.result <- docDF(file.choose(), type = 1, N = 2)
head(docDF.result, 5)







############################## 共起語 ################################################
# 「吾輩」の前後5語以内の共起語（wagahaiwa_nekodearu.txtを選択）
collocate.result <- collocate(file.choose(), node = "吾輩", span = 5)
head(collocate.result, 5)

# 「collScores」で、TとMIの両方を出力。
# 「吾輩」との共起語
# T値は、ノード（検索語）と高頻度で共起する単語に、比較的高い値が与えられる。
# MI値は、その単語自体は全体を通して低頻度だが、その単語が使われる時にノード（検索語）と共起する場合、高い値が与えられる。
collScores.result <- collScores(collocate.result, node = "吾輩", span = 5)
head(collScores.result, 5)

# 共起強度の計算結果を並び替え
# Tで並び替え
collScores.result.2 <- collScores.result[order(collScores.result$T, decreasing = TRUE), ]
head(collScores.result.2, 5)

# MIで並び替え
collScores.result.3 <- collScores.result[order(collScores.result$MI, decreasing = TRUE), ]
head(collScores.result.3, 5)


# install.packages("igraph", dependencies = TRUE)
# 追加パッケージの読み込み（Rを起動するごとに毎回）
library(igraph)
# NgramDFによる共起語の集計（wagahaiwa_nekodearu.txtを選択）
NgramDF.result <- NgramDF(file.choose(), type = 1, N = 2, pos = "名詞")
# 共起頻度2以上のペアのみを抽出(Freq=1は１より大きい値)
NgramDF.result.2 <- subset(NgramDF.result, Freq > 1)
# 共起ネットワークの描画
g <- graph.data.frame(NgramDF.result.2, directed = FALSE)
plot(g, vertex.label = V(g)$name, vertex.color = "grey")


# 共起頻度3以上のペアのみを抽出
NgramDF.result.3 <- subset(NgramDF.result, Freq > 2)
# ネットワークの描画
g.2 <- graph.data.frame(NgramDF.result.3, directed = FALSE)
plot(g.2, vertex.label = V(g.2)$name, vertex.color = "grey")


