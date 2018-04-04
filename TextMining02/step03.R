

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









