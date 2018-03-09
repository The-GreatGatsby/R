# あいうえお

getwd()
setwd("C:/Users/rstud/Documents/GitHub/R/TextMining")
getwd()


## 沖縄観光への意見データ
okinawa <- read.csv("data/H18koe.csv")
## 行数を確認
NROW(okinawa)
## 列名を確認
colnames(okinawa)

library(dplyr)
library(magrittr)

# まずはデータを要約して見る。
# optionは自由記述なので、省く。
okinawa %>% select(Region:Satis) %>% summary()

## データフレームから地域列を削除して上書き。今回の分析では使わない。
okinawa %<>% select(-Region)
## 欠損値を削除して上書き。「na.omit()」
okinawa %<>% na.omit()

okinawa %>% select(Sex:Satis) %>% summary()

# パイプ処理の場合、「data = .」にしなアカン
# xtabsは2変数を指定するが「~ x + y」にのように「~」が必要。
okinawa %>% xtabs(~ Sex + Satis , data = .)
# このようなクロス表の変数同士の関連性を調べるには、「カイ二乗検定」



############################## これから、optionの解析に入る。
# データフレームから、Age列ををベクトルに変換。「use_series()」
# それを、「levels()」で水準の種類を確認。
AgeL <- okinawa %>% use_series(Age) %>% levels() 
AgeL

library(purrr)
######## よくわからん　p115
# Ageベクトルのそれぞれの要素の頻度を確認するには、「map()」
# NROW()は行数を数える関数。そのまま使うと、全部の行数をカウントする。
# 今回は、各水準（年代別）で数えたいから、まず各水準でフィルターをかけてから、NROW()をする。
AgeL %>% map(
  ~ filter(okinawa, Age == .x) %>% NROW() #チルダ（~）で、その行が関数で始まることを明示している。
)

# map()だと出力はリストで見にくい。
# map_chr()だと、ベクトルで出力してくれる。
AgeL %>% map_chr(
  ~ filter(okinawa, Age == .x) %>% NROW()
)

# 10代の回答が4しかないから削除。10代は一行目なので、添字「-1」
AgeL <- AgeL[-1]


#########################################
# 性別・年齢で分けた12種類のファイルを作成する。
library(magrittr)
AgeL %>% map(
  ~ filter(okinawa, Age  == .x, Sex == "女性") %>% {       #Sexが女性のデータを各Ageに分ける
    tmp <-  use_series(data = ., Opinion) %>% as.character() 
    writeLines(text = tmp, con = paste0("F", (2:7)[AgeL == .x], "0.txt"))
  }
)

library(magrittr)
AgeL %>% map(
  ~ filter(okinawa, Age  == .x, Sex == "男性") %>% {
    tmp <-  use_series(data = ., Opinion) %>% as.character() 
    writeLines(text = tmp, con = paste0("M", (2:7)[AgeL == .x], "0.txt"))
  }
)
#########################################

library(RMeCab)
# 整えたテキストデータから、単語文書行列を作る。
FM <- docDF("data/okinawa", type = 1,     #type=1は形態素解析。
            pos = c("名詞","動詞","形容詞"))

# 形態素から、必要な単語のみを抽出。
FM2 <- FM %>% filter(POS2 %in% c("一般", "固有", "自立"))

# 動詞の自立語には、「ある」「いう」「できる」「思う」など不要な単語も入っている。
# これらを削除するには、「!」と「%in%」を使う。
FM2 <- FM2 %>% filter(! TERM %in% c("ある","いう","いる", "する", 
                                    "できる", "なる","思う"))
FM2 %>% NROW()

# 「$」は添字。FM2の単語文書行列に「SUMS」という列を新規追加。
# rowSumsは各行の合計を求める。SUMS列に代入。
# 1，2，3列目は形態素と品詞の情報なので除外。
FM2$SUMS <- rowSums(FM2[, -(1:3)])
summary(FM2$SUMS)

# 今回は7回以上の単語のみ抽出するが、数字はデータによる
# 各行の合計をSUMSに入れているので、7回は、「男女20～70代全部の合計で7回以上使われた単語」という意味
FM3 <- FM2 %>% filter(SUMS >= 7)
FM3 %>% NROW
colnames((FM3))

# 7回以上出現した単語を出力
FM3$TERM
################################ 168行目からオカシイ。
FM3$TERM[168]



library(stringr)
## 正規表現で数値列だけを取り出す
# matches()は指定された文字列と一致する列だけ取り出す。
# [FM]はFかMで始まり、「\\d\\d」は数字が2つ続く。
FM4 <- FM3 %>% select(matches("[FM]\\d\\d"))
## 列名を設定
colnames(FM4) <- str_extract(colnames(FM4), "[FM]\\d\\d")
## 行列の名前を設定
rownames(FM4) <- FM3$TERM
## 次元(行数と列数)を確認
dim(FM4)
## 列名と行名を確認
colnames(FM4) 
rownames(FM4)





### 7.3 意見データの対応分析
library(FactoMineR)
FM4ca <- CA(FM4, graph = FALSE)
## ggplot2 ベースのバイプロットを描く
library(factoextra)
fviz_ca_biplot(FM4ca)

# 上記の実行結果の画像で文字化けが生じている場合、以下のようにPDF画像として作成して確認してみてください
# 3行続けて実行することで画像ファイルが作成されます
# RStudio 右のFilesタブで画像ファイルをクリックすることで、適切なビューワー が立ちあがります
cairo_pdf("FM4ca.pdf", family = "JP1")# Mac の場合は family = "HiraKakuProN-W3" と変えてください
fviz_ca_biplot(FM4ca)
dev.off()


### 独立性の検定(カイ自乗検定)
# Excel ファイルの読み込み
library(readxl)

setwd("C:/Users/ishida/Documents/TextMining")

dat <- read_excel("data/sentences.xlsx")

## クロス表を生成
dat_tb <- xtabs(~ Sex + Sent, data = dat)
dat_tb

options("digits" = 7)

chisq.test(dat_tb)

### 7.5 対応分析

dat <- matrix(c(1,2,0,0,  0,2,6,0, 0,1,2,2,  0,0,0,2),
              ncol = 4, byrow = TRUE)
colnames(dat) <- c("中卒F", "高校中退F", "高卒F", "大卒F")
rownames(dat) <- c("中卒M", "高校中退M", "高卒M", "大卒M")
dat

library(FactoMineR)
library(factoextra)
datCA <- CA(dat, graph = FALSE)
fviz_ca_biplot(datCA)

# 上記の実行結果の画像で文字化けが生じている場合、以下のようにPDF画像として作成して確認してみてください
# 3行続けて実行することで画像ファイルが作成されます
# RStudio 右のFilesタブで画像ファイルをクリックすることで、適切なビューワー が立ちあがります
cairo_pdf(file = "datCA.pdf", family = "JP1") # Mac の場合は family = "HiraKakuProN-W3" と変えてください
fviz_ca_biplot(datCA)
dev.off()


dat_cp <- MASS::corresp(dat, nf = 2)
biplot(dat_cp)











