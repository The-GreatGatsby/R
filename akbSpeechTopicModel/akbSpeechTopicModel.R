# AKB総選挙演説　トピックモデル
###############################データの前処理
# 徳光さんが話しかける前のスピーチ部分を使用（徳光さんに誘導されて答えている部分は使用せず最初に自分で考えて喋った部分を採用）
# 上記ブログでは「えー」「あー」なども含まれているため、分析に必要ないと判断できる部分を除く。
# 「みなさん」「皆様」などファンに対する敬称を統一した。

setwd("C:/Users/rstud/Documents/GitHub/R/akbSpeechTopicModel")
sashi <- docDF("sashihara.txt",type=1,
               pos = c("名詞","形容詞","動詞"))
head(sashi)

# rscriptとデータの所在地を分ける。一緒だとrscriptのコードも読み込まれる。
all <- docDF("C:/Users/rstud/Documents/GitHub/R/akbSpeechTopicModel/data",
             type=1,pos = c("名詞","形容詞","動詞"), minFreq=2)
# 行数nrow()、列数ncol()を出力
dim(all) 
head(all)


library(RMeCab)
library(dplyr)
library(magrittr)
library(topicmodels)
library(lda)
library(ggplot2)
library(tm)
library(tidyr)






