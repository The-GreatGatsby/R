#あいうえお


### 8.1 ダウンロードしたファイルの整形と解析
source("http://rmecab.jp/R/Aozora.R")
Aozora

# 青空文庫の電子データには、漢字の読みがついている。
# 「Aozora()」関数はそれを消してくれる。
# 引数には、URL。
x <- Aozora("http://www.aozora.gr.jp/cards/000081/files/43754_ruby_17594.zip")

library(RMeCab)
setwd("/home/ishida/Dropbox/R/Morikita/Version2/")

miyaz <- docDF("data/NORUBY/chumonno_oi_ryoriten2.txt", type = 1)
miyaz <- docDF(x, type = 1)
library(dplyr)
miyaz %>% head()

miyaz %>% filter(POS2 == "固有名詞")
# まずは、全ての列名を抽出して、それから、POSで必要な品詞のみを抽出。
# そして、頻度列だけ列名を変える。
miyaz2 <- miyaz %>% select(everything(), FREQ = chumonno_oi_ryoriten2.txt) %>% 
  filter(POS1 %in% c("名詞","形容詞"), 
         POS2 %in% c("一般", "固有名詞", "自立"))

miyaz2 %>% arrange(FREQ) %>% tail(50)

###### ワードクラウドを作成する準備
# install.packages("wordcloud")
library(wordcloud)
## プロット作成
# min.freqは頻度の最小値。
# scaleは文字の最大・最小の大きさ
# familyは文字指定。
# brewer.palでカラーパレットを作成。
wordcloud(miyaz2$TERM, miyaz2$FREQ, min.freq = 3, 
          scale = c(6,1),family = "JP1", colors = brewer.pal(8, "Dark2"))


########### ネットワークグラフ

bigram <- NgramDF("data/NORUBY/chumonno_oi_ryoriten2.txt", type = 1, 
                  pos = c("名詞","形容詞", "動詞"))
bigram <- NgramDF(x, type = 1, pos = c("名詞","形容詞", "動詞"))
bigram %>% head()
bigram2 <- docDF("data/NORUBY/chumonno_oi_ryoriten2.txt", type = 1, N = 2, 
                 pos = c("名詞","形容詞", "動詞"), nDF = 1)
bigram2 %>% head()

library(magrittr)
# use_series()関数は、データフレームから列を取り出し、ベクトルに変換する。
# unique()関数は重複を取り除く関数。
# POS2で、品詞細分類が75パターンあることが分かる。
bigram2 %>% use_series(POS2) %>% unique()

## 漢数字が名詞と判定されて抽出されている事が分かる。
## だから、「数‐数」のペアは削除して良い。
bigram2 %>% filter(POS2 == "数-数")

# これらを削除するかは微妙
# 「ぼくら」は頻度7なので、テキストの内容を理解する上で重要かもしれない。
bigram2 %>% filter(POS2 == "代名詞-接尾")

# 品詞細分類が「数」「接尾」「非自立」以外の要素を取り出す。
# grepl()は、文字列を検索して、TRUE・FALSEで返す。
# 「!」は「それ以外」
bigram3 <- bigram2 %>% select(everything(),FREQ = chumonno_oi_ryoriten2.txt) %>%
  filter(!grepl("数|接尾|非自立", POS2) | FREQ > 5)
bigram3 %>% NROW()
bigram3 %>% head()

# bigram3では、品詞細分類が「数」「接尾」「非自立」を削除した。
# bigram4で、頻度2以上のみを抽出。
bigram4 <- bigram3 %>% select (N1, N2, FREQ) %>% 
  filter(FREQ >= 2)
bigram4 %>% NROW()

# 49ペア抽出された内、頻度が最大のペアはこれ。
bigram4 %>% filter(FREQ == max(FREQ))
# 今回は「二人」の単語ペアを削除。
bigram5 <- bigram4 %>% filter(FREQ < 25)

library(igraph)
## ネットワークグラフを作るには、データ形式がネットワークグラフ形式でなければならない。
## graph_from_data_frameは、データフレームをネットワークグラフ形式に変換する関数。
bigram6 <- graph_from_data_frame(bigram5)

# ネットワークグラフ形式になったデータをプロットする。（インタラクティブ）
tkplot(bigram6, vertex.color = "SkyBlue", vertex.size = 22)

E(bigram6)$weight <- bigram5$FREQ *2

bigram7 <- edge.betweenness.community(bigram6, 
                                      weights = E(bigram6)$weight, 
                                      directed = F)


## マージンを調整する
par(mar= c(1,0,1,0), oma = c(0,0,0,0), omi = c(0,0,0,0))
plot(bigram6, vertex.color = "SkyBlue", vertex.size = 6,
     vertex.label.cex  = 1.5, ## 形態素のサイズ
     vertex.label.dist = .5,  ## ラベル（単語）を円の外に表示。円のサイズが小さいので、単語がはみ出ると見にくくなる。だから、元から外に表示。 
     edge.width = E(bigram6)$weight, ## 辺のサイズを調整
     vertex.label.family = "JP1") ## フォントの指定

# edge.betweenness.community関数で、コミュニティを作る。
bigram7 <- edge.betweenness.community(bigram6, 
                                      weights = E(bigram6)$weight, 
                                      directed = F)

plot(bigram7, bigram6, vertex.label.family = "JP1")# Mac の場合は family = "HiraKakuProN-W3" と変えてください




