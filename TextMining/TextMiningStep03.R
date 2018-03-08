##あいおうえ


getwd()
setwd("C:/Users/rstud/Documents/GitHub/R/TextMining")
getwd()

#########################################################
# スクレイピング。URL先のサイト構造が変わったからこのコードは無効

library(rvest)
# read_html("URL")で、URL先のページ情報を全て取得する。
jobs <- read_html("http://bookmeter.com/b/4062180731")

library(dplyr)
# さっきのURLだと書評だけでなく、書籍のあらすじなども取得されている。
# jobsオブジェクトに代入された全ての情報から書評テキスト（ノード）だけを抽出。
# ノードだけを抽出するには、「html_nodes()」を使う。
# ノードの中からテキストだけを抽出。「html_text()」
# read_html() → html_nodes() → html_text()
reviews <- jobs %>% html_nodes("div[id^='review_text_']") %>% html_text()

library(magrittr)
# 抽出したテキストを表示するには「extract()」
# しかし、今はページの構造が変わっているから、NAになる
reviews %>% extract(1) 

library(RMeCab)
# 「RMeCabC()」で書評を形態素解析。
reviews %>% extract2(1) %>% RMeCabC() %>% unlist()

#reviewsオブジェクトのデータをtxt拡張子で保存。
writeLines(reviews, "data/reviews.txt")

revi <- docDF("data/reviews.txt", type = 1, pos = c("名詞","形容詞", "動詞"))
revi %>% NROW()

########################## ここまでが無効 #################
##################################################################


# 上記コードは、サイト構造の変更のため、無効である。
# だから、エラーなしの完成版が、「revi.csv」に入ってる。
revi <- read.csv("data/revi.csv", stringsAsFactors = FALSE)
revi %>% head (15)

# 品詞ごとに分けて、解析結果を確認。
# まずは、名詞。
revi %>% filter(POS1 == "名詞") %>% head(10)
# pos2を見ると、訳分らん分類もある。
# 書評の内容を推し量るのに必要な「一般・固有」のみを抽出。
revi %>% filter(POS1 == "名詞",  POS2 %in% c("一般","固有")) %>% head(15)

# 次は、動詞・形容詞。
revi %>% filter(POS1 == "動詞") %>% head(10)
revi %>% filter(POS1 == "形容詞") %>% head(10)

# 名詞は「一般・固有」、動詞・形容詞は「自立語」に限定。
revi2 <- revi %>% filter(POS2 %in% c("一般", "固有", "自立"))
revi2 %>% NROW()

# revi2のデータの頻度の列名は、FREQではなく「reviews.txt」になっている。
# だから頻度順に並び替えるには、arrange()関数で、「reviews.txt」の列名を指定。
revi2 %>% arrange(reviews.txt) %>% tail(30)


## 上記では、内容を推し量るために、必要な単語を抽出した。
## より内容を推し量るために、言葉のつながりを確認する。
## そのために、Nグラムを作成する。
########################################################
######以下は、バイグラム作成コードだが、サイト構造の変更で、reviews.txtは存在しない。

# docDFでNグラムを作れる。
bigram <- docDF("data/reviews.txt", type = 1, nDF = 1, N = 2, 
               pos = c("名詞","形容詞","動詞"))
# bigramオブジェクトに代入されたNグラムデータを「csv」にして作成。
write.csv(bigram, file = "data/bigram.csv", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")

########################################################

# 上記コードはエラーのため、完成版が以下である。
# bigramオブジェクトに、「動詞・形容詞・名詞」のバイグラムを代入
bigram  <- read.csv("data/bigram.csv", stringsAsFactors = FALSE)
# 頻度のカラム名がreviews.txtになってる。だから頻度順に並び替えるにはこのコードになる。
bigram %>% arrange(reviews.txt) %>% tail(10)


bigram %>% arrange(reviews.txt) %>% head(30)
# バイグラムのほとんどが頻度1なので、2以上に絞る。
bigram2 <- bigram %>% select (N1, N2, reviews.txt) %>% 
  filter(reviews.txt > 1)
bigram2 %>% class() # データの型を確認。

# バイグラムからネットワークグラフを作るには「igraph」
library(igraph)

# Nグラムデータからネットワークグラフを作るには、ネットワークオブジェクトに変換が必要。
# bigram2オブジェクトは今、データフレーム型なので、ネットワークオブジェクトに変換。
bigramN <- graph.data.frame(bigram2)
bigramN <- graph_from_data_frame(bigram2)

# ネットワークオブジェクトからねとワークグラフを作る。
# 「vertex」がプロット上の円。
# 「tkplot」はインタラクティブ（グラフを操作可能な）なプロットを出力。
# viewsのfull screenをクリックすると見やすさ最適化。
tkplot(bigramN, vertex.color = "SkyBlue", vertex.size = 22)
# 「plot」は静的な（画像）プロットを作る。
plot(bigramN, vertex.color = "SkyBlue", vertex.size = 18)


