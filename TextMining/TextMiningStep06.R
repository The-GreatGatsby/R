##　あいうえお





## 戦後の総理大臣の所信表明演説を分析。
## 仮定：所信表明はその時代の政治的経済的課題を反映しているはずなので、
## 内容で分類すると時代ごとにクラスター（グループ）を形成できるかも

library(RMeCab)
# docMatrix2の方が高速。
## 単語文書行列を作成する場合、入力データの長さ（文章量）が大きく異なると、解析に影響が出る。
## 長さの影響を調整することを、「正規化」と言う。
## 「weight = "tf*idf*norm"」は文章量の違いを標準化してる。p90
prime <- docMatrix2("data/prime/sjis", pos = c("名詞","形容詞","動詞"), 
                    weight = "tf*idf*norm")  

# primeオブジェクトの列数(ncol)・行数(nrow)
# ;で命令を複数繋げれる。
ncol(prime) ; nrow(prime)


library(stringr)
library(dplyr)
library(magrittr)
## 列名が長すぎるので短縮する
## 「_general-policy-speech.txt」が不要なので削除。（空白に置き換える）
colnames(prime)  %<>% str_replace("_general-policy-speech.txt", "")

# \\dは数字。{4}は4回。4回連続数字が並ぶ。
# 4回数字が並び、また4回数字が並び、「_」を挟んで、3回数字が並ぶ文字列を
# 最初の4行と最後の3行だけを取り出して、新たに追加。p61
colnames(prime)  %<>% str_replace("(\\d{4})\\d{4}_(\\d{3})", "\\1_\\2")

##############################################
###上の3回は歴代が100以上しか抽出されない。
## 歴代が2桁の場合は、「(\\d{4})\\d{4}_(\\d{2})」にする。
##############################################


### 9.3 所信表明演説のクラスター分析
# クラスター分析の方法は、データを二つ選び、単語頻度の引き算・二乗・合計・平方根する。（標準偏差てきな）
#これを、ユークリッド距離といい、dist()で計算できる。methodはデフォルトでユークリッドになっている。
# hclust()でクラスターに統合（グループ分け）される。引数にmethodを入れる。
# ward.D2（ウォード法）がスタンダード。ward.Dはエラー出るからアカン。
hc <- prime %>% t %>% dist %>% hclust("ward.D2")
### docMatrix()関数の出力では、行に単語、列に文書が並んでいる。
### Rでクラスター分析をするには、行に分類対象（今回は文書）を並べる必要がある。
### だから、t()で転置。


library(ggdendro)
## FALSEにすると、x軸にファイル名が並ぶ。今はy軸に並んでる。
ggdendrogram(hc, rotate= TRUE)

# 上記の実行結果の画像で文字化けが生じている場合、以下のようにPDF画像として作成して確認してみてください
# 3行続けて実行することで画像ファイルが作成されます
# RStudio 右のFilesタブで画像ファイルをクリックすることで、適切なビューワー が立ちあがります
cairo_pdf(file = "hc.pdf", family = "JP1")
ggdendrogram(hc, rotate= TRUE)
dev.off()


### 9.5 特異値分解

TD <- matrix (c(1,0,0,0,1,0,
                0,1,0,1,0,1, 
                0,1,0,0,0,0,
                0,1,0,0,0,0,
                0,0,1,0,0,1,
                1,1,1,1,0,0,
                0,0,1,2,1,0,
                1,1,0,0,0,0), nrow = 8, byrow = TRUE)
## 作成した行列に列名と行名を設定
colnames(TD) <- paste0("doc", 1:6)
rownames(TD) <- paste0("w", 1:8)

# 特異値分解（3つの行列の積）
TD_svd <- svd(TD)

options(digits = 3)
TD_svd$u

TD_svd$d

TD_svd$v

t(TD_svd$u[, 1:3]) %*% TD

### 9.6 潜在的意味インデキシングによる分類
install.packages("rgl")

prime.svd <- svd(prime)
prime2 <- t(prime.svd$u[, 1:3]) %*% prime
dim(prime2)

colnames(prime2) <- prime2 %>% colnames() %>%
  str_extract("\\d{4}_\\d{2,3}")
cols <- prime2 %>% colnames() %>% str_extract("\\d{3}")

# パッケージ読み込み
library(rgl)
# 別ウィンドウを開き
rgl.open()
# 座標を色分けする
rgl.lines(c(-1,1), 0,0, color = "gold")
rgl.lines(0, c(-1,1), 0, color = "gray")
rgl.lines(0,0,c(-1,1), color = "black")
# 3次元空間のカラーを指定し
rgl.bbox(color = "blue", emission = "green")
# 文書名を付置する
rgl.texts(prime2[1,], prime2[2,], prime2[3,],
          colnames(prime2), color = cols)

rgl.snapshot(file = "prime.png")

rgl.close()

library(rgl)
plot3d(t(prime2), type = "n")
text3d(t(prime2), text = colnames(prime2), col = cols, cex = 1.4)

vignette(package = "rgl")
vignette("rgl")


### 9.7 トピックモデル
install.packages(c("topicmodels","lda"))

library(RMeCab)


prime <- docDF("data/prime/utf8", type = 1, 
               pos = c("名詞","形容詞"), minFreq = 3)

dim(prime)

prime2 <- prime %>% filter(POS2 %in% c("一般","自立"))
dim(prime2)

prime2$TERM %>% duplicated() %>% which()

library(stringr)
library(magrittr)
## 数値列だけを殘したオブジェクトを作成
prime3 <-  prime2 %>% select(-c(TERM:POS2))
## 行名に形態素解析の結果を設定 
rownames(prime3) <- prime2$TERM
## 列名は短縮化
colnames(prime3)  %<>% str_replace("_general-policy-speech.txt", "")
colnames(prime3)  %<>% str_replace("(\\d{4})\\d{4}_(\\d{3})", "\\1_\\2")

library(tm)
prime3a <- prime3 %>% t() %>%  as.DocumentTermMatrix(weighting = weightTf)

### 9.7.1 トピックモデルによるモデル推定
library(topicmodels)
## トピックの数を指定
K <- 5
res1 <- prime3a %>% LDA(K)

terms(res1)

str(res1)
posterior(res1)[[1]] [1:5,1:5]
posterior(res1)[[2]]


### 9.7.2 ldaパッケージによる分析と可視化

library(topicmodels)
prime4  <- dtm2ldaformat(prime3a)

library(lda)
set.seed(123)
K <- 5
result <- lda.collapsed.gibbs.sampler(prime4$documents, K = K, 
                                      prime4$vocab, 25, 0.1, 0.1, compute.log.likelihood=TRUE)

top.topic.words(result$topics, 10, by.score=TRUE)

prime5 <- rownames(prime3a) %>% str_subset("koizumi|hatoyama|noda|abe")
prime5

prime6 <- rownames(prime3a) %>% 
  str_detect("koizumi|hatoyama|noda|abe") %>% which
prime6

cbind(prime6, prime5)

## 文書全体のトピック割合
options(digits = 3)

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
## 対象とする所信表明演説を抽出
ministers  <- topic.proportions [c(64, 74, 77, 80), ]
ministers

ministers %>% rowSums()

ministers

## 行列をデータフレームに変換し列名を設定
ministersDF <- as.data.frame(ministers) %>% 
  set_names(paste0("topic", 1:5)) %>% 
  ## num という列を追加
  mutate(num = paste0("No", c(64, 74, 77, 80)))
ministersDF

# install.packages("tidyr")

library(tidyr)

ministersDF <- ministersDF %>% 
  gather(key = topic, value = props, -num)

library(ggplot2)

ministersDF %>% ggplot(aes(x = topic, y = props, fill = num)) +       geom_bar(stat = "identity") + facet_wrap(~num)

# 上記の実行結果の画像で文字化けが生じている場合、以下のようにPDF画像として作成して確認してみてください
# 3行続けて実行することで画像ファイルが作成されます
# RStudio 右のFilesタブで画像ファイルをクリックすることで、適切なビューワー が立ちあがります
cairo_pdf(file = "ministersDF.pdf", family = "JP1")# Mac の場合は family = "HiraKakuProN-W3" と変えてください
x
dev.off()
