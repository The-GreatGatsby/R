## あいうえお

getwd()
library(RMeCab)

############################ N グラムを利用したクラスター分析 #####################

### まずは、文字のバイグラムで、森鴎外と夏目漱石を判別できるかチャレンジ。
### 文字の出現頻度を解析するから、テキスト量が同じぐらいでないといけない。（作家判別に影響するから）
### type=0は文字単位。
### docNgramはデフォルトで、バイグラム。
res <- docNgram("data/writers", type = 0)
#行数・列数の確認
ncol(res) ; nrow(res)

library(dplyr) ## パイプ処理はdplyrが必要。
res %>% tail()

# クラスター分析をして、似ている作品を近くに表示。
# このクラスター分析の分類対象は、テキストである。だから分類対象であるテキストを行側に持ってくる必要がある。
# 距離測定には、ユークリッド距離。dist()
# ウォード法でクラスターを生成。（D2じゃないとアカン、Dはエラー）
res2 <- res %>% t() %>% dist() %>% hclust("ward.D2")

library(ggdendro)
res2 %>% ggdendrogram()

# 上記の実行結果の画像で文字化けが生じている場合、以下のようにPDF画像として作成して確認してみてください
# 3行続けて実行することで画像ファイルが作成されます
# RStudio 右のFilesタブで画像ファイルをクリックすることで、適切なビューワー が立ちあがります
cairo_pdf(file = "res2.pdf", family = "JP1")# Mac の場合は family = "HiraKakuProN-W3" と変えてください
ggdendrogram(res2)
dev.off()



######################### 助詞と読点から、書き手の癖を見分ける。 ###################
res2 <- res[rownames(res) %in% c("[と-、]", "[て-、]", "[は-、]", "[が-、]", 
                                 "[で-、]", "[に-、]", "[ら-、]", "[も-、]"), ]
#　次元（行数・列数）の確認。
dim(res2)

iris %>% head()


### 主成分分析。5列目はカテゴリ変数で分析に入らないので削除。
iris_pc <- princomp(iris[ , -5])
iris %>% head()

## アヤメの種類を数値で表わす
iris.name <- as.numeric(iris[, 5])
## プロットの土台だけ描く
# scores（主成分得点）のうち、最初の2つ（1・2列目）をx・y軸とする。
plot(iris_pc$scores[, 1:2], type = "n")
## 土台に文字を重ねる
## labはラベル。iris.nameにアヤメの種類を1，2，3で表したオブジェクトがあるからそれを使用。
text(iris_pc$scores[, 1:2], lab = iris.name, 
     col = as.numeric(iris.name))



### 10.5 主成分分析による作家の判別
res2_pc <- princomp(t(res2))
res2_pc <- res2 %>% t()%>% princomp()

options(digits = 3)

summary(res2_pc)

library(ggfortify)

library(stringr)

# ファイル名が長いから縮める。
rownames(res2_pc$scores) <- res2_pc$scores %>% 
  rownames() %>% 
  str_extract("[a-z]+") %>% 
  paste0(1:8)

autoplot(res2_pc, label =TRUE, label.size = 8, loadings = TRUE, 
         loadings.label = TRUE,  loadings.label.size  = 12, 
         loadings.label.family = "JP1")

# 上記の実行結果の画像で文字化けが生じている場合、以下のようにPDF画像として作成して確認してみてください
# 3行続けて実行することで画像ファイルが作成されます
# RStudio 右のFilesタブで画像ファイルをクリックすることで、適切なビューワー が立ちあがります
cairo_pdf(file = "res2pc.pdf", family = "JP1")# Mac の場合は family = "HiraKakuProN-W3" と変えてください
autoplot(res2_pc, label =TRUE, label.size = 8, loadings = TRUE, 
         loadings.label = TRUE,  loadings.label.size  = 12, 
         loadings.label.family = "JP1")# Mac の場合は family = "HiraKakuProN-W3" と変えてください
dev.off()




