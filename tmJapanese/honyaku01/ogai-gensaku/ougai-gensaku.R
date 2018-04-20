# 森鴎外原作
library(rvest)
library(dplyr)
library(stringr)
library(RMeCab)
getwd()
setwd("C:/Users/rstud/Documents/GitHub/R/tmJapanese/honyaku01/ogai-gensaku")



Aozora <- function(url = NULL, txtname  = NULL){
  enc <-  switch(.Platform$pkgType, "win.binary" = "CP932", "UTF-8")
  if (is.null(url)) stop ("specify URL")
  tmp <- unlist (strsplit (url, "/"))
  tmp <- tmp [length (tmp)]
  
  curDir <- getwd()
  tmp <- paste(curDir, tmp, sep = "/")
  download.file (url, tmp)
  
  textF <- unzip (tmp)
  unlink (tmp)
  
  if(!file.exists (textF)) stop ("something wrong!")
  if (is.null(txtname)) txtname <- paste(unlist(strsplit(basename (textF), ".txt$")))
  if (txtname != "NORUBY")  {
    
    newDir <- paste(dirname (textF), "NORUBY", sep = "/")
    
    if (! file.exists (newDir)) dir.create (newDir)
    
    newFile <- paste (newDir,  "/", txtname, "2.txt", sep = "")
    
    con <- file(textF, 'r', encoding = "CP932" )
    outfile <- file(newFile, 'w', encoding = enc)
    flag <- 0;
    reg1 <- enc2native ("\U005E\U5E95\U672C")
    reg2 <- enc2native ("\U3010\U5165\U529B\U8005\U6CE8\U3011")
    reg3 <- enc2native ("\UFF3B\UFF03\U005B\U005E\UFF3D\U005D\U002A\UFF3D")
    reg4 <- enc2native ("\U300A\U005B\U005E\U300B\U005D\U002A\U300B")
    reg5 <- enc2native ("\UFF5C")
    while (length(input <- readLines(con, n=1, encoding = "CP932")) > 0){
      if (grepl(reg1, input)) break ;
      if (grepl(reg2, input)) break;
      if (grepl("^------", input)) {
        flag <- !flag
        next;
      }
      if (!flag){
        input <- gsub (reg3, "", input, perl = TRUE)
        input <- gsub (reg4, "", input, perl = TRUE)
        input <- gsub (reg5, "", input, perl = TRUE)
        writeLines(input, con=outfile)
      }
    }
    close(con); close(outfile)
    return (newDir);
  }
}

##################森鴎外の作品
0
Aozora("https://www.aozora.gr.jp/cards/000129/files/2595_ruby_20435.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/673_ruby_23254.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/695_ruby_22805.zip")
1
Aozora("https://www.aozora.gr.jp/cards/000129/files/45270_ruby_19021.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45209_ruby_29695.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/48209_ruby_29697.zip") 「初稿」
Aozora("https://www.aozora.gr.jp/cards/000129/files/680_ruby_23197.zip")
2
Aozora("https://www.aozora.gr.jp/cards/000129/files/678_ruby_22883.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45224_ruby_19890.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/1071_ruby_4557.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/43732_ruby_17108.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/3615_ruby_4642.zip")
3
Aozora("https://www.aozora.gr.jp/cards/000129/files/2051_ruby_22885.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/1054_ruby_23199.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2597_ruby_22937.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/4459_ruby_17993.zip")
4
Aozora("https://www.aozora.gr.jp/cards/000129/files/45244_ruby_21881.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2547_ruby.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/688_ruby_23233.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2598_ruby_20420.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/686_ruby_2541.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/685_ruby_20310.zip") 随筆
5
Aozora("https://www.aozora.gr.jp/cards/000129/files/689_ruby_23256.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/43030_ruby_17344.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2058_ruby_19627.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2599_ruby_23032.zip")
6
Aozora("https://www.aozora.gr.jp/cards/000129/files/690_ruby_23034.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/13201_ruby_9289.zip") 『新訳源氏物語』初版の序
Aozora("https://www.aozora.gr.jp/cards/000129/files/2522_ruby_5001.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45245_ruby_21882.zip")
7
Aozora("https://www.aozora.gr.jp/cards/000129/files/46234_ruby_22009.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/3336_ruby_23053.zip")
8
Aozora("https://www.aozora.gr.jp/cards/000129/files/3614_ruby_12061.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2049_ruby_19718.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45271_ruby_19022.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/42375_ruby_18247.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45618_ruby_24923.zip")
9
Aozora("https://www.aozora.gr.jp/cards/000129/files/45254_ruby_29696.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/676_ruby_23235.zip")
10
Aozora("https://www.aozora.gr.jp/cards/000129/files/50910_ruby_45862.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45255_ruby_19719.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/675_ruby_23191.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45256_ruby_19720.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2396_ruby_3993.zip")
11
Aozora("https://www.aozora.gr.jp/cards/000129/files/55722_ruby_56838.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2083_ruby_27447.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/2050_ruby_19721.zip") 随筆
Aozora("https://www.aozora.gr.jp/cards/000129/files/50911_ruby_45861.zip") # 「訳本ファウストについて」
Aozora("https://www.aozora.gr.jp/cards/000129/files/696_ruby_23259.zip")
12
Aozora("https://www.aozora.gr.jp/cards/000129/files/2079_ruby_23238.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45272_ruby_19023.zip")




res <- docNgram("./NORUBY", type = 0)
View(res)
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
View(res2)
iris %>% head()


### 主成分分析。5列目はカテゴリ変数で分析に入らないので削除。
iris_pc <- princomp(iris[ , -5])
iris_pc %>% head()

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
