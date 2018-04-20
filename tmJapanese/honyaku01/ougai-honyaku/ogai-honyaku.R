# 翻訳作品で「ファウスト」「田舎」「家常茶飯」は長いので削除
library(rvest)
library(dplyr)
library(stringr)
library(RMeCab)
getwd()
setwd("./GitHub/R/tmJapanese/ougai-honyaku")

#####################################
#########################################
# 森鴎外
d_html  <- read_html("https://www.aozora.gr.jp/index_pages/person129.html#sakuhin_list_1")
d_text <- d_html %>%
  html_nodes("ol li") %>%
  html_text()
head(d_text) # 先頭の6つを表示

## データフレーム化
d_text_open <- d_text[1:131] %>% #公開作品のみを抽出
  as.data.frame() #データフレーム化
colnames(d_text_open) <- c("text_name") #列名を設定

## 新字新仮名の作品に絞り、作品IDのみを残す。さらにランダムサンプリングで10個だけを取得
d_10 <- d_text_open %>%
  filter(str_detect(.$text_name, "新字新仮名") == TRUE) %>% #新字新仮名の作品に絞る
  mutate(title = str_replace_all(.$text_name, "（.+?）", ""), #タイトルだけの列を追加
         id = str_replace_all(.$text_name, "[^0-9]", ""), #IDの列を追加
         author = "森鴎外") %>% #作者を示す列を追加
  select(- text_name) #元のタイトルの列を削除

# ２つを連結
text_list <- rbind(a_10, d_10)
text_list



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


################################ ダウンロード####################################
# 森鴎外　翻訳作品
Aozora("https://www.aozora.gr.jp/cards/000882/files/3414_ruby_28548.zip") # これはプレヴォー マルセルの「田舎」を翻訳
Aozora("https://www.aozora.gr.jp/cards/000907/files/4251_ruby_12064.zip") # アンドレーエフ レオニード・ニコラーエヴィチ「犬」
Aozora("https://www.aozora.gr.jp/cards/000884/files/3418_ruby_28552.zip") # ディモフ オシップ「襟」
Aozora("https://www.aozora.gr.jp/cards/001193/files/50915_ruby_39238.zip") # オイレンベルク ヘルベルト「女の決闘」
Aozora("https://www.aozora.gr.jp/cards/000075/files/4250_ruby_32179.zip") #リルケ ライネル・マリア「家常茶飯　附・現代思想」
Aozora("https://www.aozora.gr.jp/cards/001194/files/50916_ruby_39174.zip") # シュミットボン ウィルヘルム「鴉」
Aozora("https://www.aozora.gr.jp/cards/000883/files/3417_ruby_28551.zip") # モルナール フェレンツ「最終の午後」
Aozora("https://www.aozora.gr.jp/cards/000366/files/50919_ruby_39173.zip") #アルチバシェッフ ミハイル・ペトローヴィチ「罪人」
Aozora("https://www.aozora.gr.jp/cards/000075/files/50920_ruby_39171.zip") # リルケ ライネル・マリア「白」
Aozora("https://www.aozora.gr.jp/cards/001195/files/50917_ruby_39236.zip") # ダビット ヤーコプ・ユリウス「世界漫遊」
Aozora("https://www.aozora.gr.jp/cards/001192/files/50914_ruby_39170.zip") # ホーフマンスタール フーゴー・フォン「痴人と死と」
Aozora("https://www.aozora.gr.jp/cards/001190/files/50913_ruby_39172.zip") # アルテンベルク ペーター「釣」
Aozora("https://www.aozora.gr.jp/cards/001062/files/50912_ruby_39237.zip") # ストリンドベリ アウグスト「一人舞台」
Aozora("https://www.aozora.gr.jp/cards/000881/files/3413_ruby_28547.zip") # ブウテ フレデリック「橋の下」
Aozora("https://www.aozora.gr.jp/cards/001025/files/50909_ruby_49057.zip") # ゲーテ ヨハン・ヴォルフガング・フォン「ファウスト」
Aozora("https://www.aozora.gr.jp/cards/001196/files/45276_ruby_19722.zip") # ランド ハンス「冬の王」
Aozora("https://www.aozora.gr.jp/cards/000883/files/3415_ruby_28549.zip") #モルナール・フェレンツの「破落戸の昇天」
Aozora("https://www.aozora.gr.jp/cards/000883/files/3416_ruby_28550.zip") #モルナール・フェレンツ「辻馬車」
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




