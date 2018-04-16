
library(rvest)
library(dplyr)
library(stringr)
library(RMeCab)

getwd()
setwd("./GitHub/R/tmJapanese/ougai-honyaku")




#####################################
# 夏目漱石
a_html  <- read_html("https://www.aozora.gr.jp/index_pages/person148.html#sakuhin_list_1")
a_text <- a_html %>%
  html_nodes("ol li") %>%
  html_text()
head(a_text) # 先頭の6つを表示


a_text_open <- a_text[1:105] %>% #公開作品のみを抽出
  as.data.frame() #データフレーム化
colnames(a_text_open) <- c("text_name") #列名を設定


a_10 <- a_text_open %>%
  filter(str_detect(.$text_name, "新字新仮名") == TRUE) %>% #新字新仮名の作品に絞る
  mutate(title = str_replace_all(.$text_name, "（.+?）", ""), #タイトルだけの列を追加
         id = str_replace_all(.$text_name, "[^0-9]", ""), #IDの列を追加
         author = "夏目漱石") %>% #作者を示す列を追加
  select(- text_name)  #元のタイトルの列を削除

View(a_10)



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
Aozora("https://www.aozora.gr.jp/cards/000883/files/3415_ruby_28549.zip")
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
Aozora("https://www.aozora.gr.jp/cards/000883/files/3416_ruby_28550.zip")
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



















