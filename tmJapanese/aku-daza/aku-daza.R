
library(rvest)
library(dplyr)
library(stringr)
library(RMeCab)


getwd()
setwd("./GitHub/R/tmJapanese")
a_html  <- read_html("http://www.aozora.gr.jp/index_pages/person879.html#sakuhin_list_1")
a_text <- a_html %>%
  html_nodes("ol li") %>%
  html_text()
head(a_text) # 先頭の6つを表示



a_text_open <- a_text[1:374] %>% #公開作品のみを抽出
  as.data.frame() #データフレーム化
colnames(a_text_open) <- c("text_name") #列名を設定


set.seed(123)
a_10 <- a_text_open %>%
  filter(str_detect(.$text_name, "新字新仮名") == TRUE) %>% #新字新仮名の作品に絞る
  mutate(title = str_replace_all(.$text_name, "（.+?）", ""), #タイトルだけの列を追加
         id = str_replace_all(.$text_name, "[^0-9]", ""), #IDの列を追加
         author = "芥川") %>% #作者を示す列を追加
  select(- text_name) %>% #元のタイトルの列を削除
  sample_n(10)  #ランダムサンプリング
a_10


# 太宰
d_html  <- read_html("http://www.aozora.gr.jp/index_pages/person35.html#sakuhin_list_1")
d_text <- d_html %>%
  html_nodes("ol li") %>%
  html_text()
head(d_text) # 先頭の6つを表示

## データフレーム化
d_text_open <- d_text[1:272] %>% #公開作品のみを抽出
  as.data.frame() #データフレーム化
colnames(d_text_open) <- c("text_name") #列名を設定

## 新字新仮名の作品に絞り、作品IDのみを残す。さらにランダムサンプリングで10個だけを取得
set.seed(123)

d_10 <- d_text_open %>%
  filter(str_detect(.$text_name, "新字新仮名") == TRUE) %>% #新字新仮名の作品に絞る
  mutate(title = str_replace_all(.$text_name, "（.+?）", ""), #タイトルだけの列を追加
         id = str_replace_all(.$text_name, "[^0-9]", ""), #IDの列を追加
         author = "太宰") %>% #作者を示す列を追加
  select(- text_name) %>% #元のタイトルの列を削除
  sample_n(10)  #ランダムサンプリング


# 芥川と太宰を連結
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

## ダウンロード
### 芥川
Aozora ("http://www.aozora.gr.jp/cards/000879/files/187_ruby_1150.zip", "187_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/158_ruby_1243.zip", "158_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/43016_ruby_16663.zip", "43016_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/43383_ruby_25741.zip", "43383_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/123_ruby_1199.zip", "123_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/123_ruby_1199.zip", "1125_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/39_ruby_881.zip", "39_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/43362_ruby_20778.zip", "43362_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/43376_ruby_25698.zip", "43376_")
Aozora ("http://www.aozora.gr.jp/cards/000879/files/101_ruby_857.zip", "101_")

### 太宰
Aozora ("http://www.aozora.gr.jp/cards/000035/files/311_ruby_20050.zip", "311_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/275_ruby_1532.zip", "275_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/1084_ruby_4753.zip", "1084_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/251_ruby_3560.zip", "251_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/42363_ruby_15857.zip", "42363_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/45671_ruby_20800.zip", "45671_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/290_ruby_19972.zip", "290_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/42356_ruby_15854.zip", "42356_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/18349_ruby_12213.zip", "18349_")
Aozora ("http://www.aozora.gr.jp/cards/000035/files/1587_ruby_18163.zip", "1587_")




getwd()
# 形態素解析----
res <- docDF("./NORUBY", type = 1, N = 2) %>%
  filter((str_detect(.$TERM, "か-、") == TRUE | #ここからフィルタリング
            str_detect(.$TERM, "が-、") == TRUE |
            str_detect(.$TERM, "く-、") == TRUE |
            str_detect(.$TERM, "し-、") == TRUE |
            str_detect(.$TERM, "ず-、") == TRUE |
            str_detect(.$TERM, "て-、") == TRUE |
            str_detect(.$TERM, "で-、") == TRUE |
            str_detect(.$TERM, "と-、") == TRUE |
            str_detect(.$TERM, "に-、") == TRUE |
            str_detect(.$TERM, "は-、") == TRUE |
            str_detect(.$TERM, "ば-、") == TRUE |
            str_detect(.$TERM, "へ-、") == TRUE |
            str_detect(.$TERM, "も-、") == TRUE |
            str_detect(.$TERM, "ら-、") == TRUE |
            str_detect(.$TERM, "り-、") == TRUE |
            str_detect(.$TERM, "れ-、") == TRUE),
         str_length(.$TERM) == 3)
res[1:10, 1:8] # 先頭10行、5列を表示




# kernlab::ksvm()でSVMを行うにはデータの形式として①特徴量は列でなければならない、
# ②同じ名前の列が複数あってはならない、という制約があるのでこの状態ではSVMの解析は無理。
# そこで、SVMでの分析ができるようにデータフレームを転置し、
# 品詞は異なるが同じ文字であるもの(たとえば、2-4行目の「が」)については頻度を合計するという処理。 
# このとき、上記の16種類の文字のうち、「ず」「れ」については今回扱ったデータでは読点との共起がなかったので列としては省略.
# SVMで分析するための形式に加工
res_trans <- res %>%
  select(-POS1, -POS2) %>%
  t() %>%
  as.data.frame() %>%
  slice(-1) %>%
  mutate_all(.funs = as.integer) %>%
  mutate( か = V1,
         が = V2 + V3 + V4,
         く = V5,
         し = V6,
         て = V7 + V8,
         で = V9 + V10,
         と = V11 + V12 + V13 + V14,
         に = V15 + V16,
         は = V17,
         ば = V18,
         へ = V19,
         も = V20,
         ら = V21,
         り = V22) %>%
  select(か, が, く ,し, て, で, と, に, は, ば, へ, も, ら, り)

# 作品一覧と連結
text_bigram <- text_list %>%
  arrange(id) %>%
  cbind(., res_trans)




# 上で求めたバイグラムでは単純に作品ごとの頻度を足しただけなので、絶対頻度を算出してる。
# しかし、各作品の文字数が異なる。そこで、バイグラムを1000語あたりの相対頻度に変換。
setwd("./NORUBY")
files <- list.files("./")
for (i in 1:length(files)) {
  assign(paste("wn", i, sep = ""), length(RMeCabText(files[i])))
}



### 連結してデータフレーム化
wn <- rbind(wn1, wn2, wn3, wn4, wn5, wn6, wn7, wn8, wn9, wn10,
            wn11, wn12, wn13, wn14, wn15, wn16, wn17, wn18, wn19, wn20) %>%
  as.data.frame()

# 相対頻度に変換して解析用データ完成
svm_dat <- text_bigram %>%
  cbind(., wn) %>%
  rename(wn = V1) %>%
  mutate_each(funs(.*1000/wn), -title, -id, -author, -wn) %>%
  select(-wn)
head(svm_dat) #先頭6行を表示






