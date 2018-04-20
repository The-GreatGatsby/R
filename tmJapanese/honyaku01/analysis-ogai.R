# 翻訳作品と原作での違いを分析。
# 「破落戸」「辻馬車」「一人舞台」は削除すべき
# 「襟」「冬の王」は、文字数が千も違うのに距離近い。
library(rvest)
library(dplyr)
library(stringr)
library(RMeCab)
library(rgl)
getwd()
setwd("C:/Users/rstud/Documents/GitHub/R/tmJapanese/honyaku01")


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


res <- docNgram("./data", type = 0)
#行数・列数の確認
ncol(res) ; nrow(res)
# クラスター分析をして、似ている作品を近くに表示。
# このクラスター分析の分類対象は、テキストである。だから分類対象であるテキストを行側に持ってくる必要がある。
# 距離測定には、ユークリッド距離。dist()
# ウォード法でクラスターを生成。（D2じゃないとアカン、Dはエラー）
res2 <- res %>% t() %>% dist() %>% hclust("ward.D2")

library(ggdendro)
res2 %>% ggdendrogram()
######################### 助詞と読点から、書き手の癖を見分ける。 ###################
res2 <- res[rownames(res) %in% c("[と-、]","[て-、]","[は-、]","[が-、]","[で-、]","[に-、]","[ら-、]",
                                 "[も-、]","[か-、]","[く-、]","[し-、]","[ず-、]","[は-、]","[ば-、]",
                                 "[へ-、]","[も-、]","[れ-、]","[り-、]","[の-、]","[や-、]","[を-、]"),]

res2 <- res[rownames(res) %in% c("[と-、]", "[て-、]", "[は-、]", "[が-、]", 
                                 "[で-、]", "[に-、]", "[ら-、]", "[も-、]"), ]

#　次元（行数・列数）の確認。
dim(res2)
###  主成分分析による作家の判別
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
  paste0(1:26)

autoplot(res2_pc,label =TRUE, label.size = 8, loadings = TRUE, 
         loadings.label = TRUE,  loadings.label.size  = 12, 
         loadings.label.family = "JP1")
plot3d(res2_pc$scores,size=5,col = )
res2_pc[6]
