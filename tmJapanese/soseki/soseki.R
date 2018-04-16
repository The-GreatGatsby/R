
library(rvest)
library(dplyr)
library(stringr)
library(RMeCab)

getwd()
setwd("./GitHub/R/tmJapanese/soseki")




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


################################ ダウンロード
### 夏目漱石
0
Aozora("https://www.aozora.gr.jp/cards/000148/files/2314_ruby_2291.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1086_ruby_5742.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/758_ruby_6056.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2669_ruby_6341.zip")
################# Aozora("https://www.aozora.gr.jp/cards/000148/files/792_ruby_2117.zip")　「思い出すことなど」は随筆なので削除
# Aozora("https://www.aozora.gr.jp/cards/000148/files/1046_ruby_4521.zip") 「カーライル博物館」は紀行
Aozora("https://www.aozora.gr.jp/cards/000148/files/769_ruby_565.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2383_ruby_2323.zip")
############### Aozora("https://www.aozora.gr.jp/cards/000148/files/760_ruby_2341.zip") 「硝子戸の中」は随筆なので削除
Aozora("https://www.aozora.gr.jp/cards/000148/files/2674_ruby_6327.zip")
1
Aozora("https://www.aozora.gr.jp/cards/000148/files/2670_ruby_6329.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/778_ruby_2283.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/777_ruby_1717.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2373_ruby_2077.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/776_ruby_6020.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/761_ruby_1861.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/770_ruby_1713.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/771_ruby_2407.zip")
# Aozora("https://www.aozora.gr.jp/cards/000148/files/759_ruby_3240.zip") 「現代日本の開花」は講演の速記なので削除
2
Aozora("https://www.aozora.gr.jp/cards/000148/files/775_ruby_2064.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/774_ruby_1640.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/773_ruby_5968.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1073_ruby_4526.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/766_ruby_2068.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/793_ruby_2448.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/784_ruby_1721.zip")
3
Aozora("https://www.aozora.gr.jp/cards/000148/files/794_ruby_4237.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/795_ruby_1725.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/768_ruby_2686.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/796_ruby_2452.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1104_ruby_4554.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/797_ruby_1729.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2680_ruby_6345.zip")
4
Aozora("https://www.aozora.gr.jp/cards/000148/files/2315_ruby_2410.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1102_ruby_4488.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/56143_ruby_50824.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2667_ruby_6347.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2370_ruby_2080.zip")
5
Aozora("https://www.aozora.gr.jp/cards/000148/files/2668_ruby_6349.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/798_ruby_2413.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2313_ruby_2287.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/757_ruby_2986.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2682_ruby_6333.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/788_ruby_2857.zip")
6
Aozora("https://www.aozora.gr.jp/cards/000148/files/751_ruby_1539.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2673_ruby_6339.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/791_ruby_1549.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/787_ruby_2295.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2376_ruby_2303.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/762_ruby_1705.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/765_ruby_2469.zip")
7
Aozora("https://www.aozora.gr.jp/cards/000148/files/754_ruby_2320.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/756_ruby_2948.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/755_ruby_3617.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2681_ruby_6323.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2679_ruby_6325.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2371_ruby_2071.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/753_ruby_1701.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/763_ruby_1709.zip")
8
Aozora("https://www.aozora.gr.jp/cards/000148/files/1750_ruby_19434.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/752_ruby_2438.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2375_ruby_2299.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1751_ruby_6331.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/780_ruby_569.zip")
# Aozora("https://www.aozora.gr.jp/cards/000148/files/781_ruby_2101.zip") 「満韓ところどころ」は紀行
Aozora("https://www.aozora.gr.jp/cards/000148/files/783_ruby_1311.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/786_ruby_2375.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/782_ruby_2315.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2372_ruby_2074.zip")
9
Aozora("https://www.aozora.gr.jp/cards/000148/files/1747_ruby_5349.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/785_ruby_1656.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/799_ruby_6024.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2675_ruby_6355.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2678_ruby.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2676_ruby_6343.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/779_ruby_2743.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1076_ruby_4527.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/789_ruby_5639.zip")
100
#Aozora("https://www.aozora.gr.jp/cards/000148/files/2672_ruby_6337.zip")  吾輩は猫である　下偏
#Aozora("https://www.aozora.gr.jp/cards/000148/files/47148_ruby_32216.zip") 吾輩は猫である　上辺
#Aozora("https://www.aozora.gr.jp/cards/000148/files/2671_ruby_6335.zip") 吾輩は猫である　中編
Aozora("https://www.aozora.gr.jp/cards/000148/files/2677_ruby_6351.zip")
# Aozora("https://www.aozora.gr.jp/cards/000148/files/772_ruby_33099.zip") 「私の個人主義」は講演の筆記なので削除



####################################
# 森鴎外
0
Aozora("https://www.aozora.gr.jp/cards/000129/files/2595_ruby_20435.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/673_ruby_23254.zip")
Aozora("https://www.aozora.gr.jp/cards/000882/files/3414_ruby_28548.zip") # これはプレヴォー マルセルの「田舎」を翻訳
Aozora("https://www.aozora.gr.jp/cards/000907/files/4251_ruby_12064.zip") # アンドレーエフ レオニード・ニコラーエヴィチ「犬」
Aozora("https://www.aozora.gr.jp/cards/000129/files/695_ruby_22805.zip")
1
Aozora("https://www.aozora.gr.jp/cards/000884/files/3418_ruby_28552.zip") # ディモフ オシップ「襟」
Aozora("https://www.aozora.gr.jp/cards/000129/files/45270_ruby_19021.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45209_ruby_29695.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/48209_ruby_29697.zip") 「初稿」
Aozora("https://www.aozora.gr.jp/cards/001193/files/50915_ruby_39238.zip") # オイレンベルク ヘルベルト「女の決闘」
Aozora("https://www.aozora.gr.jp/cards/000075/files/4250_ruby_32179.zip") #リルケ ライネル・マリア「家常茶飯　附・現代思想」
Aozora("https://www.aozora.gr.jp/cards/000129/files/680_ruby_23197.zip")
2
Aozora("https://www.aozora.gr.jp/cards/000129/files/678_ruby_22883.zip")
Aozora("https://www.aozora.gr.jp/cards/001194/files/50916_ruby_39174.zip") # シュミットボン ウィルヘルム「鴉」
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
Aozora("https://www.aozora.gr.jp/cards/000883/files/3417_ruby_28551.zip") # モルナール フェレンツ「最終の午後」
Aozora("https://www.aozora.gr.jp/cards/000366/files/50919_ruby_39173.zip") #アルチバシェッフ ミハイル・ペトローヴィチ「罪人」
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
Aozora("https://www.aozora.gr.jp/cards/000075/files/50920_ruby_39171.zip") # リルケ ライネル・マリア「白」
6
Aozora("https://www.aozora.gr.jp/cards/000129/files/690_ruby_23034.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/13201_ruby_9289.zip") 『新訳源氏物語』初版の序
Aozora("https://www.aozora.gr.jp/cards/000129/files/2522_ruby_5001.zip")
Aozora("https://www.aozora.gr.jp/cards/001195/files/50917_ruby_39236.zip") # ダビット ヤーコプ・ユリウス「世界漫遊」
Aozora("https://www.aozora.gr.jp/cards/000129/files/45245_ruby_21882.zip")
7
Aozora("https://www.aozora.gr.jp/cards/000129/files/46234_ruby_22009.zip")
Aozora("https://www.aozora.gr.jp/cards/001192/files/50914_ruby_39170.zip") # ホーフマンスタール フーゴー・フォン「痴人と死と」
Aozora("https://www.aozora.gr.jp/cards/000129/files/3336_ruby_23053.zip")
Aozora("https://www.aozora.gr.jp/cards/000883/files/3416_ruby_28550.zip")
Aozora("https://www.aozora.gr.jp/cards/001190/files/50913_ruby_39172.zip") # アルテンベルク ペーター「釣」
8
Aozora("https://www.aozora.gr.jp/cards/000129/files/3614_ruby_12061.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2049_ruby_19718.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45271_ruby_19022.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/42375_ruby_18247.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45618_ruby_24923.zip")
Aozora("https://www.aozora.gr.jp/cards/000881/files/3413_ruby_28547.zip") # ブウテ フレデリック「橋の下」
9
Aozora("https://www.aozora.gr.jp/cards/000129/files/45254_ruby_29696.zip")
Aozora("https://www.aozora.gr.jp/cards/001062/files/50912_ruby_39237.zip") # ストリンドベリ アウグスト「一人舞台」
Aozora("https://www.aozora.gr.jp/cards/000129/files/676_ruby_23235.zip")
Aozora("https://www.aozora.gr.jp/cards/001025/files/50909_ruby_49057.zip") # ゲーテ ヨハン・ヴォルフガング・フォン「ファウスト」
10
Aozora("https://www.aozora.gr.jp/cards/000129/files/50910_ruby_45862.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45255_ruby_19719.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/675_ruby_23191.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45256_ruby_19720.zip")
Aozora("https://www.aozora.gr.jp/cards/001196/files/45276_ruby_19722.zip") # ランド ハンス「冬の王」
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






