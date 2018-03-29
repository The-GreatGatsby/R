# とんかつ食べたい



# アルファベット大文字
LETTERS
# アルファベット小文字
letters
# 全て小文字に変換
tolower(LETTERS)
# 全て大文字に変換
toupper(letters)

# pasteは、文字列の結合（デフォルトでは，スペースが挟まれる）
paste("William", "Shakespeare")
paste("夏目", "漱石")

# スペースなしで結合
paste("夏目", "漱石", sep = "")
# 文字列の入った変数を結合
Kawabata <- "川端"
Yasunari <- "康成"
paste(Kawabata, Yasunari)

# 連番のついた変数を生成
# No.という文字列に、１から５までの数字をそれぞれ結合。
paste("No.", 1 : 5, sep = "")

# 文字列の文字数を出力
nchar("cat")
nchar("猫")

# 各文字列の文字数を計算
nchar(c("I", "love", "cats"))
nchar(c("私", "は", "猫", "が", "好き", "です"))


word.length <- nchar(c("I", "love", "cats"))
# 1単語あたりの平均文字数
mean(word.length)


# ワードスペクトル（1単語あたりの文字数の頻度表）
# この例では，1文字の単語が1つで，4文字の単語が2つ
table(word.length)

# 1文字目から3文字目までを取り出す　※Aは０番目ではない！！！！！！
substr("ABCDE", start = 1, stop = 3)
# 2文字目から4文字目までを取り出す
substr("あいうえお", start = 2, stop = 4)

# 動詞の過去形のベクトル
verbs <- c("asked", "had", "looked", "took")
# edという文字列を含む要素の番号を抽出
verbs.n <- grep("ed", verbs)
# 抽出した要素の番号が入っている。
verbs.n
# 抽出した番号を手がかりとして，検索条件に合致した要素を表示。添字を使う。
verbs[verbs.n]


words <- c("asked", "edited", "edition", "education", "looked")
# 単純にedという文字列を含む単語を検索
words.n <- grep("ed", words)
words[words.n]

# edという文字列で終わる単語のみを検索。「＄とperl=TRUE」
words.n.2 <- grep("ed$", words, perl = TRUE)
words[words.n.2]
# edという文字列で始まる単語のみを検索。「＾とperl=TRUE」
words.n.3 <- grep("^ed", words, perl = TRUE)
words[words.n.3]

######################　重要！！！！！！！！！
verbs.2 <- c("asked", "looked", "walked")
# edで終わる英単語をsに置換
gsub("ed$", "s", verbs.2, perl = TRUE)
#############################################


####################### 重要！！！！！！！
# 日本語の語末の「く」を「い」に置換
adverbs <- c("美しく", "高く", "速く")
gsub("く$", "い", adverbs, perl = TRUE)
############################################


################### 重要！！！！！！！！！！！
# 置換機能を用いた文字列の削除
nouns <- c("birds", "cats", "dogs")
gsub("s$", "", nouns, perl = TRUE)
##############################################


##################　重要！！！！！！！！！！！
# A and Bの形式の文字列のベクトル
and <- c("black and white", "bread and butter", "cats and dogs")
# "and"を区切りとして，文字列を分割
strsplit(and, split = " and ")
# 出力をリスト形式からベクトル形式に変換
unlist(strsplit(and, split = " and "))
###############################################

#####################　重要！！！！！！！！！！！！
# ひとかたまりの英文
ulysses <- "Stately, plump Buck Mulligan came from the stairhead, bearing a bowl of lather on which a mirror and a razor lay crossed."
# スペースを区切りとした分割
unlist(strsplit(ulysses, split = " "))
#################################################


#####################　重要！！！！！！！！！！！！
yukiguni <- "国境の長いトンネルを抜けると雪国であった。"
# 一文字ずつ、分割
unlist(strsplit(yukiguni, split = ""))
###################################################

setwd("C:/Users/rstud/Documents/GitHub/R/TextMining02")
# ファイルが作業ディレクトリにある場合
data01 <- read.csv("data/data01.csv" , header = FALSE)
data01

# マウス操作でdata01.csvを選択する場合
data01 <- read.csv(file.choose(), header = FALSE)

# マウス操作でdata02.csvを選択する場合
data02 <- read.csv(file.choose(), header = TRUE)
data02

# マウス操作でdata03.csvを選択する場合
# 冒頭の2行を読みとばす
data03 <- read.csv(file.choose(), header = TRUE, skip = 2)
data03

# マウス操作でdata04.csvを選択する場合
# 行ラベル・列ラベルがある場合は「header=FALSE」と「row.names=1」
data04 <- read.csv(file.choose(), header = TRUE, row.names = 1)
data04

# txtを読み込むには、scan
# 文単位で読み込む場合（data05.txtを選択）
# quiet=TRUEで、データの個数を非表示。
# sep="\n"で文節単位に分ける。指定しないと単語単位になる。
# what=charは数字ではなく文字をスキャンするという意味。
data05 <- scan(file.choose(), what = "char", sep = "\n", quiet = TRUE)
data05

#単語単位で読み込む場合（data05.txtを選択）
data05 <- scan(file.choose(), what = "char", quiet = TRUE)
data05


