library(RMeCab)

RMeCabC("本を読んだ")
RMeCabC("今日は良い天気だが、花粉が飛んでいて辛い")

library(dplyr)

## 出力結果が縦（リスト）だと見にくいから、「unlist」でベクトル化。
RMeCabC("今日は本を読んだ。テキストマイニングに関する本だ。とても面白い。") %>% unlist()
hon <- RMeCabC("今日は本を読んだ。") %>% unlist()
hon [names(hon) %in% c("名詞","動詞")]  ## 動詞が表層語（活用形）のまま
RMeCabC("今日は本を読んだ",1) %>% unlist() ## 第2引数で「1」を入れると、原形を出力。
?RMeCabC

file.exists("data/hon.txt") ##ファイルがあるかを確認
RMeCabText("data/hon.txt") ##RMeCabTextで名詞・動詞だけでなく全てを出力。


tmp <- tempfile() ##ファイルを一時的に作成（後に自動で削除される。）
writeLines("本を買った", con = tmp)
tmp # 一時ファイルの保存場所を表示
x <-RMeCabText(tmp)
unlink(tmp) # 一時ファイルを削除
x # 解析結果を確認

library(purrr)
x %>% map_chr(extract(9)) #xの各リストの9番目（読み方）を抽出。

######################################### ここよくわからん。 ####################################
tmp <- data.frame(BUN = "本を買った", stringAsFactor = FALSE)
x <- docDF(tmp, "BUN", type = 1)
x



######メロスメロスメロスメロスメロスメロスメロスメロスメロスメロスメロスメロスメロスメロス###########
merosu <- RMeCabFreq("data/merosu.txt")
merosu %>% head(20)

##デフォルトでは文字単位で解析になる。「type=1」は形態素解析。
## TERM数が1063個あることが分かる。
merosu <- docDF("data/merosu.txt",
                type=1 , pos=c("名詞","形容詞","動詞")) 
merosu %>% head(10) ##「merosu.txt」が語の出現頻度。分かりにくいから変える。


library(magrittr)
#出現頻度の列名を「FREQ」に変えて、上書き保存して、FREQの小さい順に並び替えた。
merosu %<>% rename(FREQ = merosu.txt) %>% arrange(FREQ) 
merosu %>% tail()
## POS1は「品詞大分類」、POS2は「品詞細分類」。

merosu %>% filter(TERM == "メロス")  ##「メロス」の出現回数を表示。
##「メロス」が2つ出現している。MeCab辞書にない固有名詞なので品詞細分類をミスっている。
## だから自分で辞書作って追加しなアカン。p22

merosu2 <- merosu %>% select(TERM, POS1, FREQ) %>%  ## 表示カラムからPOS2（品詞細分類）を削除。
                      group_by(TERM, POS1) %>%      ## POS2の表示を消しても裏側では細分類で区別されていて「メロス」は2つのまま。selectだけでは変わらない。
                                                    ## group_byでTERMとPOS1が一致する行のFREQ数を一緒にする（FREQ数は同じになるが、POS2が異なると両方出力される）
                      summarize(FREQ = sum(FREQ))   ## 単語とPOS1（品詞大分類）のペアが2行以上存在する行（POS2が異なるから）をPOS2を無視して合体させる。
                                                    ## FREQという行を作って（今回は上書き）、そこに2行以上存在したペアの合計値を代入した。
merosu2 %>% NROW() ##1063あったTERMが30ぐらい統合された。
merosu2 %>% filter(TERM == "メロス")

## group_byでPOS1でグループ化（TERMは無視）
## その合計（名詞・動詞・形容詞などの合計頻度）を「SUM」というカラムに代入。
merosu2 %>% group_by(POS1) %>% summarize(SUM = sum(FREQ)) 

## mutate()で割合を出す。
## 約6割が名詞であることが分かる。
merosu2 %>% group_by(POS1) %>% summarize(SUM = sum(FREQ)) %>% mutate(PROP = SUM / sum(SUM))

##品詞情報を指定して検索
merosu %>% filter(POS1 %in% c("動詞","形容詞"), POS2 == "自立") %>% NROW()



################蜘蛛の糸蜘蛛の糸蜘蛛の糸蜘蛛の糸蜘蛛の糸蜘蛛の糸蜘蛛の糸蜘蛛の糸#####################

## 共起語の解析。第1引数にファイル名。第2引数にnode。第3引数にspan。
res <- collocate("data/kumo.txt", node = "極楽", span = 3)

#[morphems]は、形態素の種類の数。
#[tokens]は、総単語数。
res %>% tail(15)

log2(4/((4/1808)*3*2*10))  ##ノード「極楽」と共起語「蓮池」のMI値。1.58以上なので共起関係あり。
res <- collScores(res, node = "極楽", span = 3) # 「collScores」は、T値とMI値両方出してくれる。
res %>% tail(15) 




####################単語文書行列単語文書行列単語文書行列単語文書行列単語文書行列#####################
mat <- docMatrix("data/doc") #docフォルダにあるデータを全て代入
mat #　単語文書行列の解析結果は行列サイズが大きいため、そのまま出力するのは良くない（今回は小さいからOK）
# 「less-than-n」はn回未満の単語が何種類出現しているか。今回は1未満やから0。
# 「total-tokens」は全ての単語（形態素）数。

## 形態素解析をするために単語文書行列を作った。しかし2つのメタ情報は解析には必要ないから削除。
mat <- mat[ rownames(mat) != "[[LESS-THAN-1]]", ]
mat <- mat[ rownames(mat) != "[[TOTAL-TOKENS]]", ]

# docMatrix()のデフォルトでは「名詞・形容詞」のみ。だからposで指定する。
# 単語（形態素）には、内容語と機能語がある。
# 内容語は名詞・動詞・形容詞など、文書の内容を直接表現するもの。複数の文書をテーマごとに分類できる。
# 機能語は助詞など。書き手の文体を解析できる。
mat <- docMatrix("data/doc", pos = c("名詞","形容詞","動詞","助詞"))
mat

###########################よくわからん##########################

# 局所的重みをtf(単語総頻度)、大域的重みをidf(単語が出現した文書数)と指定し、正規化（norm）している。
matW <- docMatrix("data/doc", weight = "tf*idf*norm") 
matW %>% head()
options(digits = 3) # 少数点以下3位まで表示
matW %>% head()

###########################よくわからん##########################
# applyの第1引数はデータ（今回はパイプ処理しているからナシ）
# 第2引数は、「１」ならば行単位で処理、「２」ならば列単位で処理。
# 第3引数で処理を指定。行列の列ごとに要素を2倍。
matW %>% apply(2, function(x) sum(x^2))

?docMatrix

#　データフレームからテキスト解析を行う場合、文字列が因子に変換されないように「stringAsFactor=FALSE」とすべし。
photo <- read.csv("data/photo.csv", stringsAsFactor = FALSE)
photo
res <- docMatrixDF(photo[ ,"Reply"]) # photoデータのReplyカラムだけを抽出して、単語文書行列を作成。
# 単語文書行列を作れる関数は「docMatrix()」「docMatrix2()」「docMatrixDF()」のみ。
res #単語文書行列（複数の文書を単語単位で解析した結果を行列で表したもの。）


# docDFは、「Nグラム」を作る関数。
# 「Nグラム」は、単語文書行列（形態素解析の結果をデータフレーム化したもの）になっているデータから作成
# だから、第1引数に単語文書行列のデータを取る。
# 第2引数は、単語文書"行列"なので、どの列のNグラムを作るかを指定。
# 第3引数に、形態素解析（１）か文字単位の解析（０）かを選ぶ。
# Nグラムに使う品詞を「pos」で指定。デフォルトは形態素全部。
res <- docDF(photo, column = "Reply", type = 1, pos = c("名詞","形容詞"), N = 2)
res #名詞・形容詞が2語連続で連なっている組み合わせは、この1つだけだった。


# Sというオブジェクトに「メロスは激怒した」という文章をデータフレーム化して、文字列として代入。
S <- data.frame(BUN = "メロスは激怒した", stringsAsFactors = FALSE)
# データフレーム化したSをでNグラムを作成。「docDF」はNグラムを作る関数。
# pos指定がないので全ての形態素。
# columnは列名でも列番号でもOK。
(docDF(S, column = 1, type = 1, N = 2))
?docDF

## Nグラムを作れる関数は「docDF()」「Ngram()」「NgramDF()」など。
merosu <- Ngram("data/merosu.txt", type = 0, N = 2) # 「type=0」なので、文字単位の解析になる。
merosu %>% head() #助詞と句点で書き手を分類することができる。

merosu <- Ngram("data/merosu.txt", type = 1, N = 2) # 「type=1」なので、形態素単位の解析になる。
merosu %>% head(20) #助詞と句点で書き手を分類することができる。

merosu <- Ngram("data/merosu.txt", type = 2, N = 2) # 「type=2」で品詞を表示。
merosu %>% head()

# 「nDF=1」はTERMで纏められている2語をN1とN2に分けて出力
merosu <- docDF("data/merosu.txt", type = 1, pos = c("名詞","形容詞"), N = 2, nDF = 1)
merosu %>% head(20)


#今までのNグラム出力はデータフレームだった。しかし、分析関数によっては、データ形式が行列でなければならない。
# docNgramはNグラムデータを行列で出力。
merosu <- docNgram("data/doc")
merosu %>% head()
merosu %>% rownames()

# NgramDF()は、「ネットワークグラフ」を作成する場合に使う。
merosu <- NgramDF("data/merosu.txt", type = 1, N = 2) # 「type=1」なので、形態素単位の解析になる。
merosu %>% head() # pos指定がないから全ての形態素。

# docNgram2はメモリ改善されている。docNgramとの違いは、デフォルトで「type=1」で文字単位の解析・バイグラム
res <- docNgram2("data/doc") # デフォルトでは文字単位の解析・バイグラムになっている。
res <- docNgram2("data/doc", type = 1, pos = c("名詞","形容詞")) #「type=1」で形態素のバイグラム。
res <- docNgram2("data/doc",type = 1,
                 pos = c("名詞","形容詞","記号")) # 句読点は「記号」と分類されている。
res %>% head(15)

