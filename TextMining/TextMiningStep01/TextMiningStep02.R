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

