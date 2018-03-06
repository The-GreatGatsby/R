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



merosu <- RMeCabFreq("data/merosu.txt")
merosu %>% head(20)

merosu <- docDF("data/merosu.txt",
                type=1 , pos=c("名詞","形容詞","動詞"))
merosu %>% head(10) ##「merosu.txt」が語の出現頻度。分かりにくいから変える。


library(magrittr)
#出現頻度の列名を「FREQ」に変えて、上書き保存して、FREQの小さい順に並び替えた。
merosu %<>% rename(FREQ = merosu.txt) %>% arrange(FREQ) 
merosu %>% tail()

merosu %>% filter(TERM == "メロス")  ##「メロス」の出現回数を表示。













