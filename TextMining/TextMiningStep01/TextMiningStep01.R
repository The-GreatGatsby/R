library(RMeCab)


#あいうえお

library(dplyr)
library(stringr)


names <- c("岡本 雄輝","田中 謙吾","星野 ヨシキ","Michael Jackson","Jay Gatsby","Tom Buchanan","Monster Hunter","間 一紘","Alex Yuan")
names %>% str_replace("(\\w+) (\\w+)","\\1")
names %>% str_replace("(\\w+) (\\w+)","\\2")

##########思った通りにならん
names2 <- c("a b c","d e f","g h i","j k l","m n o","p q r","s t u")
names2 %>% str_replace("(\\W) (\\w) (\\w)","\\1")




x <- "ひらがな hiragana カタカナ katakana 日本語 123456"
x %>% str_replace_all("\\p{ASCII}","") ##ASCII文字(英数字や半角記号)を空白に置換
x %>% str_replace_all("\\p{Hiragana}","")  ##ひらがなを空白に置換
x %>% str_replace_all("\\p{Katakana}","")  ##カタカナを空白に置換
x %>% str_replace_all("\\p{Han}","")  ##漢字を空白に置換

names %>% str_replace_all("\\p{ASCII}","") ##ASCII文字(英数字や半角記号)を空白に置換
names %>% str_replace_all("\\p{Hiragana}","")  ##ひらがなを空白に置換
names %>% str_replace_all("\\p{Katakana}","")  ##カタカナを空白に置換
names %>% str_replace_all("\\p{Han}","")  ##漢字を空白に置換





library(tm)

#「alice」というオブジェクトに「data/alice/」フォルダにあるすべてのデータを代入している。
alice <- VCorpus(DirSource(dir = "data/alice/"),
                 readerControl = list(language = "english"))


alice %>% inspect   
inspect(alice)

alice[[1]] %>% as.character()
as.character(alice[[1]])

alice[[2]] %>% as.character()
as.character(alice[[2]])

alice[[3]] %>% as.character()
as.character(alice[[3]])


alice1 <- alice %>% tm_map(stripWhitespace)  ##余分な空白削除
alice1[[1]] %>% as.character()
alice1[[2]] %>% as.character()
alice1[[3]] %>% as.character()


library(magrittr)
alice1 %<>% tm_map(removePunctuation) #ピリオド、カンマ、クエスチョンマーク、括弧などを削除
alice1[[1]] %>% as.character()
alice1[[2]] %>% as.character()
alice1[[3]] %>% as.character()



alice1 %>% tm_map(content_transformer(tolower))
##この状態だと、alice1にこの行でだけ、命令を実行したことになる。
##上書き保存されていないから、他の行でalice1オブジェクトに命令をしても変化は起きない。
alice1[[1]] %>% as.character()
alice1[[2]] %>% as.character()
alice1[[3]] %>% as.character()

## 上書き保存「%<>%」すると変化が分かる。
alice1 %<>% tm_map(content_transformer(tolower))
alice1[[1]] %>% as.character()
alice1[[2]] %>% as.character()
alice1[[3]] %>% as.character()
## alice1 %<>% tm_map(content_transformer(toupper))



alice1 %<>% tm_map(removeWords,stopwords("english")) 
alice1[[1]] %>% as.character()
alice1[[2]] %>% as.character()
alice1[[3]] %>% as.character()
## ストップワードを削除・上書きした。しかし、削除部分が空白になっているため、空白削除の前にやるべき。



library(SnowballC)

## ステミング（変化形を語幹に戻してカウント）
alice1 %<>% tm_map(stemDocument)
alice1[[1]] %>% as.character()


## 単語文書行列（文書を単語単位に分解した結果をマトリックス化）
dtm <- TermDocumentMatrix(alice1)
## inspect()で確認できる
dtm %>% inspect()
## 3回以上出現した単語を出力。
dtm %>% findFreqTerms(3)
## alicと相関係数が0.8以上の単語のみ出力。
dtm %>% findAssocs("alic", 0.8)

vignette("tm")

