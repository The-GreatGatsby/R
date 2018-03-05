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


