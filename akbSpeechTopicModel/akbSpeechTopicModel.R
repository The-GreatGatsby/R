# AKB総選挙演説　トピックモデル
###############################データの前処理
# 徳光さんが話しかける前のスピーチ部分を使用（徳光さんに誘導されて答えている部分は使用せず最初に自分で考えて喋った部分を採用）
# 上記ブログでは「えー」「あー」なども含まれているため、分析に必要ないと判断できる部分を除く。
# 「みなさん」「皆様」などファンに対する敬称を統一した。



library(RMeCab)
library(dplyr)
library(magrittr)
library(topicmodels)
library(lda)
library(ggplot2)
library(tm)
library(tidyr)


setwd("C:/Users/rstud/Documents/GitHub/R/akbSpeechTopicModel")
sashi <- docDF("sashihara.txt",type=1,
               pos = c("名詞","形容詞","動詞"))
head(sashi)

# rscriptとデータの所在地を分ける。一緒だとrscriptのコードも読み込まれる。
# 動詞でやったらトピックが全部「する」と「思う」になったから省く
all <- docDF("C:/Users/rstud/Documents/GitHub/R/akbSpeechTopicModel/data",
             type=1,pos = c("名詞","形容詞"), minFreq=2)
# 行数nrow()、列数ncol()を出力
dim(all) 
head(all)

# allオブジェクトから「"一般","自立","サ変接続","形容動詞語幹」のみ抽出
all2 <- all %>% filter(POS2 %in% c("一般","自立","サ変接続","形容動詞語幹"))

# 左側に表示されるTERMからPOS2までが邪魔やから削除。
all3 <- all2 %>% select(-c(TERM:POS2))
# でもTERMないと何も分らんし、行数邪魔やから、行数の所にTERMを入れる
rownames(all3) <- all2$TERM
dim(all3)[1]

# topicmodelsのLDAをするにはDocumentTermMatrixにしなアカン
all3a <- all3 %>% t() %>% as.DocumentTermMatrix(weighting=weightTf)

#LDAをする
k <- 3
res1 <- all3a %>% LDA(k)
# トピックを確認。kの数が異なるとトピックも変わる。
terms(res1)

posterior(res1)[[1]][1:3, 1:10]

# ldaするには、データの型を変えなあかん。
all4 <- dtm2ldaformat(all3a)

# 乱数を固定して出力結果を同じにする
set.seed(123)
result <- lda.collapsed.gibbs.sampler(all4$documents, K = 3,
                                      all4$vocab, 25, 0.1, 0.1, compute.log.likelihood=TRUE)

# トピック毎に、出現スコアの高いキーワードを5個抽出。
top.words <- top.topic.words(result$topics, 5,by.score=T)
print(top.words)
# 1列目から3列目をそれぞれトピック1、トピック2、トピック3。
# トピック1の出現スコアが高いキーワードを見ると「自分」「いい」「思い」など主観的なワードが上位。これはスピーチで自分自身について言及したことでトピックが形成されたのかも。
# 次にトピック2をみると「AKB」「選挙」「メンバー」というワードが上位に表れており、AKBという組織やそのメンバー、そして総選挙について言及したワードでトピックが形成されたのかも。
# 最後にトピック3をみると「皆さん」「グループ」「ファン」というワードが上位、これはファンに対して言及したワードから形成されている。
# つまり、ざっくりと各トピックを表現するならば、トピック１は「自分について」、トピック２は「AKBグループやメンバーについて」、トピック３は「ファンについて」のトピックといえるのではないでしょうか。




# 文書全体のトピック割合
topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
# ggplot2へ渡すためにデータフレームへ変形
rank_name <- colnames(all[-1:-3]) %<>% str_replace(".txt","")
all_DF <- as.data.frame(topic.proportions) %>% set_names(paste0("topic",1:3)) %>% mutate(num = rank_name)
# tidyr::gather()を使用してlongに変形
all_DF <- all_DF %>% gather(key=topic, value=props, -num)
# ggplot2で可視化
all_DF %>% ggplot(aes(x=topic, y=props, fill=num))+geom_bar(stat="identity")+facet_wrap(~num)









