#####あいうえお

install.packages(c("twitteR", "bit64", "rjson", "DBI", "httr", 
                   "base64enc"), dependencies = TRUE)
install.packages("ROAuth")

update.packages(c("twitteR", "bit64", "rjson", "DBI", "httr", 
                  "base64enc", "ROAuth"),dependencies=TRUE)




library("twitteR")
library(ROAuth)
### 以下の ################################################## を自身が取得したキーに置き換える
# Consumer Key
consumerKey <- "hQQ8mlkFhG4UoUV67xTwIbQTu"
# Consumer Secret
consumerSecret <- " dd2ldZBbaU6wJS3ElV5xQozZg1SjuVJUgITlA9Cc8K6cub59Xo"
# Access Token
accessToken <- "898910528730406912-zKbDnTeda1pHuQvXgokDmQOzxnB3XAh"
# Access Token Secret
accessSecret <- " lxpkojqoTPABQT2g3F5SZvI9CNPTmwyDpiIg7TTAn8tlb"




download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL ="https://api.twitter.com/oauth/request_token",
                         accessURL = "https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")

cred$handshake(cainfo="cacert.pem")




###################認証エラーが起きた時用################################
library(httr)
# 1. Find OAuth settings for twitter:
#    https://dev.twitter.com/docs/auth/oauth
oauth_endpoints("twitter")

# 2. Register an application at https://apps.twitter.com/
#    Make sure to set callback url to "http://127.0.0.1:1410/"
#
#    Replace key and secret below
myapp <- oauth_app("twitter",
                   key = "hQQ8mlkFhG4UoUV67xTwIbQTu",
                   secret = "lxpkojqoTPABQT2g3F5SZvI9CNPTmwyDpiIg7TTAn8tlb"
)

# 3. Get OAuth credentials
twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp)

# 4. Use API
req <- GET("https://api.twitter.com/1.1/statuses/home_timeline.json",
           config(token = twitter_token))
stop_for_status(req)
content(req)
##########################################################################



################################# 認証 エラー  #############################
options(httr_oauth_cache = TRUE)
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)
## APIの使用制限（◯分以内に◯個のリクエストが可能、みたいな制限）か何かかも
## 時間をおいてから、同じコードを実行したら普通に進めるかも。
## 上記エラーが出た際、自分のTwitter Apps管理画面から、キーとトークンの再発行して入れ替えると解決する場合もある
###########################################################################


## ユーザーのツイートを取得(リスト型)
mytweets <- userTimeline("statsbeginner",       # ユーザ名を@無しで入れる
             n=10,                  # 取得件数を指定（上限は3200?）
             maxID=NULL,           # ツイートIDの範囲指定（今回は無し）
             sinceID=NULL,         # ツイートIDの範囲指定（今回は無し）
             includeRts=TRUE,      # RTを含むかどうか
             excludeReplies=FALSE  # リプライを含むかどうか
             )
##データフレーム型に変換
mytweets.df <- twListToDF(mytweets)
print(mytweets.df)


## ツイートを検索
searchTwitter("統計",                # 検索ワード。複数の場合は+でつなぐ
              n=5,                   # 取得する件数
              lang=NULL,             # 言語（日本語に限定するなら"ja", else NULL）
              since=NULL,            # 期間指定
              until=NULL,            # 期間指定
              locale=NULL,           # ロケールを指定（日本なら"ja")
              geocode=NULL,          # 位置情報を指定
              sinceID=NULL,          # ツイートID単位で範囲指定
              maxID=NULL,            # ツイートID単位で範囲指定
              resultType="mixed",     # 目的に応じて"popular","recent","mixed"を指定
              retryOnRateLimit=120   # APIコール制限にひっかかったときのリトライ回数指定
              )






### twitteR の利用
## Windowsの場合文字コードを変換して投稿する
## ※しかしエラー。Mac、Linuxのコマンドの方はいけてる。
## tweet(iconv("2017 1 14 R から呟いてみる", from = "UTF-8", to = "CP932"))


## Mac ないし Linux の場合。※なぜかいける。
tweet(date())
### 特定のアカウントのツィートを取得
tweets <- userTimeline("mextjapan", 200)

str(tweets[[1]]) ## p168

# tweetsオブジェクトには、text(ツイート)以外のデータがたくさんある。
# statusTextは、各要素（今回はtweetsオブジェクトに、200個のツイートを含めたリスト要素がある）からtextのみを抽出。
texts <- sapply(tweets, statusText)

library(dplyr)
texts %>% head()

library(stringr)
library(magrittr)

## アカウントIDやURLや各種記号も入ってる。それを削除。
texts %<>% str_replace_all("\\p{ASCII}", "")

# 欠損値となった要素があれば省く
# textsオブジェクトに、textsのNAでなデータのみ代入。
texts <- texts[!is.na(texts)]


# Windowsの場合文字コードを変更する
# texts <- iconv(texts , from = "UTF-8", to = "CP932")
# Macの場合、上の１行は実行してもしなくとも問題ない


# 文字列を全部結合させる。（複数の文章を一つのベクトルにする感じ）
text2 <- paste(texts, collapse ="")
# 取得したテキストを、一時的にファイルに保存
xfile <- tempfile()
write(text2, xfile)


library(RMeCab)
# 形態素解析する。
mext <- docDF(xfile, type = 1, pos = "名詞")

# 名詞のみ抽出
mext <- docDF(xfile, type = 1, pos = "名詞")
#library(magrittr) # %<>% 演算子と ! を利用する
# 非自立、数、サ変を省く。（!をうまく使う）
mext %<>% filter(!POS2 %in% c("非自立", "数","サ変接続"))

head(mext)

library(dplyr)

# 列名を変更
## everything()で全ての列を抽出。
## starts_with()で、"file"で始まる列名を、FREQによって置き換える。
mext %<>% select(everything(), FREQ = starts_with("file"))
unlink(xfile) #一時ファイルを削除
mext %>% arrange(FREQ) %>% tail(40)



library (wordcloud)
#  pal <- brewer.pal(8,"Dark2")
# wordcloud (m2[,1], m2[,4], min.freq = 7, colors = pal)
wordcloud (mext$TERM, mext$FREQ, min.freq = 7, family = "JP1")


###  日本国内のトレンドの取得
woeid <- availableTrendLocations()
# 地域IDを確認
woeid %>% filter(country == "Japan")

#　"今現在の" 東京のトレンドを見る。
trends <- getTrends(1118370)
trends$name


center <- searchTwitter(searchString = "jssp_ss_2018_R", 
                        n = 1000, 
                        since = "2018-03-21"
                        )

cntDF <- twListToDF(center)
cntDF %>% head()

# 記号などを削除
texts <- cntDF$text
texts %<>% str_replace_all("\\p{ASCII}", "")
# Windowsの場合はここで文字列を変換する
texts <- iconv(texts , from = "UTF-8", to = "CP932")
# Mac や Linux で実行してはいけない

# 取得したテキストをファイルに保存
text2 <- paste(texts, collapse = "")
xfile <- tempfile()
write(text2, xfile)


# 検索した単語が、どのような文脈で使われるかを、ネットワークグラフで確認。
cnttxt <- NgramDF(xfile, type = 1, pos = c("名詞","形容詞"))
cnttxt  %>%  arrange(Freq) %>% tail(50)

# 単語が多すぎると見にくいから、バイグラムを50個前後に絞る。
cnttxt2 <- cnttxt %>% filter(Freq >= 4)

library(igraph)
cntgraph  <- graph.data.frame(cnttxt2)


tkplot(cntgraph, vertex.size = 23, vertex.color = "SkyBlue")


