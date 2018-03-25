#####����������

install.packages(c("twitteR", "bit64", "rjson", "DBI", "httr", 
                   "base64enc"), dependencies = TRUE)
install.packages("ROAuth")

update.packages(c("twitteR", "bit64", "rjson", "DBI", "httr", 
                  "base64enc", "ROAuth"),dependencies=TRUE)




library("twitteR")
library(ROAuth)
### �ȉ��� ################################################## �����g���擾�����L�[�ɒu��������
# Consumer Key
consumerKey <- "hQQ8mlkFhG4UoUV67xTwIbQTu"
# Consumer Secret
consumerSecret <- "dd2ldZBbaU6wJS3ElV5xQozZg1SjuVJUgITlA9Cc8K6cub59Xo"
# Access Token
accessToken <- "898910528730406912-zKbDnTeda1pHuQvXgokDmQOzxnB3XAh"
# Access Token Secret
accessSecret <- "lxpkojqoTPABQT2g3F5SZvI9CNPTmwyDpiIg7TTAn8tlb"




download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL ="https://api.twitter.com/oauth/request_token",
                         accessURL = "https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")

cred$handshake(cainfo="cacert.pem")




###################�F�؃G���[���N�������p################################
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



################################# �F�� �G���[  #############################
options(httr_oauth_cache = TRUE)
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)
## API�̎g�p�����i�����ȓ��Ɂ��̃��N�G�X�g���\�A�݂����Ȑ����j����������
## ���Ԃ������Ă���A�����R�[�h�����s�����畁�ʂɐi�߂邩���B
## ��L�G���[���o���ہA������Twitter Apps�Ǘ���ʂ���A�L�[�ƃg�[�N���̍Ĕ��s���ē���ւ���Ɖ�������ꍇ������
###########################################################################


## ���[�U�[�̃c�C�[�g���擾(���X�g�^)
mytweets <- userTimeline("statsbeginner",       # ���[�U����@�����œ����
             n=10,                  # �擾�������w��i�����3200?�j
             maxID=NULL,           # �c�C�[�gID�͈͎̔w��i����͖����j
             sinceID=NULL,         # �c�C�[�gID�͈͎̔w��i����͖����j
             includeRts=TRUE,      # RT���܂ނ��ǂ���
             excludeReplies=FALSE  # ���v���C���܂ނ��ǂ���
             )
##�f�[�^�t���[���^�ɕϊ�
mytweets.df <- twListToDF(mytweets)
print(mytweets.df)


## �c�C�[�g������
searchTwitter("���v",                # �������[�h�B�����̏ꍇ��+�łȂ�
              n=5,                   # �擾���錏��
              lang=NULL,             # ����i���{��Ɍ��肷��Ȃ�"ja", else NULL�j
              since=NULL,            # ���Ԏw��
              until=NULL,            # ���Ԏw��
              locale=NULL,           # ���P�[�����w��i���{�Ȃ�"ja")
              geocode=NULL,          # �ʒu�����w��
              sinceID=NULL,          # �c�C�[�gID�P�ʂŔ͈͎w��
              maxID=NULL,            # �c�C�[�gID�P�ʂŔ͈͎w��
              resultType="mixed",     # �ړI�ɉ�����"popular","recent","mixed"���w��
              retryOnRateLimit=120   # API�R�[�������ɂЂ����������Ƃ��̃��g���C�񐔎w��
              )






### twitteR �̗��p
## Windows�̏ꍇ�����R�[�h��ϊ����ē��e����
## ���������G���[�BMac�ALinux�̃R�}���h�̕��͂����Ă�B
## tweet(iconv("2017 1 14 R ����ꂢ�Ă݂�", from = "UTF-8", to = "CP932"))


## Mac �Ȃ��� Linux �̏ꍇ�B���Ȃ���������B
tweet(date())
### ����̃A�J�E���g�̃c�B�[�g���擾
tweets <- userTimeline("mextjapan", 200)

str(tweets[[1]]) ## p168

# tweets�I�u�W�F�N�g�ɂ́Atext(�c�C�[�g)�ȊO�̃f�[�^���������񂠂�B
# statusText�́A�e�v�f�i�����tweets�I�u�W�F�N�g�ɁA200�̃c�C�[�g���܂߂����X�g�v�f������j����text�݂̂𒊏o�B
texts <- sapply(tweets, statusText)

library(dplyr)
texts %>% head()

library(stringr)
library(magrittr)

## �A�J�E���gID��URL��e��L���������Ă�B������폜�B
texts %<>% str_replace_all("\\p{ASCII}", "")

# �����l�ƂȂ����v�f������ΏȂ�
# texts�I�u�W�F�N�g�ɁAtexts��NA�łȃf�[�^�̂ݑ���B
texts <- texts[!is.na(texts)]


# Windows�̏ꍇ�����R�[�h��ύX����
# texts <- iconv(texts , from = "UTF-8", to = "CP932")
# Mac�̏ꍇ�A��̂P�s�͎��s���Ă����Ȃ��Ƃ����Ȃ�


# �������S������������B�i�����̕��͂���̃x�N�g���ɂ��銴���j
text2 <- paste(texts, collapse ="")
# �擾�����e�L�X�g���A�ꎞ�I�Ƀt�@�C���ɕۑ�
xfile <- tempfile()
write(text2, xfile)


library(RMeCab)
# �`�ԑf��͂���B
mext <- docDF(xfile, type = 1, pos = "����")

# �����̂ݒ��o
mext <- docDF(xfile, type = 1, pos = "����")
#library(magrittr) # %<>% ���Z�q�� ! �𗘗p����
# �񎩗��A���A�T�ς��Ȃ��B�i!�����܂��g���j
mext %<>% filter(!POS2 %in% c("�񎩗�", "��","�T�ϐڑ�"))

head(mext)

library(dplyr)

# �񖼂�ύX
## everything()�őS�Ă̗�𒊏o�B
## starts_with()�ŁA"file"�Ŏn�܂�񖼂��AFREQ�ɂ���Ēu��������B
mext %<>% select(everything(), FREQ = starts_with("file"))
unlink(xfile) #�ꎞ�t�@�C�����폜
mext %>% arrange(FREQ) %>% tail(40)



library (wordcloud)
#  pal <- brewer.pal(8,"Dark2")
# wordcloud (m2[,1], m2[,4], min.freq = 7, colors = pal)
wordcloud (mext$TERM, mext$FREQ, min.freq = 7, family = "JP1")


###  ���{�����̃g�����h�̎擾
woeid <- availableTrendLocations()
# �n��ID���m�F
woeid %>% filter(country == "Japan")

#�@"�����݂�" �����̃g�����h������B
trends <- getTrends(1118370)
trends$name


center <- searchTwitter(searchString = "jssp_ss_2018_R", 
                        n = 1000, 
                        since = "2018-03-21"
                        )

cntDF <- twListToDF(center)
cntDF %>% head()

# �L���Ȃǂ��폜
texts <- cntDF$text
texts %<>% str_replace_all("\\p{ASCII}", "")
# Windows�̏ꍇ�͂����ŕ������ϊ�����
texts <- iconv(texts , from = "UTF-8", to = "CP932")
# Mac �� Linux �Ŏ��s���Ă͂����Ȃ�

# �擾�����e�L�X�g���t�@�C���ɕۑ�
text2 <- paste(texts, collapse = "")
xfile <- tempfile()
write(text2, xfile)


# ���������P�ꂪ�A�ǂ̂悤�ȕ����Ŏg���邩���A�l�b�g���[�N�O���t�Ŋm�F�B
cnttxt <- NgramDF(xfile, type = 1, pos = c("����","�`�e��"))
cnttxt  %>%  arrange(Freq) %>% tail(50)

# �P�ꂪ��������ƌ��ɂ�������A�o�C�O������50�O��ɍi��B
cnttxt2 <- cnttxt %>% filter(Freq >= 4)

library(igraph)
cntgraph  <- graph.data.frame(cnttxt2)


tkplot(cntgraph, vertex.size = 23, vertex.color = "SkyBlue")

