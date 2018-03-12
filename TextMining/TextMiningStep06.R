############################################# chapter09����

### l9.1 ��͂̏���
library(RMeCab)
## Windows�̏ꍇ�͈ȉ��� "data/prime/utf" �� "data/prime/sjis" �ɂ���Ȃ�
## ���g�̍�Ɗ��ɂ��킹�ēK�X�ύX
prime <- docMatrix2("data/prime/utf8", pos = c("����","�`�e��","����"), 
                    weight = "tf*idf*norm")

ncol(prime) ; nrow(prime)


library(stringr)
library(dplyr)
library(magrittr)
## �񖼂�Z�k������
colnames(prime)  %<>% str_replace("_general-policy-speech.txt", "")
colnames(prime)  %<>% str_replace("(\\d{4})\\d{4}_(\\d{3})", "\\1_\\2")


### 9.3 ���M�\�������̃N���X�^�[����
hc <- prime %>% t %>% dist %>% hclust("ward.D2")

# 
install.packages("ggdendro")

library(ggdendro)
ggdendrogram(hc, rotate= TRUE)

# ��L�̎��s���ʂ̉摜�ŕ��������������Ă���ꍇ�A�ȉ��̂悤��PDF�摜�Ƃ��č쐬���Ċm�F���Ă݂Ă�������
# 3�s�����Ď��s���邱�Ƃŉ摜�t�@�C�����쐬����܂�
# RStudio �E��Files�^�u�ŉ摜�t�@�C�����N���b�N���邱�ƂŁA�K�؂ȃr���[���[ ������������܂�
cairo_pdf(file = "hc.pdf", family = "JP1")
ggdendrogram(hc, rotate= TRUE)
dev.off()


### 9.5 ���ْl����

TD <- matrix (c(1,0,0,0,1,0,
                0,1,0,1,0,1, 
                0,1,0,0,0,0,
                0,1,0,0,0,0,
                0,0,1,0,0,1,
                1,1,1,1,0,0,
                0,0,1,2,1,0,
                1,1,0,0,0,0), nrow = 8, byrow = TRUE)
## �쐬�����s��ɗ񖼂ƍs����ݒ�
colnames(TD) <- paste0("doc", 1:6)
rownames(TD) <- paste0("w", 1:8)

# ���ْl����
TD_svd <- svd(TD)

options(digits = 3)
TD_svd$u

TD_svd$d

TD_svd$v

t(TD_svd$u[, 1:3]) %*% TD

### 9.6 ���ݓI�Ӗ��C���f�L�V���O�ɂ�镪��
install.packages("rgl")

prime.svd <- svd(prime)
prime2 <- t(prime.svd$u[, 1:3]) %*% prime
dim(prime2)

colnames(prime2) <- prime2 %>% colnames() %>%
  str_extract("\\d{4}_\\d{2,3}")
cols <- prime2 %>% colnames() %>% str_extract("\\d{3}")

# �p�b�P�[�W�ǂݍ���
library(rgl)
# �ʃE�B���h�E���J��
rgl.open()
# ���W��F��������
rgl.lines(c(-1,1), 0,0, color = "gold")
rgl.lines(0, c(-1,1), 0, color = "gray")
rgl.lines(0,0,c(-1,1), color = "black")
# 3������Ԃ̃J���[���w�肵
rgl.bbox(color = "blue", emission = "green")
# ��������t�u����
rgl.texts(prime2[1,], prime2[2,], prime2[3,],
          colnames(prime2), color = cols)

rgl.snapshot(file = "prime.png")

rgl.close()

library(rgl)
plot3d(t(prime2), type = "n")
text3d(t(prime2), text = colnames(prime2), col = cols, cex = 1.4)

vignette(package = "rgl")
vignette("rgl")


### 9.7 �g�s�b�N���f��
install.packages(c("topicmodels","lda"))

library(RMeCab)


prime <- docDF("data/prime/utf8", type = 1, 
               pos = c("����","�`�e��"), minFreq = 3)

dim(prime)

prime2 <- prime %>% filter(POS2 %in% c("���","����"))
dim(prime2)

prime2$TERM %>% duplicated() %>% which()

library(stringr)
library(magrittr)
## ���l�񂾂���k�����I�u�W�F�N�g���쐬
prime3 <-  prime2 %>% select(-c(TERM:POS2))
## �s���Ɍ`�ԑf��͂̌��ʂ�ݒ� 
rownames(prime3) <- prime2$TERM
## �񖼂͒Z�k��
colnames(prime3)  %<>% str_replace("_general-policy-speech.txt", "")
colnames(prime3)  %<>% str_replace("(\\d{4})\\d{4}_(\\d{3})", "\\1_\\2")

library(tm)
prime3a <- prime3 %>% t() %>%  as.DocumentTermMatrix(weighting = weightTf)

### 9.7.1 �g�s�b�N���f���ɂ�郂�f������
library(topicmodels)
## �g�s�b�N�̐����w��
K <- 5
res1 <- prime3a %>% LDA(K)

terms(res1)

str(res1)
posterior(res1)[[1]] [1:5,1:5]
posterior(res1)[[2]]


### 9.7.2 lda�p�b�P�[�W�ɂ�镪�͂Ɖ���

library(topicmodels)
prime4  <- dtm2ldaformat(prime3a)

library(lda)
set.seed(123)
K <- 5
result <- lda.collapsed.gibbs.sampler(prime4$documents, K = K, 
                                      prime4$vocab, 25, 0.1, 0.1, compute.log.likelihood=TRUE)

top.topic.words(result$topics, 10, by.score=TRUE)

prime5 <- rownames(prime3a) %>% str_subset("koizumi|hatoyama|noda|abe")
prime5

prime6 <- rownames(prime3a) %>% 
  str_detect("koizumi|hatoyama|noda|abe") %>% which
prime6

cbind(prime6, prime5)

## �����S�̂̃g�s�b�N����
options(digits = 3)

topic.proportions <- t(result$document_sums) / colSums(result$document_sums)
## �ΏۂƂ��鏊�M�\�������𒊏o
ministers  <- topic.proportions [c(64, 74, 77, 80), ]
ministers

ministers %>% rowSums()

ministers

## �s����f�[�^�t���[���ɕϊ����񖼂�ݒ�
ministersDF <- as.data.frame(ministers) %>% 
  set_names(paste0("topic", 1:5)) %>% 
  ## num �Ƃ������ǉ�
  mutate(num = paste0("No", c(64, 74, 77, 80)))
ministersDF

# install.packages("tidyr")

library(tidyr)

ministersDF <- ministersDF %>% 
  gather(key = topic, value = props, -num)

library(ggplot2)

ministersDF %>% ggplot(aes(x = topic, y = props, fill = num)) +       geom_bar(stat = "identity") + facet_wrap(~num)

# ��L�̎��s���ʂ̉摜�ŕ��������������Ă���ꍇ�A�ȉ��̂悤��PDF�摜�Ƃ��č쐬���Ċm�F���Ă݂Ă�������
# 3�s�����Ď��s���邱�Ƃŉ摜�t�@�C�����쐬����܂�
# RStudio �E��Files�^�u�ŉ摜�t�@�C�����N���b�N���邱�ƂŁA�K�؂ȃr���[���[ ������������܂�
cairo_pdf(file = "ministersDF.pdf", family = "JP1")# Mac �̏ꍇ�� family = "HiraKakuProN-W3" �ƕς��Ă�������
x
dev.off()