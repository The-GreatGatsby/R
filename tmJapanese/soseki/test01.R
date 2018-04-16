# �Ǔ_�ƍ�i�������ς��g���ĕ���
library(rvest)
library(dplyr)
library(stringr)
library(RMeCab)
getwd()
setwd("./GitHub/R/tmJapanese/soseki")


Aozora <- function(url = NULL, txtname  = NULL){
  enc <-  switch(.Platform$pkgType, "win.binary" = "CP932", "UTF-8")
  if (is.null(url)) stop ("specify URL")
  tmp <- unlist (strsplit (url, "/"))
  tmp <- tmp [length (tmp)]
  
  curDir <- getwd()
  tmp <- paste(curDir, tmp, sep = "/")
  download.file (url, tmp)
  
  textF <- unzip (tmp)
  unlink (tmp)
  
  if(!file.exists (textF)) stop ("something wrong!")
  if (is.null(txtname)) txtname <- paste(unlist(strsplit(basename (textF), ".txt$")))
  if (txtname != "NORUBY")  {
    
    newDir <- paste(dirname (textF), "NORUBY", sep = "/")
    
    if (! file.exists (newDir)) dir.create (newDir)
    
    newFile <- paste (newDir,  "/", txtname, "2.txt", sep = "")
    
    con <- file(textF, 'r', encoding = "CP932" )
    outfile <- file(newFile, 'w', encoding = enc)
    flag <- 0;
    reg1 <- enc2native ("\U005E\U5E95\U672C")
    reg2 <- enc2native ("\U3010\U5165\U529B\U8005\U6CE8\U3011")
    reg3 <- enc2native ("\UFF3B\UFF03\U005B\U005E\UFF3D\U005D\U002A\UFF3D")
    reg4 <- enc2native ("\U300A\U005B\U005E\U300B\U005D\U002A\U300B")
    reg5 <- enc2native ("\UFF5C")
    while (length(input <- readLines(con, n=1, encoding = "CP932")) > 0){
      if (grepl(reg1, input)) break ;
      if (grepl(reg2, input)) break;
      if (grepl("^------", input)) {
        flag <- !flag
        next;
      }
      if (!flag){
        input <- gsub (reg3, "", input, perl = TRUE)
        input <- gsub (reg4, "", input, perl = TRUE)
        input <- gsub (reg5, "", input, perl = TRUE)
        writeLines(input, con=outfile)
      }
    }
    close(con); close(outfile)
    return (newDir);
  }
}



################################ �_�E�����[�h
### �Ėڟ���
0
Aozora("https://www.aozora.gr.jp/cards/000148/files/2314_ruby_2291.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1086_ruby_5742.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/758_ruby_6056.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2669_ruby_6341.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/792_ruby_2117.zip") #�u�v���o�����ƂȂǁv�͐��M
Aozora("https://www.aozora.gr.jp/cards/000148/files/1046_ruby_4521.zip") #�u�J�[���C�������فv�͋I�s
Aozora("https://www.aozora.gr.jp/cards/000148/files/769_ruby_565.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2383_ruby_2323.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/760_ruby_2341.zip") #�u�Ɏq�˂̒��v�͐��M�Ȃ̂ō폜
Aozora("https://www.aozora.gr.jp/cards/000148/files/2674_ruby_6327.zip")
1
Aozora("https://www.aozora.gr.jp/cards/000148/files/2670_ruby_6329.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/778_ruby_2283.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/777_ruby_1717.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2373_ruby_2077.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/776_ruby_6020.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/761_ruby_1861.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/770_ruby_1713.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/771_ruby_2407.zip")
# Aozora("https://www.aozora.gr.jp/cards/000148/files/759_ruby_3240.zip") �u������{�̊J�ԁv�͍u���̑��L�Ȃ̂ō폜
2
Aozora("https://www.aozora.gr.jp/cards/000148/files/775_ruby_2064.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/774_ruby_1640.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/773_ruby_5968.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1073_ruby_4526.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/766_ruby_2068.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/793_ruby_2448.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/784_ruby_1721.zip")
3
Aozora("https://www.aozora.gr.jp/cards/000148/files/794_ruby_4237.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/795_ruby_1725.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/768_ruby_2686.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/796_ruby_2452.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1104_ruby_4554.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/797_ruby_1729.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2680_ruby_6345.zip")
4
Aozora("https://www.aozora.gr.jp/cards/000148/files/2315_ruby_2410.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1102_ruby_4488.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/56143_ruby_50824.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2667_ruby_6347.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2370_ruby_2080.zip")
5
Aozora("https://www.aozora.gr.jp/cards/000148/files/2668_ruby_6349.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/798_ruby_2413.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2313_ruby_2287.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/757_ruby_2986.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2682_ruby_6333.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/788_ruby_2857.zip")
6
Aozora("https://www.aozora.gr.jp/cards/000148/files/751_ruby_1539.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2673_ruby_6339.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/791_ruby_1549.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/787_ruby_2295.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2376_ruby_2303.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/762_ruby_1705.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/765_ruby_2469.zip")
7
Aozora("https://www.aozora.gr.jp/cards/000148/files/754_ruby_2320.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/756_ruby_2948.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/755_ruby_3617.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2681_ruby_6323.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2679_ruby_6325.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2371_ruby_2071.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/753_ruby_1701.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/763_ruby_1709.zip")
8
Aozora("https://www.aozora.gr.jp/cards/000148/files/1750_ruby_19434.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/752_ruby_2438.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2375_ruby_2299.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1751_ruby_6331.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/780_ruby_569.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/781_ruby_2101.zip") #�u���؂Ƃ���ǂ���v�͋I�s
Aozora("https://www.aozora.gr.jp/cards/000148/files/783_ruby_1311.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/786_ruby_2375.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/782_ruby_2315.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2372_ruby_2074.zip")
9
Aozora("https://www.aozora.gr.jp/cards/000148/files/1747_ruby_5349.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/785_ruby_1656.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/799_ruby_6024.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2675_ruby_6355.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2678_ruby.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/2676_ruby_6343.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/779_ruby_2743.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/1076_ruby_4527.zip")
Aozora("https://www.aozora.gr.jp/cards/000148/files/789_ruby_5639.zip")
100
#Aozora("https://www.aozora.gr.jp/cards/000148/files/2672_ruby_6337.zip")  ��y�͔L�ł���@����
#Aozora("https://www.aozora.gr.jp/cards/000148/files/47148_ruby_32216.zip") ��y�͔L�ł���@���
#Aozora("https://www.aozora.gr.jp/cards/000148/files/2671_ruby_6335.zip") ��y�͔L�ł���@����
Aozora("https://www.aozora.gr.jp/cards/000148/files/2677_ruby_6351.zip")
# Aozora("https://www.aozora.gr.jp/cards/000148/files/772_ruby_33099.zip") �u���̌l��`�v�͍u���̕M�L�Ȃ̂ō폜



####################################
# �X���O
0
Aozora("https://www.aozora.gr.jp/cards/000129/files/2595_ruby_20435.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/673_ruby_23254.zip")
Aozora("https://www.aozora.gr.jp/cards/000882/files/3414_ruby_28548.zip") # ����̓v�����H�[ �}���Z���́u�c�Ɂv��|��
Aozora("https://www.aozora.gr.jp/cards/000907/files/4251_ruby_12064.zip") # �A���h���[�G�t ���I�j�[�h�E�j�R���[�G���B�`�u���v
Aozora("https://www.aozora.gr.jp/cards/000129/files/695_ruby_22805.zip")
1
Aozora("https://www.aozora.gr.jp/cards/000884/files/3418_ruby_28552.zip") # �f�B���t �I�V�b�v�u�݁v
Aozora("https://www.aozora.gr.jp/cards/000129/files/45270_ruby_19021.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45209_ruby_29695.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/48209_ruby_29697.zip") �u���e�v
Aozora("https://www.aozora.gr.jp/cards/001193/files/50915_ruby_39238.zip") # �I�C�����x���N �w���x���g�u���̌����v
Aozora("https://www.aozora.gr.jp/cards/000075/files/4250_ruby_32179.zip") #�����P ���C�l���E�}���A�u�Ə풃�с@���E����v�z�v
Aozora("https://www.aozora.gr.jp/cards/000129/files/680_ruby_23197.zip")
2
Aozora("https://www.aozora.gr.jp/cards/000129/files/678_ruby_22883.zip")
Aozora("https://www.aozora.gr.jp/cards/001194/files/50916_ruby_39174.zip") # �V���~�b�g�{�� �E�B���w�����u��v
Aozora("https://www.aozora.gr.jp/cards/000129/files/45224_ruby_19890.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/1071_ruby_4557.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/43732_ruby_17108.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/3615_ruby_4642.zip")
3
Aozora("https://www.aozora.gr.jp/cards/000129/files/2051_ruby_22885.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/1054_ruby_23199.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2597_ruby_22937.zip")
Aozora("https://www.aozora.gr.jp/cards/000883/files/3415_ruby_28549.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/4459_ruby_17993.zip")
4
Aozora("https://www.aozora.gr.jp/cards/000129/files/45244_ruby_21881.zip")
Aozora("https://www.aozora.gr.jp/cards/000883/files/3417_ruby_28551.zip") # �����i�[�� �t�F�����c�u�ŏI�̌ߌ�v
Aozora("https://www.aozora.gr.jp/cards/000366/files/50919_ruby_39173.zip") #�A���`�o�V�F�b�t �~�n�C���E�y�g���[���B�`�u�ߐl�v
Aozora("https://www.aozora.gr.jp/cards/000129/files/2547_ruby.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/688_ruby_23233.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2598_ruby_20420.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/686_ruby_2541.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/685_ruby_20310.zip") #���M
5
Aozora("https://www.aozora.gr.jp/cards/000129/files/689_ruby_23256.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/43030_ruby_17344.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2058_ruby_19627.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2599_ruby_23032.zip")
Aozora("https://www.aozora.gr.jp/cards/000075/files/50920_ruby_39171.zip") # �����P ���C�l���E�}���A�u���v
6
Aozora("https://www.aozora.gr.jp/cards/000129/files/690_ruby_23034.zip")
# Aozora("https://www.aozora.gr.jp/cards/000129/files/13201_ruby_9289.zip") �w�V�󌹎�����x���ł̏�
Aozora("https://www.aozora.gr.jp/cards/000129/files/2522_ruby_5001.zip")
Aozora("https://www.aozora.gr.jp/cards/001195/files/50917_ruby_39236.zip") # �_�r�b�g ���[�R�v�E�����E�X�u���E���V�v
Aozora("https://www.aozora.gr.jp/cards/000129/files/45245_ruby_21882.zip")
7
Aozora("https://www.aozora.gr.jp/cards/000129/files/46234_ruby_22009.zip")
Aozora("https://www.aozora.gr.jp/cards/001192/files/50914_ruby_39170.zip") # �z�[�t�}���X�^�[�� �t�[�S�[�E�t�H���u�s�l�Ǝ��Ɓv
Aozora("https://www.aozora.gr.jp/cards/000129/files/3336_ruby_23053.zip")
Aozora("https://www.aozora.gr.jp/cards/000883/files/3416_ruby_28550.zip")
Aozora("https://www.aozora.gr.jp/cards/001190/files/50913_ruby_39172.zip") # �A���e���x���N �y�[�^�[�u�ށv
8
Aozora("https://www.aozora.gr.jp/cards/000129/files/3614_ruby_12061.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2049_ruby_19718.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45271_ruby_19022.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/42375_ruby_18247.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45618_ruby_24923.zip")
Aozora("https://www.aozora.gr.jp/cards/000881/files/3413_ruby_28547.zip") # �u�E�e �t���f���b�N�u���̉��v
9
Aozora("https://www.aozora.gr.jp/cards/000129/files/45254_ruby_29696.zip")
Aozora("https://www.aozora.gr.jp/cards/001062/files/50912_ruby_39237.zip") # �X�g�����h�x�� �A�E�O�X�g�u��l����v
Aozora("https://www.aozora.gr.jp/cards/000129/files/676_ruby_23235.zip")
Aozora("https://www.aozora.gr.jp/cards/001025/files/50909_ruby_49057.zip") # �Q�[�e ���n���E���H���t�K���O�E�t�H���u�t�@�E�X�g�v
10
Aozora("https://www.aozora.gr.jp/cards/000129/files/50910_ruby_45862.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45255_ruby_19719.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/675_ruby_23191.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45256_ruby_19720.zip")
Aozora("https://www.aozora.gr.jp/cards/001196/files/45276_ruby_19722.zip") # �����h �n���X�u�~�̉��v
Aozora("https://www.aozora.gr.jp/cards/000129/files/2396_ruby_3993.zip")
11
Aozora("https://www.aozora.gr.jp/cards/000129/files/55722_ruby_56838.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2083_ruby_27447.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/2050_ruby_19721.zip") #���M
Aozora("https://www.aozora.gr.jp/cards/000129/files/50911_ruby_45861.zip") # �u��{�t�@�E�X�g�ɂ��āv
Aozora("https://www.aozora.gr.jp/cards/000129/files/696_ruby_23259.zip")
12
Aozora("https://www.aozora.gr.jp/cards/000129/files/2079_ruby_23238.zip")
Aozora("https://www.aozora.gr.jp/cards/000129/files/45272_ruby_19023.zip")

######################################################################

#�����P�ʂł̃o�C�O����
res <- docNgram("./NORUBY", type=0)
nrow(res);ncol(res)
View(res)
res %>% tail()
res2 <- res%>% t() %>% dist() %>% hclust("ward.D2")

res2 <- res[rownames(res) %in% c("[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]",
                                 "[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]",
                                 "[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]","[��-�A]")]
dim(res2)
View(res2)

res2_pc <- princomp(t(res2))
res2_pc <- res2 %>% t() %>% princomp()




