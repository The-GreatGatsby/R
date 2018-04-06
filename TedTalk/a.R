# TED TALKSのカーネルを写経

library(ggplot2) # Data visualisation
library(ggrepel) # data visualisation
library(dplyr) # data manipulation
library(viridis) # data visualisation
library(stringr) # String manipulation
library(RColorBrewer) # Color palette
library(ggthemes) # Themes for plot 
library(tidyverse)
library(wordcloud2)
library(zoo)
library(anytime)
library(data.table)
library(treemap)
library(cowplot)
library(wordcloud)

setwd("C:/Users/rstud/Documents/GitHub/R/TedTalk")

ted = read.csv("data/ted_main.csv",header=TRUE,stringsAsFactors = FALSE)
transcript = read.csv("data/transcripts.csv",header=TRUE,stringsAsFactors = FALSE)

p1=ggplot(ted,aes(comments,..count..))+geom_histogram(fill="red2")+themefn+labs(x="Comments",y="Count",title="Distribution of comments")+scale_x_continuous(limits=c(0,1500),breaks=seq(0,1500,150))+geom_vline(aes(xintercept = median(ted$comments)),linetype=4,size=1,color="black")
p2=ggplot(ted,aes(duration,..count..))+geom_histogram(fill="pink2")+themefn+labs(x="Duration",y="Count",title="Distribution of Duration")+geom_vline(aes(xintercept = median(ted$duration)),linetype=4,size=1,color="black")+scale_x_continuous(limits=c(0,4000),breaks=seq(0,4000,500))
plot_grid(p1,p2,label_fontface = "italics",cols=1)











