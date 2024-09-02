library(ggplot2)
library(plotrix)
library(RColorBrewer)
library(NLP)
library(wordcloud)
library(Rcpp)
library(tm)

# set path
setwd('C:\\Users\\Administrator\\Desktop\\20240202作业')
getwd()

# read data
ugg_keywords_df<-read.csv('Task1_Ugg_search+queries.csv',skip = 1, header = TRUE, sep =',')
bearpaw_keywords_df<-read.csv('Task1_Bearpaw_search+queries.csv',skip = 1, header = TRUE, sep =',')

#
sum(is.na(ugg_keywords_df))
sum(is.na(bearpaw_keywords_df))
# creat searches_dataframe
ugg_search_df<-data.frame(doc_id=seq(1:nrow(ugg_keywords_df)),text=ugg_keywords_df$Suggestion)
bearpaw_search_df<-data.frame(doc_id=seq(1:nrow(bearpaw_keywords_df)),text=bearpaw_keywords_df$Suggestion)

ugg_corpus<-VCorpus(DataframeSource(ugg_search_df))
bearpaw_corpus<-VCorpus(DataframeSource(bearpaw_search_df))

# 需要排除的词库
ugg_custom_stopwords<-c(stopwords('english'),'ugg',"can","will")
bearpaw_custom_stopwords<-c(stopwords('english'),'bearpaw','bear','paw','can','will') #第一运行，出现有大量'bear'和'paw',需要排除

#排除不需要的词
ugg_corpus<-tm_map(ugg_corpus,removeWords,ugg_custom_stopwords)
bearpaw_corpus<-tm_map(bearpaw_corpus,removeWords,bearpaw_custom_stopwords)
ugg_corpus<-tm_map(ugg_corpus,stripWhitespace)
bearpaw_corpus<-tm_map(bearpaw_corpus,stripWhitespace)


# 自定义兴趣词搜索输出函数
my_freq<-function(tdm, myfile,mycolor,myname)
    {
      tdm.m<-as.matrix(tdm)
    tdm.m
    term.freq<-rowSums(tdm.m)
    freq.df<-data.frame(Terms=names(term.freq),Frequency=term.freq)
    freq.df<-freq.df[order(freq.df[,2], decreasing=TRUE),]
    write.csv(freq.df, file=myfile) 
    
    freq.df$Terms<-factor(freq.df$Terms,levels = unique(as.character(freq.df$Terms)))
    ggplot(freq.df[1:20,],aes(x=Terms,y=Frequency)) +geom_bar(stat="identity",
                                                             fill=mycolor) +coord_flip() +
      geom_text(aes(label=Frequency),colour="white", hjust=1.25, size=4.0)+ggtitle(myname)
}

# 自定义分析关联函数
my_association=function(tdm, key_word, mycolor,myname)
  {
  associations<-findAssocs(tdm, key_word, 0.05)   
  associations<-as.data.frame(associations)
  associations$terms<-row.names(associations)
  associations$terms<-factor(associations$terms,
                             levels=associations$terms) 
  ggplot(associations, aes(y=terms)) +
    geom_point (aes(x=key_word), data=associations, 
                size=3) +
    geom_text(aes(x=key_word,
                  label=key_word), 
              colour=mycolor, hjust=-.25, size=4) +
    theme(text=element_text (size=15),
          axis.title.y=element_blank())+ggtitle(myname)
}

# UGG调用兴趣词搜索函数并输出结果
tdm<-TermDocumentMatrix(ugg_corpus,control = list(weighting=weightTf))
my_freq(tdm,myfile="ugg_frequentTerms.csv",mycolor='blue',myname='Ugg Frequency')
key_word='boots'                                              # 根据兴趣词分析，关键词采用'boots'
my_association(tdm,key_word='boots',mycolor='blue',myname="Ugg boots's Frequency")
my_association(tdm,key_word='slippers',mycolor='blue',myname="Ugg slippers's Frequency")

# bearpaw调用兴趣词搜索函数并输出结果
tdm<-TermDocumentMatrix(bearpaw_corpus,control = list(weighting=weightTf))
my_freq(tdm, myfile='bearpaw_frequentTerms.csv',mycolor='red',myname='bearpaw Frequency')                                             # 根据兴趣词分析，关键词采用'boots'
my_association(tdm,key_word='boots',mycolor='red',myname="bearpaw boots's Frequency")
my_association(tdm,key_word='slippers',mycolor='red',myname="bearpaw slippers's Frequency")

pal<- brewer.pal(8, "Purples")
pal<-pal[-(1:4)]
commonality.cloud(tdm_mWC, max.words=200,
                  random.order=FALSE, colors=pal)
comparison.cloud(tdm_mWC, max.words=200, scale=c(3.5, .4),
                 random.order=FALSE, title.size=1.0,
                 colors=brewer.pal(ncol(tdm_mWC), "Dark2"))
# 4--比较搜索词与bearpaw的共享程度
# 自定义clean函数
my_stopwords<- c(stopwords("english"), "ugg", "bearpaw",'bear','paw')  #根据前面运行结果，需要同时过滤'bear','paw'
clean_vec<- function(text_vec) {
    text_vec<- removeWords(text_vec, my_stopwords)
    text_vec<- removePunctuation(text_vec)
    text_vec<- removeNumbers(text_vec)
    return(text_vec)
}

# 读取文件数据
ugg<-read.csv("Task1_Ugg_search+queries.csv", skip= 1, header=TRUE, sep = ",")
bearpaw<-read.csv("Task1_Bearpaw_search+queries.csv", skip= 1, header=TRUE, sep = ",")
# 清洗数据
ugg_vec<-clean_vec(as.vector(ugg$Suggestion))
bearpaw_vec<-clean_vec(as.vector(bearpaw$Suggestion))

ugg_vec<-paste(ugg_vec, collapse=" ")
bearpaw_vec<-paste(bearpaw_vec, collapse=" ")
合并数据并处理
all<- c(ugg_vec, bearpaw_vec)
corpusWC<- VCorpus(VectorSource(all))
tdmWC<- TermDocumentMatrix(corpusWC)
tdm_mWC<- as.matrix(tdmWC)
colnames(tdm_mWC)=c("ugg", "bearpaw")
common_words<-subset(tdm_mWC, tdm_mWC[, 1]>0 &
                       tdm_mWC[,2]>0)
difference<-abs(common_words[,1]-common_words[,2])
common_words<-cbind(common_words, difference)
common_words<-common_words[order(common_words[,3], 
                                 decreasing=TRUE), ]
top25_df<-data.frame(x= common_words[1:25, 1], y=
                       common_words[1:25, 2], 
                     labels = rownames(common_words[1:25, ]))
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels,
             gap = 20, top.labels = c("ugg", 
                                     "words", "bearpaw"), 
             main = "Words in Common", laxlab = NULL,
             raxlab = NULL, unit = NULL)

# 5--创建词云

pal<- brewer.pal(8, "Purples")
pal<-pal[-(1:4)]
commonality.cloud(tdm_mWC, max.words=200,
                  random.order=FALSE, colors=pal)
comparison.cloud(tdm_mWC, max.words=200, scale=c(3.5, .4),
                 random.order=FALSE, title.size=1.0,
                 colors=brewer.pal(ncol(tdm_mWC), "Dark2"))

