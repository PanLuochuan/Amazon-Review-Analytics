# 加载库文件
library(NLP)
library(RColorBrewer)
library(textclean)
library(sentimentr)
library(tm)
library(ggplot2)
library(wordcloud)
library(Rcpp)

# 设置路径
setwd('C:\\Users\\Administrator\\Desktop\\20240213作业')


reviews_df <- read.csv("Task2_AmazonReviewPosts.csv", header=TRUE, encoding = 'UTF8',sep = ",") 

#reviews_df <- sub('\\.+$', '', reviews_df)

reviews_vec<-as.vector(reviews_df$text)


# clean函数
clean_vec<- function(text_vec) {
  text_vec<- tolower(text_vec)
  text_vec<- replace_emoticon(text_vec, emoticon_dt = lexicon::hash_emoticons)
  return(text_vec)
}
# 执行clean函数
reviews_vec<-clean_vec(reviews_df)

# 清除无效的.
for(ireview in 1:length(reviews_vec)) {
     reviews_vec[ireview]<-gsub('\\.+$', '', reviews_vec[ireview], ignore.case = T)
  }


#原文件中无表情符，不执行表情符替换
  
#Emoji_dict<- read.csv("UnicodeEmojiDict_clean.csv", header=TRUE)
#for(ireview in 1:length(reviews.vec)) {
#  for(irow in 1:nrow(Emoji_dict)) {
#    reviews.vec[ireview]<-gsub(Emoji_dict$code[irow], Emoji_dict$text[irow], reviews.vec[ireview], ignore.case = T)
#  }
#}

write.csv(reviews_vec, file="AmazonNikeReviewPosts_clean.csv", row.names=FALSE)

reviews_vec <- as.vector(read.csv("AmazonNikeReviewPosts_clean.csv", header=TRUE)$x)
reviews_vec_sent<-get_sentences(reviews_vec)
reviews_vec_sent<-sentiment_by(reviews_vec_sent)
review_sent_df<-data.frame(Reviews=reviews_vec, Sentiment=reviews_vec_sent$ave_sentiment)
plot_data<-data.frame(Sentiment=review_sent_df$Sentiment)
ggplot(plot_data, aes(x=Sentiment)) + geom_histogram(binwidth=0.2, 
                                                     fill="darkred", colour="grey60", size=0.2) + geom_density(size=0.2)

pos_reviews<-subset(review_sent_df, review_sent_df$Sentiment > 0)
neg_reviews<-subset(review_sent_df, review_sent_df$Sentiment < 0)

pos_terms<-paste(pos_reviews$Reviews, collapse= " ")
neg_terms<-paste(neg_reviews$Reviews, collapse= " ")
all_terms<-c(pos_terms, neg_terms)
all_corpus<-VCorpus(VectorSource(all_terms))

custom_stopwords<-c(stopwords("english"), "kodak", "\\bair monarch", "\\bair\\b", "monarch", "monarchs", "amazon")
all_corpus<-tm_map(all_corpus, removeWords, custom_stopwords)
all_corpus<-tm_map(all_corpus, removeNumbers)
all_corpus<-tm_map(all_corpus, removePunctuation)
all_corpus<-tm_map(all_corpus, stripWhitespace)

all_tdm<-TermDocumentMatrix(all_corpus, control=list(weighting=weightTfIdf))
all_tdm_m<-as.matrix(all_tdm)
colnames(all_tdm_m)<-c("Positive", "Negative")  #"Positive", "Negative"

wordcloud(all_tdm_m, max.words=100, colors=c("darkgreen", "darkred")) 


pos_corpus<-VCorpus(VectorSource(pos_reviews$Reviews))
pos_corpus<-tm_map(pos_corpus, removeWords, custom_stopwords)
pos_corpus<-tm_map(pos_corpus, removeNumbers)
pos_corpus<-tm_map(pos_corpus, removePunctuation)
pos_corpus<-tm_map(pos_corpus, stripWhitespace)

pos_tdm<-TermDocumentMatrix(pos_corpus, control=list(weighting=weightTf))
pos_tdm_m<-as.matrix(pos_tdm)
freq_pos_terms<-rowSums(pos_tdm_m)
freq_pos_terms<-data.frame(Terms=names(freq_pos_terms), Frequency=freq_pos_terms)
freq_pos_terms<-freq_pos_terms[order(freq_pos_terms[,2], decreasing=T),]
write.csv(freq_pos_terms, "freq_pos_terms.csv")


loves_assoc<-findAssocs(pos_tdm, "loves", 0.1)
loves_assoc<-as.data.frame(loves_assoc)
loves_assoc$Terms<-row.names(loves_assoc)
loves_assoc$Terms<-factor(loves_assoc$Terms, levels=loves_assoc$Terms)

ggplot(loves_assoc, aes(y=Terms)) +
  geom_point (aes(x=loves), data=loves_assoc, 
              size=3) +
  geom_text(aes(x=loves,
                label=loves), 
            colour="darkblue", hjust=-.25, size=4) +
  theme(text=element_text (size=15),
        axis.title.y=element_blank())


neg_corpus<-VCorpus(VectorSource(neg_reviews$Reviews))
neg_corpus<-tm_map(neg_corpus, removeWords, custom_stopwords)
neg_corpus<-tm_map(neg_corpus, removeNumbers)
neg_corpus<-tm_map(neg_corpus, removePunctuation)
neg_corpus<-tm_map(neg_corpus, stripWhitespace)

neg_tdm<-TermDocumentMatrix(neg_corpus, control=list(weighting=weightTf))
neg_tdm_m<-as.matrix(neg_tdm)
freq_neg_terms<-rowSums(neg_tdm_m)
freq_neg_terms<-data.frame(Terms=names(freq_neg_terms), Frequency=freq_neg_terms)
freq_neg_terms<-freq_neg_terms[order(freq_neg_terms[,2], decreasing=T),]
write.csv(freq_neg_terms, "freq_neg_terms.csv")

loves_assoc<-findAssocs(neg_tdm, "squeak", 0.1)
loves_assoc<-as.data.frame(loves_assoc)
loves_assoc$Terms<-row.names(loves_assoc)
loves_assoc$Terms<-factor(loves_assoc$Terms, levels=loves_assoc$Terms)

ggplot(loves_assoc, aes(y=Terms)) +
  geom_point (aes(x=squeak), data=loves_assoc, 
              size=3) +
  geom_text(aes(x=squeak,
                label=squeak), 
            colour="darkblue", hjust=-.25, size=4) +
  theme(text=element_text (size=15),
        axis.title.y=element_blank())


###
file_path <- "C:\\Users\\ASUS\\Desktop\\0230\\Task2_AmazonReviewPosts.csv"
file_content <- read.csv(file.choose(), header = T)
reviews.vec<-as.vector(file_content$text)

remove_unicode <- function(x) iconv(enc2utf8(x), sub = "byte")
cleaned_text <- remove_unicode(reviews.vec)
cleaned_text <- gsub("<92>", "'", cleaned_text)
cleaned_text <- gsub("<91>", "'", cleaned_text)

clean.vec<- function(text.vec) {
  text.vec<- tolower(text.vec)
  text.vec<- replace_emoticon(text.vec, emoticon_dt = lexicon::hash_emoticons)
  return(text.vec)
}
reviews.vec<-clean.vec(reviews.vec)
getwd()
write.csv(reviews.vec, file="AmazonNikeReviewPosts_clean.csv", row.names=FALSE)

reviews_vec <- as.vector(read.csv("AmazonNikeReviewPosts_clean.csv", header=TRUE)$x)
reviews_vec_sent<-get_sentences(reviews_vec)
reviews_vec_sent<-sentiment_by(reviews_vec_sent)
review_sent_df<-data.frame(Reviews=reviews_vec, Sentiment=reviews_vec_sent$ave_sentiment)
plot_data<-data.frame(Sentiment=review_sent_df$Sentiment)
ggplot(plot_data, aes(x=Sentiment)) + geom_histogram(binwidth=0.2, 
                                                     fill="darkred", colour="grey60", size=0.2)

pos_reviews<-subset(review_sent_df, review_sent_df$Sentiment > 0)
neg_reviews<-subset(review_sent_df, review_sent_df$Sentiment < 0)


pos_terms<-paste(pos_reviews$Reviews, collapse= " ")
neg_terms<-paste(neg_reviews$Reviews, collapse= " ")
all_terms<-c(pos_terms, neg_terms)
all_corpus<-VCorpus(VectorSource(all_terms))

custom_stopwords<-c(stopwords("english"), "kodak", "camera", "very", "gift", "picture", "print")
all_corpus<-tm_map(all_corpus, removeWords, custom_stopwords)
all_corpus<-tm_map(all_corpus, removeNumbers)
all_corpus<-tm_map(all_corpus, removePunctuation)
all_corpus<-tm_map(all_corpus, stripWhitespace)


all_tdm<-TermDocumentMatrix(all_corpus, control=list(weighting=weightTfIdf))
all_tdm_m<-as.matrix(all_tdm)
colnames(all_tdm_m)<-c("Positive", "Negative")  #"Positive", "Negative"

wordcloud(all_tdm_m, max.words=30, colors=c("darkgreen", "darkred")) 


pos_corpus<-VCorpus(VectorSource(pos_reviews$Reviews))
pos_corpus<-tm_map(pos_corpus, removeWords, custom_stopwords)
pos_corpus<-tm_map(pos_corpus, removeNumbers)
pos_corpus<-tm_map(pos_corpus, removePunctuation)
pos_corpus<-tm_map(pos_corpus, stripWhitespace)

pos_tdm<-TermDocumentMatrix(pos_corpus, control=list(weighting=weightTf))
pos_tdm_m<-as.matrix(pos_tdm)
freq_pos_terms<-rowSums(pos_tdm_m)
freq_pos_terms<-data.frame(Terms=names(freq_pos_terms), Frequency=freq_pos_terms)
freq_pos_terms<-freq_pos_terms[order(freq_pos_terms[,2], decreasing=T),]
write.csv(freq_pos_terms, "freq_pos_terms.csv")


loves_assoc<-findAssocs(pos_tdm, "loves", 0.1)
loves_assoc<-as.data.frame(loves_assoc)
loves_assoc$Terms<-row.names(loves_assoc)
loves_assoc$Terms<-factor(loves_assoc$Terms, levels=loves_assoc$Terms)

ggplot(loves_assoc, aes(y=Terms)) +
  geom_point (aes(x=loves), data=loves_assoc, 
              size=3) +
  geom_text(aes(x=loves,
                label=loves), 
            colour="darkblue", hjust=-.25, size=4) +
  theme(text=element_text (size=15),
        axis.title.y=element_blank())


neg_corpus<-VCorpus(VectorSource(neg_reviews$Reviews))
neg_corpus<-tm_map(neg_corpus, removeWords, custom_stopwords)
neg_corpus<-tm_map(neg_corpus, removeNumbers)
neg_corpus<-tm_map(neg_corpus, removePunctuation)
neg_corpus<-tm_map(neg_corpus, stripWhitespace)

neg_tdm<-TermDocumentMatrix(neg_corpus, control=list(weighting=weightTf))
neg_tdm_m<-as.matrix(neg_tdm)
freq_neg_terms<-rowSums(neg_tdm_m)
freq_neg_terms<-data.frame(Terms=names(freq_neg_terms), Frequency=freq_neg_terms)
freq_neg_terms<-freq_neg_terms[order(freq_neg_terms[,2], decreasing=T),]
write.csv(freq_neg_terms, "freq_neg_terms.csv")

loves_assoc<-findAssocs(neg_tdm, "squeak", 0.1)
loves_assoc<-as.data.frame(loves_assoc)
loves_assoc$Terms<-row.names(loves_assoc)
loves_assoc$Terms<-factor(loves_assoc$Terms, levels=loves_assoc$Terms)

ggplot(loves_assoc, aes(y=Terms)) +
  geom_point (aes(x=squeak), data=loves_assoc, 
              size=3) +
  geom_text(aes(x=squeak,
                label=squeak), 
            colour="darkblue", hjust=-.25, size=4) +
  theme(text=element_text (size=15),
        axis.title.y=element_blank())












