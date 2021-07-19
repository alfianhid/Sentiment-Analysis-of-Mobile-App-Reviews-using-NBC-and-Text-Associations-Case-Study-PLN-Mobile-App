library(SnowballC)
library(RColorBrewer)
library(wordcloud)

# ATUR FOLDER DAN READ DATASET (AS DATAFRAME)
setwd("E:/PROGRAM KP/")
dataset_visual <- read.csv("dataset_terlabel.csv", header=TRUE)[ , c("text","score","sentiment")]
corpus_visual <- Corpus(VectorSource(dataset_visual$text))

# Remove specified stopwords
corpus_visual2 <- tm_map(corpus_visual, removeWords, c("kasih","mohon","ayan","terima","kali","tolong",
                                    "selesai","sekal","pakai","terimakasih","bintang",
                                    "coba","moga","kalo"))

#Replace words
corpus_visual3 <- tm_map(corpus_visual2, gsub, pattern="langgan",replacement="langganan")
corpus_visual3 <- tm_map(corpus_visual2, gsub, pattern="layan",replacement="layanan")

#Build a term-document matrix
tdm <- TermDocumentMatrix(corpus_visual3)
matrix <- as.matrix(tdm)
vector <- sort(rowSums(matrix), decreasing=TRUE)
data_frame <- data.frame(word = names(vector), freq=vector)
head(data_frame, 50)

#Generate the Word cloud
set.seed(1234)
wordcloud(words = data_frame$word, freq = data_frame$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Set2"))

#Explore frequent terms and their associations
findFreqTerms(tdm, lowfreq = 10)

#Find related words
myvector <- as.list(findAssocs(tdm, terms=c("aplikasi","transaksi","pelayanan",
                                    "update","harga","fitur"),
                      corlimit = c(0.15,0.15,0.15,0.15,0.15,0.15)))
myvector

#Find related words (one by one)
myv <- as.list(findAssocs(tdm, terms =c("aplikasi"),
                      corlimit = c(0.15)))
View(myv$aplikasi)

#barplot
k<-barplot(data_frame[1:15,]$freq, las = 2, names.arg = data_frame[1:15,]$word,
           cex.axis=1.2,cex.names=1.2,
           main ="Most frequent words",
           ylab = "Word frequencies",col = terrain.colors(20))
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=115)
text(k, sort(termFrequency, decreasing = T)-2,
     labels=sort(termFrequency, decreasing = T),pch=6, cex=1)