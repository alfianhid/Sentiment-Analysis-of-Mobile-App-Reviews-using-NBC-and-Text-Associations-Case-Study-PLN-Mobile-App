# import library
library(RColorBrewer) # untuk pewarnaan grafik
library(wordcloud) # untuk visualisasi grafik

# ATUR FOLDER DAN READ DATASET (AS DATAFRAME)
setwd("E:/PROGRAM KP/")
dataset_negatif <- read.csv("dataset_negatif.csv", header=TRUE)[ , c("text","score","sentiment")]
corpus_negatif <- Corpus(VectorSource(dataset_negatif$text))

# Remove specified stopwords
corpus_negatif2 <- tm_map(corpus_negatif, removeWords, c("tugas","kali","selesai","nyala","cepatbayar",
                                                         "sekal","kasih","bagus","ayan","suai","bantu",
                                                         "mudah","langgan","terimakasih","info","kalo",
                                                         "karangtanggal","murah","aplikasimaju","guys",
                                                         "hewan","intipadam","jeniskeluh","mndi","milda",
                                                         "mogakali","mulunyala","please","cepatsigap",
                                                         "heiiipln","errornya","malesan","malas","aplikasi"))

#Replace words
corpus_negatif3 <- tm_map(corpus_negatif2, gsub, pattern="layan",replacement="layanan")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="madam",replacement="padam")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="idupkan",replacement="hidupkan")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="parahplns",replacement="parah")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="semuatetangga",replacement="tetangga")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="ribetsering",replacement="sering")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="jaringlistrik",replacement="listrik")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="kantorkalilistrik",replacement="kantor")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="sampa",replacement="sampah")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="fiturpilih",replacement="fitur")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="penghunikosong",replacement="rumah kosong")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="biayasemogamobile",replacement="biaya")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="kalilambat",replacement="lambat")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="kantorkota",replacement="kantor kota")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="susahnyala",replacement="nyala")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="sianghidupyg",replacement="siang hidup")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="listrikpohon",replacement="pohon")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="robohdll",replacement="roboh")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="tugaslapang",replacement="petugas lapangan")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="kerjadenda",replacement="denda")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="kinerjakeluh",replacement="kinerja")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="ayanchat",replacement="layanan chat")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="jawansajah",replacement="jawa saja")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="siangidupyg",replacement="siang hidup")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="konsleting",replacement="korsleting")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="teknisiminggu",replacement="teknisi minggu")
corpus_negatif3 <- tm_map(corpus_negatif3, gsub, pattern="passworddicoba",replacement="password")

#Build a term-document matrix
tdm2 <- TermDocumentMatrix(corpus_negatif3)
matrix2 <- as.matrix(tdm2)
vector2 <- sort(rowSums(matrix2), decreasing=TRUE)
data_frame2 <- data.frame(word = names(vector2), freq=vector2)
head(data_frame2, 50)

#Generate the Word cloud
set.seed(456)
wordcloud(words = data_frame2$word, freq = data_frame2$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Set2"))

#Explore frequent terms and their associations
findFreqTerms(tdm2, lowfreq = 10)

#Find related words
myvector2 <- as.list(findAssocs(tdm2, terms=c("mati","ganggu","padam",
                                            "susah","error"),
                               corlimit = c(0.15,0.15,0.15,0.15,0.15)))
myvector2

#Find related words (one by one)
myv2 <- as.list(findAssocs(tdm2, terms =c("mati"),
                          corlimit = c(0.15)))
View(myv2$mati)

#barplot
k2<-barplot(data_frame2[1:9,]$freq, las = 2, names.arg = data_frame2[1:9,]$word,
           cex.axis=1.2,cex.names=1.2,
           main ="Kata Negatif yang Paling Sering Muncul",
           ylab = "Frekuensi",col = heat.colors(20))
termFrequency2 <- rowSums(as.matrix(tdm2))
termFrequency2 <- subset(termFrequency2, termFrequency2>110)
text(k2, sort(termFrequency2, decreasing = TRUE),
     labels=sort(termFrequency2, decreasing = T),pch=6, cex=1)

save.image(file = "program_KP.RData")
