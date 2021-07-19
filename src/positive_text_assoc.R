# import library
library(RColorBrewer) # untuk pewarnaan grafik
library(wordcloud) # untuk visualisasi grafik

# ATUR FOLDER DAN READ DATASET (AS DATAFRAME)
setwd("E:/PROGRAM KP/")
dataset_positif <- read.csv("dataset_positif.csv", header=TRUE)[ , c("text","score","sentiment")]
corpus_positif <- Corpus(VectorSource(dataset_positif$text))

# Remove specified stopwords
corpus_positif2 <- tm_map(corpus_positif, removeWords, c("kasih","terima","tugas","bintang","keluh","alamibatulucin",
                                                        "tolong","terimakasih","sekal","banget","sahari","close",
                                                        "nomor","kali","kerja","selesai","laku","padam",
                                                        "kalo","ganggu","maaf","aplikasimobile","mati",
                                                        "tingkat","info","lapang","llayanan","tulis",
                                                        "download","fast","berik","lumlayanan","moga",
                                                        "mobile","rating","nama","malam","trimakasih",
                                                        "live","april","januari","call","ribet","habis",
                                                        "adayg","besokmati","dahdi","diaplikasimemangnya",
                                                        "dimanaterpaksa","jugakalo","termakasih","cobaflip",
                                                        "closed","endingnya","batalnomor","kalocontoh"))

#Replace words
corpus_positif3 <- tm_map(corpus_positif2, gsub, pattern="ayan",replacement="layanan")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="langgan",replacement="langganan")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="hubung",replacement="kontak")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="llayanan",replacement="jasa")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="muas",replacement="puas")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="responnya",replacement="respon")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="bagusjasa",replacement="jasa")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="dayaupgrade",replacement="upgrade")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="internetuntuk",replacement="internet")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="tersebuttolong",replacement="tolong")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="hasilstatus",replacement="status")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="hasiltombol",replacement="tombol")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="listrikpln",replacement="listrik")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="fiturmobile",replacement="fitur")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="belistrukjadi",replacement="struk")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="bagusjasa",replacement="jasa")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="listrikhabis",replacement="listrik")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="meteransudah",replacement="meter")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="tokenhari",replacement="token")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="auto",replacement="otomatis")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="keperayaanmobile",replacement="percaya")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="rumahstabil",replacement="stabil")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="aplikasiasli",replacement="asli")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="tokped",replacement="tokopedia")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="ganggumobiletercloseselesai",replacement="ganggu close selesai")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="meresponselesa",replacement="respon")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="notifikasiterclose",replacement="notifikasi")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="teleponsaran",replacement="saran")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="ecek",replacement="periksa")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="ekstrapenuh",replacement="ekstra")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="padamproses",replacement="proses")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="statussuruh",replacement="status")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="kalobebasin",replacement="bebas")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="tokentransaksi",replacement="transaksi")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="kalomobile",replacement="mobile")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="ranting",replacement="rating")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="alhamdulillahaku",replacement="alhamdulillah")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="servicenyaman",replacement="service")
corpus_positif3 <- tm_map(corpus_positif3, gsub, pattern="tinggatkan",replacement="tingkat")

#Build a term-document matrix
tdm <- TermDocumentMatrix(corpus_positif3)
matrix <- as.matrix(tdm)
vector <- sort(rowSums(matrix), decreasing=TRUE)
data_frame <- data.frame(word = names(vector), freq=vector)
head(data_frame, 50)

#Generate the Word cloud
set.seed(123)
wordcloud(words = data_frame$word, freq = data_frame$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Set2"))

#Explore frequent terms and their associations
findFreqTerms(tdm, lowfreq = 10)

#Find related words
myvector <- as.list(findAssocs(tdm, terms=c("aplikasi","transaksi","layanan",
                                    "cepat","harga"),
                      corlimit = c(0.15,0.15,0.15,0.15,0.15)))
myvector

#Find related words (one by one)
myv <- as.list(findAssocs(tdm, terms =c("aplikasi"),
                      corlimit = c(0.15)))
View(myv$aplikasi)

#barplot
k<-barplot(data_frame[1:10,]$freq, las = 2, names.arg = data_frame[1:10,]$word,
           cex.axis=1.2,cex.names=1.2,
           main ="Kata Positif yang Paling Sering Muncul",
           ylab = "Frekuensi",col = terrain.colors(20))
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>101)
text(k, sort(termFrequency, decreasing = TRUE),
     labels=sort(termFrequency, decreasing = T),pch=6, cex=1)

save.image(file = "program_KP.RData")
