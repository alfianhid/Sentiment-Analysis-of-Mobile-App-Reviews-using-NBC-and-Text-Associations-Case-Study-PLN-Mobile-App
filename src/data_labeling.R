# IMPORT LIBRARY
library(stringr) # untuk pengolahan data string
library(ggplot2) # untuk visualisasi data
library(scales) # untuk skala dalam visualisasi data
library(ROSE) # untuk oversampling

# ATUR FOLDER DAN READ DATASET (AS DATAFRAME)
setwd("E:/PROGRAM KP/")
dataset_siap <- read.csv("dataset_bersih.csv", header=TRUE)

# READ KAMUS KATA POSITIF DAN NEGATIF
positif <- scan("list-of-positive-words.txt", what="character", comment.char=";")
negatif <- scan("list-of-negative-words.txt", what="character", comment.char=";")

# DEFINE THE SCORING FUNCTION
score.sentiment = function(myData, kata.positif, kata.negatif, .progress='none')
{
  require(plyr)
  scores = laply(myData, function(kalimat, kata.positif, kata.negatif) {
    list.kata = str_split(kalimat, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) - sum(negatif.matches)
    return (score)
  }, kata.positif, kata.negatif, .progress=.progress)
  
  scores.df = data.frame(score=scores, text=myData)
  return (scores.df)
}

# DATASET SCORING (PROSES LAMA)
hasil_skoring = score.sentiment(dataset_siap$text, positif, negatif)
View(hasil_skoring)

# mengkonversi skor menjadi sentimen negatif atau positif
hasil_skoring$sentiment <- ifelse(hasil_skoring$score < 0, "Negatif","Positif")
View(hasil_skoring)

# menghitung persentase sentimen positif dan negatif
table(hasil_skoring$sentiment) # imbalanced dataset
ggplot(hasil_skoring, aes(x=factor(sentiment), fill=factor(sentiment))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent) + xlab("Sentiment") +
  ylab("Persentase") + ggtitle("Tabel Distribusi Kolom Sentiment") + theme_grey(base_size=10)

# HANDLE IMBALANCED DATA
hasil_oversampling <- ovun.sample(sentiment~., data=hasil_skoring,
                                  method="over", p=0.5, seed=1)$data
ggplot(hasil_oversampling, aes(x=factor(sentiment), fill=factor(sentiment))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=percent) + xlab("Sentiment") +
  ylab("Persentase") + ggtitle("Tabel Distribusi Kolom Sentiment") + theme_grey(base_size=10)
table(hasil_oversampling$sentiment)

# ubah posisi kolom agar lebih mudah dibaca
dataset_terlabel <- hasil_oversampling[c(2,1,3)]
View(dataset_terlabel)

# export data sentimen positif dan negatif
data_positif <- dataset_terlabel %>%
  filter(sentiment == "Positif")
View(data_positif)
data_negatif <- dataset_terlabel %>%
  filter(sentiment == "Negatif")
View(data_negatif)

# simpan menjadi file CSV baru
write.csv(dataset_terlabel, file="dataset_terlabel.csv")
write.csv(data_positif, file="dataset_positif.csv")
write.csv(data_negatif, file="dataset_negatif.csv")
save.image(file = "program_KP.RData")
