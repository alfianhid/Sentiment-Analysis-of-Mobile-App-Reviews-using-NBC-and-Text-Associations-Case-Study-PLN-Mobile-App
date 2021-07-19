# IMPORT LIBRARY
library(RTextTools) # untuk klasifikasi teks
library(e1071) # untuk naive bayes
library(caret) # untuk proses training & testing

#Input Data
setwd("C:/Users/ALFIAN HIDAYATULLOH/Videos/")
dataset_final <- read.csv("dataset_terlabel.csv", header=TRUE)[ , c("text","score","sentiment")]

#Randomly shuffle the dataset
set.seed(100)
df_rows <- sample(nrow(dataset_final))
my_df <- dataset_final[df_rows, ]
my_df <- dataset_final[df_rows, ]
View(my_df)

# mengubah ke bentuk corpus
corpus_final <- Corpus(VectorSource(my_df$text))
inspect(corpus_final[1:10])

# mengubah ke bentuk matrix
doctm <- DocumentTermMatrix(corpus_final)
inspect(doctm[1:10, 11:20])

#Partitioning (70:30)
#my_df.train <- my_df[0:2082,]
#my_df.test <- my_df[2083:2973,]

#doctm.train <- doctm[0:2082,]
#doctm.test <- doctm[2083:2973,]

#corpus_final.train <- corpus_final[0:2082]
#corpus_final.test <- corpus_final[2083:2973]

#Partitioning (75:25)
#my_df.train <- my_df[0:2231,]
#my_df.test <- my_df[2232:2973,]

#doctm.train <- doctm[0:2231,]
#doctm.test <- doctm[2232:2973,]

#corpus_final.train <- corpus_final[0:2231]
#corpus_final.test <- corpus_final[2232:2973]

#Partitioning (80:20)
#my_df.train <- my_df[0:2379,]
#my_df.test <- my_df[2380:2973,]

#doctm.train <- doctm[0:2379,]
#doctm.test <- doctm[2380:2973,]

#corpus_final.train <- corpus_final[0:2379]
#corpus_final.test <- corpus_final[2380:2973]

#Partitioning (85:15)
#my_df.train <- my_df[0:2528,]
#my_df.test <- my_df[2529:2973,]

#doctm.train <- doctm[0:2528,]
#doctm.test <- doctm[2529:2973,]

#corpus_final.train <- corpus_final[0:2528]
#corpus_final.test <- corpus_final[2529:2973]

#Partitioning (90:10)
my_df.train <- my_df[0:2677,]
my_df.test <- my_df[2678:2973,]

doctm.train <- doctm[0:2677,]
doctm.test <- doctm[2678:2973,]

corpus_final.train <- corpus_final[0:2677]
corpus_final.test <- corpus_final[2678:2973]

write.csv(my_df.train, file = "training_data.csv")
write.csv(my_df.test, file = "testing_data.csv")

#Featured Selection
dim(doctm.train)
fivefreq <- findFreqTerms(doctm.train, 1)
length(fivefreq)

doctm.train.nb <- DocumentTermMatrix(corpus_final.train, control=list(dictionary = fivefreq))
dim(doctm.train.nb)
doctm.test.nb <- DocumentTermMatrix(corpus_final.test, control=list(dictionary = fivefreq))
dim(doctm.train.nb)

#Boolean Naive Bayes
convert_countNB <- function(x){
  y <- ifelse(x > 0, 1, 0)
  y <- factor(y, levels=c(0, 1), labels=c("No", "Yes"))
  y
}

#==========Naive Bayes============#
TrainNB = read.csv("training_data.csv", header=TRUE)[ , c("text","score","sentiment")]
TestNB = read.csv("testing_data.csv", header=TRUE)[ , c("text","score","sentiment")]

# (PROSES LAMA)
trainNB <- apply(doctm.train.nb, 2, convert_countNB)
testNB <- apply(doctm.test.nb, 2, convert_countNB)

#Training (PROSES LAMA)
classifier <- naiveBayes(trainNB, my_df.train$sentiment, laplace=1)

#Testing (PROSES LAMA)
pred <- predict(classifier, testNB)

#Tabel Prediksi vs Asli
mytable <- table("Predictions"=pred, "Actual"=my_df.test$sentiment)

# Prepare the confusion matrix
conf.mat <- confusionMatrix(mytable)
conf.mat
conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']*100

save.image(file = "program_KP.RData")
