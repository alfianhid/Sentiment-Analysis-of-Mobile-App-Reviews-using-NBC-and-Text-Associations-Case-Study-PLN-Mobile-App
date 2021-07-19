# IMPORT LIBRARY
library(tm) # untuk text mining
library(tokenizers) # untuk tokenize kalimat
library(dplyr) # untuk manipulasi data dalam bahasa R
library(textclean) # untuk normalisasi slang word
library(stopwords) # untuk stopword removal
library(parallel) # untuk mengaktifkan function 'Apply' 
library(katadasaR) # untuk stemming kalimat

# ATUR FOLDER DAN READ DATASET (AS STRING)
setwd("E:/PROGRAM KP/")
dataset_mentah <- readLines("raw_dataset.csv")

# LIHAT DATASET
dataset_mentah[1:10]

# CASE FOLDING
hasil_casefolding <- tolower(dataset_mentah)
hasil_casefolding[10]

# CLEANING
# menghapus karakter non-ASCII
hasil_cleaning1 <- gsub("[^\x20-\x7E]", "", hasil_casefolding)
hasil_cleaning1[10]

# menghapus angka
hasil_cleaning1[3]
hasil_cleaning2 <- gsub("\\d", "", hasil_cleaning1)
hasil_cleaning2[3]

# menghapus tanda baca
hasil_cleaning3 <- gsub("[[:punct:]]+", "", hasil_cleaning2)
hasil_cleaning3[3]

# menghapus spasi yang berlebihan
hasil_cleaning4 <- gsub("\\s+", " ", hasil_cleaning3)
hasil_cleaning4[3]

# SPELL NORMALIZATION (PROSES LAMA)
kamus_slang <- read.csv("colloquial-indonesian-lexicon.csv", sep=",", header=TRUE)
hasil_normalisasi <- replace_internet_slang(hasil_cleaning4, slang=paste0("\\b",
                     kamus_slang$slang, "\\b"),
                     replacement=kamus_slang$formal, ignore.case=TRUE)
hasil_cleaning4[9]
hasil_normalisasi[9]

# WORD STEMMING (PROSES LAMA)
stemming <- function(x){
  paste(lapply(x, katadasaR), collapse = " ")
}
hasil_stemming <- lapply(tokenize_words(hasil_normalisasi[]), stemming)
hasil_normalisasi[6]
hasil_stemming[6]

# TOKENIZING
hasil_tokenizing <- tokenize_words(hasil_stemming)
hasil_tokenizing[6]

# FILTERING / STOPWORD REMOVAL (PROSES LAMA)
hasil_tokenizing_char <- as.character(hasil_tokenizing)
kamus_stopword <- readLines("stopwords.txt")
hasil_stopword <- tokenize_words(hasil_tokenizing_char, stopwords=kamus_stopword)
hasil_stopword[6]

# KONVERSI DATASET KE BENTUK STRING
hasil_stopword_char <- sapply(hasil_stopword, toString)
hasil_stopword_char[1:10]

# menghapus tanda baca
hasil_stopword_bersih1 <- gsub("[[:punct:]]+", "", hasil_stopword_char)
hasil_stopword_bersih1[1:10]
# menghapus kata dengan satu s/d tiga huruf
hasil_stopword_bersih2 <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", "", hasil_stopword_bersih1)
hasil_stopword_bersih2[1:10]

# KONVERSI DATASET KE BENTUK CORPUS
dataset_corpus <- Corpus(VectorSource(hasil_stopword_bersih2))

# KONVERSI DATASET KE BENTUK DATAFRAME
df <- data.frame(text=unlist(sapply(dataset_corpus, `[`)), stringsAsFactors=FALSE)
View(df)

# CHECK & REMOVE MISSING VALUES
df[df==""] <- NA
sum(is.na(df))
dataset_hapusna <- na.omit(df) # hapus NA row

# REMOVE DUPLICATED VALUES
dataset_urut <- dataset_hapusna %>%
  arrange(text)
dataset_bersih <- distinct(dataset_urut, text, .keep_all=TRUE)
View(dataset_bersih)

# SIMPAN DATASET
write.csv(dataset_bersih, "dataset_bersih.csv")
save.image(file = "program_KP.RData")
