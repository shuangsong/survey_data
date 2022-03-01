rm(list = ls()) # clear the environment
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(xlsx)
#read in the data from excel
raw<- read_excel("/Users/cleopathy/Desktop/assignment1.xlsx", sheet = "RAW DATA - DEIDENTIFIED")
#international student excluded from analysis:
file <- raw[!raw$Q62 == 'International (Non-U.S. Citizen with temporary U.S. Visa)', ]
head(file)
colnames(file)

#reformat column name
colnames(file) <- gsub("/", "_", colnames(file))
colnames(file) <- gsub(" ", "_", colnames(file))
colnames(file) <- gsub("\\?", "", colnames(file))
colnames(file) <- gsub("\\.", "", colnames(file))
colnames(file)
str(file)






file_dup <- file
#now count how many NAs in rows 

file_dup$na_count_r <- apply(file_dup, 1, function(x) sum(is.na(x)))
#na_count is at #126 column
hist(file_dup$na_count_r, main = "Histogram of counts",xlab = "count",col = "blue",border = "red")

#count NAs in columns 
na_count_c <- sapply(file_dup, function(y) sum(length(which(is.na(y)))))
na_count_c <- data.frame(na_count_c)


hist(na_count_c$na_count_c, main = "Histogram of counts",xlab = "count",col = "green",border = "red")


#now count how manys rows are empty and how many are do not wish to complete:
#we will exclude those in the further analysis
sum(file_dup$na_count_r == 125) #so 52 records are empty and we need to exclude those in further
nrow(file_dup)
not_wish <-file_dup[file_dup$Q12== "I do not wish to complete the XX University Doctoral Exit survey.",]
nrow(not_wish)


#below are the correct code for cleaning data 

rm(list = ls()) # clear the environment
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)
#read in the data from excel
raw<- read_excel("/Users/cleopathy/Desktop/assignment1.xlsx", sheet = "RAW DATA - DEIDENTIFIED")
#international student excluded from analysis:
file <- raw
#colnames(file)

#reformat column name
colnames(file) <- gsub("/", "_", colnames(file))
colnames(file) <- gsub(" ", "_", colnames(file))
colnames(file) <- gsub("\\?", "", colnames(file))
colnames(file) <- gsub("\\.", "", colnames(file))
colnames(file)
nrow(file) #452 records
#str(file)
file <- file[!file$Q62 == 'International (Non-U.S. Citizen with temporary U.S. Visa)', ]
head(file)

file$na_count_r <- apply(file, 1, function(x) sum(is.na(x)))
file <- file[!file$na_count_r ==125, ]
file <- file[!file$Q12=="I do not wish to complete the XX University Doctoral Exit survey.",]
file$na_count_r <- NULL
nrow(file) #now the data is cleaned 
file$na_count_r <- apply(file, 1, function(x) sum(is.na(x)))
file <- file[!file$na_count_r ==125, ]
file <- file[!file$Q12=="I do not wish to complete the XX University Doctoral Exit survey.",]
file$na_count_r <- NULL
nrow(file)

write.csv(file, "/Users/cleopathy/Desktop/clean.csv")
#write.xlsx(file, "/Users/cleopathy/Desktop/clean.xlsx", colnames = TRUE)







