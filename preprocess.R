rm(list = ls()) # clear the environment
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggpubr)

#read in the data from excel
file<- read_excel("/Users/cleopathy/Desktop/assignment1.xlsx", sheet = "RAW DATA - DEIDENTIFIED")
#international student excluded from analysis:
file <- filter(file, Q62 != "International (Non-U.S. Citizen with temporary U.S. Visa)")

#reformat column name
colnames(file) <- gsub("/", "_", colnames(file))
colnames(file) <- gsub(" ", "_", colnames(file))
colnames(file) <- gsub("\\?", "", colnames(file))
colnames(file) <- gsub("\\.", "", colnames(file))
head(file)
colnames(file)
str(file)
write.csv(file,'/Users/cleopathy/Desktop/reformated.csv')
#file_dup <- file
#now count how many NAs in rows 

#file_dup$na_count_r <- apply(file_dup, 1, function(x) sum(is.na(x)))
#na_count is at #126 column
#hist(file_dup$na_count_r, main = "Histogram of counts",xlab = "count",col = "blue",border = "red")

test <- file
#count NAs in columns 
na_count_c <- sapply(test, function(y) sum(length(which(is.na(y)))))
na_count_c <- data.frame(na_count_c)
colnames(na_count_c) <-'na_number'
df <- cbind(column_name = rownames(na_count_c), na_count_c)
rownames(df) <- 1:nrow(na_count_c)
df$na_percent <- df$na_number/ nrow(test) *100
df

ggplot(df, aes(x = na_number)) + 
  geom_histogram(color = 'blue2', fill = 'cornflowerblue', position="identity")+
  labs(title="Histogram of NA in each column",x="columns", y = "NA count") +
  geom_density(alpha=.2, fill="#FF6666") 


ggplot(df, aes(x = na_number)) + 
  geom_histogram(aes(y = ..density..), color = 'blue2', fill = 'cornflowerblue', position="identity")+
  labs(title="Histogram of NA in each column",x="columns", y = "NA count") +
  theme(axis.text=element_text(size=14,face = 'bold'),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 14,face = 'bold'))+
  geom_density(alpha=.2, fill="#FF6666") 

ggsave("/Users/cleopathy/Desktop/eachcol_na.png", width = 10, height = 8,bg = 'White')


ggplot(df, aes(x=column_name, y=na_number)) + 
  labs(title = "NA count in each column", x = "All the columns", y = "NA count") +
  geom_bar(stat = "identity",color = 'cornflowerblue') +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())




write.csv(file, "/Users/cleopathy/Desktop/clean.csv")
#write.xlsx(file, "/Users/cleopathy/Desktop/clean.xlsx", colnames = TRUE)







