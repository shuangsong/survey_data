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


urg <- filter(file, Underrepresented=="Underrepresented Group")
non_urg <- filter(file, Underrepresented=="Non-Underrespresented Group")

df <- data.frame(case = c(nrow(urg), nrow(non_urg)), group = c('urg','non_urg'))
df$percent <-df$case/400 *100
  
ggplot(data=df, aes(x=case, y=group,fill = group)) +
  geom_bar(width = 0.3, stat="identity", position=position_dodge(0.3))+
  scale_fill_manual(values = c("cornflowerblue", "blue1")) +
  xlab("case count") + ylab("Group") +
  xlim(0, 350)+
  #geom_col(aes(fill = group), position = "dodge") +
  #scale_y_continuous(labels=case) +
  #scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(aes(label = case), position = position_dodge(0.3), hjust = 0)+
  ggtitle("URG and NON-URG gourp count") +
  theme(axis.text=element_text(size=14,face = 'bold'),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 14,face = 'bold'))
  
ggsave("/Users/cleopathy/Desktop/urg_percent.png", width = 8, height = 5,bg = 'White')

