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

file$na_count_r <- apply(file, 1, function(x) sum(is.na(x)))
file <- file[!file$na_count_r ==125, ]
file <- file[!file$Q12=="I do not wish to complete the XX University Doctoral Exit survey.",]
file$na_count_r <- NULL
nrow(file)

#urg dataset
unique(file$Underrepresented)
urg <- filter(file, Underrepresented=="Underrepresented Group")
non_urg <- filter(file, Underrepresented=="Non-Underrespresented Group")
head(urg)
head(non_urg)




levels<-as.factor(urg$sameuniv13)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
urg$sc_num <-as.numeric(levels)

levels<-as.factor(urg$samefield14)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
urg$sl_num <-as.numeric(levels)

levels<-as.factor(urg$recommend15)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
urg$so_num <-as.numeric(levels)

acad <- select(urg, sameuniv13) %>%
  mutate(category="Would you select xx university")
colnames(acad)<-c("condition", "category")

life <- select(urg, samefield14) %>%
  mutate(category="would you select same field of study") 
colnames(life)<-c("condition", "category")

overall <- select(urg, recommend15) %>%
  mutate(category="would you recommend university to someone considering your field of study")
colnames(overall)<-c("condition", "category")


all<-rbind(acad, life, overall)
all$group <- "urg"


levels<-as.factor(all$condition)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
all$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall
all$co_num[is.na(all$co_num)] <- mean(all$co_num, na.rm = T)
all


#urg data :
levels<-as.factor(non_urg$sameuniv13)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
non_urg$sc_num <-as.numeric(levels)

levels<-as.factor(non_urg$samefield14)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
non_urg$sl_num <-as.numeric(levels)

levels<-as.factor(non_urg$recommend15)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
non_urg$so_num <-as.numeric(levels)

cad <- select(non_urg, sameuniv13) %>%
  mutate(category="Would you select xx university")
colnames(cad)<-c("condition", "category")

lif <- select(non_urg, samefield14) %>%
  mutate(category="would you select same field of study") 
colnames(lif)<-c("condition", "category")

overal <- select(non_urg, recommend15) %>%
  mutate(category="would you recommend university to someone considering your field of study")
colnames(overal)<-c("condition", "category")


non_urg<-rbind(cad, lif, overal)
non_urg$group <- "non_urg"


levels<-as.factor(non_urg$condition)
levels <-factor(levels,c("Definitely not","Probably not","Maybe","Probably","Definitely"))
non_urg$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall
non_urg$co_num[is.na(non_urg$co_num)] <- mean(non_urg$co_num, na.rm = T)
non_urg




data_visual <-rbind(all, non_urg)
data_visual

df <-aggregate(x=data_visual$co_num,
               by=list(data_visual$category,data_visual$group),
               FUN=mean)
colnames(df) <- c("category", "group", "mean")
df$group <- toupper(df$group)
colnames(df)[3]<-'Mean_score_of_satisfaction'
df


ggplot(df, aes(x=Mean_score_of_satisfaction, y=category, fill=group, label=round(Mean_score_of_satisfaction, digits = 2))) +    
  geom_bar(width = 0.5,stat="identity", color="black", position=position_dodge())+
  geom_text(position = position_dodge(width = .9), hjust = -0.5,size = 5) + 
  theme(axis.text.y=element_text(hjust=1,size = 20)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("If you were to start your doctoral career again...") +
  scale_colour_brewer("Dark2") +
  theme(text = element_text(size=15))+
  scale_fill_brewer(palette="Paired")
theme_minimal() 
coord_flip()

ggsave("/Users/cleopathy/Desktop/overall3.png", width = 17, height = 12,bg = 'White')