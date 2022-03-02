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


#urg dataset
unique(file$Underrepresented)
urg <- filter(file, Underrepresented=="Underrepresented Group")
non_urg <- filter(file, Underrepresented=="Non-Underrespresented Group")
head(urg)
head(non_urg)




levels<-as.factor(urg$satacad)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$sc_num <-as.numeric(levels)

levels<-as.factor(urg$satlife)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$sl_num <-as.numeric(levels)

levels<-as.factor(urg$satoverall)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$so_num <-as.numeric(levels)

acad <- select(urg, satacad) %>%
  mutate(category="Your academic experience at xx university")
colnames(acad)<-c("condition", "category")

life <- select(urg, satlife) %>%
  mutate(category="Your student life experience at xx university") 
colnames(life)<-c("condition", "category")

overall <- select(urg, satoverall) %>%
  mutate(category="Your overall experience at xx university")
colnames(overall)<-c("condition", "category")


all<-rbind(acad, life, overall)
all$group <- "urg"


levels<-as.factor(all$condition)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
all$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall
all$co_num[is.na(all$co_num)] <- mean(all$co_num, na.rm = T)
all


#urg data :
levels<-as.factor(non_urg$satacad)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$sc_num <-as.numeric(levels)

levels<-as.factor(non_urg$satlife)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$sl_num <-as.numeric(levels)

levels<-as.factor(non_urg$satoverall)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$so_num <-as.numeric(levels)

cad <- select(non_urg, satacad) %>%
  mutate(category="Your academic experience at xx university")
colnames(cad)<-c("condition", "category")

lif <- select(non_urg, satlife) %>%
  mutate(category="Your student life experience at xx university") 
colnames(lif)<-c("condition", "category")

overal <- select(non_urg, satoverall) %>%
  mutate(category="Your overall experience at xx university")
colnames(overal)<-c("condition", "category")


non_urg<-rbind(cad, lif, overal)
non_urg$group <- "non_urg"


levels<-as.factor(non_urg$condition)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
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

df
###########data visualization bar plot : 
colnames(df) <- c("category", "group", "mean")
df$group <- toupper(df$group)
colnames(df)[3]<-'Mean_score_of_satisfaction'
df
ggplot(df, aes(x=Mean_score_of_satisfaction, y=category, fill=group, label=round(Mean_score_of_satisfaction, digits = 2))) +    
  geom_bar(width = 0.5,stat="identity", color="black", position=position_dodge())+
  geom_text(position = position_dodge(width = .9), hjust = -0.5,size = 5) + 
  theme(axis.text.y=element_text(hjust=1,size = 20)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Please rate your overall satisfaction with each of the following:") +
  scale_colour_brewer("Dark2") +
  theme(text = element_text(size=15))+
  xlim(0,5) +
  scale_fill_brewer(palette="Dark2")
theme_minimal() 
coord_flip()


ggsave("/Users/cleopathy/Desktop/overall1.png", width = 17, height = 12,bg = 'White')