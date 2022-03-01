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


# 1=Strongly Disagree 2=Disagree 3-Ambivalent 4-Agree 5= Strongly Agree

levels<-as.factor(file$respect9)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
file$re_num <-as.numeric(levels)

levels<-as.factor(file$intclimate10)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
file$in_num <-as.numeric(levels)

levels<-as.factor(file$socclimate11)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
file$so_num <-as.numeric(levels)

levels<-as.factor(file$collegial12)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
file$co_num <-as.numeric(levels)

respect9 <- select(file, respect9) %>%
  mutate(category="students in my program are treated with respect by faculty")
colnames(respect9)<-c("condition", "category")

intclimate10 <- select(file, respect9) %>%
  mutate(category="The intellectual climate of my program is positive") 
colnames(intclimate10)<-c("condition", "category")

socclimate11 <- select(file, respect9) %>%
  mutate(category="The social climate of my program is positive")
colnames(socclimate11)<-c("condition", "category")

collegial12 <- select(file, respect9) %>%
  mutate(category="students in my program are collegial")
colnames(collegial12)<-c("condition", "category")

overall<-rbind(respect9, intclimate10, socclimate11, collegial12)
overall$group <- "overall"


levels<-as.factor(overall$condition)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
overall$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall
overall$co_num[is.na(overall$co_num)] <- mean(overall$co_num, na.rm = T)
overall


#urg data :
levels<-as.factor(urg$respect9)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
urg$re_num <-as.numeric(levels)

levels<-as.factor(urg$intclimate10)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
urg$in_num <-as.numeric(levels)

levels<-as.factor(urg$socclimate11)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
urg$so_num <-as.numeric(levels)

levels<-as.factor(urg$collegial12)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
urg$co_num <-as.numeric(levels)

respect_urg <- select(urg, respect9) %>%
  mutate(category="students in my program are treated with respect by faculty")
colnames(respect_urg)<-c("condition", "category")

intclimate_urg<- select(urg, intclimate10) %>%
  mutate(category="The intellectual climate of my program is positive") 
colnames(intclimate_urg)<-c("condition", "category")

socclimate_urg <- select(urg, socclimate11) %>%
  mutate(category="The social climate of my program is positive")
colnames(socclimate_urg)<-c("condition", "category")

collegial_urg <- select(urg, collegial12) %>%
  mutate(category="students in my program are collegial")
colnames(collegial_urg)<-c("condition", "category")

urg_data<-rbind(respect_urg, intclimate_urg, socclimate_urg, collegial_urg)
urg_data$group <- "urg"


levels<-as.factor(urg_data$condition)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
urg_data$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall
urg_data$co_num[is.na(urg_data$co_num)] <- mean(urg_data$co_num, na.rm = T)
urg_data

#################
#non_urg data :
levels<-as.factor(non_urg$respect9)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
non_urg$re_num <-as.numeric(levels)

levels<-as.factor(non_urg$intclimate10)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
non_urg$in_num <-as.numeric(levels)

levels<-as.factor(non_urg$socclimate11)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
non_urg$so_num <-as.numeric(levels)

levels<-as.factor(non_urg$collegial12)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
non_urg$co_num <-as.numeric(levels)

respect_non_urg <- select(non_urg, respect9) %>%
  mutate(category="students in my program are treated with respect by faculty")
colnames(respect_non_urg)<-c("condition", "category")

intclimate_non_urg<- select(non_urg, intclimate10) %>%
  mutate(category="The intellectual climate of my program is positive") 
colnames(intclimate_non_urg)<-c("condition", "category")

socclimate_non_urg <- select(non_urg, socclimate11) %>%
  mutate(category="The social climate of my program is positive")
colnames(socclimate_non_urg)<-c("condition", "category")

collegial_non_urg <- select(non_urg, collegial12) %>%
  mutate(category="students in my program are collegial")
colnames(collegial_non_urg)<-c("condition", "category")

non_urg_data<-rbind(respect_non_urg, intclimate_non_urg, socclimate_non_urg, collegial_non_urg)
non_urg_data$group <- "non_urg"


levels<-as.factor(non_urg_data$condition)
levels <-factor(levels,c("Strongly Disagree","Disagree","Ambivalent","Agree","Strongly Agree"))
non_urg_data$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall
non_urg_data$co_num[is.na(non_urg_data$co_num)] <- mean(non_urg_data$co_num, na.rm = T)
non_urg_data



data_visual <-rbind(overall, urg_data, non_urg_data)
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
  geom_bar(stat="identity", color="black", position=position_dodge())+
  geom_text(position = position_dodge(width = .9), hjust = -0.5,size = 5) + 
  theme(axis.text.y=element_text(hjust=1,size = 20)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("To what extent do you agree/disagree with each of following statement?") +
  scale_colour_brewer("Dark2") +
  theme(text = element_text(size=15))+
  scale_fill_brewer(palette="Accent")
theme_minimal() 
coord_flip()

ggsave("/Users/cleopathy/Desktop/overall2.png", width = 17, height = 12,bg = 'White')