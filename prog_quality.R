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


# 1=Strongly Disagree 2=Disagree 3-Ambivalent 4-Agree 5= Strongly Agree





levels<-as.factor(file$curriculum16)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
file$cu_num <-as.numeric(levels)

levels<-as.factor(file$teaching17)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
file$te_num <-as.numeric(levels)

levels<-as.factor(file$advising18)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
file$ad_num <-as.numeric(levels)

levels<-as.factor(file$candidacy19)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
file$ca_num <-as.numeric(levels)

levels<-as.factor(file$interdisc20)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
file$in_num <-as.numeric(levels)

levels<-as.factor(file$employment21)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
file$em_num <-as.numeric(levels)

levels<-as.factor(file$progqual22)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
file$pr_num <-as.numeric(levels)


cu <- select(file, curriculum16) %>%
  mutate(category="Quality of the graduate curriculum")
colnames(cu)<-c("condition", "category")

tea <- select(file, teaching17) %>%
  mutate(category="Quality of graduate level teaching by faculty") 
colnames(tea)<-c("condition", "category")

ad <- select(file, advising18) %>%
  mutate(category="Quality of academic advising and guidance") 
colnames(ad)<-c("condition", "category")

can <- select(file, candidacy19) %>%
  mutate(category="Preparation for candidacy/comprehensive examinations") 
colnames(can)<-c("condition", "category")

inter <- select(file, interdisc20) %>%
  mutate(category="the opportunity to collaborate across discipline") 
colnames(inter)<-c("condition", "category")

em <- select(file, employment21) %>%
  mutate(category="assistance in finding employment") 
colnames(em)<-c("condition", "category")

pr <- select(file, progqual22) %>%
  mutate(category="assistance in finding employment") 
colnames(pr)<-c("condition", "category")

overall<-rbind(cu, tea, ad, can, inter, em, pr)
overall$group <- "overall"


levels<-as.factor(overall$condition)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
overall$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall
overall$co_num[is.na(overall$co_num)] <- mean(overall$co_num, na.rm = T)
overall


#urg data :
levels<-as.factor(urg$curriculum16)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$cu_num <-as.numeric(levels)

levels<-as.factor(urg$teaching17)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$te_num <-as.numeric(levels)

levels<-as.factor(urg$advising18)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$ad_num <-as.numeric(levels)

levels<-as.factor(urg$candidacy19)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$ca_num <-as.numeric(levels)

levels<-as.factor(urg$interdisc20)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$in_num <-as.numeric(levels)

levels<-as.factor(urg$employment21)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$em_num <-as.numeric(levels)

levels<-as.factor(furg$progqual22)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg$pr_num <-as.numeric(levels)

cu_urg <- select(urg, curriculum16) %>%
  mutate(category="Quality of the graduate curriculum")
colnames(cu_urg)<-c("condition", "category")

tea_urg <- select(urg, teaching17) %>%
  mutate(category="Quality of graduate level teaching by faculty") 
colnames(tea_urg)<-c("condition", "category")

ad_urg <- select(urg, advising18) %>%
  mutate(category="Quality of academic advising and guidance") 
colnames(ad_urg)<-c("condition", "category")

can_urg <- select(urg, candidacy19) %>%
  mutate(category="Preparation for candidacy/comprehensive examinations") 
colnames(can_urg)<-c("condition", "category")

inter_urg <- select(urg, interdisc20) %>%
  mutate(category="the opportunity to collaborate across discipline") 
colnames(inter_urg)<-c("condition", "category")

em_urg <- select(urg, employment21) %>%
  mutate(category="assistance in finding employment") 
colnames(em_urg)<-c("condition", "category")

pr_urg <- select(urg, employment21) %>%
  mutate(category="assistance in finding employment") 
colnames(pr_urg)<-c("condition", "category")

urg_data<-rbind(cu_urg, tea_urg, ad_urg, can_urg, inter_urg, em_urg, pr_urg)
urg_data$group <- "urg"


levels<-as.factor(urg_data$condition)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
urg_data$co_num <-as.numeric(levels)
#summary(overall)
#overall %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # fill NA with mean in column
#overall



#urg_data$co_num [is.nan(as.numeric(urg_data$co_num))] <- "NA"
#urg_data$co_num[is.na(urg_data$co_num)] <- mean(urg_data$co_num, na.rm = T)
#urg_data
#non_urg data :
#urg data :
levels<-as.factor(non_urg$curriculum16)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$cu_num <-as.numeric(levels)

levels<-as.factor(non_urg$teaching17)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$te_num <-as.numeric(levels)

levels<-as.factor(non_urg$advising18)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$ad_num <-as.numeric(levels)

levels<-as.factor(non_urg$candidacy19)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$ca_num <-as.numeric(levels)

levels<-as.factor(non_urg$interdisc20)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$in_num <-as.numeric(levels)

levels<-as.factor(non_urg$employment21)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$em_num <-as.numeric(levels)

levels<-as.factor(non_urg$progqual22)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
non_urg$pr_num <-as.numeric(levels)

cu_non_urg <- select(non_urg, curriculum16) %>%
  mutate(category="Quality of the graduate curriculum")
colnames(cu_non_urg)<-c("condition", "category")

tea_non_urg <- select(non_urg, teaching17) %>%
  mutate(category="Quality of graduate level teaching by faculty") 
colnames(tea_non_urg)<-c("condition", "category")

ad_non_urg<- select(non_urg, advising18) %>%
  mutate(category="Quality of academic advising and guidance") 
colnames(ad_non_urg)<-c("condition", "category")

can_non_urg <- select(non_urg, candidacy19) %>%
  mutate(category="Preparation for candidacy/comprehensive examinations") 
colnames(can_non_urg)<-c("condition", "category")

inter_non_urg <- select(non_urg, interdisc20) %>%
  mutate(category="the opportunity to collaborate across discipline") 
colnames(inter_non_urg)<-c("condition", "category")

em_non_urg <- select(non_urg, employment21) %>%
  mutate(category="assistance in finding employment") 
colnames(em_non_urg)<-c("condition", "category")



pr_non_urg <- select(non_urg, employment21) %>%
  mutate(category="assistance in finding employment") 
colnames(pr_non_urg)<-c("condition", "category")


non_urg_data<-rbind(cu_non_urg, tea_non_urg, ad_non_urg, can_non_urg, inter_non_urg, em_non_urg,pr_non_urg)
non_urg_data$group <- "non_urg"


levels<-as.factor(non_urg_data$condition)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
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
df$mean <- round(df$mean, 2)
df

ggplot(df, aes(x=mean, y=category, fill=group, label=mean)) +    
  geom_bar(width = 0.5,stat="identity", color="black", position=position_dodge())+
  geom_text(position = position_dodge(width = .9), hjust = -0.5,size = 4) + 
  theme(axis.text.y=element_text(hjust=1,size = 10)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Please rate the following aspects of your doctoral program:") +
  scale_colour_brewer("Dark2") +
  xlim(0,5) +
  theme(text = element_text(size=10))+
  scale_fill_brewer(palette="Set2")
theme_minimal() 
coord_flip()

ggsave("/Users/cleopathy/Desktop/program_quality.png", width = 13, height = 8,bg = 'White')