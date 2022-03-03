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

male <- file[file$Q60 == "Male",]
nrow(male)
female <- file[file$Q60 == "Female",]
nrow(female)

male_urg <- urg[urg$Q60 == "Male",]
nrow(male_urg)
female_urg <- urg[urg$Q60 == "Female",]
nrow(female_urg)
male_non_urg <- non_urg[non_urg$Q60 == "Male",]
nrow(male_non_urg)
female_non_urg <- non_urg[non_urg$Q60 == "Female",]
nrow(female_non_urg)

df <- matrix(c(nrow(female_urg), nrow(female_non_urg),nrow(male_urg),nrow(male_non_urg)), nrow=2)
colnames(df) <- c("URG" ,"NON-URG")
rownames(df) <- c("Female","Male")



# Transform this data in %
data_percentage <- apply(df, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
#rearrange the data; 
group <- c("URG","NON-URG","URG","NON-URG")
gender <- c("female","male","male",'female')

d <- data.frame(group,gender)
d$percent <-c(data_percentage[1,1], data_percentage[2,2], data_percentage[2,1], data_percentage[1,2])
d$percent <-round(d$percent,1)

p <-ggplot(d , aes(x = group, y = percent, fill = gender)) + 
  geom_bar(stat = "identity",width = 0.2, position = position_stack(vjust = 0.5)) +
  ggtitle("Gender percentage in URG and NON URG group") +
  geom_text(aes(label = paste0(percent,'%'),y=percent),position = position_dodge(width = .9),size = 5)+
  theme(axis.text.x=element_text(vjust=1,size = 8)) +
  #scale_y_discrete(size = 15) +
  scale_colour_brewer("Dark2") +
  #scale_x_discrete(labels = wrap_format(10)) +
  theme(axis.text=element_text(size=14,face = 'bold'),
        axis.title=element_text(size=12,face="bold"),
        legend.text = element_text(size = 12))+
  scale_fill_brewer(palette="Paired")
p
p+coord_flip()


ggsave("/Users/cleopathy/Desktop/demograph.png", width = 8, height = 5,bg = 'White')



#hispanic or latino:
yes <- file[file$Q63 == "Yes",]
nrow(yes)
not <- file[file$Q63 == "No",]
nrow(not)

yes_urg <- urg[urg$Q63 == "Yes",]
nrow(yes_urg)
no_urg <- urg[urg$Q63 == "No",]
nrow(no_urg)
yes_non_urg <- non_urg[non_urg$Q63  == "Yes",]
nrow(yes_non_urg)
no_non_urg <- non_urg[non_urg$Q63  == "No",]
nrow(no_non_urg)
df <- matrix(c(nrow(yes_urg), nrow(no_urg),nrow(yes_non_urg),nrow(no_non_urg)), nrow=2)
colnames(df) <- c("URG" ,"NON-URG")
rownames(df) <- c("Yes","No")



# Transform this data in %
percen <- apply(df, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
#rearrange the data; 
group <- c("URG","NON-URG","URG","NON-URG")
latino <- c("yes","no","no",'yes')

v <- data.frame(group,latino)
v$percent <-c(percen[1,1], percen[2,2], percen[2,1], percen[1,2])
v$percent <- round(v$percent, 2)


p <-ggplot(v , aes(x = group, y = percent, fill = latino)) + 
  ggtitle("Hispanic/Latino percentage in URG and NON URG group") +
  #theme(axis.text.y=element_text(hjust=1,size = 20)) +
  geom_bar(stat = "identity",width = 0.3, position = position_stack(vjust = 0.5)) +
  #scale_y_continuous(labels = sacle::percent) +
  #use positions to plot labels
  geom_text(aes(label = paste0(percent,'%'),y=percent),position = position_dodge(width = .9),size = 6)+
  scale_colour_brewer("Dark2") +
  theme(text = element_text(size=15))+
  theme(axis.text=element_text(size=14,face = 'bold'),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 14,face = 'bold'))+
  scale_fill_brewer(palette="Paired")
  theme_minimal()
p

p + coord_flip()


ggsave("/Users/cleopathy/Desktop/latino.png", width = 13, height = 6,bg = 'White')


#race/ethinicity : 
#African American/Black
#Hispanic/Latinx
#Two or More Races
#Asian
#White
nurg_white <- non_urg[non_urg$Race_RECODE == "White",]
nrow(nurg_white)

nurg_asian <- non_urg[non_urg$Race_RECODE == "Asian",]
nrow(nurg_asian)
nurg_two <- non_urg[non_urg$Race_RECODE == "Two or More Races",]
nrow(nurg_two)

nurg_na <- non_urg[non_urg$Race_RECODE == "Missing/Unknown",]
nrow(nurg_na)

#write.csv(non_urg, "/Users/cleopathy/Desktop/non_urg.csv")
nurg_race<-non_urg %>% count(Race_RECODE)
colnames(nurg_race) <- c('race','count')
nurg_race$group <- "NON_URG"

urg_race<-urg %>% count(Race_RECODE)
colnames(urg_race) <- c('race','count')
urg_race$group <- "URG"

race_data <- rbind(urg_race, nurg_race)

ggplot(race_data, aes(x=count, y=group, fill=race, label=count)) +    
  geom_bar(width = 0.5,stat="identity", color="black", position=position_dodge())+
  geom_text(position = position_dodge(width = .5), hjust = -0.5,size = 4) + 
  theme(axis.text.y=element_text(hjust=1,size = 13)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ggtitle("Race/Ethnicity") +
  scale_colour_brewer("Dark2") +
  xlim(0,400)+
  theme(text = element_text(size=15))+
  scale_fill_brewer(palette="Blues")
theme_minimal() 


ggsave("/Users/cleopathy/Desktop/race.png", width = 8, height = 5,bg = 'White')