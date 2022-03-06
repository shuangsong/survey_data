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



unique(file$Underrepresented)
urg <- filter(file, Underrepresented=="Underrepresented Group")
non_urg <- filter(file, Underrepresented=="Non-Underrespresented Group")
head(urg)
head(non_urg)

#write_xlsx(urg,'/Users/cleopathy/Desktop/urg.xlsx')
#write.csv(non_urg,'/Users/cleopathy/Desktop/non_urg.csv')
unique(file$htopicmen)

#explore faculty mentoring
htopicmen_urg <-filter(urg, htopicmen=="Very helpful" | htopicmen =="Somewhat helpful") %>%
  select(htopicmen)
colnames(htopicmen_urg)=c("rank", "category")
htopicmen_urg <- htopicmen_urg %>% group_by(rank) %>%
  mutate(category="Selection of a Dissertation Topic")


hremen_urg <-filter(urg, hresearchmen=="Very helpful" | hresearchmen =="Somewhat helpful") %>%
  select(hresearchmen)
colnames(hremen_urg)=c("rank", "category")
hremen_urg <- hremen_urg %>% group_by(rank) %>%
  mutate(category="Your Dissertation Research")


hwrmen_urg <-filter(urg, hwritingmen=="Very helpful" | hwritingmen =="Somewhat helpful") %>%
  select(hwritingmen)
colnames(hwrmen_urg)=c("rank", "category")
hwrmen_urg <- hwrmen_urg %>% group_by(rank) %>%
  mutate(category="Writing and Revising your Dissertation")


hacamen_urg <-filter(urg, hacadmen=="Very helpful" | hacadmen =="Somewhat helpful") %>%
  select(hacadmen)
colnames(hacamen_urg)=c("rank", "category")
hacamen_urg <- hacamen_urg %>% group_by(rank) %>%
  mutate(category="Academic Career Options")


hnomen_urg <-filter(urg, hnonacadmen=="Very helpful" | hnonacadmen =="Somewhat helpful") %>%
  select(hnonacadmen)
colnames(hnomen_urg)=c("rank", "category")
hnomen_urg <- hnomen_urg %>% group_by(rank) %>%
  mutate(category="Nonacademic Career Options")


hemmen_urg <-filter(urg, hemploymen=="Very helpful" | hemploymen =="Somewhat helpful") %>%
  select(hemploymen)
colnames(hemmen_urg)=c("rank", "category")
hemmen_urg <- hemmen_urg %>% group_by(rank) %>%
  mutate(category="Search for Employement or Training")

urg_data_mentor<-rbind(htopicmen_urg, hremen_urg, hwrmen_urg, hacamen_urg, hnomen_urg, hemmen_urg)



htopicmen_nurg <-filter(non_urg, htopicmen=="Very helpful" | htopicmen =="Somewhat helpful") %>%
  select(htopicmen)
colnames(htopicmen_nurg)=c("rank", "category")
htopicmen_nurg <- htopicmen_nurg %>% group_by(rank) %>%
  mutate(category="Selection of a Dissertation Topic")


hremen_nurg <-filter(non_urg, hresearchmen=="Very helpful" | hresearchmen =="Somewhat helpful") %>%
  select(hresearchmen)
colnames(hremen_nurg)=c("rank", "category")
hremen_nurg <- hremen_nurg %>% group_by(rank) %>%
  mutate(category="Your Dissertation Research")


hwrmen_nurg <-filter(non_urg, hwritingmen=="Very helpful" | hwritingmen =="Somewhat helpful") %>%
  select(hwritingmen)
colnames(hwrmen_nurg)=c("rank", "category")
hwrmen_nurg <- hwrmen_nurg %>% group_by(rank) %>%
  mutate(category="Writing and Revising your Dissertation")


hacamen_nurg <-filter(non_urg, hacadmen=="Very helpful" | hacadmen =="Somewhat helpful") %>%
  select(hacadmen)
colnames(hacamen_nurg)=c("rank", "category")
hacamen_nurg <- hacamen_nurg %>% group_by(rank) %>%
  mutate(category="Academic Career Options")


hnomen_nurg <-filter(non_urg, hnonacadmen=="Very helpful" | hnonacadmen =="Somewhat helpful") %>%
  select(hnonacadmen)
colnames(hnomen_nurg)=c("rank", "category")
hnomen_nurg <- hnomen_nurg %>% group_by(rank) %>%
  mutate(category="Nonacademic Career Options")


hemmen_nurg <-filter(non_urg, hemploymen=="Very helpful" | hemploymen =="Somewhat helpful") %>%
  select(hemploymen)
colnames(hemmen_nurg)=c("rank", "category")
hemmen_nurg <- hemmen_nurg %>% group_by(rank) %>%
  mutate(category="Search for Employement or Training")

non_urg_men<-rbind(htopicmen_nurg, hremen_nurg, hwrmen_nurg, hacamen_nurg, hnomen_nurg, hemmen_nurg)


urg_data_mentor$group <-"URG"
non_urg_men$group <-'NON-URG'
all <- rbind(urg_data_mentor, non_urg_men)
all
#library(mosaic)
#xchisq.test(category~group, data = all, correct = FALSE)
topic <-all[all$category == 'Selection of a Dissertation Topic',]
#topic
t <- table(topic$group, topic$rank)
chisq.test(t) # p value > 0.05 

topic <-all[all$category == 'Your Dissertation Research',]
#topic
t <- table(topic$group, topic$rank)
chisq.test(t) 


topic <-all[all$category == 'Writing and Revising your Dissertation',]
#topic
t <- table(topic$group, topic$rank)
chisq.test(t) 


topic <-all[all$category == 'Academic Career Options',]
#topic
t <- table(topic$group, topic$rank)
chisq.test(t) 

topic <-all[all$category == 'Nonacademic Career Options',]
#topic
t <- table(topic$group, topic$rank)
chisq.test(t) 


topic <-all[all$category == 'Search for Employement or Training',]
#topic
t <- table(topic$group, topic$rank)
chisq.test(t) 

#all p values are >0.05 meaning no difference.












#subset: #chi square 
#rank somewhat helpful
some<-all[all$rank =='Somewhat helpful',]
c1 <- some[some$category == 'Selection of a Dissertation Topic',]
c2 <- c1[,c(2,4)]
table(c2$group, c2$freq)
chisq.test(c2$group, c2$freq,correct = FALSE)


#pairwise t-test
#from very helpful: compare URG and NON-URG group of each category: 
#very helpful:

d<-urg %>% 
  mutate_at(vars(37:42),
            ~as.numeric(recode(.,
                               "Not at all helpful"=1,
                               "Not very helpful"=2,
                               "Somewhat helpful"=3,
                               "Very helpful"=4,
                               "N/A - I did not receive advice on this"=0)))








