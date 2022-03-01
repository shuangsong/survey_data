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
#str(file)

rmNArows<-function(d){
  goodRows<-apply(d,1,function(x) sum(is.na(x))!=ncol(d)) # keep rows that is not empty
  d[goodRows,]
}
file <- rmNArows(file)
file <- file[!file$Q12=="I do not wish to complete the XX University Doctoral Exit survey.",]
nrow(file)
#urg dataset
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
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Selection of a Dissertation Topic")


hremen_urg <-filter(urg, hresearchmen=="Very helpful" | hresearchmen =="Somewhat helpful") %>%
  select(hresearchmen)
colnames(hremen_urg)=c("rank", "category")
hremen_urg <- hremen_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Your Dissertation Research")


hwrmen_urg <-filter(urg, hwritingmen=="Very helpful" | hwritingmen =="Somewhat helpful") %>%
  select(hwritingmen)
colnames(hwrmen_urg)=c("rank", "category")
hwrmen_urg <- hwrmen_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Writing and Revising your Dissertation")


hacamen_urg <-filter(urg, hacadmen=="Very helpful" | hacadmen =="Somewhat helpful") %>%
  select(hacadmen)
colnames(hacamen_urg)=c("rank", "category")
hacamen_urg <- hacamen_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Academic Career Options")


hnomen_urg <-filter(urg, hnonacadmen=="Very helpful" | hnonacadmen =="Somewhat helpful") %>%
  select(hnonacadmen)
colnames(hnomen_urg)=c("rank", "category")
hnomen_urg <- hnomen_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Nonacademic Career Options")


hemmen_urg <-filter(urg, hemploymen=="Very helpful" | hemploymen =="Somewhat helpful") %>%
  select(hemploymen)
colnames(hemmen_urg)=c("rank", "category")
hemmen_urg <- hemmen_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Search for Employement or Training")

dissertation_help_area<-rbind(htopicmen_urg, hremen_urg, hwrmen_urg, hacamen_urg, hnomen_urg, hemmen_urg)


ggplot(dissertation_help_area, aes(x=category, y=freq, fill=rank, label=scales::percent(freq))) +  
  geom_bar(stat="identity", color="black", position=position_dodge())+
  ylab('Percent') +
  ggtitle("How helpful was the advice you received from your mentor in each of these areas-URG student")+
  scale_y_continuous(labels=percent) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme_minimal()


ggsave("/Users/cleopathy/Desktop/mentor_urg.png", width = 15, height = 6,bg = 'White')

# non-urg faculty interactions:

#explore faculty mentoring
htopicmen_nurg <-filter(non_urg, htopicmen=="Very helpful" | htopicmen =="Somewhat helpful") %>%
  select(htopicmen)
colnames(htopicmen_nurg)=c("rank", "category")
htopicmen_nurg <- htopicmen_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Selection of a Dissertation Topic")


hremen_nurg <-filter(non_urg, hresearchmen=="Very helpful" | hresearchmen =="Somewhat helpful") %>%
  select(hresearchmen)
colnames(hremen_nurg)=c("rank", "category")
hremen_nurg <- hremen_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Your Dissertation Research")


hwrmen_nurg <-filter(non_urg, hwritingmen=="Very helpful" | hwritingmen =="Somewhat helpful") %>%
  select(hwritingmen)
colnames(hwrmen_nurg)=c("rank", "category")
hwrmen_nurg <- hwrmen_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Writing and Revising your Dissertation")


hacamen_nurg <-filter(non_urg, hacadmen=="Very helpful" | hacadmen =="Somewhat helpful") %>%
  select(hacadmen)
colnames(hacamen_nurg)=c("rank", "category")
hacamen_nurg <- hacamen_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Academic Career Options")


hnomen_nurg <-filter(non_urg, hnonacadmen=="Very helpful" | hnonacadmen =="Somewhat helpful") %>%
  select(hnonacadmen)
colnames(hnomen_nurg)=c("rank", "category")
hnomen_nurg <- hnomen_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Nonacademic Career Options")


hemmen_nurg <-filter(non_urg, hemploymen=="Very helpful" | hemploymen =="Somewhat helpful") %>%
  select(hemploymen)
colnames(hemmen_nurg)=c("rank", "category")
hemmen_nurg <- hemmen_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Search for Employement or Training")

dissertation_help_area<-rbind(htopicmen_nurg, hremen_nurg, hwrmen_nurg, hacamen_nurg, hnomen_nurg, hemmen_nurg)


ggplot(dissertation_help_area, aes(x=category, y=freq, fill=rank, label=scales::percent(freq))) +  
  geom_bar(stat="identity", color="black", position=position_dodge())+
  ylab('Percent') +
  ggtitle("How helpful was the advice you received from your mentor in each of these areas-NON URG student")+
  scale_y_continuous(labels=percent) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme_minimal()

ggsave("/Users/cleopathy/Desktop/mentor_nurg.png", width = 15, height = 6,bg = 'White')
