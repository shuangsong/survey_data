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
#head(urg)
#head(non_urg)

#write_xlsx(urg,'/Users/cleopathy/Desktop/urg.xlsx')
#write.csv(non_urg,'/Users/cleopathy/Desktop/non_urg.csv')
unique(file$htopicadv)

#explore faculty mentoring
htopicadv_urg <-filter(urg, htopicadv=="Very helpful" | htopicadv =="Somewhat helpful") %>%
  select(htopicadv)
colnames(htopicadv_urg)=c("rank", "category")
htopicadv_urg <- htopicadv_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Selection of a Dissertation Topic")


hreadv_urg <-filter(urg, hresearchadv=="Very helpful" | hresearchadv =="Somewhat helpful") %>%
  select(hresearchadv)
colnames(hreadv_urg)=c("rank", "category")
hreadv_urg <- hreadv_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Your Dissertation Research")


hwradv_urg <-filter(urg, hwritingadv=="Very helpful" | hwritingadv =="Somewhat helpful") %>%
  select(hwritingadv)
colnames(hwradv_urg)=c("rank", "category")
hwradv_urg <- hwradv_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Writing and Revising your Dissertation")


hacaadv_urg <-filter(urg, hacadadv=="Very helpful" | hacadadv =="Somewhat helpful") %>%
  select(hacadadv)
colnames(hacaadv_urg)=c("rank", "category")
hacaadv_urg <- hacaadv_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Academic Career Options")


hnoadv_urg <-filter(urg, hnonacadadv=="Very helpful" | hnonacadadv =="Somewhat helpful") %>%
  select(hnonacadadv)
colnames(hnoadv_urg)=c("rank", "category")
hnoadv_urg <- hnoadv_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Nonacademic Career Options")


hemadv_urg <-filter(urg, hemployadv=="Very helpful" | hemployadv=="Somewhat helpful") %>%
  select(hemployadv)
colnames(hemadv_urg)=c("rank", "category")
hemadv_urg <- hemadv_urg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Search for Employement or Training")

dissertation_help_area<-rbind(htopicadv_urg, hreadv_urg, hwradv_urg, hacaadv_urg, hnoadv_urg, hemadv_urg)


ggplot(dissertation_help_area, aes(x=category, y=freq, fill=rank, label=scales::percent(freq))) +  
  geom_bar(stat="identity", color="black", position=position_dodge())+
  ylab('Percent') +
  ggtitle("How helpful was the advice you received in these area from your dissertation advisor-URG student") +
  scale_y_continuous(labels=percent) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme_minimal()
ggsave("/Users/cleopathy/Desktop/adv_urg.png", width = 15, height = 6,bg = 'White')
# non-urg faculty interactions:

#explore faculty mentoring
htopicadv_nurg <-filter(non_urg, htopicadv=="Very helpful" | htopicadv =="Somewhat helpful") %>%
  select(htopicadv)
colnames(htopicadv_nurg)=c("rank", "category")
htopicadv_nurg <- htopicadv_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Selection of a Dissertation Topic")


hreadv_nurg <-filter(non_urg, hresearchadv=="Very helpful" | hresearchadv =="Somewhat helpful") %>%
  select(hresearchadv)
colnames(hreadv_nurg)=c("rank", "category")
hreadv_nurg <- hreadv_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Your Dissertation Research")


hwradv_nurg <-filter(non_urg, hwritingadv=="Very helpful" | hwritingadv =="Somewhat helpful") %>%
  select(hwritingadv)
colnames(hwradv_nurg)=c("rank", "category")
hwradv_nurg <- hwradv_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Writing and Revising your Dissertation")


hacaadv_nurg <-filter(non_urg, hacadadv=="Very helpful" | hacadadv =="Somewhat helpful") %>%
  select(hacadadv)
colnames(hacaadv_nurg)=c("rank", "category")
hacaadv_nurg <- hacaadv_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Academic Career Options")


hnoadv_nurg <-filter(non_urg, hnonacadadv=="Very helpful" | hnonacadadv =="Somewhat helpful") %>%
  select(hnonacadadv)
colnames(hnoadv_nurg)=c("rank", "category")
hnoadv_nurg <- hnoadv_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Nonacademic Career Options")


hemadv_nurg <-filter(non_urg, hemployadv=="Very helpful" | hemployadv =="Somewhat helpful") %>%
  select(hemployadv)
colnames(hemadv_nurg)=c("rank", "category")
hemadv_nurg <- hemadv_nurg %>% group_by(rank) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  select(rank, freq) %>%
  mutate(category="Search for Employement or Training")

adv_data<-rbind(htopicadv_nurg, hreadv_nurg, hwradv_nurg, hacaadv_nurg, hnoadv_nurg, hemadv_nurg)


ggplot(adv_data, aes(x=category, y=freq, fill=rank, label=scales::percent(freq))) +  
  geom_bar(stat="identity", color="black", position=position_dodge())+
  ylab('Percent') +
  ggtitle("How helpful was the advice you received in these area from your dissertation advisor-NON URG student") +
  scale_y_continuous(labels=percent) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme_minimal()

ggsave("/Users/cleopathy/Desktop/adv_nurg.png", width = 15, height = 6,bg = 'White')