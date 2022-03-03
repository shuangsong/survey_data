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

non_urg_men<-rbind(htopicmen_nurg, hremen_nurg, hwrmen_nurg, hacamen_nurg, hnomen_nurg, hemmen_nurg)


dissertation_help_area$group <-"URG"
non_urg_men$group <-'NON-URG'
all <- rbind(dissertation_help_area, non_urg_men)
all


#subset: 
#rank somewhat helpful
some<-all[all$rank =='Somewhat helpful',]
c1 <- some[some$category == 'Selection of a Dissertation Topic',]
c2 <- c1[,c(2,4)]
table(c2$group, c2$freq)
chisq.test(c2$group, c2$freq,correct = FALSE)
#not signifinat different 
#same 
some<-all[all$rank =='Somewhat helpful',]
c1 <- some[some$category == 'Academic Career Options',]
c2 <- c1[,c(2,4)]
table(c2$group, c2$freq)
chisq.test(c2$group, c2$freq,correct = FALSE)



PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/poisons.csv"
df <- read.csv(PATH) %>%
  select(-X) %>% 
  mutate(poison = factor(poison, ordered = TRUE))
glimpse(df)

df %>%
  group_by(poison) %>%
  summarise(
    count_poison = n(),
    mean_time = mean(time, na.rm = TRUE),
    sd_time = sd(time, na.rm = TRUE)
  )

ggplot(df, aes(x = poison, y = time, fill = poison)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_classic()

anova_one_way <- aov(time~poison, data = df)
summary(anova_one_way)






