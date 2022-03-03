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

df <- file
urg <- filter(file, Underrepresented=="Underrepresented Group")
non_urg <- filter(file, Underrepresented=="Non-Underrespresented Group")

#seelct some columns i interested in and convert their values to score:
#column q39, q41, q43
levels<-as.factor(df$htopicadv)
levels <-factor(levels,c("Not at all helpful","Not very helpful","Somewhat helpful","Very helpful","N/A - I did not receive advice on this"))
df$ht_num <-as.numeric(levels)

levels<-as.factor(df$hresearchadv)
levels <-factor(levels,c("Not at all helpful","Not very helpful","Somewhat helpful","Very helpful","N/A - I did not receive advice on this"))
df$hre_num <-as.numeric(levels)

levels<-as.factor(df$hwritingadv)
levels <-factor(levels,c("Not at all helpful","Not very helpful","Somewhat helpful","Very helpful","N/A - I did not receive advice on this"))
df$hr_num <-as.numeric(levels)

levels<-as.factor(df$hacadadv)
levels <-factor(levels,c("Not at all helpful","Not very helpful","Somewhat helpful","Very helpful","N/A - I did not receive advice on this"))
df$haca_num <-as.numeric(levels)

levels<-as.factor(df$hnonacadadv)
levels <-factor(levels,c("Not at all helpful","Not very helpful","Somewhat helpful","Very helpful","N/A - I did not receive advice on this"))
df$hn_num <-as.numeric(levels)

levels<-as.factor(df$hemployadv)
levels <-factor(levels,c("Not at all helpful","Not very helpful","Somewhat helpful","Very helpful","N/A - I did not receive advice on this"))
df$he_num <-as.numeric(levels)

levels<-as.factor(df$satoverall)
levels <-factor(levels,c("Poor","Fair","Good","Very good","Excellent"))
df$sa_num <-as.numeric(levels)

#impute each column i intereseted with average
df$ht_num[is.na(df$ht_num)] <- mean(df$ht_num, na.rm = T)
df$hre_num[is.na(df$hre_num)] <- mean(df$hre_num, na.rm = T)
df$hr_num[is.na(df$hr_num)] <- mean(df$hr_num, na.rm = T)
df$haca_num[is.na(df$haca_num)] <- mean(df$haca_num, na.rm = T)
df$hn_num[is.na(df$hn_num)] <- mean(df$hn_num, na.rm = T)
df$he_num[is.na(df$he_num)] <- mean(df$he_num, na.rm = T)
df$sa_num[is.na(df$sa_num)] <- mean(df$sa_num, na.rm = T)



df %>%
  group_by(Race_RECODE) %>%
  summarise(
    count_race = n(),
    mean_topic = mean(ht_num, na.rm = TRUE),
    sd_topic = sd(ht_num, na.rm = TRUE)
  )




ggplot(df, aes(x = Race_RECODE, y = ht_num, fill = Race_RECODE)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  ggtitle("ANOVA on multiple race group") +
  xlab("Race/Ethnicity") +
  ylab("dissertation topic helpful score")+
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(label = '', position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme_classic()

anova_one_way <- aov(ht_num~Race_RECODE, data = df)
summary(anova_one_way)
#since p value >0.05, there are no signifinat difference btw those group on dissertation topic helpful score.
ggsave("/Users/cleopathy/Desktop/anova_race_topicadv.png", width = 10, height = 8,bg = 'White')



df %>%
  group_by(Underrepresented) %>%
  summarise(
    count_group = n(),
    mean_topic = mean(ht_num, na.rm = TRUE),
    sd_topic = sd(ht_num, na.rm = TRUE)
  )



ggplot(df, aes(x = Underrepresented, y = ht_num, fill = Underrepresented)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  ggtitle("ANOVA between URG and NON URG group") +
  xlab("Group(URG or NON URG)") +
  ylab("dissertation topic helpful score")+
  #scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_text(label = '', position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  theme_classic()

anova_one_way <- aov(ht_num~Underrepresented, data = df)
summary(anova_one_way)

#there is no significant difference btw urg and non urg group on dissertation topic helpful score .
ggsave("/Users/cleopathy/Desktop/urg-anova.png", width = 10, height = 8,bg = 'White')












