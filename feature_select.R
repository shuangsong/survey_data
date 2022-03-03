rm(list = ls()) # clear the environment
library(dplyr)
library(faux)
library(DataExplorer)
library(caret)
library(randomForest)
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

na_count_c <- sapply(file, function(y) sum(length(which(is.na(y)))))
na <- data.frame(na_count_c)
df <- cbind(col_name = rownames(na), na)
rownames(df) <- 1:nrow(na)
df$na_percent <- df$na_count_c/nrow(file)
df <- filter(df, na_percent<0.5) #we select the col names has less than 50% NAs 
df



new <- data.frame(matrix(ncol = 100, nrow = 400))
f<-df$col_name
colnames(new) <- f
#new

#data with less NA in column : 

ml_df <- file[intersect(names(new), names(file))]
head(ml_df)
ncol(ml_df)
#write.csv(ml_df, "/Users/cleopathy/Desktop/ml_data.csv")
#subset columns 
mydata2 = select(ml_df, -c('ResponseId', 'Q6','Q12','Q60','Q61','Q62','Q63','Q95','Q97','Q92_2','Q103','Race_RECODE','Race_Ethnicity','Q104','Q80','Q86','Q88','Q84'))

mydata2

#write.csv(mydata2, "/Users/cleopathy/Desktop/ready_to_recode.csv")

d1<-mydata2 %>% 
  mutate_at(vars(3:5),
            ~as.numeric(recode(.,
                               "Poor"=1, 
                               "Fair"=2,
                               "Good"=3,
                               "Very good"=4,
                               "Excellent"=5)))
d2 <-d1 %>% 
  mutate_at(vars(13:19),
            ~as.numeric(recode(.,
                               "Poor"=1, 
                               "Fair"=2,
                               "Good"=3,
                               "Very good"=4,
                               "Excellent"=5)))

 d19<-d18%>% 
  mutate_at(vars(24:28),
            ~as.numeric(recode(.,
                               "Poor"=1, 
                               "Fair"=2,
                               "Good"=3,
                               "Very good"=4,
                               "Excellent"=5)))

d3<-d2 %>% 
  mutate_at(vars(61:67),
            ~as.numeric(recode(.,
                               "Poor"=1, 
                               "Fair"=2,
                               "Good"=3,
                               "Very good"=4,
                               "Excellent"=5)))


d4<-d3 %>% 
  mutate_at(vars(6:9),
            ~as.numeric(recode(.,
                               "Strongly Disagree"=1,
                               "Disagree"=2,
                               "Ambivalent"=3,
                               "Agree"=4,
                               "Strongly Agree"=5)))

d5<-d4 %>% 
  mutate_at(vars(68:71),
            ~as.numeric(recode(.,
                               "Strongly Disagree"=1,
                               "Disagree"=2,
                               "Ambivalent"=3,
                               "Agree"=4,
                               "Strongly Agree"=5)))

#helpful : 
d6<-d5 %>% 
  mutate_at(vars(29:34),
            ~as.numeric(recode(.,
                               "Not at all helpful"=1,
                               "Not very helpful"=2,
                               "Somewhat helpful"=3,
                               "Very helpful"=4,
                               "N/A - I did not receive advice on this"=0)))
d7<-d6 %>% 
  mutate_at(vars(37:42),
            ~as.numeric(recode(.,
                               "Not at all helpful"=1,
                               "Not very helpful"=2,
                               "Somewhat helpful"=3,
                               "Very helpful"=4,
                               "N/A - I did not receive advice on this"=0)))
d8<-d7%>% 
  mutate_at(vars(44),
            ~as.numeric(recode(.,
                               "Not at all helpful"=1,
                               "Not very helpful"=2,
                               "Somewhat helpful"=3,
                               "Very helpful"=4,
                               "N/A - I did not receive advice on this"=0)))

d9<-d8 %>% 
  mutate_at(vars(47),
            ~as.numeric(recode(.,
                               "Not at all helpful"=1,
                               "Not very helpful"=2,
                               "Somewhat helpful"=3,
                               "Very helpful"=4,
                               "N/A - I did not receive advice on this"=0)))




d10<-d9 %>% 
  mutate_at(vars(35:36),
            ~as.numeric(recode(.,
                               "Yes"=1,
                               "No"=0)))

d11<-d10 %>% 
  mutate_at(vars(43),
            ~as.numeric(recode(.,
                               "Yes"=1,
                               "No"=0)))

d12<-d11 %>% 
  mutate_at(vars(45:46),
            ~as.numeric(recode(.,
                               "Yes"=1,
                               "No"=0)))
d13<-d12 %>% 
  mutate_at(vars(57),
            ~as.numeric(recode(.,
                               "Yes"=1,
                               "No"=0)))
d14<-d13 %>% 
  mutate_at(vars(22:23),
            ~as.numeric(recode(.,
                               "Yes"=1,
                               "No"=0)))


d15<-d14 %>% 
  mutate_at(vars(10:12),
            ~as.numeric(recode(.,
                               "Definitely not"=1,
                               "Probably not"=2,
                               "Maybe"=3,
                               "Probably"=4,
                               "Definitely"=5)))

d16<-d15 %>% 
  mutate_at(vars(58:60),
            ~as.numeric(recode(.,
                               "Definitely not"=1,
                               "Probably not"=2,
                               "Maybe"=3,
                               "Probably"=4,
                               "Definitely"=5)))
d17<-d16 %>% 
  mutate_at(vars(72:77),
            ~as.numeric(recode(.,
                               "Not an obstacle"=1,
                               "A minor obstacle"=2,
                               "A major obstacle"=3,
                               "Not applicable"=0
                               )))
d18<-d17 %>% 
  mutate_at(vars(1),
            ~as.numeric(recode(.,
                               "College C"=1,
                               "College H"=2,
                               "College B"=3,
                               "College G"=4,
                               "College I"=5,
                               "College F"=6
            )))


d20<-d19 %>% 
  mutate_at(vars(2),
            ~as.numeric(recode(.,
                               "STEM"=1,
                               "Non-STEM"=0)))
d21<-d20 %>% 
  mutate_at(vars(82),
            ~as.numeric(recode(.,
                               "Non-Underrespresented Group"=1,
                               "Underrepresented Group"=0)))






d22<-d21 %>% 
  mutate_at(vars(20),
            ~as.numeric(recode(.,
                               "Yes, and I attended"=1,
                               "No"=0,
                               "I don't remember" = 2)))

d23<-d22%>% 
  mutate_at(vars(21),
            ~as.numeric(recode(.,
                               "Neither Effective nor Ineffective"=0,
                               "Somewhat Effective"=1,
                               "Very Effective" = 2)))

d24<-d23 %>% 
  mutate_at(vars(50),
            ~as.numeric(recode(.,
                               "Yes"=1,
                               "No"=0,
                               "Not applicable"=2)))

d25<-d24 %>% 
  mutate_at(vars(53),
            ~as.numeric(recode(.,
                               "Have signed contract or made definite commitment for a 'postdoc' or other work"=1,
                               "Negotiating with one or more specific organizations"=2,
                               "Returning to, or continuing in, predoctoral employment"=3,
                               "Seeking position but have no specific prospects"=4,
                               "Other - Specify:"=5
                               )))

d26<-d25 %>% 
  mutate_at(vars(54),
            ~as.numeric(recode(.,
                               "Research, Consulting, and Legal Services"=1,
                               "Education"=2,
                               "Non-profit and membership organizations"=3,
                               "Other"=4,
                               "Public Administration (Government)"=5,
                               "Museums, Arts, Entertainment, and Recreation"=6,
                               "Technology"=7,
                               "Publishing, Broadcasting, and Library"=8,
                               "Manufacturing"=9
            )))





d27<-d26 %>% 
  mutate_at(vars(56),
            ~as.numeric(recode(.,
                               "Education, Training, and Library Occupations"=1,
                               "Architecture and Engineering Occupations"=2,
                               "Other Occupations"=3,
                               "Scientists, Social Scientists"=4,
                               "Management Occupations"=5,
                               "Computer and Mathematical Occupations"=6,
                               "Finance Professional"=7,
                               "Healthcare Practitioners"=8,
                               "Arts, Design, Entertainment, Sports, and Media Occupations"=9
            )))

d28<-select(d27,-c('Q90_2','Q92_1','Q22','finance','infotech','hresearchmen','hacadmen','hwritingmen',
                   'hnonacadmen','Q43','Q78','htopicmen','hemploymen','space','lab','library',
                   'Q33'))


na_count_c <- sapply(d28, function(y) sum(length(which(is.na(y)))))
na <- data.frame(na_count_c)
colnames(na) <-'na_number'








#write.csv(d28,'/Users/cleopathy/Desktop/recode.csv')

#load in cleaned data to do feature selection:
#install.packages('naniar')
library(naniar)
ready <-read.csv('/Users/cleopathy/Desktop/recode_rmword.csv')
#na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")
#ready <- ready %>% replace_with_na_all(condition = ~.x %in% na_strings)
ready$Q45 <- as.numeric(ready$Q45)

#imputation 
for (i in 1:ncol(ready)) {
  ready[ , i][is.na(ready[ , i])] <- mean(ready[ , i], na.rm=TRUE)
}

# Features
x <- ready %>%
  select(-X, -satoverall) %>%
  as.data.frame()

# Target variable
y <- ready$satoverall

# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

# Define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(1:64),
                   rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)

# Post prediction
postResample(predict(result_rfe1, x_test), y_test)


varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:6],
                          importance = varImp(result_rfe1)[1:6, 1])

ggplot(data = varimp_data, 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
  theme_bw() + theme(legend.position = "none") + scale_fill_brewer(palette = "Dark2")

ggsave("/Users/cleopathy/Desktop/feature_select.png", width = 10, height = 8,bg = 'White')



feature_data<-select(ready, satlife, satacad, recommend93, progqual100, progqual22, sameuniv13)
corrplot(cor(feature_data), method = "color", addCoef.col="grey", order = "AOE")
