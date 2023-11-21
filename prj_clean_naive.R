#install.packages("visdat")
library("visdat")
hos <- read.csv("dataset - Copy.csv")
df <- hos[c(1:1000),]
print(df)

df_clean = df

#removing redundant columns
df_clean$patient_id <- df_clean$hospital_id<- df_clean$icu_id <- df_clean$encounter_id <- df_clean$apache_4a_hospital_death_prob <- df_clean$apache_4a_icu_death_prob <- NULL

#Exploratory Data Analysis
#Identify Missing and NA Values in each columns
colSums(is.na(df_clean))
colSums(df_clean == "")

#visualise missing values
vis_miss(df, warn_large_data = FALSE)
#gg_miss_var(df_clean) + labs(y = "Missing Values")


#Replace the blank values into NA values
df_clean[df_clean == ""] <- NA

#Remove all NA values
df_clean <- na.omit(df_clean)

#Check for NA and blank Values after removing
colSums(is.na(df_clean))
colSums(df_clean == "")

#Check for duplicates
table(duplicated(df_clean))

summary(df_clean)
str(df_clean)
df_clean
hos <- data.frame(df_clean)
hos
#-------------------------------------------------------------------------------------------


hos$hospital_death [hos$hospital_death==0] <- "D"
hos$hospital_death [hos$hospital_death==1] <- "A"
hos
#naive bayessssss
library(caTools)
library(caret)
library(e1071)

hos = read.csv("dataset - Copy.csv")
split <- sample.split(hos, SplitRatio = 0.7)
traind<- subset(hos, split==TRUE)
testd<- subset(hos, split==FALSE)



set.seed(120)

classcl <- naiveBayes(hospital_death~., data=traind)
classcl

yp <- predict(classcl, newdata = testd)

cm <- table (testd$hospital_death, yp)

cm
confusionMatrix(cm)

hos
