library(party)

library(visdat)
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


#--------------------------------------------------------------------------------------------------------------------------------------------------------



library(party)
library(randomForest)

# Create the forest.
output.forest <- randomForest(hospital_death~., data = hos, importance =TRUE)

# View the forest results.
output.forest 

# Importance of each predictor.
out.importance <- round(importance(output.forest), 2)
print(out.importance )
