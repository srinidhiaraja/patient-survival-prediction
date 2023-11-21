library(party)
library(ggplot2)
library(visdat)
hos <- read.csv("dataset - Copy.csv")
df <- hos[c(1:1000),]
colnames(df)

df_clean = df

#removing redundant columns
df_clean$patient_id <- df_clean$hospital_id<- df_clean$icu_id <- df_clean$encounter_id <- df_clean$apache_4a_hospital_death_prob <- df_clean$apache_4a_icu_death_prob <- NULL

#Exploratory Data Analysis
#Identify Missing and NA Values in each columns
colSums(is.na(df_clean))
colSums(df_clean == "")

#visualize missing values
vis_miss(df, warn_large_data = FALSE)
#gg_var_miss(df_clean) + labs(y = "Missing Values")


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

colnames(hos)
#-----------------------------------------------------------------------------------------------------------------


# Give the chart file a name.
png(file = "decision_tree3.png")

# Create the tree.
output.tree <- ctree(
  hospital_death ~ . , 
  data = hos)

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()

