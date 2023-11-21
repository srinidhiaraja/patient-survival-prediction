
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

#---------------------------------------------------------------------------------------------------------------------------------
library(dplyr)


aids =data.frame( hos %>% group_by(aids)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(aids)

cirr = data.frame( hos %>% group_by(cirrhosis)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(cirr)



diab = data.frame( hos %>% group_by(diabetes_mellitus)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(diab)

hepa = data.frame( hos %>% group_by(hepatic_failure)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(hepa)

imm = data.frame( hos %>% group_by(immunosuppression)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(imm)

leku = data.frame( hos %>% group_by(leukemia)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(leku)

lym = data.frame(hos %>% group_by(lymphoma)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(lym)

tum = data.frame(hos %>% group_by(solid_tumor_with_metastasis)  %>%
  summarise(hospital_death = sum(hospital_death),      
            .groups = 'drop') )

print(tum)

aids
cirr
diab
hepa
imm
leku
lym
tum

png(filename = "BBar_Prj.png")

barplot(t(as.matrix(cirr)),
        main = "Survival of Each Class",
        xlab = "Class",
        col = c("red","green"),
        beside=TRUE
)


