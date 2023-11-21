
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

#------------------------------------------------------------------------------------------

a<- sum(hos$aids)
b<- sum(hos$cirrhosis)
c<- sum(hos$diabetes_mellitus)
d<- sum(hos$hepatic_failure)
e<- sum(hos$immunosuppression)
f<- sum(hos$leukemia)
g<- sum(hos$lymphoma)
h<- sum(hos$solid_tumor_with_metastasis)

a
b
c
d
e
f
g
h

y<- c("Aids","Cirrhosis","Diabetes","Hepatic Failure", "Immunosuppression",'Leukemia',"Lymphoma","Tumor" )
cc <- c("yellow","pink","magenta","green","lightblue", "orange","grey","purple")

x<- c(a,b,c,d,e,f,g,h)
png(filename ="hos_pie1.png")
pie(x,labels=y,col=cc)

legend("topright", y,
       cex = 0.69, fill = cc)

dev.off()
