

#dataset one datacleaning----------------------
# Import the Cleveland dataset-----------------------------------------------------------------------------
colNames <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
df_clev <- read.csv('processed.cleveland.data.csv',col.names = colNames)

#--------checking------------
# Have a look at the first six row
head(df_clev)

#summary information
summary(df_clev)

#structure info
str(df_clev)

#--------------cleaning--------------------------------

# Have a look at the frequency table for each categorical variable
table(df_clev $ ca)
table(df_clev $ thal)

# Assign "?" to NA
df_clev[df_clev == "?"] <- NA

#percentage of missing row
6 / nrow(df_clev)

# Filter the missing rows
df_clev <- df_clev[complete.cases(df_clev),]



# Convert variables classes
df_clev $ sex <- factor(df_clev$sex, labels=c('Female','Male'))
df_clev $ cp <- factor(df_clev $cp, labels=c('typical angina','atypical angina','non-anginal pain','asymptomatic'))
df_clev $ trestbps <- as.integer(df_clev $trestbps)
df_clev $ chol <- as.integer(df_clev $chol)
df_clev $ fbs <-factor(df_clev$fbs,labels=c('<= 120','> 120'))
df_clev $ restecg <- factor(df_clev$restecg, labels=c('normal','ST-T wave abnormality','LVH'))
df_clev $ thalach <- as.integer(df_clev $thalach)
df_clev $ exang <- factor(df_clev $exang, labels=c('No','Yes'))
df_clev $ oldpeak <- as.numeric(df_clev $oldpeak)
df_clev $ slope <- factor(df_clev $slope, labels=c('upsloping','flat','downsloping'))
df_clev $ ca <- factor(df_clev$ca, labels = c("0 major vessels","1 major vessels", "2 major vessels", "3 major vessels"))
df_clev $ thal <- factor(df_clev$thal, labels=c('normal','fixed defect','reversable defect'))
df_clev $ num <- ifelse(df_clev $ num == 0, 0, 1 )
df_clev $ num <- factor(df_clev$num, labels=c('healthy','heart disease'))


#save into cleaned dataset
data.table::fwrite(df_clev, file = "df_clev_clean.csv")


#-----------------------------dataset2 datacleaning
#--------------------------import data, modify data----------------------------
df_country<-read.csv('sdhd.csv')

# Frequency tables for column Entity and Code
freq_table_Entity <- table(df_country $Entity)
freq_table_Code <- table(df_country $Code)
# To see whetehr there is any subcategory whose counts don't equal 28
freq_table_Entity[freq_table_Entity != 28]

freq_table_Code[freq_table_Code != 28]

# Load the library dplyr
library(dplyr)
# Filter the rows whose Entity and Code don't equal to  " "
df_country <- df_country %>%
  filter(Entity != "" & Code != "")

df_country $Entity <- as.factor(df_country $Entity)
df_country $Code <- as.factor(df_country $Code)

colSums(is.na(df_country))

#save into cleaned dataset 2
data.table::fwrite(df_country, file = "df_country.csv")

library(dplyr)
# Mean death percent of each country
df_country_mean_death_rate <- df_country %>%
  group_by(Entity) %>%
  summarise(mean_death_rate = mean(Death_Percent)) %>%
  arrange(desc(mean_death_rate)) %>%
  ungroup()

df_country_mean_death_rate[c(1, nrow(df_country_mean_death_rate)), ]

summary(df_clev)
