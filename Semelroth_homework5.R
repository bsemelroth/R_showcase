rm(list=ls())
#11/12/21 Brody Semelroth

#Excercise 1

food <- read.csv("food.txt", na.strings = c("", "n/a"), stringsAsFactors = T, sep = "\t")

food$Total <- rowSums(food[ , c("Sushi", "Alligator", "FrogLegs", "Caviar", "Fugu", "Lutefisk")], na.rm = TRUE)


food$Continent <- gsub("Europe|Oceania|South America|Asia|Africa","Other",food$Continent)

#Excercise 2

caviar <- prop.table(table(food$Caviar,food$Continent),margin=2)

alligator_test <- chisq.test(food$Alligator,food$Gender)
#The p‐value of the Chi‐squared is .1669. Thus, we cannot reject the null hypothesis 
#that having eaten alligator and gender are independent.  

#Excercise 3

age_total <- cor.test(food$Age,food$Total)
age_total
#The Pearson correlation coefficient is 0.2527558 . Thus, these variables have a strong, positive linear relationship. 

#Excercsie 4
library(RSocrata)
url <- "https://noaa-fisheries-afsc.data.socrata.com/resource/vsba-nbxa.json?$SELECT=date,habitat,treatment,carapace_width WHERE experiment = '2011'"

crab <- read.socrata(url)
crab$carapace_width <- as.numeric(crab$carapace_width)
str(crab)

#Excercise 5
library(dplyr)

treatmentgroup <- group_by(crab, treatment)
treatment_summary <- summarise(treatmentgroup,
                               total_crabs=n(),
                               median_size=median(carapace_width))

treatment_test <- t.test(carapace_width~treatment, data = treatmentgroup)
treatment_test

#The p‐value of the t‐test is 7.93e-08. Thus, we can reject the null hypothesis 
#that average carapace width is equal for the treatment conditions. 

hist(crab$carapace_width)


