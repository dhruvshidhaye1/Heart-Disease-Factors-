
data <- heart_failure_clinical_records_dataset

table(data$platelets)
#Frequency of platelet occurrences within the dataset
table(data$sex)
#Frequency of females and males in dataset (Binary)
table(data$smoking)
#Frequency of patients that are smokers vs non-smokers

explore <- function(x){
  data <- c("Mean"=mean(x, na.rm=TRUE),
            "Median"=median(x, na.rm =T), 
            "Standard Deviation" = sd(x, na.rm =T),
            "Length" = length(x))
  return(data)
}

#Finding mean/median/SD/length

explore(data$age)
explore(data$ejection_fraction)

plot(data$age,data$platelets,ylab="platelets",xlab="age",main="Platelets vs. Age",col="red")
plot(data$age,data$serum_sodium,ylab="sodium levels",xlab="age",main="Sodium levels vs. Age",col="blue")


install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

decisiontree1 <- rpart(smoking~sex+high_blood_pressure+diabetes, data=data, method = "anova")
rpart.plot(decisiontree1)
tree1 <- rpart(smoking~sex+high_blood_pressure+diabetes, data=data)

decisiontree2 <- rpart(smoking~sex+high_blood_pressure+age, data=data, method = "class")
rpart.plot(decisiontree2)
tree2 <- rpart(smoking~sex+high_blood_pressure+age, data=data)

decisiontree3 <- rpart(smoking~sex+high_blood_pressure+diabetes+age, data=data, method = "class")
rpart.plot(decisiontree3)
tree3 <- rpart(smoking~sex+high_blood_pressure+diabetes+age, data=data)

table(data$sex, data$smoking)

hyponatremia <- data.frame(subset(data, serum_sodium <= 135))

decisiontree4 <- rpart(smoking~serum_sodium+age, data=hyponatremia, method = "class")
rpart.plot(decisiontree4)
