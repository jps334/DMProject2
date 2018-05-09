# Import Dataset

setwd('/Users/joaosilva/Downloads/OneDrive_1_02-05-2018')
getwd()
library('readxl')
HistoricalData <- read_excel('HistoricalData.xlsx')

HREvaluation <- read.csv2('HumanResourcesEvaluation.csv')
colnames(HREvaluation)[1] <- 'EmployeeID'

SatisfactionSurvey <- read.table('SatisfactionSurvey.txt', sep='\t', header = TRUE)

ChurnIndicator <- read.csv('ChurnIndicator.csv')
colnames(ChurnIndicator)[1] <- 'EmployeeID'



# Identyfing and Removing Duplicated Observations

SatisfactionSurvey[duplicated(SatisfactionSurvey), 1]

SatisfactionSurvey <- SatisfactionSurvey[!duplicated(SatisfactionSurvey), 1:5]

# Merge Datasets

dataset1 <- merge(HistoricalData, HREvaluation, by = 'EmployeeID')
dataset2 <- merge(SatisfactionSurvey, ChurnIndicator, by = 'EmployeeID')
                 
originaldata<- merge(dataset1, dataset2, by = 'EmployeeID')
rawdata <- originaldata
rm(dataset1,dataset2)
rm(ChurnIndicator, HistoricalData, HREvaluation, SatisfactionSurvey)
# Data Exploration

nrow(rawdata) #

ncol(rawdata)

dim(rawdata) 

sapply(rawdata, class)

#check the structure
str(rawdata$Gender)

library(plyr)
count(rawdata$Gender)
count(rawdata$`Marital Status`)
count(rawdata$Dependents) #boxplot for outliers
count(rawdata$JobType)
count(rawdata$Department)
count(rawdata$JobLevel)
count(rawdata$JobRole)
#Considering incoherency about working in a role from a different department but maybe it serves as a point of connection for synergies
count(rawdata$TypeContract)
count(rawdata$Education)
count(rawdata$EducationArea)
count(rawdata$NumCompaniesWorked) #boxplot for outliers
summary(rawdata$NumCompaniesWorked) #Shouldn't the current company count?
count(rawdata$`SalaryRise(%)`)
summary(rawdata$`SalaryRise(%)`)
count(rawdata$WeekHours)
count(rawdata$TenureWorking) #boxplot for outliers
summary(rawdata$TenureWorking)
count(rawdata$NumberProjectsLastYear) #no projects last year?
count(rawdata$TenureCompany) #boxplot for outliers
summary(rawdata$TenureCompany)
#Check for incoherencies between tenure company and age and decide what to do
count(rawdata$TenureRole) #boxplot for outliers
summary(rawdata$TenureRole)
count(rawdata$LastPromotion) #boxplot for outliers
summary(rawdata$LastPromotion)
count(rawdata$TenureManager) #boxplot for outliers
summary(rawdata$TenureManager)
count(rawdata$MonthlyIncome) #boxplot for outliers
summary(rawdata$MonthlyIncome)
count(rawdata$DistanceHomeOffice) #boxplot for outliers
summary(rawdata$DistanceHomeOffice)
count(rawdata$JobDedication)
count(rawdata$AfterHours)
count(rawdata$JobPerformance)
count(rawdata$FacilitiesSatisfaction)
count(rawdata$RoleSatisfaction)
count(rawdata$HierarchySatisfaction)
count(rawdata$BalanceWork.Life)
count(rawdata$Churn)
count(rawdata$EducationArea)

sapply(rawdata, class) # check the class for all variable
 
str(rawdata) # check the structure

# Check Duplicates

rawdata[duplicated(rawdata),] # rows

rawdata[duplicated(t(rawdata))] # columns



# Data Cleaning

#Converting Dates
library(lubridate)
numerical_dates <- rawdata[- grep("-", rawdata$BirthDate),]
numerical_dates$BirthDate <- as.Date(as.numeric(numerical_dates$BirthDate), origin = "1899-12-30")               
regular_dates <- rawdata[grep("-", rawdata$BirthDate),]
regular_dates$BirthDate <- dmy(regular_dates$BirthDate)
merged_data <- rbind(numerical_dates,regular_dates)
rawdata <- merged_data[order(merged_data$EmployeeID),]


rm(merged_data, numerical_dates, regular_dates)



#substitute nulls

colSums(is.na(rawdata))
rawdata$BirthDate[is.na(rawdata$BirthDate)] <- '1963-02-28'


library(ggplot2)

g <- ggplot(rawdata, aes(rawdata$`Marital Status`, rawdata$Dependents))
g + geom_boxplot()

rm(g)

normal <- rawdata[!is.na(rawdata$`Marital Status`),]
empty <- rawdata[is.na(rawdata$`Marital Status`),]
empty$`Marital Status` <- ifelse(as.numeric(empty$Dependents) > 0, "Married", "Single")
merged_data2 <- rbind(normal,empty)
rawdata <- merged_data2[order(merged_data2$EmployeeID),]


rm(normal, empty, merged_data2)



#remove irrelevant variables
rawdata <- subset(rawdata, select = -c(TypeContract, WeekHours))




#Transformed Variables

calculate_age <- function(birthDate, currentDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(interval(birthDate, currentDate),
                      unit = "year")
  
  period$year
  
}

rawdata$Age <- calculate_age(rawdata$BirthDate)


rawdata$originalsalary = rawdata$MonthlyIncome / (1+(rawdata$`SalaryRise(%)`/100))

rawdata1 <- subset(rawdata, rawdata$BalanceWork.Life == 'Bad')
rawdata1$balancenumber <- 1
rawdata2 <- subset(rawdata, rawdata$BalanceWork.Life == 'Medium')
rawdata2$balancenumber <- 2
rawdata3 <- subset(rawdata, rawdata$BalanceWork.Life == 'Good')
rawdata3$balancenumber <- 3
rawdata4 <- subset(rawdata, rawdata$BalanceWork.Life == 'Great')
rawdata4$balancenumber <- 4
merged_data3 <- rbind(rawdata1,rawdata2,rawdata3,rawdata4)
rawdata <- merged_data3[order(merged_data3$EmployeeID),]


rm(rawdata1, rawdata2, rawdata3, rawdata4, merged_data3)


rawdatas <- subset(rawdata, (rawdata$`Marital Status` == 'Married' | rawdata$`Marital Status` == 'Together'))
rawdatas$New_MaritalStatus <- 'Together'
rawdatatt <- subset(rawdata, (rawdata$`Marital Status` == 'Divorced' | rawdata$`Marital Status` == 'Single'))
rawdatatt$New_MaritalStatus <- rawdatatt$`Marital Status`
merged_data4 <- rbind(rawdatas,rawdatatt)
rawdata <- merged_data4[order(merged_data4$EmployeeID),]


rm(rawdatas, rawdatatt,merged_data4)


rawdatar <- subset(rawdata, (rawdata$JobType == 'Remote' | rawdata$JobType == 'Office/Remote'))
rawdatar$New_JobType <- 'Remote Possible'
rawdatan <- subset(rawdata, rawdata$JobType == 'Office')
rawdatan$New_JobType <- 'Remote Not Possible'
merged_data5 <- rbind(rawdatar,rawdatan)
rawdata <- merged_data5[order(merged_data5$EmployeeID),]

rm(rawdatar, rawdatan,merged_data5)


rawdata5 <- subset(rawdata, rawdata$Dependents > 0)
rawdata5$New_Dependents <- 'With Children'
rawdata6 <- subset(rawdata, rawdata$Dependents == 0)
rawdata6$New_Dependents <- 'Without Children'
merged_data6 <- rbind(rawdata5,rawdata6)
rawdata <- merged_data6[order(merged_data6$EmployeeID),]

rm(rawdata5, rawdata6,merged_data6)


rawdataa <- subset(rawdata, rawdata$Churn == 'Yes')
rawdataa$New_Churn <- 1
rawdataz <- subset(rawdata, rawdata$Churn == 'No')
rawdataz$New_Churn <- 0
merged_data7 <- rbind(rawdataa,rawdataz)
rawdata <- merged_data7[order(merged_data7$EmployeeID),]

rm(rawdataa, rawdataz,merged_data7)


rawdata$AvgSatisfaction <- (rawdata$balancenumber + rawdata$HierarchySatisfaction + rawdata$FacilitiesSatisfaction + rawdata$RoleSatisfaction)/4



rawdata$year_birth <- substr(rawdata$BirthDate, 1,4)
rawdataq <- subset(rawdata, rawdata$year_birth >= 1940  &  rawdata$year_birth < 1964)
rawdataq$Generation <- 'BabyBoomer'
rawdataw <- subset(rawdata, rawdata$year_birth >= 1964  &  rawdata$year_birth < 1985)
rawdataw$Generation <- 'Generation X'
rawdatae <- subset(rawdata, rawdata$year_birth >= 1985  &  rawdata$year_birth < 2000)
rawdatae$Generation <- 'Millenial'
merged_data8 <- rbind(rawdataq,rawdataw,rawdatae)
rawdata <- merged_data8[order(merged_data8$EmployeeID),]

rm(merged_data8, rawdatae, rawdataq, rawdataw)


#Inhoerencies
rawdata$Department <- ifelse(rawdata$Department == 'Information Technologies','IT', rawdata$Department )
rawdata$Department <- ifelse(rawdata$Department == 'HR','Human Resources', rawdata$Department )

rawdata22 <- subset(rawdata, (rawdata$TenureCompany>rawdata$TenureJob))
rawdata33 <- subset(rawdata, (rawdata$LastPromotion>rawdata$TenureJob))
rawdata44 <- subset(rawdata, (rawdata$TenureCompany>rawdata$TenureJob))
rawdata55 <- subset(rawdata, (rawdata$LastPromotion>rawdata$TenureRole))
rawdata66 <- subset(rawdata, (rawdata$TenureManager>rawdata$TenureRole)) # referir no report

rm(rawdata22, rawdata33, rawdata44, rawdata55, rawdata66)

rawdataaaa <- subset(rawdata, (rawdata$Age - rawdata$TenureWorking) < 18)
rawdataaaa$Age <- (rawdataaaa$Age + (18-(rawdataaaa$Age - rawdataaaa$TenureWorking)))
rawdatabbb <- subset(rawdata, (rawdata$Age - rawdata$TenureWorking) >= 18)                     
merged_data12 <- rbind(rawdataaaa,rawdatabbb)
rawdata <- merged_data12[order(merged_data12$EmployeeID),]

rm(merged_data12, rawdataaaa, rawdatabbb)

rawdata$agetest <- rawdata$Age - rawdata$TenureWorking

#Changing categorical variables into Numerical

rawdataq <- subset(rawdata, rawdata$Gender  == 'Male')
rawdataq$New_Gender <- 0
rawdataw <- subset(rawdata, rawdata$Gender  == 'Female')
rawdataw$New_Gender <- 1
merged_data11 <- rbind(rawdataq,rawdataw)
rawdata <- merged_data11[order(merged_data11$EmployeeID),]

rm(merged_data11, rawdataq, rawdataw)


rawdataq <- subset(rawdata, rawdata$AfterHours  == 'Yes')
rawdataq$New_Afterhours <- 0
rawdataw <- subset(rawdata, rawdata$AfterHours  == 'No')
rawdataw$New_Afterhours <- 1
merged_data11 <- rbind(rawdataq,rawdataw)
rawdata <- merged_data11[order(merged_data11$EmployeeID),]

rm(merged_data11, rawdataq, rawdataw)


rawdataq <- subset(rawdata, rawdata$New_JobType  == 'Remote Possible')
rawdataq$NNew_JobType <- 0
rawdataw <- subset(rawdata, rawdata$New_JobType  == 'Remote Not Possible')
rawdataw$NNew_JobType <- 1
merged_data11 <- rbind(rawdataq,rawdataw)
rawdata <- merged_data11[order(merged_data11$EmployeeID),]

rm(merged_data11, rawdataq, rawdataw)


rawdataq <- subset(rawdata, rawdata$New_Dependents  == 'With Children')
rawdataq$NNew_Dependents <- 0
rawdataw <- subset(rawdata, rawdata$New_Dependents  == 'Without Children')
rawdataw$NNew_Dependents <- 1
merged_data11 <- rbind(rawdataq,rawdataw)
rawdata <- merged_data11[order(merged_data11$EmployeeID),]

rm(merged_data11, rawdataq, rawdataw)


rawdata$New_JobRole <- unclass(as.factor(rawdata$JobRole))

rawdata$New_Education <- unclass(as.factor(rawdata$Education))

rawdata$New_EducationArea <- unclass(as.factor(rawdata$Education))

rawdata$New_department  <- unclass(as.factor(rawdata$Department))

rawdata$NNew_MaritalStatus <- unclass(as.factor(rawdata$New_MaritalStatus))

rawdata$New_Generation <- unclass(as.factor(rawdata$Generation))


#Em progresso
#Outliers

#Create a boxplot
boxplot(rawdata$Dependents, horizontal = TRUE) #outliers
boxplot(rawdata$originalsalary, horizontal = TRUE) #outliers
boxplot(rawdata$JobLevel, horizontal = TRUE) #outliers
boxplot(rawdata$NumCompaniesWorked, horizontal = TRUE)#outlier
boxplot(rawdata$`SalaryRise(%)`, horizontal = TRUE)
boxplot(rawdata$TenureWorking, horizontal = TRUE) #outliers
boxplot(rawdata$NumberProjectsLastYear, horizontal = TRUE) #outliers on both sides
boxplot(rawdata$TenureCompany, horizontal = TRUE) #outliers
boxplot(rawdata$TenureRole, horizontal = TRUE) #outliers
boxplot(rawdata$LastPromotion, horizontal = TRUE) #outliers
boxplot(rawdata$TenureManager, horizontal = TRUE) #outliers
boxplot(rawdata$MonthlyIncome, horizontal = TRUE) #outliers
boxplot(rawdata$DistanceHomeOffice, horizontal = TRUE)
boxplot(rawdata$JobDedication, horizontal = TRUE)
boxplot(rawdata$JobPerformance, horizontal = TRUE) #outliers???
boxplot(rawdata$AfterHours, horizontal = TRUE)
boxplot(rawdata$FacilitiesSatisfaction, horizontal = TRUE)
boxplot(rawdata$RoleSatisfaction, horizontal = TRUE)
boxplot(rawdata$HierarchySatisfaction, horizontal = TRUE)
boxplot(rawdata$Age, horizontal = TRUE) #outliers
boxplot(rawdata$balancenumber, horizontal = TRUE)
boxplot(rawdata$AvgSatisfaction, horizontal = TRUE) #outliers

#Create a list from outliers
boxplot.stats(rawdata$Dependents)
boxplot.stats(rawdata$originalsalary)
boxplot.stats(rawdata$JobLevel)
boxplot.stats(rawdata$NumCompaniesWorked)
boxplot.stats(rawdata$TenureWorking)
boxplot.stats(rawdata$NumberProjectsLastYear)
boxplot.stats(rawdata$TenureCompany)
boxplot.stats(rawdata$TenureRole)
boxplot.stats(rawdata$LastPromotion)
boxplot.stats(rawdata$TenureManager)
boxplot.stats(rawdata$MonthlyIncome)
boxplot.stats(rawdata$JobPerformance)
boxplot.stats(rawdata$AvgSatisfaction)
boxplot.stats(rawdata$Age)


#histograms

hist(rawdata$Dependents)
hist(rawdata$originalsalary)
hist(rawdata$JobLevel)
hist(rawdata$NumCompaniesWorked)
hist(rawdata$TenureWorking)
hist(rawdata$NumberProjectsLastYear)
hist(rawdata$TenureCompany)
hist(rawdata$TenureRole)
hist(rawdata$LastPromotion)
hist(rawdata$TenureManager)
hist(rawdata$MonthlyIncome)
hist(rawdata$JobPerformance)
hist(rawdata$AvgSatisfaction)
hist(rawdata$Age)


cat(1450*0.03) #44
rawdata2 <- subset(rawdata, rawdata$TenureCompany>30)

#rawdata4 <- subset(rawdata, rawdata$TenureWorking>45) variable removed later on so we did not check
#rawdata5 <- subset(rawdata, rawdata$TenureRole>14) variable removed later on so we did not check

#rawdata7 <- subset(rawdata, rawdata$TenureManager>15) variable removed later on so we did not check
rawdata8 <- subset(rawdata, rawdata$Age>65)
cat(18+8+13+2) # 41



rawdata10 <- subset(rawdata, rawdata$TenureCompany<=30)
rawdata9 <- subset(rawdata10, rawdata10$Age<=65)
#removemos os outliers em idade e tempo na empresa pois não consideramos realistas e podem ser erro de ficha

summary(rawdata9$Dependents)
summary(rawdata9$LastPromotion)
rawdata9$Dependents <- ifelse(rawdata9$Dependents<=3, 1, rawdata9$Dependents)
rawdata9$LastPromotion <- ifelse(rawdata9$LastPromotion<=14, 1, rawdata9$LastPromotion)
#Nestes outliers, como achamos que podiam ser ainda realistas, embora bastante extremos, substituimos por mediana, para não causar muitos problemas na distribuição
rawdata <- rawdata9
rm(rawdata2,rawdata3,rawdata4,rawdata5,rawdata6,rawdata7,rawdata8,rawdata9,rawdata10)

#Data Normalization
log_data <- rawdata
log_data$MonthlyIncome <-log(rawdata$MonthlyIncome)
log_data$originalsalary <-log(rawdata$originalsalary)
log_data$TenureCompany <- ifelse(rawdata$TenureCompany == 0, 0, log(rawdata$TenureCompany))
log_data$LastPromotion <- ifelse(rawdata$LastPromotion == 0, 0, log(rawdata$LastPromotion))
preprocesseddata2<- subset(log_data, select = -c(Age,Dependents,agetest,year_birth, AvgSatisfaction, originalsalary, AfterHours, EmployeeID, BirthDate, Gender, `Marital Status`, JobType, Department, JobRole, Education, EducationArea, Churn, New_MaritalStatus, New_JobType, New_Dependents, Generation, BalanceWork.Life))
preprocesseddata<- subset(log_data, select = -c(Age,Dependents,agetest,year_birth, AvgSatisfaction, originalsalary, EmployeeID, BirthDate, New_Gender, `Marital Status`, JobType, New_department, New_JobRole, New_Education, New_EducationArea, NNew_MaritalStatus, NNew_JobType, NNew_Dependents, New_Generation, balancenumber, New_Afterhours, Churn))
test <- cor(preprocesseddata2)

test2 <- ifelse(test>0.7, test, ' ')

#acima de 70%

#Joblevel - MonthlyIncome

#Joblevel - OriginalSalary

#Joblevel - TenureWorking

#Salary Rise - Job Performance

#Tenure Working - Monthly Income

#Tenure Working - Original salary

#Original Salary - Monthly Income

#Tenure Role - Tenure Company

#Tenure Company - Tenure Manager

#Tenure Role - Tenure Manager

preprocesseddata <- subset(preprocesseddata, select = -c(JobLevel, TenureWorking, TenureRole, `SalaryRise(%)`, TenureManager))
preprocesseddata2 <- subset(preprocesseddata2, select = -c(JobLevel, TenureWorking, TenureRole, `SalaryRise(%)`, TenureManager))
preprocesseddata3 <- subset(log_data, select = c(AfterHours,JobDedication,MonthlyIncome,NumberProjectsLastYear,DistanceHomeOffice,HierarchySatisfaction,New_Churn))
#em progresso ainda
#########################################################################################################
#                                            DATA REDUCTION                                             #
#########################################################################################################

#-------------------------------------------------------------------------------------------------------#
#------------------------------Reduce the number of variables using PCA---------------------------------#
#-------------------------------------------------------------------------------------------------------#


#log transform

#apply PCA
pca_data <- prcomp(subset(preprocesseddata2, select = -c(New_Churn)), center = TRUE, scale. = TRUE)

# Analyze the results: std of each componente and loadings
print(pca_data)

# how many components?
# plot method
plot(pca_data, type = "l")

# Summary method - describe the importance of the PCs
# First row - describe again the standard deviation associated with each PC. 
# Second row - shows the proportion of the variance in the data explained by each 
# component.
# Third row - describe the cumulative proportion of explained variance.
summary(pca_data)

#pca with 5 components due to the first big drop in the elbow graphic
pca5 <- pca_data$x[,1:6]
pca5[,6] <- preprocesseddata$New_Churn
colnames(pca5)[6] <- 'Churn'
pca5 <- as.data.frame(pca5)

#pca with 11 components due to putting a cuttoff at 1.0 of standard deviation
pca11 <- pca_data$x[,1:12]
pca11[,12] <- preprocesseddata$New_Churn
colnames(pca11)[12] <- 'Churn'
pca11 <- as.data.frame(pca11)

#pca with 20 components due to the being over 95% of the cumulative proportion
pca20 <- pca_data$x[,1:21]
pca20[,21] <- preprocesseddata$New_Churn
colnames(pca20)[21] <- 'Churn'
pca20 <- as.data.frame(pca20)


# Prep Training and Test data.
set.seed(750)


trainingRowIndex <- sample(1:nrow(preprocesseddata), 0.66*nrow(preprocesseddata))
trainData <- preprocesseddata[trainingRowIndex, ]
testData <- preprocesseddata[-trainingRowIndex, ]

trainingRowIndex2 <- sample(1:nrow(preprocesseddata2), 0.66*nrow(preprocesseddata2))
trainData2 <- preprocesseddata2[trainingRowIndex, ]
testData2 <- preprocesseddata2[-trainingRowIndex, ]

trainingRowIndex5 <- sample(1:nrow(pca5), 0.66*nrow(pca5))
trainData5 <-pca5[trainingRowIndex5, ]
testData5 <- pca5[-trainingRowIndex5, ]

trainingRowIndex11 <- sample(1:nrow(pca11), 0.66*nrow(pca11))
trainData11 <-pca11[trainingRowIndex11, ]
testData11 <- pca11[-trainingRowIndex11, ]


trainingRowIndex20 <- sample(1:nrow(pca20), 0.66*nrow(pca20))
trainData20 <-pca20[trainingRowIndex20, ]
testData20 <- pca20[-trainingRowIndex20, ]

trainingRowIndextest <- sample(1:nrow(preprocesseddata3), 0.66*nrow(preprocesseddata3))
trainDatatest <-preprocesseddata3[trainingRowIndextest, ]
testDatatest <- preprocesseddata3[-trainingRowIndextest, ]

# Build Logistic Model
logitmod5 <- glm(Churn ~ PC1 + PC2 + PC3 + PC4 + PC5, family = "binomial", data=trainData5)
logitmod11 <- glm(Churn ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, family = "binomial", data=trainData11)
logitmod20 <- glm(Churn ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20, family = "binomial", data=trainData20)
logitmod <- glm(New_Churn ~ Gender + Department + JobRole + Education + EducationArea + NumCompaniesWorked + NumberProjectsLastYear + TenureCompany + LastPromotion + MonthlyIncome + DistanceHomeOffice + JobDedication + AfterHours + JobPerformance + FacilitiesSatisfaction + RoleSatisfaction + HierarchySatisfaction + BalanceWork.Life + New_MaritalStatus + New_JobType + New_Dependents + Generation, family = "binomial", data=trainData)
test <- glm(New_Churn ~ AfterHours + JobDedication + MonthlyIncome + NumberProjectsLastYear + NumberProjectsLastYear + DistanceHomeOffice + HierarchySatisfaction, family = "binomial", data=trainDatatest)

summary(logitmod)

pred <- predict(logitmod, testData, type = "response")
pred5 <- predict(logitmod5, testData5, type = "response")
pred11 <- predict(logitmod11, testData11, type = "response")
pred20 <- predict(logitmod20, testData20, type = "response")
test_pred<- predict(test, testDatatest, type = "response")
head(pred)

# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$New_Churn

y_pred5_num <- ifelse(pred5 > 0.5, 1, 0)
y_pred5 <- factor(y_pred5_num, levels=c(0, 1))
y_act5 <- testData5$Churn

y_pred11_num <- ifelse(pred11 > 0.5, 1, 0)
y_pred11 <- factor(y_pred11_num, levels=c(0, 1))
y_act11 <- testData11$Churn


y_pred20_num <- ifelse(pred20 > 0.5, 1, 0)
y_pred20 <- factor(y_pred20_num, levels=c(0, 1))
y_act20 <- testData20$Churn

y_predtest_num <- ifelse(test_pred > 0.5, 1, 0)
y_predtest <- factor(y_predtest_num, levels=c(0, 1))
y_acttest <- testDatatest$New_Churn

head(y_pred)

library(caret)

caret::confusionMatrix(y_predtest, y_acttest, positive = '1')
#This was a test to see the results of using the top variables from decision trees, it actually gives better performance than the PCA 5
caret::confusionMatrix(y_pred, y_act, positive = '1')
caret::confusionMatrix(y_pred5, y_act5, positive = '1')
caret::confusionMatrix(y_pred11, y_act11, positive = '1')
caret::confusionMatrix(y_pred20, y_act20, positive = '1')
#both the one without pca and the pca with 20 perform the best, but we must be careful about overfitting


#Now, since the pca vectors are independent, we test Multinominal Naive Bios

# train a naive bayes model
install.packages('klaR')
library(klaR)
model5 <- NaiveBayes(factor(Churn)~. , data = pca5, fL = 1)
model11 <- NaiveBayes(factor(Churn)~. , data = pca11, fL = 1)
model20 <- NaiveBayes(factor(Churn)~. , data = pca20, fL = 1)
model <- NaiveBayes(factor(New_Churn)~. , data = preprocesseddata, fL = 1)


# make predictions
x_test5 <- testData5[,1:5]
y_test5 <- testData5[,6]
predictions5 <- predict(model5, x_test5)

x_test11 <- testData11[,1:11]
y_test11 <- testData11[,12]
predictions11 <- predict(model11, x_test11)

x_test20 <- testData20[,1:20]
y_test20 <- testData20[,21]
predictions20 <- predict(model20, x_test20)

x_test <- subset(testData, select = -c(New_Churn))
y_test <- testData[,22]
predictions <- predict(model, x_test)


#summarize results
caret::confusionMatrix(predictions5$class, y_test5, positive = '1')
caret::confusionMatrix(predictions11$class, y_test11, positive = '1')
caret::confusionMatrix(predictions20$class, y_test20, positive = '1')
caret::confusionMatrix(predictions$class, y_test, positive = '1')
#clearly there is a performance boost from pca, which makes sense considering the naive assumption

#Decision Tress
#Another interesting approach is would be a classification tree due to the type of problem and the different type of variables
install.packages('C50')
library(C50)
tree_model <- C5.0(subset(trainData, select = -c(New_Churn)), as.factor(trainData[,22]))
tree_model
summary(tree_model)
#Here you can see the importance of each variable, which can be useful for warning the company about critical factors
tree_pred <- predict(tree_model, testData)
install.packages('gmodels')
library(gmodels)
gmodels::CrossTable(testData$New_Churn, tree_pred,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual churn', 'predicted churn'))
#Very interesting results predicts a lot more churn than previous models


#forest model
library(randomForest)
forest_model <- randomForest(as.factor(New_Churn) ~ ., ntree = 100, data=trainData2)
print(forest_model)
forest_pred=predict(forest_model, testData2)
caret::confusionMatrix(forest_pred, testData2$New_Churn, positive = '1')


#mlp
library(monmlp)

multiperceptron <- monmlp.fit(data.matrix(subset(trainData2, select = -c(New_Churn))),data.matrix(subset(trainData2, select = c(New_Churn))), hidden1=2)
mlp.pred <- monmlp.predict(x = data.matrix(subset(testData2, select = -c(New_Churn))), weights = multiperceptron)
y_predmlp_num <- ifelse(mlp.pred > 0.5, 1, 0)
y_predmlp <- factor(y_predmlp_num, levels=c(0, 1))
caret::confusionMatrix(y_predmlp, testData2$New_Churn, positive = '1')


multiperceptron5 <- monmlp.fit(data.matrix(subset(trainData5, select = -c(Churn))),data.matrix(subset(trainData5, select = c(Churn))), hidden1=2)
mlp5.pred <- monmlp.predict(x = data.matrix(subset(testData5, select = -c(Churn))), weights = multiperceptron5)
y_predmlp5_num <- ifelse(mlp5.pred > 0.5, 1, 0)
y_predmlp5 <- factor(y_predmlp5_num, levels=c(0, 1))
caret::confusionMatrix(y_predmlp5, testData2$New_Churn, positive = '1')

multiperceptron11 <- monmlp.fit(data.matrix(subset(trainData11, select = -c(Churn))),data.matrix(subset(trainData11, select = c(Churn))), hidden1=2)
mlp11.pred <- monmlp.predict(x = data.matrix(subset(testData11, select = -c(Churn))), weights = multiperceptron11)
y_predmlp11_num <- ifelse(mlp11.pred > 0.5, 1, 0)
y_predmlp11 <- factor(y_predmlp11_num, levels=c(0, 1))
caret::confusionMatrix(y_predmlp11, testData2$New_Churn, positive = '1')

multiperceptron20 <- monmlp.fit(data.matrix(subset(trainData20, select = -c(Churn))),data.matrix(subset(trainData20, select = c(Churn))), hidden1=2)
mlp20.pred <- monmlp.predict(x = data.matrix(subset(testData20, select = -c(Churn))), weights = multiperceptron20)
y_predmlp20_num <- ifelse(mlp20.pred > 0.5, 1, 0)
y_predmlp20 <- factor(y_predmlp20_num, levels=c(0, 1))
caret::confusionMatrix(y_predmlp20, testData2$New_Churn, positive = '1')


#KNN
library(class)
knn_test_pred5 <- knn(train = trainData5, test = testData5, cl = trainData5$Churn, k = 21)
CrossTable(x = testData5$Churn, y = knn_test_pred5, prop.chisq = FALSE)

knn_test_pred11 <- knn(train = trainData11, test = testData11, cl = trainData11$Churn, k = 21)
CrossTable(x = testData11$Churn, y = knn_test_pred11, prop.chisq = FALSE)

knn_test_pred20 <- knn(train = trainData20, test = testData20, cl = trainData20$Churn, k = 21)
CrossTable(x = testData20$Churn, y = knn_test_pred20, prop.chisq = FALSE)
#This one actually performs better the smaller the number of vsriables is

knn_test_pred2 <- knn(train = trainData2, test = testData2, cl = trainData2$New_Churn, k = 21)
CrossTable(x = testData2$New_Churn, y = knn_test_pred2, prop.chisq = FALSE)


#NN

library(neuralnet)
trainData2[c(13)]
nn5 <- neuralnet(Churn ~ PC1 + PC2 + PC3 + PC4 + PC5, data=trainData5, linear.output = F, hidden = 3)
nn11 <- neuralnet(Churn ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data=trainData11, linear.output = F, hidden = 3)
nn20 <- neuralnet(Churn ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20, data=trainData20, linear.output = F, hidden = 3)
nn <- neuralnet(New_Churn ~ NumCompaniesWorked + NumberProjectsLastYear + TenureCompany + LastPromotion + MonthlyIncome + DistanceHomeOffice + JobDedication + JobPerformance + FacilitiesSatisfaction + RoleSatisfaction + HierarchySatisfaction + balancenumber + New_department + New_JobRole + New_Education + New_EducationArea + New_Gender + New_Afterhours + NNew_MaritalStatus + NNew_Dependents + NNew_JobType + New_Generation, data=trainData2, linear.output = F, hidden = 3)

validation.prediction=compute(nn, testData2[,-c(13)])
y_prednn_num <- ifelse(validation.prediction$net.result > 0.5, 1, 0)
y_prednn <- factor(y_pred_num, levels=c(0, 1))
confusionMatrix(y_prednn, testData2$New_Churn)

validation5.prediction=compute(nn5, testData5[,-c(6)])
y_pred5nn_num <- ifelse(validation.prediction$net.result > 0.5, 1, 0)
y_pred5nn <- factor(y_pred5_num, levels=c(0, 1))
confusionMatrix(y_pred5nn, y_act5)

validation11.prediction=compute(nn11, testData11[,-c(12)])
y_pred11nn_num <- ifelse(validation11.prediction$net.result > 0.5, 1, 0)
y_pred11nn <- factor(y_pred11_num, levels=c(0, 1))
confusionMatrix(y_pred11nn, y_act11)

validation20.prediction=compute(nn20, testData20[,-c(21)])
y_pred20nn_num <- ifelse(validation20.prediction$net.result > 0.5, 1, 0)
y_pred20nn <- factor(y_pred20_num, levels=c(0, 1))
confusionMatrix(y_pred20nn, y_act20)
#PCA 11 is actually the worse performer, while the PCA 20 is the best by a slight margin


#################################
# Cutoff
################################


#Logistic Regression
#pca5
# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(pred5>cut, 1, 0), y_act5)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)

#No pca
# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(pred>cut, 1, 0), y_act)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)


#Decision Tree Columns

# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(test_pred>cut, 1, 0), y_acttest)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)




#PCA11

# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(pred11>cut, 1, 0), y_act11)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)


#PCA20


# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(pred20>cut, 1, 0), y_act20)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)

#NN

#Non PCA turned declassified
# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(validation.prediction$net.result>cut, 1, 0), testData2$New_Churn)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)


#pca5
# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(validation5.prediction$net.result>cut, 1, 0), y_act5)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)

#PCA11

# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(validation11.prediction$net.result>cut, 1, 0), y_act11)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)


#PCA20


# create empty accuracy table
accT = c()
# compute accuracy per cutoff
for (cut in seq(0,1,0.1)){
  cm <- confusionMatrix(ifelse(validation20.prediction$net.result>cut, 1, 0), y_act20)
  accT = c(accT, cm$overall[1])
}

plot(accT ~ seq(0,1,0.1), xlab = "Cutoff Value", ylab = "", type = "l", ylim = c(0, 1))
lines(1-accT ~ seq(0,1,0.1), type = "l", lty = 2)
legend("topright", c("accuracy", "overall error"), lty = c(1, 2), merge = TRUE)




#################################
# ROC
################################

library(pROC)



#NOPCA Only numeric
r <- roc(testData2$New_Churn, y_prednn_num)

plot.roc(r)
# compute auc
auc(r)

#PCA5
r5 <- roc(y_act5, y_pred5nn_num)

plot.roc(r5)
# compute auc
auc(r5)

#PCA11
r11 <- roc(y_act11, y_pred11nn_num)

plot.roc(r11)
# compute auc
auc(r11)

#PCA20
r20 <- roc(y_act20, y_pred20nn_num)

plot.roc(r20)
# compute auc
auc(r20)

#Logistic regression


#Decision Tree Columns

r <- roc(y_acttest, predtest)

plot.roc(r)
# compute auc
auc(r)


#NOPCA
r <- roc(y_act, pred)

plot.roc(r)
# compute auc
auc(r)

#PCA5
r5 <- roc(y_act5, pred5)

plot.roc(r5)
# compute auc
auc(r5)

#PCA11
r11 <- roc(y_act11, pred11)

plot.roc(r11)
# compute auc
auc(r11)

#PCA20
r20 <- roc(y_act20, pred20)

plot.roc(r20)
# compute auc
auc(r20)




#KNN

#NOPCA
r <- roc(y_act, as.numeric(knn_test_pred2))

plot.roc(r)
# compute auc
auc(r)

#PCA5
r5 <- roc(y_act5, as.numeric(knn_test_pred5))

plot.roc(r5)
# compute auc
auc(r5)

#PCA11
r11 <- roc(y_act11, as.numeric(knn_test_pred11))

plot.roc(r11)
# compute auc
auc(r11)

#PCA20
r20 <- roc(y_act20, as.numeric(knn_test_pred20))

plot.roc(r20)
# compute auc
auc(r20)


#tree 


rtree <- roc(y_act, as.numeric(tree_pred))

plot.roc(rtree)
# compute auc
auc(rtree)


#Naive Bayes 

#NOPCA only numeric
r <- roc(y_act, as.numeric(predictions$class))


plot.roc(r)
# compute auc
auc(r)

#PCA5
r5 <- roc(y_act5, as.numeric(predictions5$class))

plot.roc(r5)
# compute auc
auc(r5)

#PCA11
r11 <- roc(y_act11,as.numeric(predictions11$class))

plot.roc(r11)
# compute auc
auc(r11)

#PCA20
r20 <- roc(y_act20, as.numeric(predictions20$class))

plot.roc(r20)
# compute auc
auc(r20)

