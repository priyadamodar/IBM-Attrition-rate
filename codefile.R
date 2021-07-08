#Importing necessary libraries
install.packages("ggplot2")
install.packages("InformationValue")
install.packages("corrplot")
install.packages("MASS")
install.packages("devtools")
install.packages("waffle")
install.packages("plotrix")
library(ggplot2)
library(waffle)
library(InformationValue)
library(corrplot)
library(MASS)
library(devtools)
library(plotrix)

#Importing the dataset
HR.Employee.Attrition <- read.csv("C:/Users/gidha/Downloads/HR-Employee-Attrition.csv", header=TRUE)
summary(HR.Employee.Attrition)

#Data Preparation and Cleaning
colSums(is.na(HR.Employee.Attrition))
Emp_atrn_1=na.omit(HR.Employee.Attrition)
Emp_atrn_1$BusinessTravel=as.character(Emp_atrn_1$BusinessTravel)
Emp_atrn_1$BusinessTravel[Emp_atrn_1$BusinessTravel=="Non-T"] = "Non-Travel"
Emp_atrn_1$BusinessTravel=as.factor(Emp_atrn_1$BusinessTravel)
Emp_atrn_1$Department=as.character(Emp_atrn_1$Department)
Emp_atrn_1$Department[Emp_atrn_1$Department=="R & D"] = "Research & Development"
Emp_atrn_1$Department=as.factor(Emp_atrn_1$Department)
summary(Emp_atrn_1$Department)

#Exploratory Data Analysis
#Considering variable "Distance From Home"
boxplot(Emp_atrn_1$DistanceFromHome,main="Distance from Home",xlab="Hours",col = "light blue",outcol="dark red",horizontal = TRUE, notch = TRUE) 
outlier_distance<-boxplot(Emp_atrn_1$DistanceFromHome)$out
Emp_atrn_1=Emp_atrn_1[-which(Emp_atrn_1$DistanceFromHome %in% outlier_distance),]
boxplot(Emp_atrn_1$DistanceFromHome,main="Distance from Home",xlab="Hours",col = "light blue",outcol="dark red",horizontal = TRUE, notch = TRUE) 

#Considering variable "Education"
boxplot(Emp_atrn_1$Education,main="Education",xlab="Education level ",col = "orange",outcol="dark red",horizontal = TRUE, notch = TRUE)
outliers_edu=boxplot(Emp_atrn_1$Education)$out
Emp_atrn_1=Emp_atrn_1[-which(Emp_atrn_1$Education %in% outliers_edu),]
summary(Emp_atrn_1$Education)
boxplot(Emp_atrn_1$Education,main="Education",xlab="Education level ",col = "orange",outcol="dark red",horizontal = TRUE, notch = TRUE)

#Buidling a prediction model
#Converting the target variable to binary (0,1)
Emp_atrn_1$Attrition<-ifelse(Emp_atrn_1$Attrition=="Yes",1,0)

#Splitting the data as training and test data set
sample<-sample(1:nrow(Emp_atrn_1),0.80*nrow(Emp_atrn_1))
training_data<-Emp_atrn_1[sample,]
test_data<-Emp_atrn_1[-sample,]

#Applying the Logistic Regression Model
#exclduing columns which has only one unique value
model<-glm(Attrition~ï..Age+BusinessTravel+DailyRate+Department+DistanceFromHome+Education+EducationField+EmployeeNumber+EnvironmentSatisfaction+Gender+HourlyRate+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+MonthlyIncome+MonthlyRate+NumCompaniesWorked+OverTime+PercentSalaryHike+PerformanceRating+RelationshipSatisfaction+StockOptionLevel+TotalWorkingYears+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,data = training_data,family = "binomial")
summary(model)

#Model tuning using variable reduction (StepAIC)
model2<-stepAIC(object = model,direction = "backward")
model2$anova

#Predicting the target variable
pred<-predict(model2,newdata = test_data,type = "response")
head(pred)

#Finding the optimal cut-off frequency and Improving the accuracy 
y_pred<-ifelse(pred>0.5,"1","0")
y_predicted<-factor(y_pred,levels = c(0,1))
head(y_predicted,30)
head(training_data$Attrition,30)

opt_cutoff<-optimalCutoff(test_data$Attrition,pred)
opt_cutoff
y_pred<-ifelse(pred>opt_cutoff,"1","0")
y_predicted<-factor(y_pred,levels = c(0,1))
head(y_predicted,30)
head(training_data$Attrition,30)
mean(y_predicted==training_data$Attrition)

#Visualizations for significant variables 
colnames(Emp_atrn_1)[1]<-"Age"
#Age 
ggplot(Emp_atrn_Yes,aes(Age))+geom_area(stat = "bin",aes(fill = factor(Gender)))+xlab("AGE(in Years)")+labs(fill = "Gender")+scale_fill_manual(values=c("light green","blue"))+theme_bw()
#Correlation plot
corrplot(model2,method = "circle")
corrdata<-cor(Emp_atrn_1[,c(1,6:7,11,14,17,26,28:31,33:35)])
corrplot(corrdata,order = "AOE",tl.cex = 0.8,tl.col = "Black")
#Marital Status
#filter the data for Attrition="Yes"
#1=yes, 0=no
Emp_atrn_1_yes=Emp_atrn_1[Emp_atrn_1$Attrition=="1",]
#print the data
head(Emp_atrn_1_yes,5)
#get individual counts
summary(Emp_atrn_1_yes$MaritalStatus)
#Divorced=31
#Married=79
#Single=107
parts=c('Divorced'=31,'Married'=79, 'Single'=107)
waffle(parts,rows=10,colors = c("red","blue","light green"))

#Yearsincurrentrole
ggplot(Emp_atrn_1_yes,aes(Emp_atrn_1_yes$Gender,Emp_atrn_1_yes$YearsInCurrentRole)) + geom_jitter(aes(color=Emp_atrn_1_yes$Gender,shape=Emp_atrn_1_yes$Gender),width = 0.1,size=2) +theme_bw() +labs(title="Years in Current role")

#Environmentsatisfaction
jobenv_freq=table(Emp_atrn_1_yes$EnvironmentSatisfaction)
pie(jobenv_freq,labels=c("low","Medium","High","Very High"),main="Environment satisfaction", col = c("cyan","cadetblue3","cyan4","darkblue"))

#Education
ggplot(Emp_atrn_1_yes)+geom_bar(aes(factor(Education)),width = 0.70,fill="orange")+theme_bw()+scale_x_discrete(breaks=c("1","2","3","4","5"),labels=c("Below College","College","Bachelor","Master","Doctor"))+xlab("Education")

#Jobsatisfaction
boxplot(Emp_atrn_1$JobSatisfaction~Emp_atrn_1$Attrition,
        data=Emp_atrn_1,
        main="Job Satisfaction vs Attrition",
        xlab="Job Satisfaction",
        ylab="Attrition",
        col=c("light green","red"),
        border="brown",horizontal=TRUE)











                            







                            


