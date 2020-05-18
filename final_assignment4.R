
library(car) # advanced scatter plots 
library(corrplot) # plot correlations 
library(dplyr) # data aggregates 
library(Hmisc) # for correlation test of multiple variables 
library(gplots)
library(psych)
library(gmodels) # cross tabulation
library(gplots) # plot means with CI 
library(ggplot2)
set.seed(123)
options(scipen=99)
dev.off()
install.packages("xlsx")
library(xlsx)
qwe<-read.xlsx(file.choose(),2, header=TRUE)
View(qwe)
str(qwe)
#Rename the column names
names(qwe)<-c("ID","cust_age_months","churn_rate","CHI_score_0", "CHI_score_0_1","sup_case_0", "sup_case_0_1","SP_0","SP_0_1","login_0_1","blog_articles_0_1","views_0_1","days_since_last_login")



#Checked for missing values. As the number of rows returned is 0, there are no missing values
qwe[!complete.cases(qwe),]

#Replace the values of churn rate 1 to yes and 0 to no
levels(qwe$churn_rate)
qwe$churn_rate <- as.factor(qwe$churn_rate)
qwe$churn_rate<-gsub("1", "yes", qwe$churn_rate)
qwe$churn_rate<-gsub("0", "no", qwe$churn_rate)


#Changing the necessary variable to numeric
qwe$cust_age_months <- as.numeric(qwe$cust_age_months)
qwe$CHI_score_0  <- as.numeric(qwe$CHI_score_0)
qwe$CHI_score_0_1  <- as.numeric(qwe$CHI_score_0_1)
qwe$sup_case_0  <- as.numeric(qwe$sup_case_0)
qwe$sup_case_0_1  <- as.numeric(qwe$sup_case_0_1)
qwe$SP_0 <- as.numeric(qwe$SP_0)
qwe$SP_0_1 <- as.numeric(qwe$SP_0_1)
qwe$login_0_1 <- as.numeric(qwe$login_0_1)
qwe$blog_articles_0_1 <- as.numeric(qwe$blog_articles_0_1)
qwe$views_0_1 <- as.numeric(qwe$views_0_1)
qwe$days_since_last_login <- as.numeric(qwe$days_since_last_login)

#Checking for imbalance
count<-table(qwe$churn_rate)
count
#no  yes 
#6024  323
churn <- prop.table(table(qwe$churn_rate))
churn
barplot(churn, main = "Customer Churn", col=c("steelblue"))

#no        yes 
#0.94910982 0.05089018 
#it's seen that the data seems skewed
#There is a lot of imbalance in the data. With this information, the prediction can be very biased towards NO class.


#There is a lot of imbalance in the data. With this information, the prediction can be very biased towards NO class.
library(ROSE)
# balanced data set with both over and under sampling
qwe_both1 <- ovun.sample(churn_rate~., data=qwe, p=0.4, seed=1, method="both")$data
table(qwe_both1$churn_rate)
#minority class - yes is oversampled with replacement and majority class-no is undersampled without replacement
#   no  yes 
#3826 2521 

#The balanced dataset has 50.4% of 'no' Class and 49.5% of 'yes' class
View(qwe_both1)
str(qwe_both1)
churn1 <- prop.table(table(qwe_both1$churn_rate))
churn1
# no       yes 
#0.6028045 0.3971955 

#Selecting the most important varaibles for building a model
#Going forward with forward selection
qwe_both1$churn_rate<-as.factor(qwe_both1$churn_rate)
full<-glm(churn_rate~.,data=qwe_both1[,-c(1)], family="binomial")
full
null <- glm(churn_rate~1.,data=qwe_both1,  family="binomial")
null

#Forward Selection
step(null, scope = list(lower=null, upper=full), direction="forward")

#Variables to consider: CHI_score_0 + cust_age_months + days_since_last_login + 
#CHI_score_0_1 + views_0_1 + sup_case_0 + sup_case_0_1 + login_0_1 +   blog_articles_0_1

#-------------------------------------------------------------------------------------------------------
#Question 1
#Is Wall's belief about the dependence of churn rates on customer age supported by the data? 
#univaraite analysis of customer age 
hist(qwe$cust_age_months, main = "Customer Age", col=c("steelblue"), freq=F)
lines(density(qwe$cust_age_months), col="yellow", lwd=3) #To show the line for numeric 
box()
summary(qwe$cust_age_months)
# From the graph we see that the data is right-skewed with mean>median. 
#lowest age being 0 months and the highest being 67 months. Mean age of the customer is 14 months approx. 


#univariate analysis of Churn_rate
t <- table(qwe$churn_rate)
summary(qwe$churn_rate)
table(qwe$churn_rate) 
barplot(t, main = "Bar Plot", xlab = "Churn Rate", ylab = "Frequency", col="steelblue")
ptab<-prop.table(t) #Check the percentage
ptab
#94.9% of the instances are NO while 5% of the instances are YES. 

#Bivariate analysis to check if there is dependany of churn rate on customer age
par(mfrow=c(1,2))
boxplot(cust_age_months~churn_rate, data=qwe, main="churn rate with respect to age", 
        xlab="churn rate", ylab="age(in months)",
        col=c("orange", "lightblue4"))

chrunrate_yes<-qwe[qwe$churn_rate=="yes",]
chrunrate_no<-qwe[qwe$churn_rate=="no",]
plot(density(chrunrate_yes$cust_age_months), col="red", lwd=2.5, main="chrun rate by customer age")
lines(density(chrunrate_no$cust_age_months), col="blue", lwd=2.5)
legend("topright",
       legend = c("chrun_rate=yes", "chrun_rate=no"),
       fill = c("red", "blue"))

# too many outliers present
#the median age for customer with churn rate yes and no lie on the same range, indicating that there is no dependency determined 
#for customer age to churn rate being yes or no. 
# From the plots, we can conclude that the lines overlap which show that there is no dependance between the two variables.
#---------------------------------------------------------------------------------------------------------
#Question 2 
#logistic regression model that best predicts the probability that a customer leaves
xtabs(~ cust_age_months+churn_rate, data=qwe_both1)
#Customer churn is the most for the customers to have left for 12 months


library(ggplot2)
options(scipen=99)

#Customer churn is the most for the customers to have left for 12 months
logit <- glm(churn_rate~CHI_score_0 + cust_age_months + days_since_last_login + 
               CHI_score_0_1 + views_0_1 + sup_case_0 + sup_case_0_1 + login_0_1 + 
               blog_articles_0_1, family = "binomial", data = qwe_both1)
summary(logit)

#Residuals 
#  Min       1Q   Median       3Q      Max  
#-1.8948  -1.0326  -0.7408   1.1752   2.0837  

#All the variables in the model have a significant relationship with response variable except blog_articles_0_1
#With 95% confidence, For every one unit increase in CHI_score_0, the log of odds for customer churn='yes' decreases by 0.004. 
#With 95% confidence, For every one unit increase in cust_age_months, the log of odds for customer churn='yes' increases by 0.016. 
#With 95% confidence, For every one unit increase in days_since_last_login, the log of odds for customer churn='yes' increases by 0.01. 
#With 95% confidence, For every one unit increase in CHI_score_0_1, the log of odds for customer churn='yes' decreases by 0.009.  
#With 95% confidence, For every one unit increase in views_0_1, the log of odds for customer churn='yes' decreases by 0.0001. 
#With 95% confidence, For every one unit increase in sup_case_0, the log of odds for customer churn='yes' decreases by 0.016. 
#With 95% confidence, For every one unit increase in sup_case_0_1, the log of odds for customer churn='yes' increases by 0.12. 
#With 95% confidence, For every one unit increase in login_0_1 , the log of odds for customer churn='yes' increases by 0.001. 

#Null Deviance: 8528.6  wirh 6246 degrees of freedom- null model
#Residence deviance with inclusion of all varaibles: 8014.1 with 6337 degrees of freedom

#to check if the model is good we take the difference of the deviances
dev<- with(logit, null.deviance - deviance) 
dev #514.461 with 9 degrees of freedom

#The number of predictors
dev1<-with(logit, df.null, df.residual)
dev1 #6346

#Finding the p-value for the model
pvalue<-with(logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
pvalue # 4.598665e-105
#As the p-value <0.05, we can say that the model is significantly better than a null model with 9 degrees of freedom and chi-square = 385.43



#Prediction on dataset
Pred12 <- predict(logit, newdata = qwe_both1, type = "response")
Pred12
range(Pred12)

#Representation using a plot

churnrate_pred <- data.frame(churn_prob=logit$fitted.values,churn_rate=qwe_both1$churn_rate,ID=qwe_both1$ID)
churnrate_pred <- churnrate_pred[order(churnrate_pred$churn_prob, decreasing=FALSE),]
churnrate_pred$rank <- 1:nrow(churnrate_pred)
churnrate_pred

## We can plot the data...
ggplot(data=churnrate_pred, aes(x=rank, y=churn_prob)) +
  geom_point(aes(color=churn_rate), alpha=1, shape=4, stroke=2) +
  xlab("Index") + ylab("Predicted probability of customer churn")

#Interpretation: Probability of customer churn not happening has a low probability than the customer churn ='yes' which has a high probability


#Prediction on data using the best threshold value from the ROC CURVE

#Library forplotting ROC curve using the training dataset
library(ROCR)
CTpred <- prediction(Pred12, qwe_both1$churn_rate)
CTperf <- performance(CTpred, "tpr", "fpr")
plot(CTperf)
auc <- performance(CTpred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc
#AUC is 65.42% which is a good model
#Finding the best threshold value 
opt.cut <- function(CTperf){
  cut.ind <- mapply(FUN = function(x,y,p){d=(x-0)^2+(y-1)^2
  ind<- which(d==min(d))
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])},CTperf@x.values, CTperf@y.values, CTperf@alpha.values)
}
print(opt.cut(CTperf))
#cutoff       0.4392878

#taking cutpoint as  0.4392878, we predict the data
class <- ifelse(Pred12 >=  0.4392878, "YES", "NO")
class

class <- as.factor(class)
table1 <- table(qwe_both1$churn_rate,class)
TN1 <- table1[1]
FN1 <- table1[2]
FP1 <- table1[3]
TP1 <- table1[4]

table1
#class
#    NO  YES
#no  2955  871
#yes 1253 1268


#Accuracy
(table1[1]+table1[4])/nrow(qwe_both1)
#66.53% accuracy

#Recall
TP1/(TP1+FN1)
#50.29% recall

#Precision
TP1/(TP1+FP1)
#59.28% Precision

#Accuracy of the model with the new threshold data cutpoint is good. 
#This is a good model. 
#Better metrix to use to determine the model is the good is using the p-value determined for the model.
#precision and recall arent very strong for the given model. 


#Question -The probability that a customer672 leaves
cust672<- predict(logit, newdata = qwe_both1[672,], type = "response")
cust672

#0.18 is the probability that the customer will leave. 
#The threshold that is set is 0.4392. if the predicted value is more than 0.4392 it means customer churn is yes. 
#Here, 0.30<0.45 which means the prediction is NO
#The actual value of the customer_churn is "no"

#Question -The probability that a customer354 leaves
cust354<- predict(logit, newdata = qwe_both1[354,], type = "response")
cust354
#0.4351 is the probability that the customer will leave. 
#The threshold that is set is 0.4392. if the predicted value is more than 0.4392 it means customer churn is yes. 
#Here, 0.33<0.45 which means the prediction is NO
#The actual value of the customer_churn is "no"

#Question -The probability that a customer5203 leaves

cust5203<- predict(logit, newdata = qwe_both1[5203,], type = "response")
cust5203
#0.4717 is the probability that the customer will leave. 
#The threshold that is set is 0.4392. if the predicted value is more than 0.4392 it means customer churn is yes. 
#Here, 0.4717334>0.4392 which means the prediction is YES
#The actual value of the customer_churn is "no"


#--------------------------------------------------------------------------------------------------------------

#Question 3
#provide the list of 100 customers with the highest churn probabilities and the top three drivers of churn for each customer
View(qwe)
Pred100<- predict(logit, newdata = qwe_both1, type = "response")
Pred_order <- Pred100[order(Pred100, decreasing = T)]
final<-head(Pred_order,n=100)
final

#The input features which have the least p-value and highest coefficient from the model. These are the two metrics with which we determine the key drivers
summary(logit)
coefficients((logit))
#From the coefficients, we see that customer age in months, days since last login and login_0_1 has the highest coefficients and p_value<0.05
