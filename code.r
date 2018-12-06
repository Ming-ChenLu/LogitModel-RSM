setwd("/Users/Amy/Desktop/Learning from Big Data/dataset")
library(dplyr)
library(aod)
library(car)
library(caret)
library(lmtest)
library(estout)
library(ROSE)

#With original 61430 obs, removing the rows that contain NA to 50557 obs
AB_data = read.csv("AB_data_05_June_2018_anonymized.csv")
AB_data = AB_data[complete.cases(AB_data),]
attach(AB_data)

#Combinging dataset into each user per row####
df_user = AB_data[!duplicated(user),2:3]

time_first_click <- AB_data %>%
  group_by(user) %>%
  arrange(timest) %>%
  filter(row_number()==1) %>%
  select("user","timestamp_Clickstream") %>%
  rename(time_first_click = timestamp_Clickstream)
time_last_click <- AB_data %>%
  group_by(user) %>%
  arrange(timest) %>%
  filter(row_number()==n()) %>%
  select("user","timestamp_Clickstream") %>%
  rename(time_last_click = timestamp_Clickstream)

others <- AB_data %>%
  group_by(user) %>%
  summarise(Outcome_EA=max(Outcome_EA),Outcome_CU=max(Outcome_CU),
            Outcome_CV=max(Outcome_CV),Outcome_RE=max(Outcome_RE),
            Outcome_DB=max(Outcome_DB),
            link_CU = max(as.numeric(levels(contact.us))[contact.us]), link_CV = max(as.numeric(levels(CV))[CV]),
            link_RE = max(as.numeric(levels(event))[event]), link_DB = max(as.numeric(levels(brochure))[brochure]),
            max_depth = max(link_depth2),sum_click = n())

df_user = df_user %>%
  merge(time_first_click, by.x = 1, by.y = 1) %>%
  merge(time_last_click, by.x = 1, by.y = 1) %>%
  merge(others, by.x = 1, by.y = 1,all.x = FALSE)

#recode intermediate outcome to (-1,1)
df_user[,c(6:9)] = lapply(df_user[,c(6:9)], 
                             FUN = function(x) recode(x, "0 = -1"))

#Overall Logit Model####
#Question to answer: How likely is the intermediate outcome affect the end outcome?
#Goal: Find a logistic model that is complex enough to fit the data, but simple to interpret (smooth but not overfitting).
#Univariate analysis to identify important covariates - fit one covariate at a time
df_logit = df_user[,c(5:13)]
#splitting data
table(df_logit[,1]) #showing it's imbalance data
idx0 = which(df_logit[,1]==0)
idx1 = which(df_logit[,1]==1)
set.seed(123)
train_id = c(sample(idx0, round(8637*0.7)), sample(idx1, round(1071*0.7)))
train = df_logit[train_id,]
test = df_logit[-train_id,]

#descriptive analysis
summary(train) #all variables are binary
sapply(train, sd) #variation
ftable(xtabs(~link_CU+Outcome_EA+Outcome_CU, data=train))

#logit
l = glm(Outcome_EA~Outcome_CU+Outcome_CV+Outcome_DB+Outcome_RE, data = train, family = "binomial")
l1 = glm(Outcome_EA~., data = train, family = "binomial")
l2 = glm(Outcome_EA~. - Outcome_RE - link_RE , data = train, family = "binomial")

train_over = ovun.sample(Outcome_EA~Outcome_CU+Outcome_CV+Outcome_DB+Outcome_RE, 
                         data = train, method = "over")$data
l_over = glm(Outcome_EA~Outcome_CU+Outcome_CV+Outcome_DB+Outcome_RE, 
             data = train_over, family = "binomial")
train_over1 = ovun.sample(Outcome_EA~., data = train, method = "over")$data
l_over1 = glm(Outcome_EA~., 
              data = train_over1, family = "binomial")
train_over2 = ovun.sample(Outcome_EA~Outcome_CU+Outcome_CV+Outcome_DB+link_CU+link_CV+link_DB, 
                          data = train, method = "over")$data
l_over2 = glm(Outcome_EA~., 
              data = train_over2, family = "binomial")

predict.l = predict(l_over2, test[,-1], type = "response")
length(predict.l) #correct
table(round(predict.l), df_logit[-train_id, 1]) #accuracy=0.89, but may be due to imbalanced data
accuracy.meas(test[,1], predict.l, threshold = 0.5)
roc.curve(test[,1],predict.l)

wald.test(b = coef(l1), Sigma = vcov(l1), Terms = 2:9) #overall effect of intermediate outcome
#McFadden's R squared
nullmod <- glm(Outcome_EA~1, data= df_logit, family="binomial")
1-logLik(l)/logLik(nullmod)

#fits the data better than the null model
pchisq(6741-6397,8)
lrtest(nullmod, l) #likelihood ratio test
anova(l, test="Chisq") #Analysis of deviance table
varImp(l) #importance of individual variable

## odds ratios and 95% CI
exp(cbind(OR = coef(l), confint(l)))
##Holding other variables at a fixed value, the odds of clicking EA after cliking CU is 252% higher than not clicking it.

#Test additional hypotheses about the differences in the coefficients
k <- cbind(0, 1, -1, 0, 0)
wald.test(b = coef(logit), Sigma = vcov(logit), L = k)

#exporting sumaarize result
eststo(l)
eststo(l1)
eststo(l2)
esttab(filename = "test", csv = TRUE)
