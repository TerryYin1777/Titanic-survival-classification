library("rpart")
library("rpart.plot")
library("dplyr")
library("ggplot2")
titanic = read.csv("Titanic-train.csv")
titanic$Name = NULL
titanic$PassengerId = NULL
titanic$Ticket = NULL
titanic$Cabin = NULL
#EDA

titanic$Survived = as.factor(titanic$Survived)
titanic$Pclass = as.factor(titanic$Pclass)
ggplot(titanic,aes(Sex, fill = Survived))+
  geom_bar(position = "fill",width = 0.5)+
  labs( y = "Percentile", title = "Percentile of survived by sex")

ggplot(titanic,aes(Pclass, fill = Survived))+
  geom_bar(position = "fill",width = 0.5)+
  labs( x = "class", y = "Percentile", title = "Percentile of survived by class")

titanic = titanic %>% group_by(Age) %>% summarize(survive_rate = sum(Survived)/n())
ggplot(titanic,aes(x = Age, y = survive_rate))+
  geom_line()+
  labs(  title = "Survive Rate by age")


#Data Preparation
see = titanic %>% group_by(Age) %>% summarise(count = n())
see2 = titanic %>% filter(Embarked == " ")

# Age has missing values, and age <1 should be regarded as 1, and.5 shoud be rounded
titanic$Age[!is.na(titanic$Age)][titanic$Age[!is.na(titanic$Age)]<1] = 1
titanic$Age[!is.na(titanic$Age)][titanic$Age[!is.na(titanic$Age)] %% 1 !=0] = round(titanic$Age[!is.na(titanic$Age)][titanic$Age[!is.na(titanic$Age)] %% 1 !=0])
titanic$Age[is.na(titanic$Age)]=round(mean(titanic$Age[!is.na(titanic$Age)]))
# Fare has 15 records of zero, using average fare of class to identify. Detect outliers
fare = titanic %>% select(Pclass,Fare) %>% filter(Fare!=0) %>% group_by(Pclass) %>% summarise(mean = mean(Fare))
titanic = left_join(titanic,fare,by = "Pclass")
titanic$Fare[titanic$Fare==0] = titanic$mean[titanic$Fare==0]
titanic$mean = NULL

# Delete the outliers by class
titanic_class1 = titanic %>% filter(Pclass == 1)
up_line1 = as.numeric(quantile(titanic_class1$Fare,0.75)+1.5*IQR(titanic_class1$Fare))
low_line1 = as.numeric(quantile(titanic_class1$Fare,0.25)-1.5*IQR(titanic_class1$Fare))

titanic_class2 = titanic %>% filter(Pclass == 2)
up_line2 = as.numeric(quantile(titanic_class2$Fare,0.75)+1.5*IQR(titanic_class2$Fare))
low_line2 = as.numeric(quantile(titanic_class2$Fare,0.25)-1.5*IQR(titanic_class2$Fare))

titanic_class3 = titanic %>% filter(Pclass == 3)
up_line3 = as.numeric(quantile(titanic_class3$Fare,0.75)+1.5*IQR(titanic_class3$Fare))
low_line3 = as.numeric(quantile(titanic_class3$Fare,0.25)-1.5*IQR(titanic_class3$Fare))
# Two records no embarked.
titanic = titanic %>% mutate(check = ifelse(Embarked %in% c('C','Q','S'),1,NA))
titanic = na.omit(titanic)
titanic$check = NULL


# split data
set.seed(1234)
ind = sample(2,nrow(titanic),replace = T, prob = c(0.8,0.2))
Ti_train = titanic[ind==1,]
Ti_test = titanic[ind==2,]

#CART modeling
m1 = rpart(Ti_train$Survived~ ., Ti_train[-1], method = "class")
rpart.plot(m1)
summary(m1)

#prediction
p1 = predict(m1,Ti_test,type = "class")
table(Ti_test[,1],p1)

#gain chart
prob_p1 = predict(m1,Ti_test)
prob_p1 = as.data.frame(prob_p1)
prob_p1$actual = Ti_test$Survived
write.csv(prob_p1,"gain_chart.csv")








##Model 2 using random for the Ages
titanic2 = read.csv("Titanic-train.csv")
titanic2$Name = NULL
titanic2$PassengerId = NULL
titanic2$Ticket = NULL
titanic2$Cabin = NULL

#age <1 should be regarded as 1, and.5 shoud be rounded, missing values are replaced by random numbers
titanic2$Age[!is.na(titanic2$Age)][titanic2$Age[!is.na(titanic2$Age)]<1] = 1
titanic2$Age[!is.na(titanic2$Age)][titanic2$Age[!is.na(titanic2$Age)] %% 1 !=0] = round(titanic2$Age[!is.na(titanic2$Age)][titanic2$Age[!is.na(titanic2$Age)] %% 1 !=0])
titanic2$Age[is.na(titanic2$Age)] = sample(1:max(titanic2$Age[!is.na(titanic2$Age)]),length(titanic2$Age[is.na(titanic2$Age)]),replace = T)

# Fare has 15 records of zero, using average fare of class to identify. Detect outliers
fare = titanic2 %>% select(Pclass,Fare) %>% filter(Fare!=0) %>% group_by(Pclass) %>% summarise(mean = mean(Fare))
titanic2 = left_join(titanic2,fare,by = "Pclass")
titanic2$Fare[titanic2$Fare==0] = titanic2$mean[titanic2$Fare==0]
titanic2$mean = NULL

# Two records no embarked.
titanic2 = titanic2 %>% mutate(check = ifelse(Embarked %in% c('C','Q','S'),1,NA))
titanic2 = na.omit(titanic2)
titanic2$check = NULL

# split data
Ti_train2 = titanic2[ind==1,]
Ti_test2 = titanic2[ind==2,]

#CART modeling
m2 = rpart(Ti_train2$Survived~ ., Ti_train2[-1], method = "class")
rpart.plot(m2)
summary(m2)
m2
m1
#prediction
p2 = predict(m2,Ti_test2,type = "class")
table(Ti_test2[,1],p2)

#gain chart
prob_p2 = predict(m2,Ti_test2)
prob_p2 = as.data.frame(prob_p2)
prob_p2$actual = Ti_test2$Survived
write.csv(prob_p2,"gain_chart2.csv")
