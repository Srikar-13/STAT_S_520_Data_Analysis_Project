Class1<- read.csv('/Users/saisrikar/S520 Project Data.csv')
Class1
Class2<-read.csv('/Users/saisrikar/S520 Project Data_1.csv')
Class2

summary(Class1)
summary(Class2)

MidtermScore1=Class1$Midterm.Exam.Score
MidtermScore1=MidtermScore1*100/53
MidtermScore1
MidtermScore2=Class2$Midterm.Exam.Score..percentage.
MidtermScore2

summary(MidtermScore1)
summary(MidtermScore2)

boxplot(MidtermScore1,MidtermScore2,names=c("Class_1","Class_2"),ylab="MidtermScores")

plot(density(MidtermScore2))
lines(density(MidtermScore1), col="red")

qqnorm(MidtermScore1,main='Normal Q-Q Plot for Class_1')
qqnorm(MidtermScore2,main='Normal Q-Q Plot for Class_2')

# using log
log_MidtermScore1=log(MidtermScore1)
log_MidtermScore2=log(MidtermScore2)

qqnorm(log_MidtermScore1,main='Normal Log Q-Q Plot for Class_1')
qqnorm(log_MidtermScore2,main='Normal Log Q-Q Plot for Class_2')


#Welch test

t.test(MidtermScore2,MidtermScore1,alternative ="greater")



####################################################################################

#2 Inferential

Female_table=Class1[Class1$Sex=="Female",]
Female_table

Female_MidtermScore1 = Female_table$Midterm.Exam.Score
Female_MidtermScore1=Female_MidtermScore1*100/53
summary(Female_MidtermScore1)

Female_ProblemSetScore1=Female_table$Problem.Sets.Score..percentage.
summary(Female_ProblemSetScore1)

#sum_scores_avg_fe<- sum(Female_table$Midterm.Exam.Score)/nrow(Female_table)

Male_table=Class1[Class1$Sex=="Male",]
Male_table
Male_MidtermScore1 = Male_table$Midterm.Exam.Score
Male_MidtermScore1=Male_MidtermScore1*100/53
summary(Male_MidtermScore1)

Male_ProblemSetScore1=Male_table$Problem.Sets.Score..percentage.
summary(Male_ProblemSetScore1)

#sum_scores_avg_ma<- sum(Male_table$Midterm.Exam.Score)/nrow(Male_table)


# I've calculated the total sum of scores in each section gender wise and divided
# that by total no of male or female students respectively to obtain the averages
# gender wise. Based on the my observation is that the average score of males seems 
# to be higher than the females

boxplot(Female_MidtermScore1,Male_MidtermScore1,names=c("Female","Male"),ylab="MidtermScores")

plot(density(Male_MidtermScore1))
lines(density(Female_MidtermScore1), col="red")

qqnorm(Female_MidtermScore1,main='Normal Q-Q Plot for Female')
qqnorm(Male_MidtermScore1,main='Normal Q-Q Plot for Male')

# using log
log_Female_MidtermScore1=log(Female_MidtermScore1)
log_Male_MidtermScore1=log(Male_MidtermScore1)

qqnorm(log_Female_MidtermScore1,main='Normal Log Q-Q Plot for Female')
qqnorm(log_Male_MidtermScore1,main='Normal Log Q-Q Plot for Male')


#Welch test

t.test(Male_MidtermScore1,Female_MidtermScore1)


# Problemset score male and female 

boxplot(Female_ProblemSetScore1,Male_ProblemSetScore1,names=c("Female","Male"),ylab="ProblemSetScores")

plot(density(Female_ProblemSetScore1))
lines(density(Male_ProblemSetScore1), col="red")

qqnorm(Female_ProblemSetScore1,main='Normal Q-Q Plot for Female')
qqnorm(Male_ProblemSetScore1,main='Normal Q-Q Plot for Male')

# using log
log_Female_ProblemSetScore1=log(Female_ProblemSetScore1)
log_Male_ProblemSetScore1=log(Male_ProblemSetScore1)

qqnorm(log_Female_ProblemSetScore1,main='Normal Log Q-Q Plot for Female')
qqnorm(log_Male_ProblemSetScore1,main='Normal Log Q-Q Plot for Male')


#Welch test

t.test(Male_ProblemSetScore1,Female_ProblemSetScore1)


#########################################################################################

#3 Regression

plot(ProblemSetScore1, MidtermScore1, pch="*")

plot(ProblemSetScore2, MidtermScore2, pch="*")

sd(ProblemSetScore1)
sd(MidtermScore1)
cor(ProblemSetScore1,MidtermScore1)

par(mfrow=c(1,2))
qqnorm(ProblemSetScore1, main="Normal QQ plot of ProblemSetScore1") 
qqnorm(MidtermScore1, main="Normal QQ plot of MidtermScore1")

# abline(a, b) graphs a line with intercept a # and slope b
b = sd(MidtermScore1) / sd(ProblemSetScore1) ### THIS IS WRONG 
a = mean(MidtermScore1) - b * mean(ProblemSetScore1) 
plot(ProblemSetScore1, MidtermScore1, pch="*") 
abline(a, b, col="blue")

# Cor line 
b = cor(ProblemSetScore1, MidtermScore1) * sd(MidtermScore1) / sd(ProblemSetScore1) 
a = mean(MidtermScore1) - b * mean(ProblemSetScore1)
plot(ProblemSetScore1, MidtermScore1, pch="*")
abline(a, b, col="red")

quantile(ProblemSetScore1, 0.95)
quantile(MidtermScore1, 0.95) 

h = quantile(ProblemSetScore1, 0.95) 
a+b*h

# using class2 as there is zero correlation in class1
sd(ProblemSetScore2)
sd(MidtermScore2)
cor(ProblemSetScore2,MidtermScore2)

par(mfrow=c(1,2))
qqnorm(ProblemSetScore2, main="Normal QQ plot of ProblemSetScore2") 
qqnorm(MidtermScore2, main="Normal QQ plot of MidtermScore2")

# abline(a, b) graphs a line with intercept a # and slope b
b = sd(MidtermScore2) / sd(ProblemSetScore2) ### THIS IS WRONG 
a = mean(MidtermScore2) - b * mean(ProblemSetScore2) 
plot(ProblemSetScore2, MidtermScore2, pch="*") 
abline(a, b, col="blue")

# Cor line 
b = cor(ProblemSetScore2, MidtermScore2) * sd(MidtermScore2) / sd(ProblemSetScore2) 
a = mean(MidtermScore2) - b * mean(ProblemSetScore2)
plot(ProblemSetScore2, MidtermScore2, pch="*")
abline(a, b, col="red")

quantile(ProblemSetScore2, 0.95)
quantile(MidtermScore2, 0.95)

h = quantile(ProblemSetScore2, 0.95) 
a+b*h
# This value is lesser than the 95percentile of the midtermscore2.











