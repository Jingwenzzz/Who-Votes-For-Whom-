load("data.Rda")
df2$v8a <- gsub("conservative", "Conservative", df2$v8a)
df2$v8a <- gsub("labour", "Labour", df2$v8a)
df2$v8a <- gsub("sdp/lib alliance", "SDP–Liberal Alliance", df2$v8a)
df2$v8a <- gsub("scottish national", "Scottish National", df2$v8a)
df2$v8a <- gsub("plaid cymru", "Plaid Cymru", df2$v8a)
df2$v8a <- gsub("green/ecology", "Green and Ecology", df2$v8a)
df2$v8a <- gsub("refused", "Refused", df2$v8a)
df2$v8a <- gsub("none/dont know", "None/do not know", df2$v8a)
df2$v8a[is.na(df2$v8a)] <- "Missing values"

# Figure 1 
library(ggplot2)
DV <- ggplot(df2, aes(x=v8a))
DV+geom_bar(col = "black", fill = "white") + theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank()) + ggtitle("Figure 1: Distribution of Vote Choice") + ylab("Count")

# gender dummy 
df2$female <- NA
df2$female[df2$v58b=="female"] <- 1
df2$female[df2$v58b=="male"] <- 0

#Binary variable "conservative"(conservative = 0 means voted for the Labour, #conservative = 1 means voted for the Conservative)
df2$conservative <- NA
df2$conservative[df2$v8a == "Labour"] <- 0
df2$conservative[df2$v8a == "Conservative"] <- 1

# rename income, put 12 classes(too many) into 7 classes
df2$Income <- NA
df2$Income[df2$v64 == "less than 3000"] <- "1"
df2$Income[df2$v64 == "3000-4999"|df2$v64 =="5000-5999"] <- "2"
df2$Income[df2$v64 == "6000-7999"|df2$v64 == "8000-9999"] <- "3"
df2$Income[df2$v64 == "10000-11999"|df2$v64 == "12000-14999"] <- "4"
df2$Income[df2$v64 == "15000-17999"|df2$v64 == "18000-19999"] <- "5"
df2$Income[df2$v64 == "20000-24999"|df2$v64 == "25000-29999"] <- "6"
df2$Income[df2$v64 == "30000+"] <- "7"
df2$Income[df2$v64 == "refused"|df2$v64 == "dont know"|df2$v64 == "not answered"] <- "4"

#Index for party position, adding up Respondent's political positions #regarding 7 salient topics like defense(v23a), #unemployment/inflation(v28a),taxation/govt(v29a), nationalisation(v34a), #redistribution(v35a), crime - law and order(v39a), welfare(v40a).
#missing values takes the median on the scale, which is 6. 96 is "lefter than K=11", since it is only 1 observation has a value of #96, I assign "11" to "96".
df2$v23a[df2$v23a >96] <- 6
df2$v28a[df2$v28a >96] <- 6
df2$v29a[df2$v29a >96] <- 6
df2$v34a[df2$v34a >96] <- 6
df2$v35a[df2$v35a >96] <- 6
df2$v39a[df2$v39a >96] <- 6
df2$v40a[df2$v40a >96] <- 6
df2$v39a[df2$v39a == 96] <- 11
df2$poli_position = (df2$v23a+df2$v28a+df2$v29a+df2$v34a+df2$v35a+df2$v39a+df2$v40a)/7


# table 1 for data description
library(dplyr)
myvars <- select(df2,"v58c","v55","Income","soc_iss","gov_eval","female","conservative","poli_position")
myvars <- na.omit(myvars)
colnames(myvars) <-c("Age","Education","Income","Welfare","Government","Female","Conservative","Position")
myvars$income <- as.numeric(myvars$Income)
library(stargazer)
stargazer(myvars,type = "latex",out="descriptivenew.html",digits = 2,
          title="Descriptive statistics",df=FALSE,header=FALSE)

# Model 1, 2, 3
model1 <- glm(Conservative~Welfare+Income+Age+Female+Education,family = binomial(link = logit),data = myvars)
summary(model1)

model2 <- glm(Conservative~Position+Female+Education+Age,family = binomial(link = logit),data = myvars)
summary(model2)

model3 <- glm(Conservative~Government+Female+Education+Age,family = binomial(link = logit),data = myvars)
summary(model3)


# Regression table for 3 models
stargazer(model1, model2, model3, header=FALSE, type='latex', title = "GLM(binomial)- Estimation of the models: Hypothesis 1,2 and 3", font.size = "footnotesize")

# Quantities of Interest:Model 1
model1.1 <- glm(Conservative~Welfare+as.numeric(Income)+Age+Female+Education,family = binomial(link = logit),data = myvars)
#summary(model1.1)
m1_mus <- coef(model1.1)
m1_vcov <- vcov(model1.1)
S1 = MASS::mvrnorm(1000,m1_mus,m1_vcov)
income <- min(myvars$Income,na.rm = T):max(myvars$Income,na.rm = T)
values1 <- cbind(1,
                 21,#median of welfare index
                 income,
                 45, # median of age
                 1, #female,
                 16) # median of educ
eta1 <- S1 %*% t(values1)
pi1 <- exp(eta1)/(1+exp(eta1))
p1.mean <- apply(pi1,2,mean)# mean prob of voting conservative of each individual based on the income values we have
pi1.qu <- t(apply(pi1,2,quantile,prob=c(0.025,0.975)))# CI for 1000 simulated values
# plot for model1
plot.df1 <- data.frame(x=income,y=p1.mean,lower=pi1.qu[,1],upper=pi1.qu[,2])

ggplot(plot.df1, aes(x=x, y=y))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3)+
  labs(y="Probability to vote for the Conservative",x="Income")+ggtitle("Figure 2: Model 1")

# Quantities of Interest:Model 2
m2_mus <- coef(model2)
m2_vcov <- vcov(model2)
S2 = MASS::mvrnorm(1000,m2_mus,m2_vcov)
position <- min(myvars$Position,na.rm = T):max(myvars$Position,na.rm = T)
values2 <- cbind(1,
                 position,
                 1, #female,
                 16, # median of educ
                 45) # median of age
eta2 <- S2 %*% t(values2)
pi2 <- exp(eta2)/(1+exp(eta2))
p2.mean <- apply(pi2,2,mean)# mean prob of voting conservative of each individual based on the position values we have
pi2.qu <- t(apply(pi2,2,quantile,prob=c(0.025,0.975)))# CI for 1000 simulated values
# plot for model2
plot.df2 <- data.frame(x=position,y=p2.mean,lower=pi2.qu[,1],upper=pi2.qu[,2])

ggplot(plot.df2, aes(x=x, y=y))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3)+
  labs(y="Probability to vote for the Conservative",x="Political position on salient issues")+ggtitle("Figure 3: Model 2")

#Quantities of Interest:Model 3
m3_mus <- coef(model3)
m3_vcov <- vcov(model3)
S3 = MASS::mvrnorm(1000,m3_mus,m3_vcov)
goveval <- min(myvars$Government,na.rm = T):max(myvars$Government,na.rm = T)
#summary(myvars$Age) # median of age is 45
#summary(myvars$Education) # median of age when completed (continuous) full- time education is 16
values3 <- cbind(1,
                 goveval,
                 1, #female,
                 16, # median of educ
                 45) # median of age
eta3 <- S3 %*% t(values3)
pi3 <- exp(eta3)/(1+exp(eta3))
p3.mean <- apply(pi3,2,mean)# mean prob of voting conservative of each individual based on the soc_iss values we have
pi3.qu <- t(apply(pi3,2,quantile,prob=c(0.025,0.975)))# CI for 1000 simulated values
# plot for model3
plot.df3 <- data.frame(x=goveval,y=p3.mean,lower=pi3.qu[,1],upper=pi3.qu[,2])
ggplot(plot.df3, aes(x=x, y=y))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3)+
  labs(y="Probability to vote for the Conservative",x="Evaluation on government performance")+ggtitle("Figure 4: Model 3")



