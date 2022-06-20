EC1<- read.csv("C:/Users/marie/Desktop/MBAX 6330/Appendix A - N3120.csv")
EC2<- read.csv("C:/Users/marie/Desktop/MBAX 6330/Appendix B - N9318.csv")
##Dummy Code
EC2$isFinance <- ifelse(EC2$vertical == "finance", 1, 0)
EC2$isFitness <- ifelse(EC2$vertical == "fitness", 1, 0)
EC2$isHealthC <- ifelse(EC2$vertical == "healthca", 1, 0)
EC2$isHomeImp <- ifelse(EC2$vertical == "homeimp", 1, 0)
EC2$isLegal <- ifelse(EC2$vertical == "legal", 1, 0)
EC2$isOnline <- ifelse(EC2$vertical == "online", 1, 0)
EC2$isRealEst <- ifelse(EC2$vertical == "realesta", 1, 0)
EC2$isSecurity <- ifelse(EC2$vertical == "security", 1, 0)
EC2$isTherapy <- ifelse(EC2$vertical == "therapy", 1, 0)

#Removing columns that aren't needed
EC1<-subset(EC1,select = -c(ï..vertical,Cust_Psim))
ECMHW<-subset(EC1,select = -c(ï..vertical,Cust_Psim), Cust_MHW ==1)
table(ECMHW$zip)
ECL360<-subset(EC1,select = -c(ï..vertical,Cust_Psim), Cust_L360 ==1)
table(ECL360$zip)
getwd()
write.csv(ECL360,"C:/Users/marie/Downloads/ECl.csv", row.names = FALSE)
ECLobby<-subset(EC1,select = -c(ï..vertical,Cust_Psim), Cust_Lobb ==1)
table(ECLobby$zip)
#Analyzing Variable to see if we should run spotlights or floodlights
# Spotlight Tenure in relation to Referral
table(EC1$tenure) 
#spotlights 3, 5, and 8, the assumption is that if they spend a longer time with the company, they are more favorable to the company (referral)

hist(ECMHW$org_size)


summary(glm(Cust_L360~referral + tenure + referral*tenure,data=EC1, family = binomial ))
summary(glm(Cust_L360~ tenure +referral + referral*tenure,data=EC1, family = binomial ))

EC1spotlight5<-EC1
EC1spotlight3$tenure<-EC1$tenure-3
summary(glm(Cust_L360~tenure + referral + tenure*referral,data=EC1spotlight3, family = binomial ))

EC1spotlight5<-EC1
EC1spotlight5$tenure<-EC1$tenure-5
summary(glm(Cust_L360~tenure + referral + tenure*referral,data=EC1spotlight5, family = binomial ))

### Note that 8 is no longer significantly for referrals
EC1spotlight8<-EC1
EC1spotlight8$tenure<-EC1$tenure-8
summary(glm(Cust_L360~tenure + referral + tenure*referral,data=EC1spotlight8, family = binomial ))

## The ^^^ interaction doesn't seem significant

table(EC1$touches) #Is it meaningful to spotlight test this? 
summary(glm(Cust_L360~referral + touches + referral*touches,data=EC1, family = binomial ))
table(ECMHW$org_size)


### Appendix D - Head Start Script ###
### To make the verticals variable more useful in youranalysis, you need to create dummy variables for verticals
### Each dummy variable will be a binary indicator for whether or not a given client falls within each
### industry/service vertical (0 = no, 1 = yes).
### To create these variables you can run a series of ifelse commands
### (Note: I renamed my datasets EC1 (Appendix A) and EC2 (Appendix B) for ease of scripting)

min(EC1$Psim_vol)

### Creating Training and test sets for EC1
set.seed(3456)
smp_size <- floor(0.75 * nrow(EC1))
EC1Trainindex<-  sample(seq_len(nrow(EC1)), size = smp_size)
EC1train<-EC1[EC1Trainindex,]
EC1test<-EC1[-EC1Trainindex,]

### Storing this: +PsimRev07+PsimRev08+PsimRev09+PsimRev10+PsimRev11+PsimRev12+PsimRev13+PsimRev14+PsimRev15+

##Optimizing Regression model for L360
summary(glm(Cust_L360~.-Cust_MHW-Cust_Lobb-isHomeImp-Psim_ACH-Psim_CC-Psim_mob,data=EC1, family = binomial ))

L360model<-glm(Cust_L360~zip+org_size+tenure+latepay+Psim_vol+Psim_dsf+touches*referral+isFinance+isHealthC+isLegal+isOnline+isRealEst+isSecurity+isTherapy,data=EC1, family = binomial)

summary(glm(Cust_L360~zip+org_size+tenure+latepay+Psim_vol+Psim_dsf+touches*referral+isFinance+isFitness+isHealthC+isLegal+isOnline+isRealEst+isSecurity+isTherapy,data=EC1, family = binomial ))

summary(glm(Cust_L360~.-Cust_MHW-Cust_Lobb,data=EC1, family = binomial ))
#Testing our model
EC2$L360model_prob <- predict(L360model, EC2, type = "response")
nrow(subset(EC1test,EC1test$L360model_prob>.7))
### Model for Market Hardware
MHWModel<-glm(Cust_MHW~zip+org_size+tenure+latepay+Psim_vol+Psim_dsf+touches*referral+isFinance+isHealthC+isLegal+isOnline+isHomeImp+isRealEst+isSecurity+isTherapy,data=EC1)
#before code
summary(glm(Cust_MHW~.-Cust_L360-Cust_Lobb,data=EC1))

EC2$MHWmodel_prob <- predict(MHWModel, EC2, type = "response")
## Model Customer Lobby 
#Before code
summary(glm(Cust_Lobb~.-Cust_L360-Cust_MHW,data=EC1))
#After Code
LobbyModel<-glm(Cust_Lobb~zip+org_size+tenure*latepay+Psim_vol+Psim_dsf+touches*referral+isFinance+isHealthC+isLegal+isOnline+isHomeImp+isRealEst+isSecurity+isTherapy,data=EC1)

EC2$Lobbymodel_prob <- predict(LobbyModel, EC2, type = "response")

nrow(subset(EC1test,EC2$Lobbymodel_prob>.7))
#CustL360- isHomeImp
#CustLobby - isHealthC
#CustMHW - ishomeimp
corEC1<-cor(EC1)



## Code for Question 2 once we build our models
library(dplyr)
EC2$L360model_prob <- predict(L360Model, EC2, type = "response")
EC1000L360 <- arrange(EC2, -L360model_prob)

EC2$Lobbymodel_prob <- predict(LobbyModel, EC2, type = "response")
EC1000Lobby <- arrange(EC2, -Lobbymodel_prob)

EC2$MHWmodel_prob <- predict(MHWModel, EC2, type = "response")
EC1000MWH <- arrange(EC2, MHWmodel_prob)

install.packages('writexl')
library(writexl)
library(readxl)
write_xlsx(EC2, "C:/Users/marie/Downloads/EC2prospects.xlsx")

write.xlsx(EC2, "C:/Users/marie/Downloads/EC2prospects.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
### Now you have indicator variables to use in your analysis.
### Note: If you use these do NOT include the original 'vertical' variable in your analysis!
### One last note (it is important) - when you build your model, you will need to treat one of these vertical dummies as the "reference category" (or a baseline group).  Q: How do you do that?  A: By excluding it from the model.
### So for example, if I left "IsHealthC" (Is the client in Health Care or not) out of the model, and included 
### the other dummy variables, the coefficients on the others would be interpreted as:
# isHomeImp vs. HC 
# isSecurity vs. HC
# isTherapy vs. HC 
# isLegal vs. HC 
# isFinance vs. HC 
# isOnline vs. HC 
# isFitness vs. HC 
# isRealEst vs. HC
### AKA - the coefficient will tell you what is the difference between being in a given vertical VERSUS the 
### reference category.
### Hope this helps!  Happy hunting!
