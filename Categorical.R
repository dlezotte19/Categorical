Data2 <- data.frame("Sex" = c("Male","Female"),
                   "Group" = c(rep("LD",2),rep("SED",2)), 
                   "Age" = c(rep("12-14",4),rep("15-16",4),rep("17-18",4)),
                   "Low" = c(79,34,14,5,63,26,32,15,36,16,36,12),
                   "High" = c(18,14,5,8,10,11,3,7,13,1,5,2))

Data.fit <- glm(cbind(High,Low)~Sex + Group + Age, family = binomial, data=Data2)
Data.fit <- glm(cbind(High,Low)~factor(Sex) + factor(Group) + factor(Age), family = binomial, data=Data2)

summary(Data.fit)
drop1(Data.fit,test="Chisq")
Data.fit2 <- glm(cbind(High,Low)~Sex+Group+Age, family = binomial, data = Data2)
anova(Data.fit,Data.fit2,test="Chisq")

confint(Data.fit)

Data.gf[,3] <- rowSums(Data.gf[,1:2])
Data2[13,] <- c(NA,NA,NA,sum(Data2[,4]),sum(Data2[,5]),sum(Data2[,6]))

Observed <- Data2[1:12,4:5]

Expected <- data.frame("Low" = c(76.76,37.98,15.03,10.28,57.77,29.28,27.7,17.41,38.77,13.45,32.44,11.08),
                       "High" = c(20.23,10.01,3.96,2.71,15.23,7.72,7.3,4.59,10.22,3.55,8.55,2.92))
