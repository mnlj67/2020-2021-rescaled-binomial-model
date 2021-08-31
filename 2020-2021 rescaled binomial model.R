# Set up.
PL<- read.csv("C:/Users/chenjun/Desktop/2020-2021 Premier League.csv")
View(PL)

#add rates
PL$rate = PL$FTHG/(PL$FTHG+PL$FTAG)
PL$rate[is.nan(PL$rate)] <- 0.5
View(PL$rate)

#boxplot
boxplot(rate~HS, data=PL,
        main = "Boxplot of rate on HS",
        xlab = "HS",
        ylab = "rate")#positive

boxplot(rate~HST, data=PL,
        main = "Boxplot of rate on HST",
        xlab = "HST",
        ylab = "rate")#positive

boxplot(rate~HY, data=PL,
        main = "Boxplot of rate on HY",
        xlab = "HY",
        ylab = "rate")#negative

boxplot(rate~HR, data=PL,
        main = "Boxplot of rate on HR",
        xlab = "HR",
        ylab = "rate")#negative

#fit the model 
glm1 = glm(rate ~ HS + HY, weights = FTHG+FTAG, data = PL, family = binomial())
##weights = number of trials
summary(glm1)

# Using numbers of each category
glm2 = glm(cbind(FTHG, FTAG) ~ HS + HY, data = PL, family = binomial())
summary(glm2)

#dispersion parameter
glm2$deviance / glm2$df.residual
#[1] 1.402959

#try to add hometeam
glm3 = glm(rate~HS + HY + HomeTeam, weights = FTHG + FTAG, data = PL, family = binomial())
summary(glm3)

#predict
predict(glm3, newdata=data.frame(HS = 20, HY = 2, HomeTeam = 'Fulham'), type  = "response")

predict(glm3, newdata=data.frame(HS = 20, HY = 2, HomeTeam = 'Man City'), type  = "response")

predict(glm3, newdata=data.frame(HS = 20, HY = 2, HomeTeam = 'Man United'), type  = "response")

#accuracy
# Split the data up
matchdata <- cbind(rate=PL$rate, X, FTHG =PL$FTHG, FTAG=PL$FTAG)
View(matchdata)
training <- matchdata[1:150,]
testing <- matchdata[-(1:150),]

# Fit the model on the training data only
glm4 <- glm(rate ~ .-FTHG-FTAG, training, weights = FTHG + FTAG, family = binomial())
summary(glm4)

# Predict on the testing
pred <- predict(glm4, testing, type = "response")

# Now produce a table, where we want to compare the truth to our prediction
res <- table(truth = ifelse(testing$rate>0.5, 1, 0),
             prediction = ifelse(pred > 0.5, 1, 0))
res

# Hence overall accuracy (%) is
(res[1,1]+res[2,2])/sum(res)*100

