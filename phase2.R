

### **Importing Libraries**

options(warn=-1)
install.packages("plyr")
install.packages("GGally")
install.packages("caret")
install.packages("ggcorrplot")

library(plyr)
library(ggplot2)
library(GGally)
library(caret)
library(ggcorrplot)
library(pROC)

healthCare <- read.csv("/content/HealthCare.csv")

summary(healthCare)

healthCare$hypertension <- mapvalues(healthCare$hypertension,
                           from = c(0, 1),
                           to = c("No", "Yes"))

healthCare[(healthCare$gender=="Other"),]

healthCare <- healthCare[!(healthCare$gender=="Other"),]

colMeans(is.na(healthCare))

healthCare[c("bmi")][is.na(healthCare[c("bmi")])] <- mean(healthCare$bmi, na.rm=TRUE)

healthCare[c("health_bills")][is.na(healthCare[
  c("health_bills")])] <- median(healthCare$health_bills, na.rm=TRUE)

### __Question 1__
#### __Part A__

n1 <- 300
n2 <- 300
set.seed(123)
smokeRows <- sample(nrow(healthCare), n1)
smokeSample <- healthCare[smokeRows,]

workRows <- sample(nrow(healthCare[-smokeRows,]), n2)
workSample <- healthCare[workRows,]

p1 <- sum(smokeSample$smoking_status == "smokes") / n1
p2 <- sum(workSample$work_type == "Govt_job") / n2

(n1 * (1 - p1) >= 10 && n1 * p1 >= 10)

(n2 * (1 - p2) >= 10 && n2 * p2 >= 10)

SE <- sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
SE


diff <- p1 - p2

CI = 0.95
zStar <- qnorm((1 - CI) / 2, lower.tail = FALSE)
ME <- SE * zStar

low <- diff - ME
up <- diff + ME

print(paste("the 95% CI=(",low,"up to ",up,")"), quote = FALSE)


smokingVsWorkType <- table(smokeSample$smoking_status, workSample$work_type)
smokingVsWorkType

chisq.test(smokingVsWorkType)$expected

chisq.test(smokingVsWorkType)

set.seed(123)
sampleSize <- 12
rows <- sample(1:nrow(healthCare), sampleSize)
smallSample <- healthCare[rows, ]["ever_married"]
pObserved <- sum(smallSample$ever_married == "Yes") / sampleSize
pObserved

set.seed(123)
simCount <- 3000
nullSample <- c(rep(1, 6), rep(0, 6))
simSamples <- replicate(simCount, sample(nullSample, size = sampleSize, replace = TRUE))
proportions <- colSums(simSamples) / sampleSize
pValue <- sum(proportions >= pObserved) / simCount
pValue

### __Question 3__

#### __Part A__

n <- length(healthCare$smoking_status)
smokingDist <- table(healthCare$smoking_status) / n
smokingDist

sampleSize <- 100
set.seed(123)
randomRows <- sample(nrow(healthCare), sampleSize)
randomSample <- healthCare[randomRows,]["smoking_status"]

randomDist <- table(randomSample$smoking_status)
randomDist

chisq.test(randomDist, p=smokingDist)

set.seed(123)
prob <- ifelse(healthCare$smoking_status=="never smoked", 0.7, 0.3)
biasedRows <- sample(nrow(healthCare), sampleSize, prob = prob)
biasedSample <- healthCare[biasedRows,]["smoking_status"]


biasedDist <- table(biasedSample$smoking_status)
biasedDist

chisq.test(biasedDist, p=smokingDist)


nSmoking <- 200
nMarriage <- 200
set.seed(123)
smokingRows <- sample(nrow(healthCare), nSmoking)
smokingSample <- healthCare[smokingRows,]

marriageRows <- sample(nrow(healthCare[-smokingRows,]), nMarriage)
marriageSample <- healthCare[marriageRows,]


marriageVsSmoking <- table(marriageSample$ever_married, smokingSample$smoking_status)
marriageVsSmoking

chisq.test(marriageVsSmoking)$expected

chisq.test(marriageVsSmoking)

### __Question 4__



bmiModel <- lm(health_bills ~ bmi, data = healthCare)
summary(bmiModel)

###### __b__

###### __c__

bmiScatter <- ggplot(healthCare, aes(x = bmi))
bmiScatter <- bmiScatter + geom_point(aes(y = health_bills, color = "Linear"), size = 2, alpha = 0.5)
bmiScatter <- bmiScatter + stat_smooth(aes(x = bmi, y = health_bills, linetype = "Linear Fit"),
              method = "lm", formula = y ~ x, se = F, color = "mediumpurple3") 
bmiScatter <- bmiScatter + scale_color_manual(name = "Relation", values = c("mediumspringgreen", "thistle1")) 
bmiScatter <- bmiScatter + scale_linetype_manual(name = "Fit Type", values = c(2, 2)) 
bmiScatter <- bmiScatter + xlab("BMI")
bmiScatter <- bmiScatter + ylab("Health Bills")
bmiScatter <- bmiScatter + ggtitle("Relation between BMI & Health Bills")
bmiScatter <- bmiScatter + theme_bw()
bmiScatter <- bmiScatter + theme(plot.title = element_text(hjust = 0.5))
bmiScatter

##### __Age__

###### __a__

ageModel <- lm(health_bills ~ age, data = healthCare)
summary(ageModel)

###### __b__

ageScatter <- ggplot(healthCare, aes(x = age))
ageScatter <- ageScatter + geom_point(aes(y = health_bills, color = "Linear"), size = 2, alpha = 0.5)
ageScatter <- ageScatter + stat_smooth(aes(x = age, y = health_bills, linetype = "Linear Fit"),
              method = "lm", formula = y ~ x, se = F, color = "mediumvioletred") 
ageScatter <- ageScatter + scale_color_manual(name = "Relation", values = c("plum2", "thistle1")) 
ageScatter <- ageScatter + scale_linetype_manual(name = "Fit Type", values = c(2, 2)) 
ageScatter <- ageScatter + xlab("Age")
ageScatter <- ageScatter + ylab("Health Bills")
ageScatter <- ageScatter + ggtitle("Relation between Age & Health Bills")
ageScatter <- ageScatter + theme_bw()
ageScatter <- ageScatter + theme(plot.title = element_text(hjust = 0.5))
ageScatter

#### __Part C__


bmiANOVA <- anova(bmiModel)
bmiANOVA


getAdjR2 <- function(anovaTable, pred) {
  n <- length(healthCare[pred])
  SSR <- anovaTable[pred, "Sum Sq"]
  SSE <- anovaTable["Residuals", "Sum Sq"]
  SST <- SSE + SSR
  adjR2 <- 1 - (SSE / SST * (5109 - 1) / (5109 - 1 - 1))
  return(adjR2)
}

getAdjR2(bmiANOVA, "bmi")

ageANOVA <- anova(ageModel)
ageANOVA

getAdjR2(ageANOVA, "age")

#### __Part E__

nSample <- 100
nTrain <- 90
set.seed(123)
sampleRows <- sample(nrow(healthCare), nSample)
sampleData <- healthCare[sampleRows,]

# Split data into train and test
index <- createDataPartition(sampleData$health_bills, p = .90, list = FALSE)
train <- sampleData[index, ]
test <- sampleData[-index, ]

bmiSampleModel <- lm(health_bills ~ bmi, data = train)
summary(bmiSampleModel)

ageSampleModel <- lm(health_bills ~ age, data = train)
summary(ageSampleModel)


confint(bmiSampleModel)

confint(ageSampleModel)


bmiModelPreds <- predict(bmiSampleModel, newdata = test, type=c("response"))


summary(healthCare$health_bills)

actual <- test$health_bills

abs(bmiModelPreds - actual)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    Cor <- abs(cor(x, y))
    txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
    if(missing(cex.cor)) {
        cex.cor <- 0.4 / strwidth(txt)
    }
    text(0.5, 0.5, txt,
         cex = 1 + cex.cor * Cor)
}

pairs(health_bills ~ bmi + age + avg_glucose_level, data=healthCare,
      upper.panel = panel.cor,  
      lower.panel = panel.smooth, col="plum2")

numericVars <- healthCare[c("age", "avg_glucose_level", "bmi", "health_bills")]
corr <- cor(numericVars)
p.mat <- cor_pmat(numericVars)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, p.mat = p.mat, sig.level = 0.05)

selectedModel <- lm(health_bills ~ bmi + age, data = healthCare)
summary(selectedModel)

#### __Part C__


#### __Part E__

full <- lm(health_bills ~ bmi + age + avg_glucose_level, data = healthCare)
summary(full)

step1dropBMI <- lm(health_bills ~ age + avg_glucose_level, data = healthCare)
summary(step1dropBMI)

step1dropAge <- lm(health_bills ~ bmi + avg_glucose_level, data = healthCare)
summary(step1dropAge)

step1dropGlucose <- lm(health_bills ~ bmi + age, data = healthCare)
summary(step1dropGlucose)


full <- lm(health_bills ~ bmi + age + avg_glucose_level, data = healthCare)
summary(full)


step1selectBMI <- lm(health_bills ~ bmi, data = healthCare)
summary(step1selectBMI)

step1selectAge <- lm(health_bills ~ age, data = healthCare)
summary(step1selectAge)

step1selectGlucose <- lm(health_bills ~ avg_glucose_level, data = healthCare)
summary(step1selectGlucose)



step2selectAge <- lm(health_bills ~ bmi + age, data = healthCare)
summary(step2selectAge)

step2selectGlucose <- lm(health_bills ~ bmi + avg_glucose_level, data = healthCare)
summary(step2selectGlucose)


step3selectGlucose <- lm(health_bills ~ bmi + age + avg_glucose_level, data = healthCare)
summary(step3selectGlucose)

step1selectBMI <- lm(health_bills ~ bmi, data = healthCare)
summary(step1selectBMI)

step1selectAge <- lm(health_bills ~ age, data = healthCare)
summary(step1selectAge)

step1selectGlucose <- lm(health_bills ~ avg_glucose_level, data = healthCare)
summary(step1selectGlucose)

step2selectAge <- lm(health_bills ~ bmi + age, data = healthCare)
summary(step2selectAge)

step2selectGlucose <- lm(health_bills ~ bmi + avg_glucose_level, data = healthCare)
summary(step2selectGlucose)

step3selectGlucose <- lm(health_bills ~ bmi + age + avg_glucose_level, data = healthCare)
summary(step3selectGlucose)

best <- full

data <- data.frame(residuals=best$residuals, bmi=healthCare$bmi)
 
bmiRes <- ggplot(data = data, aes(bmi, residuals))
bmiRes <- bmiRes + geom_point(color = "steelblue1")
bmiRes <- bmiRes + stat_smooth(method = lm, color="mediumvioletred")
bmiRes <- bmiRes + ggtitle("Residuals vs. bmi")
bmiRes <- bmiRes + theme_bw()
bmiRes <- bmiRes + theme(plot.title = element_text(hjust = 0.5))
bmiRes

data <- data.frame(residuals=best$residuals, age=healthCare$age)
 
ageRes <- ggplot(data = data, aes(age, residuals))
ageRes <- ageRes + geom_point(color = "mediumspringgreen")
ageRes <- ageRes + stat_smooth(method = lm, color="mediumorchid3")
ageRes <- ageRes + ggtitle("Residuals vs. age")
ageRes <- ageRes + theme_bw()
ageRes <- ageRes + theme(plot.title = element_text(hjust = 0.5))
ageRes

data <- data.frame(residuals=best$residuals, avg_glucose_level=healthCare$avg_glucose_level)
 
glucoseRes <- ggplot(data = data, aes(avg_glucose_level, residuals))
glucoseRes <- glucoseRes + geom_point(color = "plum1")
glucoseRes <- glucoseRes + stat_smooth(method = lm, color="mediumorchid3")
glucoseRes <- glucoseRes + ggtitle("Residuals vs. avg_glucose_level")
glucoseRes <- glucoseRes + theme_bw()
glucoseRes <- glucoseRes + theme(plot.title = element_text(hjust = 0.5))
glucoseRes

modelHist <- ggplot(data=best, aes(best$residuals))
modelHist <- modelHist + geom_histogram(bins=20, fill="darkorchid2")
modelHist <- modelHist + ylab("Frequency")
modelHist <- modelHist + ggtitle("Histogram of Residuals")
modelHist <- modelHist + theme_bw()
modelHist <- modelHist + theme(plot.title = element_text(hjust = 0.5))
modelHist


#**QQ Plot**



modelQQ <- ggplot(best, aes(sample=best$residuals))
modelQQ <- modelQQ + stat_qq(col="dodgerblue") 
modelQQ <- modelQQ + stat_qq_line() 
modelQQ <- modelQQ + ylab("Sample Quantiles")
modelQQ <- modelQQ + xlab("Theoritical Quantiles")
modelQQ <- modelQQ + ggtitle("Normal probability plot of residuals")
modelQQ <- modelQQ + theme_bw()
modelQQ <- modelQQ + theme(plot.title = element_text(hjust = 0.5))
modelQQ


modelRes <- ggplot(data = best, aes(best$fitted, abs(best$residuals)))
modelRes <- modelRes + geom_point(color = "darkorange")
modelRes <- modelRes + stat_smooth(method = lm, color="chartreuse2")
modelRes <- modelRes + ggtitle("Absolute value of residuals vs. fitted")
modelRes <- modelRes + theme_bw()
modelRes <- modelRes + theme(plot.title = element_text(hjust = 0.5))
modelRes

modelRes <- ggplot(data = best, aes(best$fitted, best$residuals))
modelRes <- modelRes + geom_point(color = "gold4")
modelRes <- modelRes + stat_smooth(method = lm, color="deeppink2")
modelRes <- modelRes + ggtitle("Residuals vs. fitted")
modelRes <- modelRes + theme_bw()
modelRes <- modelRes + theme(plot.title = element_text(hjust = 0.5))
modelRes


#### __Part G__

trainControl <- trainControl(method="cv", number=5)


myCrossVal <- train(health_bills ~ bmi + age, data=healthCare,
                  trControl=trainControl, method="lm")
myCrossVal

bestCrossVal <- train(health_bills ~ bmi + age + avg_glucose_level, data=healthCare,
                  trControl=trainControl, method="lm")
bestCrossVal

myModel <- glm(heart_disease ~ age + gender, family = binomial, data = healthCare)
summary(myModel)


exp(cbind(coef(myModel)))

#### __Part B__


maleProb <- function(female) {
    OR <- exp(coef(myModel)["genderMale"])
    return ((OR * (female) / (1 - female)) / (1 + (OR * (female) / (1 - female) )))
}

female <- matrix(c(1:99) / 100, nrow = 1, ncol = 99)
male <- apply(female, 2, maleProb)

data <- data.frame(male=male, female=c(1:99) / 100)
 
orPlot <- ggplot(data = data, aes(female, male))
orPlot <- orPlot + geom_point(color="magenta")
orPlot <- orPlot + ylab("P(heart disease | male)")
orPlot <- orPlot + xlab("P(heart disease | female)")
orPlot <- orPlot + ggtitle("Gender Odds Ratio")
orPlot <- orPlot + theme_bw()
orPlot <- orPlot + theme(plot.title = element_text(hjust = 0.5))
orPlot

predictions <- predict(myModel, type=c("response"))

roc_curve <- roc(heart_disease ~ predictions, data = healthCare, percent=TRUE,  
    partial.auc=c(100, 90),
    partial.auc.correct=TRUE,
    partial.auc.focus="sens",    
    plot=TRUE,
    auc.polygon=TRUE,
    max.auc.polygon=TRUE,
    grid=TRUE,
    print.auc=TRUE,
    show.thres=TRUE)

fullLogit <- glm(heart_disease ~ . - id, family = binomial, data = healthCare)
summary(fullLogit)

step1dropWork <- glm(heart_disease ~ . - id - work_type, family = binomial, data = healthCare)
summary(step1dropWork)

step2dropResidence <- glm(heart_disease ~ . - id - work_type - Residence_type, family = binomial, data = healthCare)
summary(step2dropResidence)

step3dropHyper <- glm(heart_disease ~ . - id - work_type - Residence_type - hypertension, family = binomial, 
    data = healthCare)
summary(step3dropHyper)

step4dropBmi <- glm(heart_disease ~ . - id - work_type - Residence_type - hypertension - bmi, family = binomial, 
    data = healthCare)
summary(step4dropBmi)

step5dropMarried <- glm(heart_disease ~ . - id - work_type - Residence_type - hypertension - bmi - ever_married, 
    family = binomial, data = healthCare)
summary(step5dropMarried)

bestGlm <- glm(heart_disease ~ gender + age + avg_glucose_level + health_bills, 
    family = binomial, data = healthCare)
summary(bestGlm)

prob = predict(bestGlm, type=c("response"))

healthCare$heart <- mapvalues(healthCare$heart_disease,
                           from = c(0, 1),
                           to = c("No", "Yes"))

utility <- function(thresholds) {
    confusion_matrix = table(healthCare$heart, prob <= thresholds)
    utility = confusion_matrix["Yes", "TRUE"] - confusion_matrix["Yes", "FALSE"] +
            confusion_matrix["No", "TRUE"] -  4 * confusion_matrix["No", "FALSE"]
    return (utility)
}

thresholds <- matrix(c(1:5109) / 5110, nrow = 1, ncol = 5110)

utilities = apply(thresholds, 2, utility)

data <- data.frame(utilities=utilities, thresholds=c(1:99) / 100)
 
ggplot(data = data, aes(thresholds, utilities)) + geom_point()

### __Question 7__

summary(healthCare$health_bills)

newHealth <- healthCare
newHealth$high_medical_costs <- healthCare$health_bills > 3032.2

summary(newHealth)

medicalModel <- glm(high_medical_costs ~ .-health_bills - id,family = binomial, data = newHealth)
summary(medicalModel)

step1dropWorkType <- glm(high_medical_costs ~ .-health_bills - id - work_type, family = binomial, data = newHealth)
summary(step1dropWorkType)

step2dropSmoke <- glm(high_medical_costs ~ .-health_bills - id - work_type - smoking_status, 
    family = binomial, data = newHealth)
summary(step2dropSmoke)


step3dropResidence <- glm(high_medical_costs ~ .-health_bills - id - work_type - smoking_status - Residence_type, 
    family = binomial, data = newHealth)
summary(step3dropResidence)


step4dropAge <- glm(high_medical_costs ~ .-health_bills - id - work_type - smoking_status - Residence_type - age, 
    family = binomial, data = newHealth)
summary(step4dropAge)


step5dropMarried <- glm(high_medical_costs ~ gender + hypertension + heart_disease + avg_glucose_level + bmi + stroke, 
    family = binomial, data = newHealth)
summary(step5dropMarried)


step6dropGender <- glm(high_medical_costs ~ hypertension + heart_disease + avg_glucose_level + bmi + stroke, 
    family = binomial, data = newHealth)
summary(step6dropGender)

