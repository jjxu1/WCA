# Load libraries

suppressMessages(library(ggplot2)) # Data Visualization
suppressMessages(library(readr)) # CSV file I/O, e.g. the read_csv function
suppressMessages(library(corrplot)) #visualization of correlation matrix
suppressMessages(library(gridExtra)) # Grid Graphics
suppressMessages(library(MASS)) # Modern Applied Statistics with S
suppressMessages(library(dummies)) # to create dummy variable
suppressMessages(library(car)) #vif function
suppressMessages(library(ROCR)) # Thresold value

# Load data
attira <- read.csv('~/Documents/WA_Fn-UseC_-HR-Employee-Attrition.csv')

# Remove variables with one value: Employee Count, Over18, StandardHours. Employee Number
attira <- attira[,-c(9,10,22,27)]

# Changing the class of variable from numeric to factor
attira$Education <- factor(attira$Education)
attira$EnvironmentSatisfaction <- factor(attira$EnvironmentSatisfaction)
attira$JobInvolvement <- factor(attira$JobInvolvement)
attira$JobLevel <- factor(attira$JobLevel)
attira$JobSatisfaction <- factor(attira$JobSatisfaction)
attira$PerformanceRating <- factor(attira$PerformanceRating)
attira$RelationshipSatisfaction <- factor(attira$RelationshipSatisfaction)
attira$StockOptionLevel <- factor(attira$StockOptionLevel)
attira$WorkLifeBalance <- factor(attira$WorkLifeBalance)

attira$Attrition <- ifelse(attira$Attrition == "Yes",1,0)
attira <- dummy.data.frame(attira, sep = ".")
# Remove one dummy variable from each factor variable
attira <- attira[,-c(3,7,11,16,25,26,32,37,39,50,51,58,61,65,69,75)]

# Feature selection
vif_output <- lm(Attrition ~., data = attira)
vif_res <- car::vif(vif_output)
summary(vif_res)
print(vif_res)

# Use VIF function to test multicollinearity
vif_names <- names(vif_res)
while(any(vif_res > 2)){
  var_with_max_vif <- names(which(vif_res == max(vif_res)))
  vif_names <- vif_names[!(vif_names) %in% var_with_max_vif]
  def_form <- as.formula(paste("Attrition ~" ,paste(vif_names, collapse = " +"),sep = ""))
  vif_output <- lm(def_form, data = attira)
  vif_res <- car::vif(vif_output)
}
summary(vif_res)
print(vif_res)

# Removing the Multicollinearity variable
attira <- attira[,-c(4,6,7,10,13,25,26,35,36,42,43,47,48,52,55,59,60,61)]

# Split the data into traindata and testdata
set.seed(2017)
train <- sample(1:nrow(attira), nrow(attira)*.7)
test = -train
modeldata <- attira[train,]
validationdata <- attira[test,]

# Fitting the Logistic Regression Model
logmodel <- glm(Attrition ~., family=binomial(link="logit"), data = modeldata)
print(summary(logmodel))

# Accessing the predictive ability of the logistic regression model
log_pred <- predict(logmodel,newdata=validationdata,type='response')
log_pred <- ifelse(log_pred>=0.5,1,0)
caret::confusionMatrix(factor(log_pred),factor(validationdata$Attrition))

# Based upon the p-value of anova
anova(logmodel, test = "Chisq")