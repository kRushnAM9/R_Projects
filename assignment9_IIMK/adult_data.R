##Loading data
adult <- read.csv("/Users/chiliveruramakrishna/Downloads/adult.csv")

## saving data
write.csv(adult,"data/adult.csv")
saveRDS(adult,"data/adult.rds")

## Loading packages for data analysis
library(caret)
library(tidymodels)
library(ggplot2)
library(e1071)
library(randomForest)
library(xgboost)
library(rpart)
library(dplyr)
library(tidyr)
library(janitor)
library(recipes)
library(stringr)
library(yardstick)
library(pROC)
library(vip)
library(corrplot)
library(umap)
library(corrplot)
library(recipes)
library(tidymodels)
library(tidyverse)
library(magrittr)
library(kernlab)

##basic structure overview
View(adult)
summary(adult)
dim(adult)
glimpse(adult)
str(adult)

##data types and classes
class(adult)
typeof(adult)

## Descriptive stat
is.na(adult)
glimpse(adult$workclass)

## replacig ? with na so that we can have a clear understanding
adult <- adult %>%
  mutate(across(everything(),~na_if(.x,"?")))

adult[adult == "?"] <- NA

## Checking descriptive stat 
is.na(adult)
colSums(is.na(adult))
str(adult)

## dropping of na values
adult <- na.omit(adult)

## Descriptive statistics
sapply(adult, mean)
sapply(adult, median)

summary(adult$workclass)
prop.table(table(adult$workclass))*100
round(prop.table(table(adult$education))*100,2)
round(prop.table(table(adult$education.num))*100,2)
round(prop.table(table(adult$marital.status))*100,2)
round(prop.table(table(adult$relationship))*100,2)
round(prop.table(table(adult$sex))*100,2)
round(prop.table(table(adult$race))*100,2)
prop.table(table(adult$income))

## one - hot key encoding
adult$sex <- ifelse(adult$sex == "Male",1,0)
view(adult)

## Grouping other cols
#education
adult$education_group <- case_when(
  adult$education %in% c("Preschool","1st-4th","5th-6th","7th-8th","9th","10th","11th","12th","HS-grad","Some-college") ~ "HS_or_below",
  adult$education == "Bachelors" ~ "Bachelors",
  adult$education %in% c("Assoc-acdm","Assoc-voc") ~ "Assoc",
  adult$education == "Masters" ~ "Masters",
  adult$education == "Prof-school" ~ "Prof-school",
  adult$education == "Doctorate" ~ "Doctorate"
)

#employment
adult$emp_status <- case_when(
  adult$workclass %in% c("Federal-gov","Local-gov","State-gov") ~ "Public",
  adult$workclass %in% c("Self-emp-not-inc","Self-emp-inc") ~ "Self-emp",
  adult$workclass == "Private" ~ "Private",
  TRUE ~ "Other"
)

#Marital status
adult$marital_status <- case_when(
  adult$marital.status %in% c("Married-AF-spouse","Married-civ-spouse") ~ "marrried",
  adult$marital.status %in% c("Divorced","Separated","Married-spouse-absent","Widowed") ~ "prvsly_married",
  adult$marital.status == "Never-married" ~ "single"
)

#realtionship
adult$realtionships <- case_when(
  adult$relationship %in% c("Husband","Wife") ~ 'Spouse',
  adult$relationship == "Not-in-family" ~ "Not-in-family",
  adult$relationship %in% c("Own-child","Other-relative") ~ "in-family",
  adult$relationship == "Unmarried" ~ 'Single'
)

#occupation
adult$occupations <- case_when(
  adult$occupation %in% c("Priv-house-serv","Protective-serv","Tech-support","Handlers-cleaners","Transport-moving","Craft-repair") ~ "Services",
  adult$occupation %in% c("Machine-op-inspct","Sales","Adm-clerical") ~ "Operations",
  adult$occupation %in% c("Exec-managerial","Prof-specialty") ~ "Management",
  adult$occupation == "Farming-fishing" ~ "Agri",
  adult$occupation == "Armed-Forces" ~ "Defence",
  TRUE ~ "other"
)

adult$country <- if_else(adult$native.country %in% c("United-States","Outlying-US(Guam-USVI-etc)"), "US", "Non-US")


adult$income <- ifelse(adult$income == ">50K",1,0)

adult$race <- case_when(
  adult$race %in% c("Amer-Indian-Eskimo","Asian-Pac-Islander") ~ "EI",
  adult$race == "White" ~ "White",
  adult$race == 'Black' ~ "Black",
  TRUE ~ "other"
)


## so the data has been categorized, now selecting the cleaned cols for analysis
adult_income <- adult %>%
  select(age,sex,race,education_group,emp_status,marital_status,realtionships,occupations,country,income,capital.gain,capital.loss,hours.per.week)
View(adult_income)

adult_income$capital.gain <- if_else(adult_income$capital.gain > 0,1,0)
adult_income$capital.loss <- if_else(adult_income$capital.loss > 0,1,0)

## hours per week standardisation
adult_income$hrs_per_week <- (adult_income$hours.per.week - min(adult_income$hours.per.week))/ (max(adult_income$hours.per.week)-min(adult_income$hours.per.week))

adult_income <- adult_income %>% select (-hours.per.week)

adult_incomes <- adult_income
View(adult_incomes)
adult_incomes$age <- sqrt(adult_incomes$age)


#checking whether there are any NA values or infinte NAN values
colSums(is.na(adult_incomes)) #checks for na values
sapply(adult_incomes,function(x) any(is.infinite(x)|is.na(x))) #checks for inifinte NaN values

#changing the income col into a factor
adult_incomes$income <- as.factor(adult_incomes$income)


str(adult_income)
str(adult_incomes)
View(adult_incomes_r)

## multi labelling of data using recipes
rec <- recipe(income~.,data = adult_incomes) %>%
  step_dummy(all_nominal_predictors(),one_hot = TRUE) %>%
  step_normalize(all_numeric_predictors())

rec_prep <- prep(rec)
adult_incomes_r <- bake(rec_prep,new_data = NULL) 

## Lets do the SVM
mymodel <- svm(income~.,data = adult_incomes_r)
summary(mymodel)

mymodel1 <- svm(income~.,adult_incomes)
summary(mymodel1)

pred <- predict(mymodel,adult_incomes) #this isn't working caz the training and testing datasets are same! C'mon

## Trying another method - tidymodels() 

#splitting data into training and testing datasets
set.seed(123)
split <- initial_split(adult_incomes,prop = 0.8,strata = income)
train_data <- training(split)
test_data <- testing(split)

#defining recipe on raw data
rec <- recipe(income~.,data = adult_incomes) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors())

#defining model
svm_spec <- svm_linear(mode = "classification") %>%
  set_engine("kernlab")

#building workflow
svm_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(svm_spec)

#fitting model on raw data
svm_fit <- svm_wf %>%
  fit(data = train_data)


# doing predictions
predictions <- predict(svm_fit,new_data = test_data)

# adding predictions back to test set:
test_results <- test_data %>%
  select(income) %>%
  bind_cols(predictions)

# finding the accuracy and confusuin matrics metrics
metrics(test_results, truth = income, estimate = .pred_class)
conf_mat(test_results, truth = income, estimate = .pred_class)

### Now lets use some functions like yardstick, pROC and ROCR
#yardstick
pred_class <- predict(svm_fit, new_data = test_data)
pred_prob <- predict(svm_fit, new_data = test_data, type = "prob")

results <- bind_cols(
  test_data %>% select(income),
  pred_class,
  pred_prob
)

#Accuracy,Precision,Recall and F1
metrics(results, truth = income, estimate = .pred_class)

conf_mat(results, truth = income, estimate = .pred_class)

f_meas(results, truth = income, estimate = .pred_class)
precision(results, truth = income, estimate = .pred_class)
recall(results, truth = income, estimate = .pred_class)

#ROC curve
roc_curve(results, truth = income, ".pred_0") %>%
  autoplot()

#table format
library(gt)

results %>%
  mutate(correct = income == .pred_class) %>%
  slice_sample(n = 10) %>%
  gt()
