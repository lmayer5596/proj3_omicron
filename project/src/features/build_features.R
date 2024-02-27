library(data.table)
library(caret)
library(Metrics)

test <- fread('./project/volume/data/raw/Stat_380_test.csv')
train <- fread('./project/volume/data/raw/Stat_380_train.csv')
covar <- fread('./project/volume/data/raw/covar_data.csv')
format <- fread('./project/volume/data/raw/Example_sub.csv')

test$ic50_Omicron <- 0
test$train <- 0
train$train <- 1
master <- rbind(test, train)

master[master == ''] <- NA
master$dose_3[is.na(master$dose_3)] <- 'none'
master$priorSxAtFirstVisit[is.na(master$priorSxAtFirstVisit)] <- 'none'
master$priorSxAtFirstVisitSeverity[is.na(master$priorSxAtFirstVisitSeverity)] <- 'none'
master$Sx_severity_most_recent[is.na(master$Sx_severity_most_recent)] <- 0

master$days_sinceDose3[is.na(master$days_sinceDose3)] <- 0
master[days_sinceDose3 <= 0, days_sinceDose3 := 21]

master$days_dose23interval[is.na(master$days_dose23interval)] <- 0
master[days_dose23interval <= 0, days_dose23interval := 220]

master$days_sinceSxLatest[is.na(master$days_sinceSxLatest)] <- 0
master[days_sinceSxLatest <= 0, days_sinceSxLatest := 400]

master$days_sincePosTest_latest[is.na(master$days_sincePosTest_latest)] <- 0
master[days_sincePosTest_latest <= 0, days_sincePosTest_latest := 28]

train <- merge(master[train == 1], covar, by = 'sample_id')
test <- merge(master[train == 0], covar, by = 'sample_id')

train <- train[, !c('sample_id')]
test <- test[, !c('sample_id', 'ic50_omicron')]

format$ic50_Omicron <- 0

View(test)


#treat negative values as NA- turn them into NA then treat all NA as the same
#there is a window of a too small data or a too big date which wouldn't affect anything
#try 21 for days since dose 3
#add None for missing values in a character column

#after main data is clean- merge with covar then use lecture 9

fwrite(train, './project/volume/data/interim/train.csv')
fwrite(test, './project/volume/data/interim/test.csv')
fwrite(format, './project/volume/data/interim/submit.csv')
