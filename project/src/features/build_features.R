#reads in all raw data sets
test <- fread('./project/volume/data/raw/Stat_380_test.csv')
train <- fread('./project/volume/data/raw/Stat_380_train.csv')
covar <- fread('./project/volume/data/raw/covar_data.csv')
format <- fread('./project/volume/data/raw/Example_sub.csv')

#merges all data together into one data table
test$ic50_Omicron <- 0
test$train <- 0
train$train <- 1
master <- rbind(test, train)

#creates column to save row order
master$sort_col <- 1:nrow(master)

#turns all blanks into NA
master[master == ''] <- NA

#turns NAs in categorical variables into 'none' or 0
master$dose_3 <- 1 * (!is.na(master$dose_3))
master[is.na(master$priorSxAtFirstVisit)]$priorSxAtFirstVisit <- 'N'
master$priorSxAtFirstVisit <- 1 * (master$priorSxAtFirstVisit == 'Y')
master$Sx_severity_most_recent[is.na(master$Sx_severity_most_recent)] <- 0

master$priorSxAtFirstVisitSeverity[is.na(master$priorSxAtFirstVisitSeverity)] <- 0
master[priorSxAtFirstVisitSeverity == 'Mild', priorSxAtFirstVisitSeverity := 1]
master[priorSxAtFirstVisitSeverity == 'Moderate', priorSxAtFirstVisitSeverity := 2]
master[priorSxAtFirstVisitSeverity == 'Severe', priorSxAtFirstVisitSeverity := 3]

#master[train == 1, .(avg_omc = mean(ic50_Omicron, na.rm = TRUE)), by = posTest_beforeVisit]
master$posTest_beforeVisit <- 1 * (master$posTest_beforeVisit == 'Yes')
#master[posTest_beforeVisit == 'No', posTest_beforeVisit := 0]
#master[posTest_beforeVisit == 'Yes', posTest_beforeVisit := 1]
#master$posTest_beforeVisit <- 1*(master$posTest_beforeVisit == 'Yes')

#master[priorSxAtFirstVisit == 'N', priorSxAtFirstVisit := 0]
#master[priorSxAtFirstVisit == 'Y', priorSxAtFirstVisit := 1]

#removes negative numbers and zeroes and turns them into a larger number at the upper end of the range
master$days_sinceDose2[is.na(master$days_sinceDose2)] <- 0
master[days_sinceDose2 <= 0, days_sinceDose2 := max(master$days_sinceDose2)]

master$days_sinceDose3[is.na(master$days_sinceDose3)] <- 0
master[days_sinceDose3 <= 0, days_sinceDose3 := max(master$days_sinceDose3)]

master$days_dose23interval[is.na(master$days_dose23interval)] <- 0
master[days_dose23interval <= 0, days_dose23interval := max(master$days_dose23interval)]

master$days_sinceSxLatest[is.na(master$days_sinceSxLatest)] <- 0
master[days_sinceSxLatest <= 0, days_sinceSxLatest := max(master$days_sinceSxLatest)]

master$days_sincePosTest_latest[is.na(master$days_sincePosTest_latest)] <- 0
master[days_sincePosTest_latest <= 0, days_sincePosTest_latest := max(master$days_sincePosTest_latest)]
#View(master)
#splits data back into test and train
#master <- master[, !c('posTest_beforeVisit', 'priorSxAtFirstVisit')]

#maintains order so rows do not shuffle
setkey(master, sample_id)
setkey(covar, sample_id)

master <- merge(master, covar, all.x = TRUE)

train <- master[train == 1][order(sort_col)]
test <- master[train == 0][order(sort_col)]

#removes unwanted columns
train <- train[, !c('sample_id', 'train', 'sort_col')]
test <- test[, !c('sample_id', 'train', 'sort_col')]

format$ic50_Omicron <- 0

#treat negative values as NA- turn them into NA then treat all NA as the same
#there is a window of a too small data or a too big date which wouldn't affect anything
#try 21 for days since dose 3
#add None for missing values in a character column

#after main data is clean- merge with covar then use lecture 9

#moves edited data into the interim folder
fwrite(train, './project/volume/data/interim/train.csv')
fwrite(test, './project/volume/data/interim/test.csv')
fwrite(format, './project/volume/data/interim/submit.csv')

