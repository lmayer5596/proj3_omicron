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

#changes dose_3 to numeric
master$dose_3[!is.na(master$dose_3)] <- 1
master$dose_3[is.na(master$dose_3)] <- 0
#master[dose_3 == 'BNT162b2', dose_3 := 1]
#master[dose_3 == 'mRNA1272', dose_3 := 2]
#master[dose_3 == 'AZD1222', dose_3 := 3]

master[days_sinceDose2 <= 0, days_sinceDose2 := NA]
master[days_sinceDose3 <= 0, days_sinceDose3 := NA]
master[days_sinceSxLatest <= 0, days_sinceSxLatest := NA]
master[days_sincePosTest_latest <= 0, days_sincePosTest_latest := NA]

master$days_since_recent <- 0
for (i in 1:nrow(master)) {
  master$days_since_recent[i] <- min(master$days_sinceDose2[i], master$days_sinceDose3[i], master$days_sincePosTest_latest[i])
}

#maintains order so rows do not shuffle
setkey(master, sample_id)
setkey(covar, sample_id)

#merges unnamed data
master <- merge(master, covar, all.x = TRUE)

#separates back into train and test
train <- master[train == 1][order(sort_col)]
test <- master[train == 0][order(sort_col)]

#removes unwanted columns
train <- train[, !c('sample_id', 'train', 'sort_col')]#, 'dose_2', 'dose_3')]#, 'days_sinceDose2', 'days_sinceDose3', 'days_sincePosTest_latest')]
test <- test[, !c('sample_id', 'train', 'sort_col')]#, 'dose_2', 'dose_3')]#, 'days_sinceDose2', 'days_sinceDose3', 'days_sincePosTest_latest')]

format$ic50_Omicron <- 0

#moves edited data into the interim folder
fwrite(train, './project/volume/data/interim/train.csv')
fwrite(test, './project/volume/data/interim/test.csv')
fwrite(format, './project/volume/data/interim/submit.csv')

