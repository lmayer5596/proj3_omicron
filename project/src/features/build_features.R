library(data.table)
library(caret)
library(Metrics)

test <- fread('./project/volume/data/raw/Stat_380_test.csv')
train <- fread('./project/volume/data/raw/Stat_380_train.csv')
covar <- fread('./project/volume/data/raw/covar_data.csv')
format <- fread('./project/volume/data/raw/Example_sub.csv')

View(test)
train <- merge(train, covar, by = 'sample_id')
test <- merge(test, covar, by = 'sample_id')

train <- train[, !c('sample_id')]
test <- test[, !c('sample_id')]
test$ic50_Omicron <- 0

format$ic50_Omicron <- 0

fwrite(train, './project/volume/data/interim/train.csv')
fwrite(test, './project/volume/data/interim/test.csv')
fwrite(format, './project/volume/data/interim/submit.csv')
