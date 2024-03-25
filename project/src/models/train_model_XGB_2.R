test <- fread('./project/volume/data/interim/test.csv')
train <- fread('./project/volume/data/interim/train.csv')
submit <- fread('./project/volume/data/interim/submit.csv')

#reorder train so similar rows are together
train <- train[order(ic50_Omicron, age, days_sinceDose2)]

# subset out only the columns to model
drops <- c('age', 'sex', 'centre', 'Sx_severity_most_recent', 'priorSxAtFirstVisitSeverity', 'dose_2')

train <- train[, !drops, with = FALSE]
test <- test[, !drops, with = FALSE]

#save data before loss
y.train <- train$ic50_Omicron
y.test <- test$ic50_Omicron

#using dummy variables to predict
dummies <- dummyVars(ic50_Omicron~ ., data = train)
x.train <- predict(dummies, newdata = train)
x.test <- predict(dummies, newdata = test)

dtrain <- xgb.DMatrix(x.train, label = y.train, missing = NA)
dtest <- xgb.DMatrix(x.test, missing = NA)

#control cv split
num_folds <- 10
rows_per_fold <- nrow(train) / num_folds

folds <- lapply(1:num_folds, function(fold) {
  ((fold - 1) * rows_per_fold + 1):(fold * rows_per_fold)
})

hyper_perm_tune <- NULL

#cross validation
param <- list(  objective           = "reg:linear",
                booster             = "gbtree",
                eval_metric         = "rmse",
                tree_method         = 'hist',
                eta                 = 0.005,
                max_depth           = 5, #depth- move up from best value
                min_child_weight    = 93, #depth
                gamma               = .05, #depth
                subsample           = 1,#1 means all rows
                colsample_bytree    = .4
)

XGBm <- xgb.cv(params = param, nfold = 10, folds = folds, nrounds = 10000, missing = NA, data = dtrain, print_every_n = 1, early_stopping_rounds = 25)

best_ntrees <- unclass(XGBm)$best_iteration

new_row <- data.table(t(param))

new_row$best_ntrees <- best_ntrees

test_error <- unclass(XGBm)$evaluation_log[best_ntrees, ]$test_rmse_mean
new_row$test_error <- test_error
hyper_perm_tune <- rbind(new_row, hyper_perm_tune)
hyper_perm_tune

#use model
#best_ntrees = 220
watchlist <- list(train = dtrain)
XGBm <- xgb.train(params = param, nrounds = best_ntrees, missing = NA, data = dtrain, watchlist = watchlist, print_every_n = 1)
pred <- predict(XGBm, newdata = dtest)

submit$ic50_Omicron <- pred
#View(submit)
fwrite(submit, './project/volume/data/processed/submit26.csv')
