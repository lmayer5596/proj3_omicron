test <- fread('./project/volume/data/interim/test.csv')
train <- fread('./project/volume/data/interim/train.csv')
submit <- fread('./project/volume/data/interim/submit.csv')

#save data before loss
y_train <- train$ic50_Omicron
y_test <- test$ic50_Omicron

#using dummy variables to predict
dummies <- dummyVars(ic50_Omicron~ ., data = train)
x_train <- predict(dummies, newdata = train)
x_test <- predict(dummies, newdata = test)

dtrain <- xgb.DMatrix(x_train, label = y_train, missing = NA)
dtest <- xgb.DMatrix(x_test, missing = NA)

hyper_perm_tune <- NULL

#cross validation
param <- list(  objective           = "reg:linear",
                gamma               =0.00,
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.02,
                max_depth           = 15,
                min_child_weight    = 1,
                subsample           = 1.0,
                colsample_bytree    = 1.0,
                tree_method = 'hist'
)

XGBm <- xgb.cv(params = param, nfold = 5, nrounds = 10000, missing = NA, data = dtrain, print_every_n = 1, early_stopping_rounds = 25)

best_ntrees <- unclass(XGBm)$best_iteration

new_row <- data.table(t(param))

new_row$best_ntrees <- best_ntrees

test_error <- unclass(XGBm)$evaluation_log[best_ntrees, ]$test_rmse_mean
new_row$test_error <- test_error
hyper_perm_tune <- rbind(new_row, hyper_perm_tune)

#use model
watchlist <- list(train = dtrain)
XGBm <- xgb.train(params = param, nrounds = best_ntrees, missing = NA, data = dtrain, watchlist = watchlist, print_every_n = 1)
pred <- predict(XGBm, newdata = dtest)

submit$ic50_Omicron <- pred
View(submit)
