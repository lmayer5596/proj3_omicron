test <- fread('./project/volume/data/interim/test.csv')
train <- fread('./project/volume/data/interim/train.csv')
submit <- fread('./project/volume/data/interim/submit.csv')

#reorder train so similar rows are together
train <- train[order(ic50_Omicron, age, days_sinceDose2)]

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


train_control <- trainControl(method = 'cv', number = folds, search = 'grid', verboseIter = TRUE, returnData = FALSE, returnResamp = 'all', classProbs = FALSE, summaryFunction = RMSE, allowParallel = TRUE)
grid <- expand.grid(eta = .005, 
                    max_depth = seq(25, 4, by = -3), 
                    min_child_weight = seq(1, 61, by = 5), 
                    gamma = seq(0, 2, by = .25), 
                    subsample = seq(1, .6, by = -.5), 
                    colsample_bytree = seq(1, .6, by = -.5))
grid2 <- expand.grid(eta = .005, 
                    max_depth = c(25, 22, 19, 16, 13, 10, 7, 4), 
                    min_child_weight = c(1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 56, 61), 
                    gamma = c(0, .25, .5, .75, 1, 1.25, 1.5, 1.75, 2), 
                    subsample = c(1, .95, .9, .85, .8, .75, .7, .65, .6), 
                    colsample_bytree = c(1, .95, .9, .85, .8, .75, .7, .65, .6))

model = train(x = as.matrix(x.train), y = as.factor(y.train), trControl = train_control, tuneGrid = grid2, method = 'xgbTree')
#hyper_perm_tune8 <- NULL

#cross validation
param <- list(  objective           = "reg:linear",
                booster             = "gbtree",
                eval_metric         = "rmse",
                tree_method         = 'hist',
                eta                 = 0.005,
                max_depth           = 6, #depth- move up from best value
                min_child_weight    = 50, #depth
                gamma               = 5, #depth
                subsample           = .95,#1 means all rows
                colsample_bytree    = .5
)

XGBm <- xgb.cv(params = param, nfold = 10, folds = folds, nrounds = 10000, missing = NA, data = dtrain, print_every_n = 1, early_stopping_rounds = 25)

best_ntrees <- unclass(XGBm)$best_iteration

new_row <- data.table(t(param))

new_row$best_ntrees <- best_ntrees

test_error <- unclass(XGBm)$evaluation_log[best_ntrees, ]$test_rmse_mean
new_row$test_error <- test_error
hyper_perm_tune8 <- rbind(new_row, hyper_perm_tune8)
hyper_perm_tune8

#use model
best_ntrees = 220
watchlist <- list(train = dtrain)
XGBm <- xgb.train(params = param, nrounds = best_ntrees, missing = NA, data = dtrain, watchlist = watchlist, print_every_n = 1)
pred <- predict(XGBm, newdata = dtest)

submit$ic50_Omicron <- pred
#View(submit)
fwrite(submit, './project/volume/data/processed/submit23.csv')
