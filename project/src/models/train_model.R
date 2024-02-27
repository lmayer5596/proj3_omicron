#takes the edited datasets from the interim folder
test <- fread('./project/volume/data/interim/test.csv')
train <- fread('./project/volume/data/interim/train.csv')
submit <- fread('./project/volume/data/interim/submit.csv')
View(train)

#saves training result column
train_y <- train$ic50_Omicron

#creates a dummy variable to start prediction based on the row_sum column
dummies <- dummyVars(ic50_Omicron ~ ., data = train)
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

test <- data.table(test)
train <- data.table(train)

#cross validation
train <- as.matrix(train)
gl_model <- cv.glmnet(train, train_y, alpha = 1,family = "gaussian")
bestlam <- gl_model$lambda.min

gl_model <- glmnet(train, train_y, alpha = 1,family="gaussian")

saveRDS(gl_model,"./project/volume/models/gl_model.model")

test$ic50_Omicron <- 0
test <- as.matrix(test)

#use model
pred <- predict(gl_model, s = bestlam, newx = test)
predict(gl_model, s = bestlam, newx = test, type = "coefficients")

#puts the final result into the submission file format
submit$ic50_Omicron <- pred

#saves the final submission as a csv
fwrite(submit, './project/volume/data/processed/submit.csv')

#reformats the output into a data.table
train <- data.table(train)
train$ic50_Omicron <- train_y
test <- data.table(test)


#uses a logistic model to fit the train data
glm_model <- glm(ic50_Omicron~., data = train)

#saves dummy variable and model
saveRDS(dummies, './project/volume/models/omicron_glm.dummies')
saveRDS(glm_model, './project/volume/models/omicron_glm.model')

#predicts the result on the test data
test$ic50_Omicron <- predict(glm_model, newdata = test, type = 'response')

#puts the final result into the submission file format
submit$ic50_Omicron <- test$ic50_Omicron

#saves the final submission as a csv
fwrite(submit, './project/volume/data/processed/submit.csv')
