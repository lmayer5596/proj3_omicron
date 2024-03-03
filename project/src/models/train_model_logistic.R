#takes the edited datasets from the interim folder
test <- fread('./project/volume/data/interim/test.csv')
train <- fread('./project/volume/data/interim/train.csv')
submit <- fread('./project/volume/data/interim/submit.csv')

#saves training result column
train_y <- train$ic50_Omicron

#creates a dummy variable to start prediction based on the row_sum column
dummies <- dummyVars(ic50_Omicron ~ ., data = train)
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

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
