#takes the edited datasets from the interim folder
test <- fread('./project/volume/data/interim/test.csv')
train <- fread('./project/volume/data/interim/train.csv')
submit <- fread('./project/volume/data/interim/submit.csv')

#saves training result column
train_y <- train$ic50_Omicron
test$ic50_Omicron <- 0

#creates a dummy variable to start prediction based on the row_sum column
master <- rbind(train, test)
dummies <- dummyVars(ic50_Omicron ~ ., data = master)
train <- predict(dummies, newdata = train)
test <- predict(dummies, newdata = test)

#cross validation
train <- as.matrix(train)
trest <- as.matrix(test)

gl_model <- cv.glmnet(train, train_y, alpha = 0.75, family = "gaussian")
bestlam <- gl_model$lambda.min

gl_model <- glmnet(train, train_y, alpha = 0.75, family="gaussian")

plot_glmnet(gl_model)

saveRDS(gl_model,"./project/volume/models/gl_model.model")

#use model
pred <- predict(gl_model, s = bestlam, newx = test)
predict(gl_model, s = bestlam, newx = test, type = "coefficients")

#puts the final result into the submission file format
submit$ic50_Omicron <- pred
View(submit)
mean(submit$ic50_Omicron)
#saves the final submission as a csv
#fwrite(submit, './project/volume/data/processed/submit10.csv')
