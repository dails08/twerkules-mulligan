library(jsonlite)
library(ggplot2)
library(reshape2)
library(class)

raw_data = fromJSON(txt = "uber_data_challenge v2.json")

raw_data$signup_date = as.POSIXct(raw_data$signup_date)
raw_data$last_trip_date = as.POSIXct(raw_data$last_trip_date)

record_count = nrow(raw_data)

current_date = max(raw_data$last_trip_date)

raw_data$active = ifelse(difftime(current_date, raw_data$last_trip_date, units = "days") < 30, 1, 0)
raw_data$active = difftime(current_date, raw_data$last_trip_date, units = "days") < 30
#raw_data$active = factor(raw_data$active)

sum(raw_data$active)/length(raw_data$active)

str(raw_data)
ggplot(melt(raw_data), aes(value)) + geom_histogram(color = "black", fill = "white") + facet_wrap(~variable, scales = "free") + labs(title = "Distribution of variables")
colSums(is.na(raw_data))

raw_data = raw_data[-4]

sum(raw_data$active)/length(raw_data$active)

#shuffle the data
shuffled_index = sample(seq(1, nrow(raw_data)), replace = FALSE)
shuffled = raw_data[shuffled_index,]

#split the data into training and test sets for validation
training_data = shuffled[seq(1, .75*record_count),]
test_data = shuffled[seq(.75*record_count+1, record_count),]
nrow(training_data) + nrow(test_data)

#train a model on the training set
active_model_1 = glm(data = training_data, formula = active ~ trips_in_first_30_days + weekday_pct + avg_dist + avg_rating_by_driver + uber_black_user)

#make predictions against the test set
preds = predict.glm(active_model_1, newdata = test_data) > .5
matches = (predict.glm(active_model_1, newdata = test_data) > .5) == test_data$active
sum(matches, na.rm = TRUE)/length(matches)

#derive the confusion matrix
confusion_matrix_combinations = data.frame(true_class = test_data$active, pred_class = preds)
confusion_matrix = table(confusion_matrix_combinations$true_class, confusion_matrix_combinations$pred_class)
confusion_matrix
ggplot(confusion_matrix_combinations) + geom_bin2d(aes(true_class, pred_class)) + labs(title="active_model_1 Confusion Matrix", x="True Class", y="Predicted Class")

confusion_matrix
#Percentage of non-retained rider correctly identified
100*confusion_matrix[1,1]/(confusion_matrix[1,1] + confusion_matrix[1,2])
#Percentage of riders identified as non-retained who are actually not retained
100*confusion_matrix[1,1]/(confusion_matrix[1,1] + confusion_matrix[2,1])

thresholds = seq(0, 1, .05)

results = data.frame(FNR = c(), TNR = c(), TH = c())

for (threshold in thresholds){
  preds = predict.glm(active_model_1, newdata = test_data) > threshold
  matches = (predict.glm(active_model_1, newdata = test_data) > threshold) == test_data$active
  
  confusion_matrix_combinations = data.frame(true_class = test_data$active, pred_class = preds)
  confusion_matrix = table(confusion_matrix_combinations$true_class, confusion_matrix_combinations$pred_class)
  
  TNR = confusion_matrix[1,1]/sum(confusion_matrix[1,])
  FNR = 1 - (confusion_matrix[2,2]/sum(confusion_matrix[2,]))
  
  result = data.frame(FNR = c(FNR), TNR = c(TNR), TH = c(threshold))
  results = rbind(results, result)
}

ggplot(results) + geom_line(aes(FNR, TNR), size = 1) + geom_abline(slope = 1, intercept = 0, linetype="dashed") + labs(title = "ROC Curve for active_model_1")


?knn

solid_train_data = na.omit(training_data)
solid_test_data = na.omit(test_data)
train_cl = factor(solid_train_data$active)
test_cl = factor(solid_test_data$active)
solid_train_data = solid_train_data[c('trips_in_first_30_days','avg_surge','surge_pct','weekday_pct','avg_dist')]
solid_test_data = solid_test_data[c('trips_in_first_30_days','avg_surge','surge_pct','weekday_pct','avg_dist')]


results = knn(solid_train_data, solid_test_data, cl = train_cl, k=3)

thresholds = seq(1, 20)
results = data.frame(FNR = c(), TNR = c(), TH = c())
start = proc.time()
for (threshold in thresholds){
  preds = knn(solid_train_data, solid_test_data, cl = train_cl, k=threshold)

  confusion_matrix_combinations = data.frame(true_class = test_cl, pred_class = factor(preds))
  confusion_matrix = table(confusion_matrix_combinations$true_class, confusion_matrix_combinations$pred_class)
  
  TNR = confusion_matrix[1,1]/sum(confusion_matrix[1,])
  FNR = 1 - (confusion_matrix[2,2]/sum(confusion_matrix[2,]))
  
  result = data.frame(FNR = c(FNR), TNR = c(TNR), TH = c(threshold))
  results = rbind(results, result)
}
print("Calculation duration:")
proc.time() - start
confusion_matrix
ggplot(results) + geom_line(aes(FNR, TNR), size = 1) + geom_abline(slope = 1, intercept = 0, linetype="dashed") + labs(title = "ROC Curve for knn")


FNR
TNR
