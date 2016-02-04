library(R.matlab)
library(h2o)
library(hydroGOF)

#convert timestamp of Ciao dataset to 11 values
convert_time_stamp = function(time_stamp) 
{
  if(!require(lubridate)) {
    install.packages("lubridate")
  }
  library(lubridate)
  date = as.Date(as.POSIXct(time_stamp, origin="1970-01-01"))
  res = 0
  res = year(date) - 2000
  if (month(date) > 4) {
    res = res + 1
  }
  res
}

# time_point: TRUE if timestamp is already converted to 1..11
rating_prediction = function(filename = "rating_with_timestamp.mat", time_point = TRUE)
{
  rating = readMat(filename)
  rating = rating$rating
  
  rating = as.data.frame(rating)
  
  colnames(rating) = c("User","Product","Category","Rating","Helpfulnesss","Timestamp")
  
  rating$User = as.factor(rating$User)
  # rating$V2 = as.factor(rating$V2)
  rating$Category = as.factor(rating$Category)
  
  if (time_point == FALSE) {
    rating$Timestamp = sapply(rating$Timestamp, FUN=convert_time_stamp)
  }
  
  train_rating = rating [rating$Timestamp != 11,]
  test_rating = rating [rating$Timestamp == 11,]
  
  localH20 = h2o.init(nthreads = -1)
  
  train_rating_h2o = as.h2o (train_rating)
  test_rating_h2o = as.h2o (test_rating)
  
  print ("All data")
  dnn = h2o.deeplearning(x=c(1:3,5:6),y=4, training_frame = train_rating_h2o, validation_frame = test_rating_h2o)
  
  print (sqrt(dnn@model$validation_metrics@metrics$MSE)) # 1.026661
  
  # Contain product which are already rated before
  rating_new = test_rating[! (test_rating$Product %in% train_rating$Product & test_rating$User %in% train_rating$Use),]
  h2o_rate_new = as.h2o (rating_new)
  
  dnn = h2o.deeplearning(x=c(1:3,5:6),y=4, training_frame = train_rating_h2o, validation_frame = h2o_rate_new)
  print ("Cold start problem")
  print (sqrt(dnn@model$validation_metrics@metrics$MSE)) # 1.006675
  
  # Product never been rated
  rating_old = test_rating[test_rating$Product %in% train_rating$Product,]
  h2o_rate_old = as.h2o (rating_old)
  
  dnn = h2o.deeplearning(x=c(1:3,5:6),y=4, training_frame = train_rating_h2o, validation_frame = h2o_rate_old)
  
  print ("Existing product and users rating prediction")
  print (sqrt(dnn@model$validation_metrics@metrics$MSE)) 
  
  h2o.shutdown(prompt = FALSE)
}

# Utility function
