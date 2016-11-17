library(R.matlab)
library(h2o)
library(hydroGOF)
# library(MBESS)

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
# evaluation_method = "division" (use 10 parts for train and 1 for test), "LOO" (leave one out)
# testing_periods: periods will be in test, other will be used for training
rating_prediction = function(filename = "epinions_rating_with_timestamp.mat", time_point = TRUE, 
                             hiddens = c(200,200),
                             epochs = 100,
                             evaluation_method = "division",
                             testing_periods = c(11))
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
  
  if (evaluation_method == "division") {
    train_rating = rating [!rating$Timestamp %in% testing_periods,]
    test_rating = rating [rating$Timestamp %in% testing_periods,]
    
    localH20 = h2o.init(nthreads = -1)
    
    train_rating_h2o = as.h2o (train_rating)
    test_rating_h2o = as.h2o (test_rating)
    
    print ("All data")
    dnn = h2o.deeplearning(x=c(1:3,5:6),y=4, training_frame = train_rating_h2o, 
                           validation_frame = test_rating_h2o,
                           hidden = hiddens,
                           epochs = epochs)
    
    rmse_value = sqrt(dnn@model$validation_metrics@metrics$MSE)
    
    # print (ci.rmsea(rmsea = rmse_value, df = nrow(test_rating) - 1, N = nrow(test_rating)))
    
    print (rmse_value) # 1.026661
    
    
    # Product never been rated
    rating_new = test_rating[! (test_rating$Product %in% train_rating$Product & test_rating$User %in% train_rating$Use),]
    h2o_rate_new = as.h2o (rating_new)
    
    #   dnn = h2o.deeplearning(x=c(1:3,5:6),y=4, training_frame = train_rating_h2o, 
    #                          validation_frame = h2o_rate_new,
    #                          hidden = hiddens,
    #                          epochs = epochs)
    #   print ("Cold start problem")
    #   print (sqrt(dnn@model$validation_metrics@metrics$MSE)) # 1.006675
    
    print ("Cold start prediction")
    p_new = h2o.predict(dnn, newdata = h2o_rate_new)
    rmse_value = hydroGOF::rmse(as.vector(rating_new$Rating), as.vector(p_new))
    
    # print (ci.rmsea(rmsea = rmse_value, df = nrow(rating_new) - 1, N = nrow(rating_new)))
    print (rmse_value)
    
    # Contain product which are already rated before
    rating_old = test_rating[test_rating$Product %in% train_rating$Product & test_rating$User %in% train_rating$Use,]
    h2o_rate_old = as.h2o (rating_old)
    
    #   dnn = h2o.deeplearning(x=c(1:3,5:6),y=4, training_frame = train_rating_h2o, 
    #                          validation_frame = h2o_rate_old,
    #                          hidden = hiddens,
    #                          epochs = epochs)
    
    print ("Existing product and users rating prediction")
    # print (sqrt(dnn@model$validation_metrics@metrics$MSE)) 
    p_old = h2o.predict(dnn, newdata = h2o_rate_old)
    rmse_value = hydroGOF::rmse(as.vector(rating_old$Rating), as.vector(p_old))
    
    # print (ci.rmsea(rmsea = rmse_value, df = nrow(rating_old) - 1, N = nrow(rating_old)))
    print (rmse_value)
    
    h2o.shutdown(prompt = FALSE)
  }
  
  if (evaluation_method == "LOO") {
    localH20 = h2o.init(nthreads = -1)
    rating_h2o = as.h2o (rating)
    
    dnn = h2o.deeplearning(x=c(1:3,5:6),y=4,
                           training_frame = rating_h2o, nfolds = nrow(rating), hidden = hiddens,
                           epochs = epochs)
    
    rmse_value = sqrt(dnn@model$validation_metrics@metrics$MSE)
    
    print (rmse_value)
    
    h2o.shutdown(prompt = FALSE)
  }
}

trust_prediction = function (filename = "epinion_trust_with_timestamp.mat")
{
  h2o.init()
  
  trust_data = readMat(filename)
  trust_data = as.data.frame(trust_data$trust)
  colnames (trust_data) = c("Trustor","Trustee","Timestamp")
  
  trust_train = trust_data [trust_data$Timestamp != 11,]
  trust_test = trust_data [trust_data$Timestamp == 11,]
  
  h2o_trust_train = as.h2o (trust_train)
  h2o_trust_test = as.h2o (trust_test)
  
  # dnn = h2o.deeplearning(x=1:2,y=3,training_frame = h2o_trust_train, validation_frame = h2o_trust_test)
  
  rating = readMat("epinions_rating_with_timestamp.mat")
  rating = rating$rating
  
  rating = as.data.frame(rating)
  
  colnames(rating) = c("User","Product","Category","Rating","Helpfulnesss","Timestamp")
  
  rating$User = as.factor(rating$User)
  # rating$V2 = as.factor(rating$V2)
  rating$Category = as.factor(rating$Category)
  
  h2o.shutdown(prompt = TRUE)
}
