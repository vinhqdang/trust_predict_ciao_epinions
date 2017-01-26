# library(R.matlab)
# library(h2o)
# library(hydroGOF)
# # library(MBESS)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(R.matlab, h2o, hydroGOF)

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
# max_mem_size: use to configure h2o server, maybe changed based on your computer
# nthreads: use to configure h2o, -1 means all possible CPU cores
# l1, l2: regularization 
# only use for cross-valdiation
rating_prediction = function(filename = "epinions_rating_with_timestamp.mat", time_point = TRUE, 
                             hiddens = c(200,200),
                             evaluation_method = "division",
                             training_periods = 1:10,
                             testing_periods = c(11),
                             max_mem_size = "8g",
                             nthread = 0,
                             learning_rate=0.001,
                             activation_func="RectifierWithDropout",
                             dropout_ratio = 0.5,
                             l1 = 0.00001,
                             l2 = 1e-5,
                             initial_weight_distribution="Uniform",
                             regression_stop = 0.90,
                             stopping_metric="MSE",
                             stopping_tolerance="0.02",
                             stop_rounds=5,
                             nfold = 5,
                             nb_epoch=50,
                             var_importance = FALSE)
{
  dnn = NULL
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
  
  dnn = perform_learning (total_df = rating,
                          eval_way = evaluation_method,
                          hiddens = hiddens,
                          training_periods = training_periods,
                          testing_periods = testing_periods,
                          max_mem_size = max_mem_size,
                          nthread = nthread,
                          learning_rate = learning_rate,
                          activation_func = activation_func,
                          dropout_ratio = dropout_ratio,
                          l1 = l1,
                          l2 = l2,
                          initial_weight_distribution = initial_weight_distribution,
                          regression_stop = 0.9,
                          stopping_metric = stopping_metric,
                          stopping_tolerance = stopping_tolerance,
                          stop_rounds = stop_rounds,
                          nfold = nfold,
                          nb_epoch = nb_epoch,
                          var_importance = var_importance)
  
  dnn
}

# rating prediction with trust information
rate_trust_prediction = function (trust_file = "epinion_trust_with_timestamp.mat",
                                  rating_file = "epinions_rating_with_timestamp.mat",
                                  time_point = TRUE, 
                                  hiddens = c(200,200),
                                  evaluation_method = "division",
                                  training_periods = 1:10,
                                  testing_periods = c(11),
                                  max_mem_size = "8g",
                                  nthread = 0,
                                  learning_rate=0.001,
                                  activation_func="RectifierWithDropout",
                                  dropout_ratio = 0.5,
                                  l1 = 0.00001,
                                  l2 = 1e-5,
                                  initial_weight_distribution="Uniform",
                                  regression_stop = 0.90,
                                  stopping_metric="MSE",
                                  stopping_tolerance="0.02",
                                  stop_rounds=5,
                                  nfold = 5,
                                  nb_epoch=50,
                                  var_importance = FALSE)
{
  trust_data = readMat(trust_file)
  trust_data = as.data.frame(trust_data$trust)
  colnames (trust_data) = c("Trustor","Trustee","Timestamp")
  
  rating = readMat(rating_file)
  rating = rating$rating
  
  rating = as.data.frame(rating)
  
  colnames(rating) = c("User","Product","Category","Rating","Helpfulnesss","Timestamp")
  
  rating$User = rating$User + max(rating$Product) + 1000
  trust_data$Trustor = trust_data$Trustor + max(rating$Product) + 1000
  
  rating$User = as.factor(rating$User)
  # rating$V2 = as.factor(rating$V2)
  rating$Category = as.factor(rating$Category)
  
  if (time_point == FALSE) {
    rating$Timestamp = sapply(rating$Timestamp, FUN=convert_time_stamp)
  }
  
  # set category for friend
  friend_category = max(as.integer(rating$Category)) + 1
  
  trust_data$Rating = 5
  trust_data$Category = friend_category
  
  # reorder the columns
  trust = trust_data[,c(1,2,5,4,3)]
  
  # remove helpfulness
  rating$Helpfulnesss = NULL
  
  # unify col names
  colnames(rating) = c("src","dst","category","rating","timestamp")
  colnames(trust) = c("src","dst","category","rating","timestamp")
  
  # temporary switch category to integer
  rating$category = as.integer(rating$category)
  
  # add Type col to recognize after merge
  rating$Type = "Rating"
  trust$Type = "Trust"
  
  # create a big dataframe
  total_df = rbind (rating, trust)
  
  total_df$category = as.factor(total_df$category)
  total_df$Type = as.factor(total_df$Type)
  
  dnn = perform_learning (total_df = total_df,
                          eval_way = evaluation_method,
                          hiddens = hiddens,
                          training_periods = training_periods,
                          testing_periods = testing_periods,
                          max_mem_size = max_mem_size,
                          nthread = nthread,
                          learning_rate = learning_rate,
                          activation_func = activation_func,
                          dropout_ratio = dropout_ratio,
                          l1 = l1,
                          l2 = l2,
                          initial_weight_distribution = initial_weight_distribution,
                          regression_stop = 0.9,
                          stopping_metric = stopping_metric,
                          stopping_tolerance = stopping_tolerance,
                          stop_rounds = stop_rounds,
                          nfold = nfold,
                          nb_epoch = nb_epoch,
                          var_importance = var_importance)
    
  dnn
}

perform_learning = function (total_df,
                             eval_way = "division",
                             plot_file = "plot.png",
                             hiddens = c(200,200),
                             training_periods = 1:10,
                             testing_periods = c(11),
                             max_mem_size = "8g",
                             nthread = 0,
                             learning_rate=0.001,
                             activation_func="RectifierWithDropout",
                             dropout_ratio = 0.5,
                             l1 = 0.00001,
                             l2 = 1e-5,
                             initial_weight_distribution="Uniform",
                             regression_stop = 0.90,
                             stopping_metric="MSE",
                             stopping_tolerance="0.02",
                             stop_rounds=5,
                             nfold = 5,
                             nb_epoch=50,
                             var_importance = FALSE) {
  h2o.init(nthread = nthread)
  
  if (eval_way == "division") {
    train = total_df [total_df$timestamp %in% training_periods,]
    test = total_df [total_df$timestamp %in% testing_periods & total_df$Type == "Rating",]
    
    train_rating_h2o = as.h2o (train)
    test_rating_h2o = as.h2o (test)
    
    print ("All data")
    dnn = h2o.deeplearning(x=c(1:3,5),y=4, training_frame = train_rating_h2o, 
                           activation = activation_func,
                           validation_frame = test_rating_h2o,
                           hidden = hiddens,
                           epochs = nb_epoch,
                           rate = learning_rate,
                           hidden_dropout_ratios = rep(dropout_ratio, length(hiddens)),
                           l1 = l1,
                           l2 = l2,
                           initial_weight_distribution=initial_weight_distribution,
                           regression_stop = regression_stop,
                           stopping_metric = stopping_metric,
                           stopping_rounds = stop_rounds,
                           variable_importances = var_importance)
    
    png (plot_file)
    plot (dnn)
    dev.off()
    
    rmse_value = sqrt(dnn@model$validation_metrics@metrics$MSE)
    
    # print (ci.rmsea(rmsea = rmse_value, df = nrow(test_rating) - 1, N = nrow(test_rating)))
    
    print (rmse_value) # 1.026661
    
    # Product never been rated
    rating_new = test_rating[! (test_rating$Product %in% train_rating$Product 
                                & test_rating$User %in% train_rating$User),]
    h2o_rate_new = as.h2o (rating_new)
    
    print ("Cold start prediction")
    p_new = h2o.predict(dnn, newdata = h2o_rate_new)
    rmse_value = hydroGOF::rmse(as.vector(rating_new$Rating), as.vector(p_new))
    
    # print (ci.rmsea(rmsea = rmse_value, df = nrow(rating_new) - 1, N = nrow(rating_new)))
    print (rmse_value)
    
    # Contain product which are already rated before
    rating_old = test_rating[test_rating$Product %in% train_rating$Product 
                             & test_rating$User %in% train_rating$User,]
    h2o_rate_old = as.h2o (rating_old)
    
    # only new item
    rating_new_item = test_rating[! (test_rating$Product %in% train_rating$Product),]
    print ("Only new items")
    h2o_rate_new_item = as.h2o (rating_new_item)
    p_new = h2o.predict(dnn, newdata = h2o_rate_new_item)
    rmse_value = hydroGOF::rmse(as.vector(rating_new_item$Rating), as.vector(p_new))
    print (rmse_value)
    
    # only new user
    rating_new_user = test_rating[! (test_rating$User %in% train_rating$User),]
    print ("Only new users")
    h2o_rate_new_item = as.h2o (rating_new_user)
    p_new = h2o.predict(dnn, newdata = h2o_rate_new_item)
    rmse_value = hydroGOF::rmse(as.vector(rating_new_user$Rating), as.vector(p_new))
    print (rmse_value)
    
    print ("Existing product and users rating prediction")
    # print (sqrt(dnn@model$validation_metrics@metrics$MSE)) 
    p_old = h2o.predict(dnn, newdata = h2o_rate_old)
    rmse_value = hydroGOF::rmse(as.vector(rating_old$Rating), as.vector(p_old))
    
    # print (ci.rmsea(rmsea = rmse_value, df = nrow(rating_old) - 1, N = nrow(rating_old)))
    print (rmse_value)
  }
  
  if (eval_way == "LOO") {
    localH20 = h2o.init(nthreads = -1)
    rating_h2o = as.h2o (total_df)
    
    dnn = h2o.deeplearning(x=c(1:3,5),y=4,
                           training_frame = rating_h2o, nfolds = nfold, hidden = hiddens,
                           activation = activation_func,
                           epochs = nb_epoch,
                           rate = learning_rate,
                           hidden_dropout_ratios = rep(dropout_ratio, length(hiddens)),
                           l1 = l1,
                           l2 = l2,
                           initial_weight_distribution=initial_weight_distribution,
                           regression_stop = regression_stop,
                           stopping_metric = stopping_metric,
                           stopping_rounds = stop_rounds,
                           variable_importances = var_importance)
    
    rmse_value = sqrt(dnn@model$validation_metrics@metrics$MSE)
    
    print (rmse_value)
    
    png (plot_file)
    plot (dnn)
    dev.off()
    # h2o.shutdown(prompt = FALSE)
    
    dnn
  }
  
  dnn
}
