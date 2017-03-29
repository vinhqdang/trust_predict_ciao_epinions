# library(R.matlab)
# library(h2o)
# library(hydroGOF)
# # library(MBESS)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(R.matlab, h2o, hydroGOF, scales)

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
  
  rating$Helpfulnesss = NULL
  colnames(rating) = c("src","dst","category","rating","timestamp")
  rating$Type = "Rating"
  
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
  h2o.init(nthread = nthread, max_mem_size = max_mem_size)
  
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
    rating_new = test[! (test$src %in% train[train$Type == "Rating,"]$src 
                                & test$dst %in% train[train$Type == "Rating,"]$dst),]
    h2o_rate_new = as.h2o (rating_new)
    
    print ("Cold start prediction")
    p_new = h2o.predict(dnn, newdata = h2o_rate_new)
    rmse_value = hydroGOF::rmse(as.vector(rating_new$Rating), as.vector(p_new))
    
    # print (ci.rmsea(rmsea = rmse_value, df = nrow(rating_new) - 1, N = nrow(rating_new)))
    print (rmse_value)
    
    # Contain product which are already rated before
    rating_old = test_rating[test$src %in% train[train$Type == "Rating,"]$src 
                             & test$dst %in% train[train$Type == "Rating,"]$dst,]
    h2o_rate_old = as.h2o (rating_old)
    
    # only new item
    rating_new_item = test[!test$dst %in% train[train$Type == "Rating,"]$dst,]
    print ("Only new items")
    h2o_rate_new_item = as.h2o (rating_new_item)
    p_new = h2o.predict(dnn, newdata = h2o_rate_new_item)
    rmse_value = hydroGOF::rmse(as.vector(rating_new_item$Rating), as.vector(p_new))
    print (rmse_value)
    
    # only new user
    rating_new_user = test_rating[! (test_rating$User %in% train_rating$User),]
    print ("Only new users")
    h2o_rate_new_user = as.h2o (rating_new_user)
    p_new = h2o.predict(dnn, newdata = h2o_rate_new_user)
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
  
  pre <- h2o.predict (dnn, test_rating_h2o)
  
  print ("RMSE value of the model = ")
  rmse (as.vector(pre$predict), test$rating)
  
  dnn
}

# some utility functions

# display a histogram of input vector with percentage in y-axis
percentage_hist = function (x) {
  h = hist(x)
  h$density = h$counts/sum(h$counts)*100
  plot(h,freq=FALSE, ylab = "Percentage", xlab = "Rating Score", main = "")
}

hist0 <- function(...,col='skyblue',border=T) hist(...,col=col,border=border) 

# display side by side histogram
multi_hist2 <- function (x1, x2, 
                        group1 = "Epinions", group2 = "Ciao") {
  dat1 = data.frame(x=x1, dataset=group1)
  dat2 = data.frame(x=x2, dataset=group2)
  dat = rbind(dat1, dat2)
  
  ggplot(dat, aes(x, fill=dataset, colour=dataset)) +
    geom_histogram(aes(y=2*(..density..)/sum(..density..)), breaks=seq(0.5,5.5,0.25), 
                   alpha=0.6, 
                   position="identity", lwd=0.2) +
    ggtitle("") +
    scale_y_continuous(labels=percent_format()) +
    ylab("Frequency") + xlab("Rating score") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=16))
}

# how to use multihist
# multi_hist(epinions$Rating, ciao$Rating+0.25)

# 3 histograms side by side
multi_hist3 <- function (x1, x2, x3,
                         group1 = "Epinions1", group2 = "Epinions2", group3 = "Ciao",
                         xlab_text = "Rating Score")
  {
  
  max_value = max (max(x1), max(x2), max(x3))
  min_value = min (min(x1), min(x2), min(x3))
  
  x_range = seq (min_value - 0.5, max_value + 0.5, 0.2)
  
  # shift the data so they won't overlap
  x1 = x1-0.3
  x2 = x2-0.1
  x3 = x3+0.1
  
  library(ggplot2)
  dat1 = data.frame(x=x1, dataset=group1)
  dat2 = data.frame(x=x2, dataset=group2)
  dat3 = data.frame(x=x3, dataset=group3)
  dat = rbind(dat1, dat2, dat3)
  
  
  
  ggplot(dat, aes(x, fill=dataset, colour=dataset)) +
    geom_histogram(aes(y=2*(..density..)/sum(..density..)), 
                   breaks=x_range,
                   alpha=0.6, 
                   position="identity", lwd=0.2) +
    ggtitle("") +
    scale_y_continuous(labels=percent_format()) +
    ylab("Frequency") + xlab(xlab_text) +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=16)) 
}

# process Massa dataset
rating_predit_massa = function () {
  trust = read.table("trust_data_massa.txt", skip = 2, sep = " ")
  rating = read.table("ratings_data_massa.txt", skip = 2, sep = " ", skipNul = TRUE)
}

count_occurences =function (input_vector) {
  res = c()
  for (x in unique (input_vector)) {
    res = c(res, length(which(input_vector==x)))
  }
  res
}

# count trustor distribution of 3 datasets
trustor_dist = function () {
  epi1 = read.table("trust_data_massa.txt", skip = 2, skipNul = TRUE)
  epi2 = as.data.frame(readMat("epinion_trust_with_timestamp.mat")$trust)
  ciao = as.data.frame(readMat("ciao_trust.mat")$trust)
  
  epi1_trustor = count_occurences(epi1$V1)
  epi2_trustor = count_occurences(epi2$V1)
  ciao_trustor = count_occurences(ciao$V1)
}

mean_baseline = function (rating_data, item_id) {
  predict_rate = mean (rating_data$Rating)
  if (item_id %in% rating_data$Product) {
    predict_rate = mean (rating_data[rating_data$Product == item_id,]$Rating)
  }
  predict_rate
}
