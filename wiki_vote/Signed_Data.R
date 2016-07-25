#search_len = 1 means looking to friends
#search_len = 2 means looking to friends of friends also etc.
processData = function (filename = "soc-sign-epinions.txt",nb_test = 20, test_ratio = 0.1, min_subdata_size=200)
{
  
  library (rnn)
  data = read.table (filename, skip = 4, header = TRUE)
  data_len = nrow (data)
  
  test_indexes = sample (1:nrow(data),nb_test)
  
  predict_accs = c()
  
  processing_time = c()
  
  count = 0
  while (count < nb_test) {
    p1 = proc.time()
    i = sample (1:nrow(data),1)
    
    print ("Primary neighbor set")
    cur_edge = data[i,]
    seed_set = c(cur_edge$Trustor, cur_edge$Trustee)
    
    subData = data[(data$Trustor %in% seed_set & !data$Trustee %in% seed_set) |
                     (data$Trustee %in% seed_set & !data$Trustor %in% seed_set) ,]
    seed_set = union (union(seed_set, subData$Trustor), subData$Trustee)
    
    # remove neutral votes
    subData = subData[subData$Sign != 0,]
    
    if (nrow(subData) > min_subdata_size) {
      print ("Remove data from primary")
      subData = subData[sample(nrow(subData), min_subdata_size), ]
    }
    while (nrow(subData) <  min_subdata_size) {
      print ("Adding data")
      new_subData = data[(data$Trustor %in% seed_set & !data$Trustee %in% seed_set) |
                       (data$Trustee %in% seed_set & !data$Trustor %in% seed_set) ,]
      if (nrow(subData) + nrow(new_subData) <= min_subdata_size) {
        subData = rbind(subData, new_subData)
      }
      else {
        select_size = min_subdata_size - nrow(subData)
        new_subData = new_subData[sample(nrow(new_subData), select_size), ]
        subData = rbind(subData, new_subData)
      }
      seed_set = union (union(seed_set, subData$Trustor), subData$Trustee)
      # remove neutral votes
      subData = subData[subData$Sign != 0,]
    }     
    count = count + 1
    print (paste ("Processing test number", count))
    predict_acc = processSubData(subData, test_ratio = test_ratio)
    predict_accs = c(predict_accs, predict_acc)
    print (paste ("Current average accuracy =", mean(predict_accs)))
#     
    p2 = proc.time()
    print (p2 - p1)
    
    processing_time = c(processing_time, (p2-p1))
    print (paste ("Current average processing time =", mean(processing_time)))
  }
  
  print (paste ("Mean of accuracy = ", mean (predict_accs)))
  print (paste ("Standard deviation of accuracy =", sd (predict_accs)))
  
  predict_accs
  
}

processSubData = function (subdf, test_ratio = 0.1)
{
  library (rnn)
  
#   print (head(subdf))
#   print (max(subdf$Trustor))
#   print (max(subdf$Trustee))
  bit_length = ceiling (log2 (max (max(subdf$Trustor), max(subdf$Trustee))))
  # print (bit_length)
  
  X1 = int2bin (subdf$Trustor, length = bit_length)
  X2 = int2bin (subdf$Trustee, length = bit_length)
  Y = int2bin (subdf$Sign, length = bit_length)
#   X1 = subdf$Trustor
#   X2 = subdf$Trustee
#   Y = subdf$Sign
#   print (dim(X1))
#   print (dim (X2))
  predict_values = c()
  
  # use maximum 200 samples for training
  if (round(nrow(subdf) * test_ratio) > 200) {
    test_ratio = 100 / nrow(subdf)
  }
  
  #10% for testing
  test_indexes = sample(1:nrow(subdf),max (2, round(nrow(subdf) * test_ratio)))
  
  correct_outputs = subdf$Sign[test_indexes]
  
  X1_train = X1[-test_indexes,]
  X2_train = X2[-test_indexes,]
  Y_train = Y[-test_indexes,]
  X_train = array (c (X1_train, X2_train), dim = c(dim(X1_train), 2))
  Y_train = array (Y_train, dim = c(dim(Y_train), 1))
#   print (nrow (subdf))
#   print (length(test_indexes))
#   print (dim(X_train))
#   print (dim (Y_train))
  model = trainr (Y = Y_train,
                  X = X_train,
                  learningrate = 0.05,
                  hidden_dim = c(128,128),
                  start_from_end = FALSE,
                  numepochs = 20
                  )
  
  X1_test = X1[test_indexes,]
  X2_test = X2[test_indexes,]
  Y_test = Y[test_indexes,]
  X_test = array(c (X1_test, X2_test), dim = c(dim(X1_test),2))
  Y_test = array(Y_test, dim = c(dim(Y_test),1))
  
  Y_predict = predictr(model, X_test)
  Y_predict = bin2int(Y_predict)
  
  Y_predict1 = ifelse (Y_predict < (2^(bit_length-1)),1,-1)
  Y_predict2 = ifelse (Y_predict != 1, -1,1)
  
  t1 = table (correct_outputs, Y_predict1)
  t2 = table (correct_outputs, Y_predict2)
  
  acc1 = sum (correct_outputs == Y_predict1) / length (correct_outputs)
  acc2 = sum (correct_outputs == Y_predict2) / length (correct_outputs)
  
  print (t1)
  
  print (t2)
  
  print (paste ("Accuracy 1= ", acc1))
  print (paste ("Accuracy 2=", acc2))
  
  acc1
}

# Embeddedness of an edge (link) is the number of nodes that are neighbors of the nodes of that edge. 
edge_embeddedness = function (filename = "soc-sign-epinions.txt", sample_ratio = 10)
{
  data = read.table (filename, skip = 4, header = TRUE)
  
  edge_embeddedness = c()
  
  count = 0
  
  for (i in sort(sample(nrow(data), nrow(data)/sample_ratio))) {
    count = count + 1
    print (paste("Processing element",count,"/",nrow(data)/sample_ratio))
    cur_line = data[i,]
    
    # plus 1 because igraph counts from 1 while the dataset starts from 0
    cur_trustor = cur_line$Trustor
    cur_trustee = cur_line$Trustee
    
    trustor_friends = c (data[data$Trustor==cur_trustor,]$Trustee, data[data$Trustee==cur_trustor,]$Trustor)
    trustor_friends = trustor_friends[trustor_friends != cur_trustee]
    trustee_friends = c (data[data$Trustor==cur_trustee,]$Trustee, data[data$Trustee==cur_trustee,]$Trustor)
    trustee_friends = trustee_friends[trustee_friends != cur_trustor]
    
    common_friends = intersect(trustor_friends, trustee_friends)
    
    cur_edge_embeddedness = length(common_friends)
    
    edge_embeddedness = c (edge_embeddedness, cur_edge_embeddedness)
  }
  
  
  
  # plot 
  x = edge_embeddedness
  
  for (threshold in c(5,10,25)) {
    print (paste (length(x[x<=threshold])," is less than",threshold, "the percentage is", length(x[x<=threshold]) / length(x)))
  }
  library(plyr)
  max_break = round_any(max(x), 50)
  hist_plot (x, breaks = c(1,2,3,4,5,10,25, max_break))
  
  edge_embeddedness
}

# utility functions
# count neighborhood size of edges

countEdgeNeighborSize = function (filename = "soc-sign-epinions.txt", sample_ratio = 10, min_neighbor_size=0)
{
  data = read.table (filename, skip = 4, header = TRUE)
  
  # library(igraph)
  
  #create igraph object from the data frame
  # g = graph.data.frame(d = data)
  
  # include neighborhood size of all edges
  cover_size = c()
  
  count = 0
  
  for (i in sort(sample(nrow(data), nrow(data)/sample_ratio))) {
    count = count + 1
    print (paste("Processing element",count,"/",nrow(data)/sample_ratio))
    cur_line = data[i,]
    
    # plus 1 because igraph counts from 1 while the dataset starts from 0
    cur_trustor = cur_line$Trustor
    cur_trustee = cur_line$Trustee
    
    # minus 1 to remove the link itself
    cur_edge_neighborhood_size = 
      nrow (data[data$Trustor==cur_trustor | data$Trustee==cur_trustor | data$Trustor==cur_trustee | data$Trustee==cur_trustee,]) - 1
    
#     if (cur_edge_neighborhood_size <= 1) {
#       print (paste("i=",i))
#       print (paste("trustor=",cur_trustor))
#       print (paste("trustee=",cur_trustee))
#       readkey()
#     }
    
    cover_size = c (cover_size, cur_edge_neighborhood_size)
  }
  
  # plot 
  x = cover_size[cover_size >= min_neighbor_size]
  
  
  # percentage of edge size smaller than certain values
  for (i in 1:10) {
    threshold = i * 100
    print (paste (length(x[x<=threshold])," is less than",threshold, "the percentage is", length(x[x<=threshold]) / length(x)))
  }
  
  hist_plot (x, breaks = c(10,100,1000))
  
  cover_size
}

# histogram with log scale and percentage
hist_plot = function (x, breaks = c(10,100,1000)) {
  library (ggplot2)
  
  p_hist = ggplot() + aes(x)+ geom_histogram(aes(y = (..count..)/sum(..count..))) + scale_x_log10(breaks=breaks) + scale_y_continuous(labels = scales::percent) + labs (y = "", x = "") + theme(axis.text=element_text(size=44), axis.title=element_text(size=48,face="bold"))
  print (p_hist)
}

# cumulative distribution function
plot_cdf = function (x)
{
  dens = density (x)
  cdf <- cumsum(dens$y * diff(dens$x[1:2]))
  cdf <- cdf / max(cdf) # to correct for the rounding errors
  plot(dens$x,cdf,type="s")
}

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}

plotTrainingTime = function (process_epi, process_sla, process_wiki) {
  plot (x, process_epi, type = "b", pch = 19, col = "red", xlab = "Training size", ylab = "Running time (seconds)", ylim = c(0,15))
  lines(x, process_sla, pch = 18, type = "b", lty = 2, col = "blue")
  lines(x, process_wiki, pch = 17, type = "b", lty = 3, col = "green")
  legend(100, 15, legend=c("Epinions", "Slashdot", "Wikipedia"),col=c("red", "blue", "green"), lty=1:3, cex=0.8, pch = c(19,18,17))
}

plotTrainingTimeAsLayers = function (df) {
  x = 1:4
  plot (x, df[df$training_size==100,]$time, type = "b", pch = 19, col = "red",
        xlab = "# of hidden layers", xaxt = "n", cex.lab=1.5, cex.axis=1.5,
        ylab = "Running time (seconds)", ylim = c(0,max(df$time)+1))
  axis(1,at=1:4,labels = 1:4)
  lines(x, df[df$training_size==200,]$time, pch = 18, type = "b", lty = 2, col = "blue")
  lines(x, df[df$training_size==300,]$time, pch = 17, type = "b", lty = 3, col = "darkcyan")
  legend(1, max(df$time), legend=c("Training size = 100", "Training size = 200", "Training size = 300"),
         col=c("red", "blue", "darkcyan"),
         lty=1:4, cex=1.5, pch = 19:17)
}

comparePredictors = function(x_len, acc1, acc2) {
  len1 = round (x_len * acc1)
  len2 = round (x_len * acc2)
  
  x1 = c(rep(1, len1), rep(0, x_len-len1))
  x2 = c(rep(1, len2), rep(0, x_len-len2))
  
  print (t.test(x1,x2))
}


# fraction of existing triads over possible triads
fractionOfTriads = function(nbNodes, nbTriads) {
  nbTriads / (nbNodes * (nbNodes-1) * (nbNodes - 2) / 6)
}


# using DNN
Sign_dnn = function (filename = "soc-sign-epinions.txt", num_layers=2, max_categorical_features = 1000)
{
  data = read.table (filename, skip = 4, header = TRUE)
  data_len = nrow (data)
  data$Sign = as.factor(data$Sign)
  data$Trustor = as.factor(data$Trustor)
  data$Trustee = as.factor(data$Trustee)
  p1 = proc.time()
  dnn = h2o.deeplearning(x = 1:2,y=3,training_frame = as.h2o(data), hidden = c(400,200), nfolds = 5, 
                         max_categorical_features = max_categorical_features)
  p2 = proc.time()
  proc_time = p2 - p1
  print (proc_time)
  dnn
}