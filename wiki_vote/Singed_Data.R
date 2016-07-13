#search_len = 1 means looking to friends
#search_len = 2 means looking to friends of friends also etc.
processData = function (filename = "soc-sign-epinions.txt",nb_test = 20, test_ratio = 0.1, search_len = 1, min_subdata_size=200)
{
  library (rnn)
  data = read.table (filename, skip = 4, header = TRUE)
  data_len = nrow (data)
  
  test_indexes = sample (1:nrow(data),nb_test)
  
  predict_accs = c()
  
  count = 0
  while (count < nb_test) {
    i = sample (1:nrow(data),1)
    test_trustor = data[i,]$Trustor
    subData = data [data$Trustor == test_trustor | data$Trustee == test_trustor,]
    
#     search_range = 1
#     while (search_range < search_len) {
#       subData = data [data$Trustor %in% subData$Trustor | data$Trustor %in% subData$Trustee |
#                         data$Trustee %in% subData$Trustor | data$Trustee %in% subData$Trustee,]
#       search_range = search_range + 14
#     }
    # subData$Sign = ifelse(subData$Sign == -1,0,1)
    
    # remove neutral votes
    subData = subData[subData$Sign != 0,]
    # print (nrow(subEpinions))
    if (nrow(subData) <  min_subdata_size) {
      print ("Adding data")
      subData = data [data$Trustor %in% subData$Trustor | data$Trustor %in% subData$Trustee |
                        data$Trustee %in% subData$Trustor | data$Trustee %in% subData$Trustee,]
    }      
    count = count + 1
    print (paste ("Processing test number", count))
    predict_acc = processSubData(subData, test_ratio = test_ratio)
    predict_accs = c(predict_accs, predict_acc)
    print (paste ("Current average accuracy =", mean(predict_accs)))
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
edge_embeddedness = function (filename = "soc-sign-epinions.txt")
{
  data = read.table (filename, skip = 4, header = TRUE)
  
  library (igraph)
  
  g = make_empty_graph(n = max(max(data$Trustor), max(data$Trustee)) + 1)
  
  for (i in 1:nrow(data)) {
    print (paste ("Processing edge number",i))
    add_edges(graph = g, edges = c(data[i,]$Trustor + 1, data[i,]$Trustee + 1))
  }
}

# utility functions
# count number of common friends of two neighbors

countCommonFriends = function (filename = "soc-sign-epinions.txt")
{
  data = read.table (filename, skip = 4, header = TRUE)
  common_friends_count = c()
  
  for (i in sort(unique(data$Trustor))) {
    for (j in sort(unique(data$Trustee))) {
      print (paste ("Processing trustor",i,"and trustee",j))
      if (i == j) {
        next
      }
      else {
        count = 0
        for (k in nrow(data)) {
          # do not count the current processing line
          if (data[k,]$Trustor != i | data[k,]$Trustee != j) {
            if (data[k,]$Trustor == i | data[k,]$Trustor == j | data[k,]$Trustee == i | data[k,]$Trustee == j) {
              count = count + 1
            }
          }
        }
        common_friends_count = c(common_friends_count, count)
      }
    }
  }
  
  common_friends_count
}