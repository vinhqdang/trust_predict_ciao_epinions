wiki_rfa=read.csv("wiki_rfa_simple.txt",sep=";",header = TRUE)
wiki_rfa$SRC = as.factor(wiki_rfa$SRC)
wiki_rfa$TGT = as.factor(wiki_rfa$TGT)
wiki_rfa$VOT = as.factor(wiki_rfa$VOT)
wiki_rfa$RES = as.factor(wiki_rfa$RES)
wiki_rfa$YEA = as.factor(wiki_rfa$YEA)


library (h2o)
h2o.init (nthreads = -1)

n_sample = 2500
data = wiki_rfa[c(sample(which(wiki_rfa$VOT == 1),n_sample), 
                  sample(which(wiki_rfa$VOT == -1),n_sample)),]

test_ratio = 0.2
indexes = sample(1:nrow(data), size = test_ratio * nrow(data))
test = data[indexes,]
train = data[-indexes,]

data_h2o = as.h2o (data)
train_h2o = as.h2o (train)
test_h2o = as.h2o (test)

hidden = c(400,200,100)
epoch = 10000
rate = 0.0005

dnn = h2o.deeplearning(x=c(1,2,4,5),y=3, 
                             training_frame = data_h2o, 
                             nfolds = 5, rate = rate,
                             hidden = hidden,
                             epochs = epoch,
                       train_samples_per_iteration = -1) 

dnn

h2o.shutdown(prompt = FALSE)