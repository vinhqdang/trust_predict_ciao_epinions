wiki_rfa=read.csv("wiki_rfa_simple.txt",sep=";",header = TRUE)
wiki_rfa$SRC = as.factor(wiki_rfa$SRC)
wiki_rfa$TGT = as.factor(wiki_rfa$TGT)
wiki_rfa$VOT = as.factor(wiki_rfa$VOT)
wiki_rfa$RES = as.factor(wiki_rfa$RES)
wiki_rfa$YEA = as.factor(wiki_rfa$YEA)

n_sample = 5000
data = wiki_rfa[c(sample(which(wiki_rfa$VOT == 1),n_sample), 
                  sample(which(wiki_rfa$VOT == -1),n_sample)),]

hiddens =c (c(100),c(100,100),c(200,100),c(200,200),c(200,100,50),c(200,200,100,50))

rates = c(0.005, 0.001, 0.0001)

epochs = c(10,20,50,100,1000)

for (hidden in hiddens) {
  for (rate in rates) {
    for (epoch in epochs) {
      dnn = h2o.deeplearning(x=c(1,2,4,5),y=3, 
                             training_frame = as.h2o(data), 
                             nfolds = 5, rate = rate,
                             hidden = hidden,
                             epochs = epoch) 
    }
  }
}