# Item-rating prediction in social-based recommedation networks

In this study, we aim to predict the rating scores of a user to an item, given two training datasets.

- Rating dataset.
- Trust dataset.

We showed that, a simple deep feed-forward neural network [1] can perform well in predicting rating score, better than other complex existing models in literature.

## Datasets

### Rating dataset

Given as a set of tuple:

``(u,i,t,r)``


wherein:

- u: user ID who gives the rate to the item
- i: item ID
- t: timestamp when the rate is made
- r: rating score, from 1 to 5

### Trust dataset

Given as a set of tuple:

``(u1,u2,t,v)``

wherein:

- u1: trustor ID
- u2: trustee ID
- u3: timestamp when the relation is declared
- v: value (trust/distrust)

### Combined dataset

The idea is to combine rating and trust dataset, by considering that, a trust relation is a rating of ``5`` from a user to another user.

## Training & Predicting

You should install [R >= 3.3.2](r-project.org). Currently the neural network is built by [h2o](https://www.h2o.ai/), but I plan to update the code to run with [mxnet](http://vinhqdang.github.io/2017/03/23/deep-learning-in-r). However there is no concrete roadmap.

```{r}
install.packages ("h2o") # will take a while, be patient.
								# h2o requires JDK, please install JDK first
# init h2o server
h2o.init ()

# load data
# some datasets are provided in MAT format, use R.matlab instead
rating <- read.table (...)
trust <- read.table (...)

# combine dataset
rating[[2]] <- rating[[2]] + max (max (trust[[1]]), trust[[2]]) + 1
trust$score <- 5
combine_data <- rbind (rating, trust)

# set categorical variables
colnames (combine_data) <- c("Link Source","Link Destination","Timestamp","Link Value")
combine_data[[1]] <- as.factor (combine_data[[1]])
combine_data[[2]] <- as.factor (combine_data[[2]])

# building the model
my_model <- h2o.deeplearning (x=1:3,y=4,hiddens = c(2048,1024),...)
# there are a lot of hyper-parameters for tuning. 
# deep learning model is known as sensitive for these hyper-parameters, so be careful.

# predicting
pre <- h2o.predict (my_model, test)
rmse (pre$predict, test$score)

```

You can refer to the file ``Process.R`` for some reference code.

With a few lines of code, the RMSE and MAE of the predicting scores are better than existing methods, such as mTrust (2012), eTrustRec (2015), TrustSVD (2016).

# References

[1] Deep Learning (2016), MIT Press. Ian Goodfellow et al. [http://www.deeplearningbook.org/](http://www.deeplearningbook.org/) 

[2] Tang, Jiliang, Huiji Gao, and Huan Liu. "mTrust: discerning multi-faceted trust in a connected world." WSDM. ACM, 2012.

[3] Tang, Jiliang, et al. "Trust evolution: Modeling and its applications." IEEE Transactions on Knowledge and Data Engineering 27.6 (2015): 1724-1738.

[4] Guo, Guibing, Jie Zhang, and Neil Yorke-Smith. "A novel recommendation model regularized with user trust and item ratings." IEEE Transactions on Knowledge and Data Engineering 28.7 (2016): 1607-1620.