u.data = read.table(file = "ml-100k/u.data")
colnames(u.data) = c("userID",'movieID',"rating","timestamp")

u.item = read.table(file = "ml-100k/u.item2")
for (i in 1:19) {
  u.item[[i]] = as.factor(as.character(u.item[[i]]))
}
colnames(u.item) = c("unknown" ,"Action",  "Adventure", "Animation",
                       "Childrens" , "Comedy" , "Crime", "Documentary", "Drama","Fantasy",
                     "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",
                     "Thriller","War", "Western")
u.item$movieID = seq.int (nrow (u.item))

u.user = read.table(file = "ml-100k/u.user", sep = "|")
colnames(u.user) = c("userID","age","gender","occupation","zip_code")

all_data = u.data
all_data = merge(all_data, u.item, by = "movieID")
all_data = merge(all_data, u.user, by = "userID")

all_data$userID = as.factor(all_data$userID)
all_data$movieID = as.factor(all_data$movieID)

library (h2o)

localH2O = h2o.init()

h2o.data = as.h2o (all_data)

h2o.dl = h2o.deeplearning(x=c(1:2,5:27), y = 4, training_frame = h2o.data, nfolds = 5)

h2o.dl

h2o.shutdown(prompt = FALSE)