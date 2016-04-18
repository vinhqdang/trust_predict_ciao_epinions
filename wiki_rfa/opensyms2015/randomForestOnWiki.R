
library(RMySQL)
library(randomForest)
library(AUC)
library(rpart)
library(FactoMineR)#pour PCA


######################Functions######################

rf <- function(indexes, dataset, results) {
  test = dataset[indexes,]
  train = dataset[-indexes,]
  rf <- randomForest(factor(is_elected) ~ ., data = train, ntree=500, do.trace = FALSE, importance = TRUE, keep.forest = TRUE)
  importancy <- round(importance(rf), 2)
  pred <- predict(rf, test)
    
#     rf <- rpart(factor(is_elected) ~ ., data= train, method = "class")
#     pred <- predict(rf, test, type = "class")
#     importancy <- 0
  
  
  t <- table(test$is_elected, pred)
  accuracy <- sum(test$is_elected==pred) / nrow(test)
  auc <- auc(roc(pred,factor(test$is_elected)))
  
  results$accuracy <- c(results$accuracy, accuracy)
  results$auc <- c(results$auc, auc)
  results$importancy <- results$importancy+importancy
  results$t <- results$t+t
  #   object <- list("accuracy" = accuracy, "auc" = auc, "t" = t, "importancy" = importancy)
  return(results)
}

####################Main###############################

con <- dbConnect(MySQL(),user="hadoop", password="hadoop",dbname="wikidata", host="hadoop-master")
on.exit(dbDisconnect(con))

#sélection de tous les profils utilisateurs qui se sont présentés, construit sur tout ce qui précède les un an avant l'élection
sql1 = "SELECT user, is_elected, timestamp as date_election, Nb_page_revision, Nb_distinct_page_revision, Nb_distinct_category_revision, Nb_talk_page_discussion, Nb_talk_page_unique, Nb_talk_user_discussion, Nb_talk_user_unique
FROM userProfile_Before1Year";

userProfileBefore1Year <- dbGetQuery(con, sql1)

userProfileBefore1YearWithoutSocial <- userProfileBefore1Year[,c(-7,-8,-9,-10)]

#sélection de tous les profils utilisateurs qui se sont présentés, construit les un an avant l'élection
sql1 = "SELECT user, is_elected, timestamp as date_election, Nb_page_revision, Nb_distinct_page_revision, Nb_distinct_category_revision, Nb_talk_page_discussion, Nb_talk_page_unique, Nb_talk_user_discussion, Nb_talk_user_unique
FROM userProfile_12Months";

userProfile12Months <- dbGetQuery(con, sql1)
userProfile12MonthsWithoutSocial <- userProfile12Months[,c(-7,-8,-9,-10)]
userProfileBefore1Year12Months = as.data.frame(c(userProfileBefore1Year[,c(1,2,3)],userProfile12Months[,c(-1,-2,-3)]+userProfileBefore1Year[,c(-1,-2,-3)]))

# userProfileBefore1Year12Months <- userProfileBefore1Year12Months[userProfileBefore1Year12Months$Nb_page_revision>0,]

userProfileBefore1Year12MonthsWithoutSocial = as.data.frame(c(userProfileBefore1YearWithoutSocial[,c(1,2,3)],userProfile12MonthsWithoutSocial[,c(-1,-2,-3)]+userProfileBefore1YearWithoutSocial[,c(-1,-2,-3)]))
# userProfileBefore1Year12MonthsWithoutSocial <- userProfileBefore1Year12MonthsWithoutSocial[userProfileBefore1Year12MonthsWithoutSocial$Nb_page_revision>0,]

rm(userProfile12Months, userProfile12MonthsWithoutSocial)

#sélection de tous les profils utilisateurs qui se sont présentés, construit sur tout ce qui précède les un an avant l'élection en considérant la période de campagne
sql1 = "SELECT user, is_elected, timestamp as date_election, Nb_page_revision, Nb_distinct_page_revision, Nb_distinct_category_revision, Nb_talk_page_discussion, Nb_talk_page_unique, Nb_talk_user_discussion, Nb_talk_user_unique
FROM userProfile_12MonthsWithCampain";

userProfile12MonthsWithCampain <- dbGetQuery(con, sql1)

userProfile12MonthsWithCampainWithoutSocial <- userProfile12MonthsWithCampain[,c(-7,-8,-9,-10)]
userProfileBefore1Year12MonthsWithCampain = as.data.frame(c(userProfileBefore1Year[,c(1,2,3)],userProfile12MonthsWithCampain[,c(-1,-2,-3)]+userProfileBefore1Year[,c(-1,-2,-3)]))
# userProfileBefore1Year12MonthsWithCampain <- userProfileBefore1Year12MonthsWithCampain[userProfileBefore1Year12MonthsWithCampain$Nb_page_revision>0,]

userProfileBefore1Year12MonthsWithCampainWithoutSocial = as.data.frame(c(userProfileBefore1YearWithoutSocial[,c(1,2,3)],userProfile12MonthsWithCampainWithoutSocial[,c(-1,-2,-3)]+userProfileBefore1YearWithoutSocial[,c(-1,-2,-3)]))
# userProfileBefore1Year12MonthsWithCampainWithoutSocial <- userProfileBefore1Year12MonthsWithCampainWithoutSocial[userProfileBefore1Year12MonthsWithCampainWithoutSocial$Nb_page_revision>0,]

rm(userProfile12MonthsWithCampain, userProfile12MonthsWithCampainWithoutSocial, userProfileBefore1Year, userProfileBefore1YearWithoutSocial)

#sélection de toutes les données sociales admin avant campagne
sql1 = "SELECT * FROM user_socialGraphAttributes_AdminBeforeCampain";


userSocialAdminProfileBeforeCampain <- dbGetQuery(con, sql1)
colnames(userSocialAdminProfileBeforeCampain)[3]<- "date_election"
colnames(userSocialAdminProfileBeforeCampain)[1]<- "user"
userSocialAdminProfileBeforeCampain <- userSocialAdminProfileBeforeCampain[,c(-11)]

userProfileBefore1Year12Months[, "date_election"] <- sapply(userProfileBefore1Year12Months[, "date_election"], as.character)
userProfileBefore1Year12Months[, "date_election"] <- sapply(userProfileBefore1Year12Months[, "date_election"], substr, 2, 20)

# userSocialAdminProfileBeforeCampain <- merge(userSocialAdminProfileBeforeCampain, userProfileBefore1Year12Months[,c(1,3,7,8,9,10)], by=c("user", "date_election"))


#sélection de toutes les données sociales admin avant élection (on considère la pério de campagne)
sql1 = "SELECT * FROM user_socialGraphAttributes_AdminBeforeElection";

userSocialAdminProfileBeforeElection <- dbGetQuery(con, sql1)
colnames(userSocialAdminProfileBeforeElection)[3]<- "date_election"
colnames(userSocialAdminProfileBeforeElection)[1]<- "user"
userSocialAdminProfileBeforeElection <- userSocialAdminProfileBeforeElection[,c(-11)]

userProfileBefore1Year12MonthsWithCampain[, "date_election"] <- sapply(userProfileBefore1Year12MonthsWithCampain[, "date_election"], as.character)
userProfileBefore1Year12MonthsWithCampain[, "date_election"] <- sapply(userProfileBefore1Year12MonthsWithCampain[, "date_election"], substr, 2, 20)

# userSocialAdminProfileBeforeElection <- merge(userSocialAdminProfileBeforeElection, userProfileBefore1Year12MonthsWithCampain[,c(1,3,7,8,9,10)], by=c("user", "date_election"))


#sélection de tous les utiisateurs qui se sont présentés
# sql1 = "SELECT * FROM user_socialGraphAttributes_duringCampain";
# 
# #pour chacun d'eux l'objectif est de construire leur profil en utilisant les données qui précèdent les un an avant l'élection
# userSocialProfileDuringCampain <- dbGetQuery(con, sql1)
# colnames(userSocialProfileDuringCampain)[3]<- "date_election"
# colnames(userSocialProfileDuringCampain)[1]<- "user"
# userSocialProfileDuringCampain <- userSocialProfileDuringCampain[,c(-2)]
# 
# userProfileDuringCampain <- as.data.frame(c(userProfileBefore1Year12MonthsWithCampain[,c(1,2,3)],userProfileBefore1Year12MonthsWithCampain[,c(-1,-2,-3)]-userProfileBefore1Year12Months[,c(-1,-2,-3)]))
# 
# userSocialProfileDuringCampain <- merge(userSocialProfileDuringCampain, userProfileDuringCampain[,c(1,3,7,8,9,10)], by=c("user", "date_election"))
# 


#sélection de toutes les données sociales normales avant campagne (tous les utilisateurs)
sql1 = "SELECT * FROM user_socialGraphAttributes_allUsersBeforeCampain";

userSocialAllProfileBeforeCampain <- dbGetQuery(con, sql1)
colnames(userSocialAllProfileBeforeCampain)[3]<- "date_election"
colnames(userSocialAllProfileBeforeCampain)[1]<- "user"
userSocialAllProfileBeforeCampain <- userSocialAllProfileBeforeCampain[,c(-6,-7,-8,-10,-11)]

userSocialAllProfileBeforeCampain <- merge(userSocialAllProfileBeforeCampain, userProfileBefore1Year12Months[,c(1,3,9)], by=c("user", "date_election"))

#sélection de toutes les données sociales normales avant election (tous les utilisateurs)
sql1 = "SELECT * FROM user_socialGraphAttributes_allUsersBeforeElection";

userSocialAllProfileBeforeelection <- dbGetQuery(con, sql1)
colnames(userSocialAllProfileBeforeelection)[3]<- "date_election"
colnames(userSocialAllProfileBeforeelection)[1]<- "user"
userSocialAllProfileBeforeelection <- userSocialAllProfileBeforeelection[,c(-6,-7,-8,-10,-11)]

userSocialAllProfileBeforeelection <- merge(userSocialAllProfileBeforeelection, userProfileBefore1Year12MonthsWithCampain[,c(1,3,9)], by=c("user", "date_election"))

dbDisconnect(con)

userProfileBefore1Year12MonthsWithoutSocial[, "date_election"] <- sapply(userProfileBefore1Year12MonthsWithoutSocial[, "date_election"], as.character)
userProfileBefore1Year12MonthsWithoutSocial[, "date_election"] <- sapply(userProfileBefore1Year12MonthsWithoutSocial[, "date_election"], substr, 2, 20)

# userProfileActivityDuringCampain <- as.data.frame(c(userProfileBefore1Year12MonthsWithoutSocial[,c(1,2,3)],userProfileBefore1Year12MonthsWithCampainWithoutSocial[,c(-1,-2,-3)]-userProfileBefore1Year12MonthsWithoutSocial[,c(-1,-2,-3)]))

userProfileBefore1Year12MonthsWithCampainWithoutSocial[, "date_election"] <- sapply(userProfileBefore1Year12MonthsWithCampainWithoutSocial[, "date_election"], as.character)
userProfileBefore1Year12MonthsWithCampainWithoutSocial[, "date_election"] <- sapply(userProfileBefore1Year12MonthsWithCampainWithoutSocial[, "date_election"], substr, 2, 20)

userProfileOnlyAllSocial <- merge(userSocialAllProfileBeforeCampain, userSocialAdminProfileBeforeCampain, by=c("user", "date_election","is_elected"))
userProfileWithAllSocial <- merge(userProfileBefore1Year12MonthsWithoutSocial, userProfileOnlyAllSocial, by=c("user", "date_election","is_elected"))

userProfileWithSocialAdmin <- merge(userProfileBefore1Year12MonthsWithoutSocial, userSocialAdminProfileBeforeCampain, by=c("user", "date_election","is_elected"))
userProfileWithSocialNormal <- merge(userProfileBefore1Year12MonthsWithoutSocial, userSocialAllProfileBeforeCampain, by=c("user", "date_election","is_elected"))

userProfileOnlyAllSocialWithCampain <- merge(userSocialAllProfileBeforeelection, userSocialAdminProfileBeforeElection, by=c("user", "date_election","is_elected"))
userProfileWithAllSocialWithCampain <- merge(userProfileBefore1Year12MonthsWithCampainWithoutSocial, userProfileOnlyAllSocialWithCampain, by=c("user", "date_election","is_elected"))

userProfileWithSocialAdminWithCampain <- merge(userProfileBefore1Year12MonthsWithCampainWithoutSocial, userSocialAdminProfileBeforeElection, by=c("user", "date_election","is_elected"))
userProfileWithSocialNormalWithCampain <- merge(userProfileBefore1Year12MonthsWithCampainWithoutSocial, userSocialAllProfileBeforeelection, by=c("user", "date_election","is_elected"))


results_AllWithoutSocial <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_AllWithAllSocial <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_AllWithAllSocialLimited <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_OnlyAllSocial <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_OnlyAllSocialLimited <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)

results_OnlySocialNormal <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_OnlySocialNormalLimited <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)

results_OnlySocialAdmin <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_OnlySocialAdminLimited <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)

results_AllWithSocialAdmin <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_AllWithSocialAdminLimited <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_AllWithSocialNormal <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)
results_AllWithSocialNormalLimited <- list("accuracy" = NULL, "auc" = NULL, "t" = 0, "importancy" = 0)


#randomForest sur tous les types de profil générée précédemment
for(i in 1:30){
#   indexes = sample(1:nrow(userProfileVitesseCampain), size=0.2*nrow(userProfileVitesseCampain))
#   results_Vitesse <- rf(indexes, userProfileVitesseCampain[,c(-1,-2)], results_Vitesse)

  indexes = sample(1:nrow(userProfileAllWithSocial), size=0.2*nrow(userProfileAllWithSocial))

  results_OnlySocialAdmin <- rf(indexes, userProfileOnlySocial[,c(-1,-2,-6,-7,-8,-11,-12,-13,-14,-15)], results_OnlySocialAdmin)
  results_OnlySocial <- rf(indexes, userProfileOnlySocial[,c(-1,-2,-6,-7,-8,-10,-11,-12,-13)], results_OnlySocial)

  results_AllWithSocialAdmin <- rf(indexes, userProfileAllWithSocial[,c(-1,-2,-9,-10,-11,-13,-14,-15,-16,-17,-18)], results_AllWithSocialAdmin)
  results_AllWithSocial <- rf(indexes, userProfileAllWithSocial[,c(-1,-2,-9,-10,-11,-13,-14,-15,-16)], results_AllWithSocial)
  
}
  results_WithSocialAdminWithCampain <- rf(indexes, userProfileAllWithSocialWithCampain[,c(-1,-2,-14,-15,-16,-17,-18)], results_WithSocialAdminWithCampain)
    
  results_OnlySocialAdminWithCampain <- rf(indexes, userProfileOnlySocialWithCampain[,c(-1,-2,-11,-12,-13,-14,-15)], results_OnlySocialAdminWithCampain)
  results_OnlySocialWithCampain <- rf(indexes, userProfileOnlySocialWithCampain[,c(-1,-2,-11,-12,-13)], results_OnlySocialWithCampain)
  results_WithSocialWithCampain <- rf(indexes, userProfileAllWithSocialWithCampain[,c(-1,-2,-14,-15,-16)], results_WithSocialWithCampain)
  
  indexes = sample(1:nrow(userProfileAllWithSocialDuringCampain), size=0.2*nrow(userProfileAllWithSocialDuringCampain))
  results_AllDuringCampain <- rf(indexes, userProfileAllWithSocialDuringCampain[,c(-1,-2,-10,-11,-14,-15,-16)], results_AllDuringCampain)
  results_OnlySocialDuringCampain <- rf(indexes, userProfileOnlySocialDuringCampain[,c(-1,-2,-7,-8,-11,-12,-13)], results_OnlySocialDuringCampain)
  results_OnlySocialAdminDuringCampain <- rf(indexes, userProfileOnlySocialDuringCampain[,c(-1,-2,-7,-8,-11,-12,-13,-14,-15)], results_OnlySocialAdminDuringCampain)
  
  indexes = sample(1:nrow(userProfileBefore1Year12Months), size=0.2*nrow(userProfileBefore1Year12Months))  
  results_ActivityDuringCampain <- rf(indexes, userProfileActivityDuringCampain[,c(-1,-3)], results_ActivityDuringCampain) 
#   results_Activity12Months <- rf(indexes, userProfileActivity12Months[,c(-1,-3)], results_Activity12Months) 
  results_All2 <- rf(indexes, userProfileBefore1Year12Months[,c(-1,-3,-7,-8)], results_All2)
  results_WithCampain2 <- rf(indexes, userProfileBefore1Year12MonthsWithCampain[,c(-1,-3,-7,-8)], results_WithCampain2)
  results_AllWithoutSocial <- rf(indexes, userProfileBefore1Year12MonthsWithoutSocial[,c(-1,-3)], results_AllWithoutSocial)
  results_WithCampainWithoutSocial <- rf(indexes, userProfileBefore1Year12MonthsWithCampainWithoutSocial[,c(-1,-3)], results_WithCampainWithoutSocial)
  

}

boxplot(results_AllWithSocial$accuracy,xlab="Accuracy of predictions based on social with admin attributes" , horizontal=T, outline=FALSE)

boxplot(data.frame(results_OnlySocialAdmin$accuracy,results_OnlySocial$accuracy,results_AllWithoutSocial$accuracy,results_All2$accuracy, results_AllWithSocial$accuracy, results_AllWithSocialAdmin$accuracy),
        xlab="Profiles",ylab="Accuracy", outline=FALSE, border=c("black"), 
        names=c("SocialAdmin","Social","Revisions","Revisions+\n Social not Admin","Revisions+\n Social","Revisions+\n SocialAdmin"))

boxplot(data.frame(results_OnlySocialAdminWithCampain$accuracy,results_OnlySocialWithCampain$accuracy,results_WithCampainWithoutSocial$accuracy,results_WithCampain2$accuracy, results_WithSocialWithCampain$accuracy, results_WithSocialAdminWithCampain$accuracy),
        xlab="Period",ylab="accuracy", outline=FALSE, border=c("black"), 
        names=c("SocialAdmin","Social","Activity","Activity+\n basicSocial","Activity+\n Social","Activity+\n SocialAdmin"))

boxplot(data.frame(results_OnlySocialDuringCampain$accuracy,results_ActivityDuringCampain$accuracy,results_AllDuringCampain$accuracy),
        xlab="Period",ylab="accuracy", outline=FALSE, border=c("black"), 
        names=c("Social","Activity","Activity+\n Social"))


boxplot(data.frame(results_OnlySocial$accuracy,results_AllWithoutSocial$accuracy,results_AllWithSocial$accuracy),
        xlab="Profiles",ylab="accuracy", outline=FALSE, border=c("black"), 
        names=c("Social","Revisions","Revisions+\n Social"))

#######################################

indexes = sample(1:nrow(userProfileAllWithSocial[,c(-14,-15,-16)]), size=0.2*nrow(userProfileAllWithSocial[,c(-14,-15,-16)]))
t <- userProfileAllWithSocial[,c(-1,-2,-14,-15,-16)]
test = t[indexes,]
train = t[-indexes,]

fit <- rpart(factor(is_elected) ~ ., data= train, method = "class")
pred <- predict(fit, test, type = "class")
t <- table(test$is_elected, pred)
accuracy <- sum(test$is_elected==pred) / nrow(test)
auc <- auc(roc(pred,factor(test$is_elected)))
plot(fit)
text(fit)


#######################################
#social admin
res.pca = PCA(userProfileOnlySocial[,c(-1,-2,-11,-12,-13,-14,-15)], scale.unit=TRUE, ncp=5,  quanti.sup=c(1), quali.sup = 1,graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)     
#social
res.pca = PCA(userProfileOnlySocial[,c(-1,-2,-11,-12,-13)], scale.unit=TRUE, ncp=5,  quanti.sup=c(1), quali.sup = 1,graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)     
#revision
res.pca = PCA(userProfileBefore1Year12MonthsWithoutSocial[,c(-1,-3)], scale.unit=TRUE, ncp=5,  quanti.sup=c(1), quali.sup = 1,graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)     
#revision+social
res.pca = PCA(userProfileAllWithSocial[,c(-1,-2,-14,-15,-16)], scale.unit=TRUE, ncp=5,  quanti.sup=c(1), quali.sup = 1,graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)    
#revision+expl:social
res.pca = PCA(userProfileAllWithSocial[,c(-1,-2,-14,-15,-16)], scale.unit=TRUE, ncp=5,  quanti.sup=c(1,5,6,7,8,9,10,11,12,13), quali.sup = 1,graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)    
res.pca = PCA(userProfileAllWithSocial[,c(-1,-2,-3,-10,-11,-13,-14,-15,-16)], scale.unit=TRUE, ncp=5,  quanti.sup=c(4,5,6,7,8,9), graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)
#expl:revision+social
res.pca = PCA(userProfileAllWithSocial[,c(-1,-2,-14,-15,-16)], scale.unit=TRUE, ncp=5,  quanti.sup=c(1,2,3,4), quali.sup = 1, graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)  
#principal: revision+expl:social
res.pca = PCA(userProfileAllWithSocial[,c(3,4,5,17,18)], scale.unit=TRUE, ncp=5,  quanti.sup=c(1), graph=T, axes=c(1,2))
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=1)

res.pca$var
dimdesc(res.pca, axes=c(1,2))



