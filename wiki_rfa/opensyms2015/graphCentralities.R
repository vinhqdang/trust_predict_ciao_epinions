################# code pour générer des graphes utilisateurs d'interaction avant leur campagne et pendant leur campagne
################# avec les admin élus précédemment (pour tous les utilisateurs qui se sont présentés après 2007
################# génération de fichier gexf à lire avec gephi !
library(RMySQL)
library(igraph)

con <- dbConnect(MySQL(),user="hadoop", password="hadoop",dbname="wikidata", host="hadoop-master")
on.exit(dbDisconnect(con))

#sélection de tous les utilisateurs qui se sont présentés après 2007-01-01
# sql1 = "SELECT user, is_elected , timestamp, DATE_SUB(timestamp, INTERVAL 2 WEEK) as twoWeeksBefore FROM wikielect WHERE DATEDIFF('2007-01-01', timestamp)<=0";
sql1 = "SELECT user, is_elected , timestamp, DATE_SUB(timestamp, INTERVAL 2 WEEK) as twoWeeksBefore FROM wikielect";

#pour chacun d'eux l'objectif est de sélectionner leurs discussions avec les élus
presentedUsers <- dbGetQuery(con, sql1)



for(i in 1:nrow(presentedUsers)) {
  username <- presentedUsers[i,"user"]
  print(username)
  is_elected <- presentedUsers[i,"is_elected"]
  date_election <- substr(presentedUsers[i,"timestamp"],2,20)
  twoWeeksBefore <- presentedUsers[i,"twoWeeksBefore"]
  
  #sélection de tous les utilisateurs qui ont été élus avant la date de campagne de l'utilisateur
  sql1 = paste("SELECT user, is_elected, timestamp FROM wikielect WHERE DATEDIFF('",twoWeeksBefore,"', timestamp)>0 AND is_elected =1", sep = "")
  
  #pour chacun d'eux l'objectif est de sélectionner leurs discussions avec les élus
  electedUsersBefore <- dbGetQuery(con, sql1)
  
  
  #talks de tous les présentés avant date de campagne
  sql2 = paste("SELECT user1,user2 FROM wikitalkuser_userelection WHERE DATEDIFF('",date_election,"',timestamp)>=0 ", sep = "")
  userTalksOnProfiles <- dbGetQuery(con, sql2)
  
  talksBetweenAdminsAndTheUser <- userTalksOnProfiles[(tolower(userTalksOnProfiles$user1) %in%  tolower(electedUsersBefore$user) | tolower(userTalksOnProfiles$user1)==tolower(username)) & (tolower(userTalksOnProfiles$user2) %in%  tolower(electedUsersBefore$user) | tolower(userTalksOnProfiles$user2)==tolower(username)), ]

  #GENERER POUR CHAQUE UTILISATEUR LE GRAPHE "SOCIAL" SOUS FORME DE FICHIERS, LE BUT EST D'AVOIR DE NOUVEAUX PARAM A INCLURE DANS SON PROFIL: centralité, nombre membres dans sa communauté, degré
  #nodes2 <- as.data.frame(unique(unlist(talksBetweenAdminsAndTheUser$user1,talksBetweenAdminsAndTheUser$user2)))
  nodes2 <- as.data.frame(union(unique(tolower(talksBetweenAdminsAndTheUser$user1)),unique(tolower(talksBetweenAdminsAndTheUser$user2))))
  
  colnames(nodes2) <- c("id")
  if(length(nodes2[nodes2$id==username,])>0){ #si l'utilisateur a discuté avec des admins avant
    nodes2$label <- nodes2$id
    edges2 <- lapply(talksBetweenAdminsAndTheUser, tolower)
  #   write.gexf(nodes=nodes2, edges=edges2, output = "/home/romain/Bureau/wiki/test.gexf")
  #   write.csv(nodes2, file = "/home/romain/Bureau/wiki/nodes.csv",row.names=FALSE)
  #   write.csv(edges2, file = "/home/romain/Bureau/wiki/edges.csv",row.names=FALSE)
    
    #création du graphe avec igraph à partir des dataframe
    g <- graph.data.frame(edges2, directed=T, vertices=nodes2)
  
    #simplication du graphe en enlevant les arrêtes multiples et les boucles, somme les poids des arêtes
    E(g)$weight <- 1  #on ajoute un poids aux arêtes puis on simplifie le graph
    g <- simplify(g,edge.attr.comb=list(weight="sum", "ignore"))
    #chercher le noeud de l'utilisateur et calculer différents attributs
    deg <- as.numeric(degree(g, v=V(g)[username]))
    clos <- as.numeric(closeness(g, v=V(g)[username],weights=E(g)$weight))
    btw <- as.numeric(betweenness(g, v=V(g)[username], directed=T, weights=E(g)$weight))
    eigenVecC <- as.numeric(evcent(g, directed = T,weights=E(g)$weight)$vector[username]) #eigen vector centrality
    alphaC <- as.numeric(alpha.centrality(g,nodes=V(g)[username], weights=E(g)$weight)) #eigen vector centrality
    pageRank <- as.numeric(page.rank(g,directed=T,vids=V(g)[username], weights=E(g)$weight)$vector)  
  
    #communauté par randomWalk
    wc <- walktrap.community(g,weights=E(g)$weight)
    com<-community.to.membership(g, wc$merges, steps= which.max(wc$modularity)-1)
    V(g)$membership <- com$membership
  #   plot(wc$modularity, pch=20)
  #   V(g)$color <- com$membership+1
  #   g$layout <- layout.fruchterman.reingold
  #   plot(g, vertex.label=NA)
    #groupe de l'utilisateur : V(g)[username]$membership
    #liste de tous les utilisateurs du groupe de notre user
    userListInHisGroup <- V(g)[which(V(g)$membership==V(g)[username]$membership)]$name
    userCountInHisGroup <- length(userListInHisGroup)
    userListInHisGroup <- paste(as.list(userListInHisGroup),collapse=",")
    
    sql2 <- paste("INSERT INTO user_socialGraphAttributes_AdminBeforeElection
      VALUES ('",username,"',",is_elected,",'",date_election,"',",deg,",",clos,",",btw,",",eigenVecC,",",alphaC,",",pageRank,",",userCountInHisGroup,",'",userListInHisGroup,"');",sep="")
    dbSendQuery(con, sql2)
  }else{
    sql2 <- paste("INSERT INTO user_socialGraphAttributes_AdminBeforeElection
      VALUES ('",username,"',",is_elected,",'",date_election,"',",-1,",",-1,",",-1,",",-1,",",-1,",",-1,",",-1,",'",-1,"');",sep="")
    dbSendQuery(con, sql2)
  }


}
dbDisconnect(con)



