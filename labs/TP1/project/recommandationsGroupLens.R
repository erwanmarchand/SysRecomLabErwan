#### Recuperation des données

rm(list = ls())

library(Matrix)

u.data <- read.csv(file='u.data.csv', sep='|', header=T)
u.item <- read.csv(file='u.item.csv', sep='|', header=T)
u.user <- read.csv(file='u.user.csv', sep='|', header=T)

m.sparse <- sparseMatrix(u.data$user.id,u.data$item.id,x=u.data$rating)
rownames(m.sparse) <- paste('u', 1:nrow(m.sparse), sep='')
colnames(m.sparse) <- paste('i', 1:ncol(m.sparse), sep='')

m <- as.matrix(m.sparse)

m[m==0] <- NA


mx <- merge(u.user, u.data, 1)

#### 1) Quelle est la moyenne des votes par profession ("job") et par âge?

## moyenne par profession

meanPerJob <-aggregate(mx[, 7], list(mx$job), mean)
colnames(meanPerJob) <- c("job","mean Rating")

## moyenne par age

meanPerAge <-aggregate(mx[, 7], list(mx$age), mean)
colnames(meanPerAge) <- c("age","mean Rating")

## moyenne par profession et age

meanPerProfessionAndAge <-aggregate(mx[, 7], list(mx$job,mx$age), mean)
colnames(meanPerProfessionAndAge) <- c("job","age","mean Rating")


#### 2) Quels sont les 10 films les plus similaires à "Star Trek V: The Final Frontier (1989)" selon respectivement la mesure du cosinus et de la corrélation avec la matrice de votes. 

## Cosinus entre un vecteur v et chaque colonne de la matrice m
cosinus.vm <- function(v,m) { 
  n <- sqrt(colSums(m^2))
  return ((v %*% m)/(n * sqrt(sum(v^2))))
}

max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

name.star.trek <- 'Star Trek V: The Final Frontier (1989)'
indice.star.trek <- u.item[u.item$movie.title==name.star.trek,]$movie.id
notes.star.trek <- m.sparse[,indice.star.trek]

wcos <- cosinus.vm(notes.star.trek , m.sparse)
similarMoviesCos.index <- max.nindex(wcos,11)
similarMoviesCos.index <- similarMoviesCos.index[similarMoviesCos.index!=indice.star.trek ]
similarMoviesCos.names <- u.item[similarMoviesCos.index,]$movie.title
similarMoviesCos.names <- data.frame(similarMoviesCos.names)
colnames(similarMoviesCos.names) <- c(paste("Most similar movies from",name.star.trek, sep = ' : '))

wcor <- as.vector(cor(notes.star.trek, as.matrix(m.sparse)))
similarMoviesCor.index <- max.nindex(wcor,11)
similarMoviesCor.index <- similarMoviesCor.index[similarMoviesCor.index!=indice.star.trek ]
similarMoviesCor.names <- u.item[similarMoviesCor.index,]$movie.title
similarMoviesCor.names <- data.frame(similarMoviesCor.names)
colnames(similarMoviesCor.names) <- c(paste("Most similar movies from",name.star.trek, sep = ' : '))


#### 3) 


n.voisins <- 20 + 1

votes.communs <- (colSums((m.sparse[,indice.star.trek] * m.sparse) > 0))
distance.star.trek <- sqrt(colSums((m.sparse[,indice.star.trek] - m.sparse)^2))
i.distance.star.trek <- min.nindex(distance.star.trek, n.voisins)
i.distance.star.trek <- i.distance.star.trek[i.distance.star.trek!=indice.star.trek]

wcos.voisin <- t(as.matrix(cosinus.vm(m.sparse[,indice.star.trek], m.sparse[,i.distance.star.trek])))
m.sparse.star.trek.na <- as.matrix(m.sparse[which(is.na(m[,indice.star.trek])),i.distance.star.trek]) # on ne considere pas les utilisateurs ayant deja votés pour star trek

m.star.trek.na <- as.matrix(m.sparse.star.trek.na)
m.star.trek.na[m.star.trek.na==0] <- NA

wcos.sums.star.trek.na <- m.star.trek.na
wcos.sums.star.trek.na[!is.na(wcos.sums.star.trek.na)] <- 1
wcos.sums.star.trek.na[is.na(wcos.sums.star.trek.na)] <- 0
wcos.sums.star.trek.na <- abs(wcos.sums.star.trek.na %*% (as.matrix(wcos.voisin)))
wcos.sums.star.trek.na[wcos.sums.star.trek.na==0] <- NA

mean.star.trek = mean(m[,indice.star.trek], na.rm = TRUE)

means.voisins.na = colMeans(m.star.trek.na, na.rm = TRUE) # moyenne pour chaque voisin sans vote des utilisateurs ayant votés pour star trek
means.voisins.na[is.nan(means.voisins.na)] <- NA

temp <- t(t(m.star.trek.na)-means.voisins.na)
temp[is.na(temp)] <- 0
notes.star.trek.predicted.na <- temp%*%wcos.voisin/wcos.sums.star.trek.na
notes.star.trek.predicted.na[notes.star.trek.predicted.na == 0] <- NA
notes.star.trek.predicted.na <- mean.star.trek + notes.star.trek.predicted.na 

notes.star.trek.predicted.na.no.na <- merge(rownames(notes.star.trek.predicted.na) [!is.na(notes.star.trek.predicted.na)], as.matrix(notes.star.trek.predicted.na[!is.na(as.matrix(notes.star.trek.predicted.na))]), by = "row.names")

colnames(notes.star.trek.predicted.na.no.na) <- c(" ","Identifiant utilisateur","vote prédit")
notes.star.trek.predicted.na.no.na[1] <- NULL
notes.star.trek.predicted.na.no.na

#### 4)

m.sparse.star.trek <- as.matrix(m.sparse[which(!is.na(m[,indice.star.trek])),i.distance.star.trek]) # on ne considere que les utilisateurs ayant deja votés pour star trek

m.star.trek <- as.matrix(m.sparse.star.trek)
m.star.trek[m.star.trek==0] <- NA

wcos.sums.star.trek <- m.star.trek
wcos.sums.star.trek[!is.na(wcos.sums.star.trek)] <- 1
wcos.sums.star.trek[is.na(wcos.sums.star.trek)] <- 0
wcos.sums.star.trek <- abs(wcos.sums.star.trek %*% (as.matrix(wcos.voisin)))
wcos.sums.star.trek[wcos.sums.star.trek==0] <- NA

means.voisins = colMeans(m.star.trek, na.rm = TRUE) # moyenne pour chaque voisin avec vote des utilisateurs ayant votés pour star trek
means.voisins[is.nan(means.voisins)] <- NA

temp <- t(t(m.star.trek)-means.voisins)
temp[is.na(temp)] <- 0
notes.star.trek.predicted <- temp%*%wcos.voisin/wcos.sums.star.trek 
notes.star.trek.predicted[notes.star.trek.predicted == 0] <- NA
notes.star.trek.predicted <- mean.star.trek + notes.star.trek.predicted 

notes.star.trek.predicted.no.na <- merge(as.matrix(notes.star.trek.predicted),as.matrix(notes.star.trek), by = "row.names")

colnames(notes.star.trek.predicted.no.na) <- c("Identifiant utilisateur","prediction","vote réel")

notes.star.trek.predicted.no.na <- na.omit(notes.star.trek.predicted.no.na)

erreur.quadratique <- sqrt(mean((notes.star.trek.predicted.no.na[,"prediction"] - notes.star.trek.predicted.no.na[,"vote réel"])^2, na.rm=TRUE))



# 5

indices.star.trek <- grep("trek", as.character(u.item$movie.title), ignore.case=TRUE)
indices.star.wars <- c(172,181)

votes.new.user <- user.votes <- rep(0, ncol(m.sparse))
votes.new.user[indices.star.trek] <- 5
votes.new.user[indices.star.wars] <- 1
#votes.new.user[votes.new.user==0] <- NA

votes.communs.new.user <- (rowSums((votes.new.user * m.sparse) > 0))
distance.new.user <- sqrt(rowSums((votes.new.user - m.sparse)^2))
i.distance.new.user <- min.nindex(distance.new.user, 20) #pas besoin de supprimer notre utilisateur de cette liste car il n'apparait pas dans m.sparse

wcos.new.user.voisins <- as.vector(cosinus.vm(votes.new.user, t(m.sparse[i.distance.new.user,])))

m.sparse.new.user <- as.matrix(m.sparse[i.distance.new.user,])

m.new.user <- as.matrix(m.sparse.new.user)
m.new.user[m.new.user==0] <- NA

wcos.sums.new.user <- m.new.user
wcos.sums.new.user[!is.na(wcos.sums.new.user)] <- 1
wcos.sums.new.user[is.na(wcos.sums.new.user)] <- 0
wcos.sums.new.user <- abs(t(wcos.sums.new.user) %*% (as.matrix(wcos.new.user.voisins)))
wcos.sums.new.user[wcos.sums.new.user==0] <- NA

mean.new.user = mean(votes.new.user[votes.new.user>0], na.rm = TRUE)

means.voisins.new.user = rowMeans(m.new.user, na.rm = TRUE) # moyenne pour chaque voisin
means.voisins.new.user[is.nan(means.voisins.new.user)] <- NA

temp <- t(t(m.new.user)-means.voisins)
temp[is.na(temp)] <- 0

notes.new.user.predicted <- t(temp)%*%wcos.new.user.voisins/wcos.sums.new.user
notes.new.user.predicted[notes.new.user.predicted == 0] <- NA
notes.new.user.predicted <- mean.new.user + notes.new.user.predicted

indices.movies.recommended.new.user <- min.nindex(distance.new.user, 10)
u.item$movie.title[indices.movies.recommended.new.user]


# 6

new.user.job <- 'engineer'
new.user.gender <- 'M'
new.user.age <- '40'


ratio.chances <- function(rating.vec, seuil=3) {
  sum(rating.vec > seuil) / sum(rating.vec <= seuil)
}

OddsToP <- function(o) {
  o/(1+o)
}

recommended.movie.content.base <- function(job, gender, age){

  probabilities.to.like = rep(0, nrow(u.item))
  
  for (movie in c(1:nrow(u.item))) {
    
    i <- (mx$item==movie & mx$rating>3)           # ceux qui aiment
    ni <- (mx$item==movie & mx$rating<=3)         # et ceux qui n'aiment pas
    
    #on ne considere le film que s'il y a au minimum 3 votes, sinon on laisse la probabilité à 0
    if (sum(i) + sum(ni) >= 3){
      
      E_11 <- (table(mx[i, 'job'])/sum(table(mx[i, 'job'])))[job]
      E_12 <- (table(mx[ni, 'job'])/sum(table(mx[ni, 'job'])))[job]
    
      if (is.na(E_12) | E_12 == 0) {
        E_1 <- 0
      }
      else {
        E_1 <- E_11 / E_12
      }
      
      if(age<50){
        E_21 <- table(mx[i, 'age'] < 50)/sum(table(mx[i, 'age'] < 50))
        E_22 <- table(mx[ni, 'age'] < 50)/sum(table(mx[ni, 'age'] < 50))
      }
      else {
        E_21 <- table(mx[i, 'age'] >= 50)/sum(table(mx[i, 'age'] >= 50))
        E_22 <- table(mx[ni, 'age'] >= 50)/sum(table(mx[ni, 'age'] >= 50))
      }
      
      #si on a pas suffisament de votes on arrete
      if (nrow(E_21) < 2 | nrow(E_22) < 2){
        E_2 <- 0
      }
      else {
        E_2 <- (E_21 / E_22)["TRUE"]
      }
      
      E_31 <- (table(mx[i, 'gender'])/sum(table(mx[i, 'gender'])))[gender]
      E_32 <- (table(mx[ni, 'gender'])/sum(table(mx[ni, 'gender'])))[gender]
      
      if (is.na(E_32) | E_32 == 0) {
        E_3 <- 0
      }
      else {
        E_3 <- E_31 / E_32
      }
      
      probabilities.to.like[movie] <- OddsToP(ratio.chances(mx[mx$item.id==movie, 'rating'])  * E_1 * E_2 * E_3)
      
    }
    else {
      probabilities.to.like[movie] <- 0
    }
    
  }
  
  probabilities.to.like[is.nan(probabilities.to.like)] <- 0
  
  indices.movies.recommended.new.user <- max.nindex(probabilities.to.like, 10)
  recommended.movies <- data.frame(u.item$movie.title[indices.movies.recommended.new.user])
  colnames(recommended.movies) <- c("Recommended movies")
  recommended.movies
}

recommended.movie.content.base(new.user.job, new.user.gender, new.user.age)
