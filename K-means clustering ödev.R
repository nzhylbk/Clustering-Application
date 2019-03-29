# Setting working directory
setwd("/Users/nezihyalabik/Desktop/School book 2/Pazarlama analizi/R scripts/week4")
#install.packages("cluster")
library(cluster)
# Read the Data
ct<-read.csv("customerTransactions.csv",header=T)

# Checking basic stats of the data
summary(ct)

# We need only integer data so others should be removed.
# Clustering customers based on their transaction counts can be another option also. Let's add a new calculated field.

ct$total<- as.integer(rowSums(ct[-1:-2]))

ct3 = ct$total
str(ct3)

# Applying Elbow technique
wssplot <- function(data, nc=100, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss) 
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
} 

wssplot(ct[-1:-2], nc=5)

# We get best reduction from SSE wher k=2. Thus, we set k=2 

model <- kmeans(ct[-1:-2],centers=2)

model$cluster
model$size
summary(model)


# Plot Clusters in 2D
clusplot(ct[-1:-2], model$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# Show customers and their corresponding clusters:
table(ct$CustomerLastName,model$cluster)



#################################################################
### Hierarchical clustering model                             ###
#################################################################


#Find Hierarchical clustering using Euclidean distance and wards method in matrix.
d <- dist(ct[-1:-2], method = "euclidean") 
H_Model <- hclust(d, method="ward.D")


# display dendogram
plot(H_Model) 


# cut tree into 2 clusters
groups <- cutree(H_Model, k=2)

# show groups
table(ct$CustomerLastName,groups)

# draw dendogram with red borders around the 2 clusters
rect.hclust(H_Model, k=2, border="red") 

