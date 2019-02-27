# Clustering with KMeans

#Load all libraries
library(readr)
library(ggplot2)
library(GGally)
library(DMwR)
library(NbClust)
library(factoextra)

set.seed(5580)

# import Customer and Product CSV files
cust <- read.csv("Customer.csv")
prod <- read_delim("product.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# view first 5 observations from loaded datasets
head(cust)
head(prod)

### CLUSTERING ON CUSTOMER DATASET ###
# summary 
summary(cust)

# visualize data
ggpairs(cust[, which(names(cust) != "CUSTOMER_SK")], 
        upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), 
        title = "Customers before outlier removal") 

boxplot(cust$TOTAL_SPENDING) 

# remove outlier
cust.clean <- cust[cust$CUSTOMER_SK != 1, ] 

#summary of cleaned data
summary(cust.clean)

# visualize cleaned data 
ggpairs(cust.clean[, which(names(cust.clean) != "CUSTOMER_SK")], 
        upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), 
        title = "Customers after outlier removal") 

boxplot(cust.clean$TOTAL_SPENDING)

# Normalize data using scale and exclude ITEM_SK column
head(cust.clean)
cust.norm = scale(cust.clean[-c(1,6)]) 
summary(cust.norm)

# Using the elbow method to find the optimal number of clusters
wcss <- vector()
for (i in 1:15) wcss[i] <- sum(kmeans(cust.norm, i)$withinss)
plot(1:15, wcss, type = "b", main = paste('Cluster of customers'),
     xlab = 'Number of Clusters', ylab = 'WCSS')

# Alternative for better visualization - Elbow method
fviz_nbclust(cust.norm, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Customer Number of Clusters - Elbow Method")

# Number of clusters: 4

# K-means using k=4 for products based on results of elbow plot.
ckm = kmeans(cust.norm, 4, 300) 

# Denormalize data by reversing scale function
cust.realCenters = unscale(ckm$centers, cust.norm) 
cust.realCenters 

# Bind clusers to cleansed Data
clusteredCust = cbind(cust.clean, ckm$cluster) 
names(clusteredCust)[8] <- "CLUSTER"

# Visualizing clustering results
plot(clusteredCust[-c(1,6)], col=clusteredCust$CLUSTER) 


############ CLUSTERING ON PRODUCT DATASET ##############
# summary 
summary(prod)

# visualize data
ggpairs(cust[, which(names(cust) != "ITEM_SK")], 
        upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), 
        title = "Products before outlier removal") 

boxplot(prod$BASKETS) 

# remove outlier
prod.clean <- prod[prod$ITEM_SK != 11740941, ] 

#summary of cleaned data
summary(prod.clean)

# visualize cleaned data 
ggpairs(prod.clean[, which(names(prod.clean) != "ITEM_SK")], 
        upper = list(continuous = ggally_points),
        lower = list(continuous = "points"), 
        title = "Products after outlier removal") 

boxplot(prod.clean$BASKETS)

# Normalize data using scale and exclude ITEM_SK column
head(prod.clean)
prod.norm = scale(prod.clean[-1]) 
summary(prod.norm)

# Using the elbow method to find the optimal number of clusters
wcss <- vector()
for (i in 1:15) wcss[i] <- sum(kmeans(prod.norm, i)$withinss)
plot(1:15, wcss, type = "b", main = paste('Cluster of products'),
     xlab = 'Number of Clusters', ylab = 'WCSS')

# Alternative for better visualization - Elbow method Products
fviz_nbclust(prod.norm, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Product Number of Clusters - Elbow Method")

# Number of clusters: 5

# K-means using k=5 for products based on results of elbow plot.
pkm = kmeans(prod.norm, 5, 150) 

# Denormalize data by reversing scale function
prod.realCenters = unscale(pkm$centers, prod.norm) 
prod.realCenters 

# Bind clusters to cleansed Data
clusteredProd = cbind(prod.clean, pkm$cluster) 
names(clusteredProd)[6] <- "CLUSTER"

# Visualizing clustering results
plot(clusteredProd[,2:5], col=clusteredProd$CLUSTER) 

## Export clustered results to CSV file
write.csv(clusteredCust, file = "results_customer.csv", col.names = FALSE)
write.csv(clusteredProd, file = "results_product1.csv", col.names = FALSE)

