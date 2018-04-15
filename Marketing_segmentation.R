library(readxl)
library(dplyr)
#setwd("~/dev/R/Kaggle")
customer_df <- read_xlsx("Online Retail.xlsx", sheet = 1)
customer_data <- customer_df

summary(customer_data)

# Notice that we CustomerID variable has 135080 NA. As we work with this variable, we will remove the missing values.
customer_data <- subset(customer_data, !is.na(CustomerID))

#range(customer_data$InvoiceDate)

prop.table(table(customer_data$Country))

#Notice that 90% of customers ara in the United Kingdom. If we choose to use the whole data set, we will end
# with a geographical clustering. To avoid that, we are going to use just consutmers from United Kingdom.
# Create a data set for United Kingdom
customer_data <- subset(customer_data, Country == "United Kingdom")

# Create a new variable to specify purchase whithout return
customer_data$ReturnedItem <- grepl("C", customer_data$InvoiceNo)
customer_data$PurchaseReturned <- ifelse(customer_data$ReturnedItem == T, 0, 1)

# Create customer-level dataset 
customer <- data_frame(CustomerID = unique(customer_data$CustomerID))

# Create recency: how many days from the last purchase? smaller number indicates recent activity on the customer's account
customer <- customer %>%
  merge(customer_data %>%
    mutate(Recency = as.Date("2011-12-10") - as.Date(customer_data$InvoiceDate)) %>%
    filter(PurchaseReturned == 1) %>%
    group_by(CustomerID) %>%
    summarize(Recency = min(Recency)) %>%
    mutate(Recency = as.numeric(Recency)),
  by = "CustomerID", all = T, sort = T)

# Create frequency (number of invoices with purchase) and Remove customers who have not made any purchases in the past year
customer <- customer %>%
  merge(
    customer_data %>%
      select(CustomerID, InvoiceNo, PurchaseReturned) %>%
      distinct(CustomerID, InvoiceNo, PurchaseReturned) %>%
      group_by(CustomerID) %>%
      summarize(Frequency = sum(PurchaseReturned, na.rm = T)) #%>%
      #filter(Frequency > 0), 
    ,by = "CustomerID", all = T, sort = T
  )
customer = subset(customer, Frequency > 0)
# Monetary: the amount value spent by a customer during the year
customer <- customer %>%
  merge(
    customer_data %>%
      select(Quantity, UnitPrice, CustomerID) %>%
      mutate(Amount = Quantity * UnitPrice) %>%
      group_by(CustomerID) %>%
      summarize(Monetary = sum(Amount, na.rm = T)),
  by = "CustomerID", all.x = T, sort = T
  )

# Identify customers with negative monetary value numbers, as they were presumably returning purchases from the preceding year
customer$Monetary <- ifelse(customer$Monetary < 0, 0, customer$Monetary)

## Pareto Principle is the concept that 80% of the results generally come from 20% of the causes.
#In this context, it implies that  about 80% of sales would be produced by the top 20% of customers. 
#These 20% represent the high-value, important customers a business would want to protect.

#In this dataset, 80% of the annual sales are produced by the top 29% of customers, so the percentage 
#isn’t quite 20%, but it’s not that far off and it does illustrate that there’s a small portion of customers
# associated with high value.

customer <- customer[order(-customer$Monetary),]
pareto.cutoff <- 0.8 * sum(customer$Monetary)

customer$Pareto <- ifelse(cumsum(customer$Monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")
customer$Pareto <- factor(customer$Pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)

round(prop.table(table(customer$pareto)), 2)
remove(pareto.cutoff)

#Preprocess data

#k-means clustering works better with a normally-distributed data. As we can see below, our variables are skewed.
# Before clustering, we should log transform and standardize them, otherwise, input variables with larger variances will have greater influence on the results. 
sclaes::skewness(customer$Recency)
sclaes::skewness(customer$Frequency)
sclaes::skewness(customer$Monetary)

# Log-transform skewed variables
customer$Recency_log <- log(customer$Recency)
customer$Frequency_log <- log(customer$Frequency)
customer$Monetary_log <- log(customer$Monetary + .1) # because monetary has 0 value, we should add .1 (small value to avoid errors)

# Z-score
customer$Recency_z <- scale(customer$Recency_log, center = T, scale = T)
customer$Frequency_z <- scale(customer$Frequency_log, center = T, scale = T)
customer$Monetary_z <- scale(customer$Monetary_log, center = T, scale = T)

#Visualizing data

# Visualizing the data might be helpfull to select the best number of clusters we need. In the graphs below, 
#the outcome we’re probably most interested in, customer Monetary value, is plotted on the y-axis. 
#Frequency of purchases is on the x-axis, and Recency of purchase by color. 

library(ggplot2)
ggplot(data = customer, aes(x = Frequency, y = Monetary, col = Recency, shape = Pareto)) + geom_point() +
  labs(x = "Frequency: Number of Purchases", y = "Annual sales") +
  scale_y_continuous(labels = scales::percent)

# It is hard to make or find group of clusters with the original data. We will use the log transformed
ggplot(data = customer, aes(x = Frequency_log, y = Monetary_log, col = Recency_log, size = Pareto)) + geom_point() +
  labs(x = "Frequency: Number of Purchases", y = "Annual sales") +
  scale_y_continuous(labels = scales::percent)

# The plot shows that the data is represented in a continuous way. It will be hard to guess the number of clusters
# in a non separated data points.
# We can sse that the data points aggregated in the top right corner represent high amout and high frequency of purchase and belong to the top 20%.
# A few data points are plotted in the bottom left corner represent low annual slaes and low frequency of purchase.
#Depending on the aim of the company bisness, we may focus in this area also to attract these customers to buy more often.

#Still we cannot guess the exact number of clusters we should use. For this reason, we are going to try a range of values

k = 7

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

for (i in 1:k) {
  print(i)
  output <- kmeans(customer[, 8:10], centers = i, nstart = 20)
  
  # Create variables cluster_i to customer data set
  ClusterNo <- paste("Cluster", i, sep = "_")
  customer[,(ClusterNo)] <- output$cluster
  customer[,(ClusterNo)] <- factor(customer[,(ClusterNo)], levels = c(1:k))
  
  print(ggplot(customer, aes(x = Frequency_log, y = Monetary_log, col = customer[, (ClusterNo)])) + geom_point())
  
  # Collect model information
  models[i,("k")] <- i
  models[i,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
  models[i,("betweenss")] <- output$betweenss
  models[i,("totss")] <- output$totss # betweenss + tot.withinss
  models[i,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership

  print(models)
  
  cluster_centers <- ddply(customer, .(customer[, (ClusterNo)]), summarize,
          monetary=round(median(Monetary),2),
          frequency=round(median(Frequency),1),
          recency=round(median(Recency), 0))
  names(cluster_centers)[names(cluster_centers)=="customer[, (ClusterNo)]"] <- "Cluster"
  print(cluster_centers)
}

# Notice that we increase complexity of models as we increase k.
#cluster 5 gives us more insight comparing to the previous one and an easy understanding clusters compared to the latters.

#Let's plot the variance explained by the clusters
# Graph variance explained by number of clusters
ggplot(models, aes(x = k, y = rsquared)) +
  geom_point() + geom_line() +
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = 1:k) + 
  labs(x = "k (Number of Clusters)", y = "Variance Explained")
# AS you can see, cluster2 explains just 49% of the variance, whereas k = 5 explains 73% of the variance.

# What k to choose depends on the question we tray to answer? 

#If the goal is to understand a range of customer behavior from 
#high-to-low value customers, maybe we would recommend the 5-cluster solution which 
#gives us a cluster 2: high monetary - high frequency and recent purchase;
#cluster 3: medium value(monetary) - medium frequency - relatively recent purchase;
#cluster 1 and 5: low value - low frequency - with recent purchase for cluster 1 and last purchase for cluster 5;
#cluster 4: 0 value. 





