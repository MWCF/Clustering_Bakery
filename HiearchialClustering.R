Revised_Data <- read.csv("C:/Users/Wai_f/documents/Revised_Data.csv")
View(Revised_Data)
library(data.table)
Revised_Data1<-Revised_Data[,-c(1,2)]
library(reshape2)
Bakery_data <- Revised_Data1
Bakery_data$Count =1
View(Bakery_data)
data_pivot <- dcast(Bakery_data, Transaction~Revised_Item, fun.aggregate = sum, value.var = "Count")
data_clust <-data_pivot[,-c(1)]
smp_size <- floor(0.25*nrow(data_clust))
set.seed(100)
smp_data <- sample(seq_len(nrow(data_clust)), size = smp_size)
data_clust_smp<- data_clust[smp_data, ]
data_scale <- scale(data_clust_smp)
library(cluster)
agnes1<-agnes(data_scale, diss=FALSE, metric="euclidean", stand=TRUE, method="ward")
(d3 <- as.dendrogram(agnes1)) # 3 branches
plot(d3)
cutree(agnes1,k=5)
