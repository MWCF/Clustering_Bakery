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
dim(data_scale)
qr(data_scale)$rank
Data_Clean<-data_scale[,-c(4,13,15,23,28,33,34,45,50,58)]# remove no data columns
library(NbClust)
NBClust<-NbClust(data = Data_Clean, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D", index = "all")
#based on NbClust majority rule, best number of clusters is 4
#Nbclust result on euclidean distance model with ward D method
#*** : The Hubert index is a graphical method of determining the number of clusters.
#In the plot of Hubert index, we seek a significant knee that corresponds to a 
#significant increase of the value of the measure i.e the significant peak in Hubert
#index second differences plot. 
#
#*** : The D index is a graphical method of determining the number of clusters. 
#In the plot of D index, we seek a significant knee (the significant peak in Dindex
#                                                    second differences plot) that corresponds to a significant increase of the value of
#the measure. 
#
#******************************************************************* 
#  * Among all indices:                                                
# * 5 proposed 2 as the best number of clusters 
# 2 proposed 3 as the best number of clusters 
# 6 proposed 4 as the best number of clusters 
# 3 proposed 5 as the best number of clusters 
# 3 proposed 6 as the best number of clusters 
# 1 proposed 9 as the best number of clusters 
# 3 proposed 10 as the best number of clusters 

#***** Conclusion *****                            

#* According to the majority rule, the best number of clusters is  4 
library(cluster)
agnes2<-agnes(Data_Clean, diss=FALSE, metric="euclidean", stand=TRUE, method="ward")
(d4 <- as.dendrogram(agnes2)) # create dendrogram
plot(d4)
d4cut<-cutree(agnes2,k=4)
write.csv(data_clust_smp,'C:/Users/Wai_f/documents/smp_data.csv')

