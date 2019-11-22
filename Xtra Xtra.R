setwd("C:/Users/Gebruiker/Downloads")
flowers <- read.csv("exercise.csv")

#inventory
summary(flowers)
str(flowers)

# 2.- Find missing values and change them by the median of the column. If the missing value is a categorical variable remove the row

flowers$Sepal.Length[is.na(flowers$Sepal.Length)] <- median(flowers$Sepal.Length, na.rm=TRUE)
flowers$Sepal.Width[is.na(flowers$Sepal.Width)] <- median(flowers$Sepal.Width, na.rm=TRUE)
flowers$Petal.Width[is.na(flowers$Petal.Width)] <- median(flowers$Petal.Width, na.rm=TRUE)
flowers$Petal.Length[is.na(flowers$Petal.Length)] <- median(flowers$Petal.Length, na.rm=TRUE)

flowers<- na.omit(flowers)
summary(flowers)
str(flowers)


### 3.- Make a boxplot of every numerical variable. Remove outliers.

help(boxplot)

boxplot(flowers$Sepal.Length,
        main="Sepal Length", 
        sub=paste("Outlier rows: ", 
        boxplot.stats(flowers$Sepal.Length)$out))
boxplot(flowers$Sepal.Length)$out

outSepLen<- boxplot(flowers$Sepal.Length, plot= FALSE)$out

flowers[which(flowers$Sepal.Length %in% outSepLen),]
flowers <- flowers[-which(flowers$Sepal.Length %in% outSepLen),]


#sepal width
outSepWid<- boxplot(flowers$Sepal.Width, plot= TRUE)$out

flowers[which(flowers$Sepal.Length %in% outSepLen),]
flowers <- flowers[-which(flowers$Sepal.Width %in% outSepWid),]

boxplot(flowers$Sepal.Width)


# Petal
outPetLen<- boxplot(flowers$Petal.Length, plot= TRUE)$out
outPetWid<- boxplot(flowers$Petal.Width, plot= TRUE)$out

flowers[which(flowers$Petal.Length %in% outPetLen),]

flowers <- flowers[-which(flowers$Petal.Length %in% outPetLen),]
flowers <- flowers[-which(flowers$Petal.Width %in% outPetWid),]

boxplot(flowers)

### 4.- Rename the “Species” column for "Plants"


names(flowers)[names(flowers) == 'Species'] <- 'Plants'

names(flowers$Plants)<-"Berend"
names(flowers)

names(flowers)<- c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width",  "Plants")
colnames(flowers)[5] <- "Berend"


colnames(flowers)[5] <- "Plants"
names(flowers)


### 5.- Create one extra column named "Petal.Area" which is Petal.Length * Petal.Width
Petal.Area <- flowers$Petal.Length*flowers$Petal.Width

flowers <- cbind(flowers, as.data.frame(Petal.Area))


### 6.- Plot the correlation matrix as a heatmap

numflowers <- flowers
numflowers$Plants <- NULL

corflowers <- cor(numflowers)
corflowers

library(reshape2)

melted.corflow <- melt(corflowers)


library(ggplot2)

ggplot(data = melted.corflow,
       aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(corflowers){
  corflowers[upper.tri(corflowers)] <- NA 
  return(corflowers)
  }
# Get upper triangle of the correlation matrix
get_upper_tri <- function(corflowers){
  corflowers[lower.tri(corflowers)]<- NA
  return(corflowers)
}

upper_tri <- get_upper_tri(corflowers)
upper_tri

# Melt the correlation matrix
melt.cor.flow <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melt.cor.flow, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


# function to reorder cor matrix according to correlation
reorder_corflow <- function(corflowers){
  # Use correlation between variables as distance
  dd <- as.dist((1-corflowers)/2)
  hc <- hclust(dd)
  corflowers <-corflowers[hc$order, hc$order]
}

# Reorder the correlation matrix
corflowers <- reorder_corflow(corflowers)
upper_tri <- get_upper_tri(corflowers)

# round off values 'upper tri' for nice visualisation in the last step
upper_tri <- round(upper_tri, digits = 2)

# Melt the correlation matrix
melt.cor.flow <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melt.cor.flow, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# Print the heatmap
print(ggheatmap)

#make it pretty :)
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



  ################################
 ###   Exercise with ggplot   ###
################################

# 1.- Plot Petal.Width vs Petal.Length and different colours according to species

help(ggplot)

ggplot(flowers, aes(flowers$Petal.Width, 
                    flowers$Petal.Length, 
                    color = flowers$Plants))+
  geom_point()


# 2.- Plot a histogram of Petal.Width variable

ggplot(flowers, aes(flowers$Petal.Width))+
         geom_histogram(color = "black", fill = "white")+
          stat_bin(binwidth = 0.25, boundary = 0)

ggplot(flowers, aes(x=Petal.Width)) +
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(Petal.Width)),
             color="blue", linetype="dashed", size=1)

