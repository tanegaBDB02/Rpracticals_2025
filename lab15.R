# V. Linear Regression
# (1) Enter the following data sets:
#   x = c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
#   y = c(0.87, 7.12, 14.01, 24.37, 39.058, 58.62, 83.92)
#   Use the lm() function to fit the above data y = f(x) to a third degree polynomial.
# (2) Print the coefficient of x term
# (3) Print the coefficient of x2term
# (4) Print the coefficient of x3term
# (5) Plot the original data and the fitted curve in the same graph. Both should show the
#   data points clearly.


x <- c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
y <- c(0.87, 7.12, 14.01, 24.37, 39.058, 58.62, 83.92)
model <- lm(y ~ poly(x, 3, raw=TRUE))
summary(model)

coef(model)

plot(x, y, main="Third Degree Polynomial Fit", xlab="x", ylab="y", pch=19)

curve(predict(model, data.frame(x=x)), add=TRUE, col="lightpink", lwd=2)


#VI. Multiple linear regression
# In this example we load the trees data set shipping with the
# R-package datasets. The trees data set provides measurements of the girth, height and volume
# of timber in 31 felled black cherry trees, also known as Prunus serotina. The height of the trees
# is given in feet (ft) and the volume is given in cubic feet (cft). The girth is the diameter of the
# tree (in inches) measured at 4 ft 6 in (approx. 1.37 m) above the ground.

# library(datasets)
# head(trees)
# (i) Read the trees dataset into a dataframe, and convert all the quantities into SI unit of
# metres. Volume will be in m3. The conversions are 1in = 0.0254m, 1f t = 0.3048m and 1cf t = 0.028317m3.

library(datasets)
head(trees)

df <- trees
df$Girth <- df$Girth * 0.0254
df$Height <- df$Height * 0.3048
df$Volume <- df$Volume * 0.028317

# (ii) It is always a good idea to visualize the data we are working with. However, instead
# of a scatter plot, which is fine for the comparison of two variables, we plot a scatter
# plot matrix to account for multiple variables. The lattice package provides the handy
# splom() function for plotting scatter plot matrices. List your observations about the
# correlations between the three quantities.
# library(lattice)
# splom(df, xlab = "Scatter Plot Matrix")
# Our goal is to build a linear regression model with Volume being the dependent variable
# and Height and Girth being the independent (explanatory) variables. We will do this
# in two ways: Use matrix methods, and also by step-wise fitting of one coefficient at a
# time. A multiple regression model is built by finding the optimal set of coefficients β.
# The regression model isyi = β0 + Xi βixij + εi (1) with the objective function being ε =Pi=1(yi − xTi β)2.

library(lattice)
splom(df, xlab = "Scatter Plot Matrix")


#(iii) Save the column “Volume” as the response vector y from the dataframe.
y <- df$Volume

#(iv) Initialise the intercept β0 to value 1 for each tree (row number)
beta0 <- rep(1, nrow(df))

#(v) Build the X matrix by grouping the “Girth”, “Height” and the β0 columns.
X <- cbind(beta0, df$Girth, df$Height)



# (vi) The following equation allows us to solve for the unknowns, namely the regression coefficients.
# β = (XTX)−1XT y (2)
# Use the following command to get the solutions, make sure to compare this with the
# above equation. solve() yields the inverse of a matrix, t() yields the transpose and
# %*% does matrix multiplication.
# model1 <- solve(t(X) %*% X) %*% t(X)%*% y # solving for beta
# What is the solution? Print the values of β0,β1 and β2, since our current model is of the
# form V olume = β0 + β1 ∗ (Girth) + β2 ∗ (Height).

model1 <- solve(t(X) %*% X) %*% t(X) %*% y
model1

#(vii) Predict the volume for girth values of 0.3,0.4 and 0.5 with heights of 20, 21 and 22 respectively.
newdata_X <- cbind(1, c(0.3, 0.4, 0.5), c(20, 21, 22))
predicted_volume_matrix <- newdata_X %*% model1
predicted_volume_matrix

# (viii) Instead of using the solve() function let’s get the solutions using lm(). Use the formula
# Volume~Girth+Height to get the solutions, and compare it with those obtained by
# solve(). The numbers should match, if not you have bug(s)! Save the output of the
# lm() function as model2.
model2 <- lm(Volume ~ Girth + Height, data = df)
model2

# (ix) We can use the predict() function to predict multiple volume values:
# newdata <- data.frame("Girth"=(0.3,0.4,0.5),"Height"=c(20,21,22))
# predict(model2,data=newdata)

newdata <- data.frame(Girth = c(0.3, 0.4, 0.5), Height = c(20, 21, 22))
predict(model2, newdata = newdata)


#VII. Non-linear regression
# 
# (1) Enter the following data sets into R:
# xdat = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2,
#            2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)
# ydat = c(-0.09, 0.59, 0.42, 2.03, 3.43, 1.84, 5.30, 4.40, 6.67, 7.40,
#          7.34, 8.76, 10.25, 10.93, 13.78, 14.21, 17.82, 21.83, 23.04,
#          24.25, 27.48)

xdat <- c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)
ydat <- c(-0.09, 0.59, 0.42, 2.03, 3.43, 1.84, 5.30, 4.40, 6.67, 7.40, 7.34, 8.76, 10.25, 10.93, 13.78, 14.21, 17.82, 21.83, 23.04, 24.25, 27.48)


#(2) Create a data frame of the above data
df <- data.frame(xdat, ydat)

# (3) Use the nls() function to fit the above data to a function of the form y = f(x) where n
# is to be found out. Use a reasonable start guess for n based on your visual examination
# of the data. What is the final value of the power you get from the fit?

model <- nls(ydat ~ a * xdat^n, data = df, start = list(a = 1, n = 2))

#(4) Print the summary of the fit results including the point estimate of power, the standard
#error, and t-value.
summary(model)

#(5) How many degrees of freedom are there?
model$df.residual

#(6) Create a sequence of numbers from 1 to 3 with spacing of 0.1. Use the fitted function to
#generate the fitted curve. Plot the curve along with the original data points (xdat,ydat)
xseq <- seq(1, 3, by = 0.1)
yfit <- predict(model, newdata = data.frame(xdat = xseq))
plot(xdat, ydat)
lines(xseq, yfit, col = "deeppink")


#VIII. Clustering methods


# (1) Hierarchical Clustering In a research study by Spellman et al. all genes in the yeast
# genome (>6000) that exhibited oscillatory behaviour (which in turn suggested cell cycle
# regulation) were identified. To do so, they measured gene expression over time, using
# six different types of cell cycle synchronization experiments. Here we will use only the
# genes that were identified as showing oscillatory behaviour, numbering to 700.

#(i) Load the packages tidyverse,dplyr and RcolorBrewer.
library(tidyverse)
library(dplyr)
library(RColorBrewer)

# (ii) Read the csv file spellman-wide.csv to a dataframe called spellman. Print the
# dimensions of this dataframe.
spellman <- read.csv("/home/ibab/R/spellman-wide.csv")
dim(spellman)

# (iii) Print the first 5 rows and first 8 columns of spellman dataframe. This data is
# provided in a tidy wide format with genes in columns. The first two columns give
# the experiment name and the corresponding time point at which the sample was
# collected.
spellman[1:5, 1:8]

# (iv) We will carry out hierarchical clustering of this data through the agglomerative
# approach using the hclust() function. The method argument to this function
# determines the group distance function (i.e. single linkage, complete linkage,
# average linkage etc). The input is a dissimilarity matrix such as distance matrix.
# We will convert an arbitary square matrix to a distance object by applying the
# as.dist function to the matrix. We are interested in clustering genes to discover
# what sets of genes are expressed in similar patterns (since genes that are expressed
# in a similar manner are likely regulated by the same sets of transcription factors).
# The correlation coefficient is a suitable measure of linear association between
# variables. Therefore correlation is a measure of similarity and we will convert it
# into a dissimilarity matrix as follows.
# spellman_cor <- select(spellman, -time,-expt) %>%
#   cor(use="pairwise.complete.obs")
# spellman_dist <- as.dist(1-spellman_cor)
# The use argument to the cor() function specifies that when there are missing
# values, the function should use all available observations that have data necessary
# to calculate a correlation for any given pair of genes. Then the correlation is turned
# into a distance measure by subtracting it from 1 (perfectly correlated genes will
# have distance 0) and passing it to the as.dist function. What does %>% do?

spellman_cor <- spellman %>%
  select(-time, -expt) %>%
  cor(use = "pairwise.complete.obs")

spellman_dist <- as.dist(1 - spellman_cor)

# (v) Generate the hierarchical clustering using the ‘complete’ linkage method.
# spellman_tree <- hclust(spellman_dist, method="complete")
# plot(spellman_tree)
# The plot is a cluster dendrogram plot and looks beyond messy. Does it improve
# if you decrease the font size? What happens when you set cex=0.1
spellman_tree <- hclust(spellman_dist, method = "complete")
plot(spellman_tree)
plot(spellman_tree, cex = 0.1) # reduce font size

# (vi) Using dendextend package to manipulate the clusters: Load this package. Create
# a dendrogram using the tree, plot this without the labels using leaflab=’none’.
# spellman_dend <- as.dendrogram(spellman_tree)
library(dendextend)
spellman_dend <- as.dendrogram(spellman_tree)
plot(spellman_dend, leaflab = 'none')

# (vii) Broadly by looking at the dendrogram we see that there are 4 clusters. Use the
# cutree function to group the dendrogram branches into 4 groups.
# clusters <- cutree(spellman_dend, k=4)
# table(clusters)
# Print the cluster assignments for the first 6 genes as clusters[1:6].
clusters <- cutree(spellman_dend, k = 4)
table(clusters)
clusters[1:6]

# (viii) Let’s colour the branches using different colours.
# plotc<- color_branches(spellman_tree,k=4) Plot this as plot(plotc,leaflab=’none’).
# Repeat this with k=8 and print the frequency table of cluster number and number
# of genes in each cluster.
plotc <- color_branches(spellman_tree, k = 4)
plot(plotc, leaflab = 'none')

plotc8 <- color_branches(spellman_tree, k = 8)
plot(plotc8, leaflab = 'none')
table(cutree(spellman_tree, k = 8))

# (ix) Let’s put together the above in a dataframe. The data frame has gene names as
# rows and cluster number as the column.
# clusters_df <- data.frame(gene=names(clusters),cluster=clusters)
# Print the cluster to which the gene YALO22C belongs to.
clusters_df <- data.frame(gene = names(clusters), cluster = clusters)
filter(clusters_df, gene == "YAL022C")

# (x) Get all the names of the genes in the 3rd cluster using the filter() function
# on the clusters_df dataframe and selecting the gene column. Store this as
# cluster3_genes.
cluster3_genes <- clusters_df %>%
  filter(cluster == 3) %>%
  pull(gene)

# (xi) We will now generate a heatmap showing the expression of all the genes for the
# first cluster. Carry out the following commands and study their behaviour.
# spellman_long <- gather(spellman, gene, expression, -expt,-time)
# head(spellman_long)
# The heat map generation involves the following two steps (play around with the
# options to see how the heat map changes)
# color_scheme <- rev(brewer.pal(8,"RdBu")) # generate the color scheme to use
# spellman_long %>%
#   filter(gene %in% cluster3_genes & expt == "alpha") %>%
#   ggplot(aes(x = time, y = gene)) +
#   geom_tile(aes(fill = expression)) +
#   scale_fill_gradientn(colors=color_scheme, limits = c(-2,2)) +
#   theme(axis.text.y = element_text(size = 6)) # set size of y axis labels

spellman_long <- gather(spellman, gene, expression, -expt, -time)
head(spellman_long)

color_scheme <- rev(brewer.pal(8, "RdBu"))

spellman_long %>%
  filter(gene %in% cluster3_genes & expt == "alpha") %>%
  ggplot(aes(x = time, y = gene)) +
  geom_tile(aes(fill = expression)) +
  scale_fill_gradientn(colors = color_scheme, limits = c(-2, 2)) +
  theme(axis.text.y = element_text(size = 6))


# (xii) Combining heat map and dendrogram: We will do this using a sub-tree, otherwise
# the visual result becomes too cluttered.

# (a) The function cutree is capable to horizontal and vertical cuts. But it cannot
# return an object representing the sub-trees themselves. This is achieved
# using the cut() function, which returns the upper cut of trees as well as
# lower cut of trees. We will use the lower sub-trees.
# sub_trees <- cut(spellman_dend, h=1.48)
# sub_trees$lower
sub_trees <- cut(spellman_dend, h = 1.48)
sub_trees$lower

# (b) The lower sub-trees is a list and we can extract any particular sub-tree by
# indexing into the list: cluster3_tree <- sub_trees$lower[[3]]. Print
# this sub-tree.
cluster3_tree <- sub_trees$lower[[3]]
print(cluster3_tree)

# (c) Plot this tree as a dendrogram by setting some parameters as given below.
# cluster3.tree %>%
#   set("labels_cex", 0.45) %>%
#   set("labels_col", "red") %>%
#   plot(horiz = TRUE) # plot horizontally
cluster3_tree %>%
  set("labels_cex", 0.45) %>%
  set("labels_col", "red") %>%
  plot(horiz = TRUE)

# (d) Load the package gplots. We will use the function heatmap.2 within this
# package to combine the dendrogram and the heatmap.
# alpha_factor <- filter(spellman, expt == "alpha")
# alpha_mtx <- as.matrix(dplyr::select(alpha_factor, -time, -expt))
# row_names(alpha_mtx) <- alpha_factor$time
# transposed_alpha_mtx <- t(alpha_mtx)
# heatmap.2(transposed_alpha_mtx,
#           Rowv = cluster3_tree, # use the dendrogram previously calculated
#           Colv = NULL, # don’t mess with my columns! (i.e. keep current ordering )
#           dendrogram = "row", # only draw row dendrograms
#           breaks = seq(-2, 2, length.out = 9), # OPTIONAL: set break points for colors
#           col = color_scheme, # use previously defined colors
#           trace = "none", density.info = "none", # remove distracting elements of plot
#           xlab = "Time (mins)")

library(gplots)

alpha_factor <- filter(spellman, expt == "alpha")
alpha_mtx <- as.matrix(select(alpha_factor, -time, -expt))
rownames(alpha_mtx) <- alpha_factor$time
transposed_alpha_mtx <- t(alpha_mtx)

heatmap.2(transposed_alpha_mtx,
          Rowv = cluster3_tree,
          Colv = NULL,
          dendrogram = "row",
          breaks = seq(-2, 2, length.out = 9),
          col = color_scheme,
          trace = "none",
          density.info = "none",
          xlab = "Time (mins)")


# (2) K-means clustering In this exercise we will look at AirBNB data to study how prices
# and number of reviews are related.

#(i) Load the packages tidyverse, ggplot2 and dplyr.
# Loading the required packages
library(tidyverse)
library(ggplot2)
library(dplyr)


#(ii) Read the csv file listings_airbnb.csv and print the number of rows and the
#names of the columns. Call the dataframe as listings.
# Reading the dataset
listings <- read.csv("/home/ibab/R/listings_airbnb.csv")

cat("Number of rows:", nrow(listings), "\n")
cat("Column names:", colnames(listings), "\n")


# (iii) Let’s make a scatter plot of the price and number_of_reviews columns as follows.
# ggplot(listings, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
#   geom_point(alpha = 0.25) +
#   xlab("Number of reviews") +
#   ylab("Price")
# Write down at least 2 important observations from this plot.
# Scatter plot
ggplot(listings, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")


# (iv) When comparing variables with different units, it is important to normalize the
# variables. Use the scale() function to do this.
# listings[,c("price","number_of_reviews")] <- scale(listings[,c("price","number_of_reviews")])
# Normalizing price and number_of_reviews columns
listings[,c("price", "number_of_reviews")] <- scale(listings[,c("price", "number_of_reviews")])


# (v) Create a subset of the data by selecting from the scaled data the columns ‘price’,
# ‘minimum nights’ and ‘number of reviews’, call this airbnb_2cols. Print this subset.
airbnb_2cols <- listings %>% select(price, minimum_nights, number_of_reviews)
print(airbnb_2cols)

# (vi) We will now do the K-means clustering on this 2-column data. Print the output.
# set.seed(23)
# km_out <- kmeans(na.omit(airbnb_2cols,centers=3,nstart=20)
set.seed(23)
km_out <- kmeans(na.omit(airbnb_2cols), centers = 3, nstart = 20)
print(km_out)

                 
# (vii) How do we arrive at number of clusters in a robust way? We try different cluster
# sizes up to a threshold and make a ‘scree’ plot. Keeping nclus_max=10, write a
# for loop in which you execute the kmeans() function as given above and for each
# iteration, give the number of centers as the iterative index, and store the value
# of ‘within-sum-of-squares (wss)’ output for each iteration. This is extracted as
# km_ou$tot.withinss.
wss <- numeric(10)
for (k in 1:10) {
  km_out <- kmeans(na.omit(airbnb_2cols), centers = k, nstart = 20)
  wss[k] <- km_out$tot.withinss
  cat("K:", k, " WSS:", wss[k], "\n")
}

# (viii) Now we produce a ‘scree’ plot that allows us to gauge what is the appropriate
# number of clusters for this data. Follow the steps below, and find a point where
# the WSS decreases slower after adding another cluster point. This will be the
# optimum number of clusters.
# wss_df <- tibble(clusters=1:nclusters,wss = wss)
# scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
# geom_point(size = 4)+
# geom_line() +
# scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
# xlab(’Number of clusters’)
# scree_plot +
# geom_hline(
# yintercept = wss,
# linetype = ’dashed’,
# col = c(rep(’)

wss_df <- tibble(clusters = 1:10, wss = wss)
scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab("Number of clusters") +
  ylab("Within Sum of Squares")
scree_plot + geom_hline(
  yintercept = wss[5], linetype = "dashed", col = "beige"
)
        
# (ix) You should find that the optimum number of clusters is 5. Using K=5, repeat the
# calculation of km_out using the kmeans() function and use the following lines to
# plot the data according to the cluster ID.
# km_out<- na.omit(airbnb_2cols)
# km_out$cluster_id <- factor(km.out$cluster)
# ggplot(km_out, aes(number_of_reviews, price, color = cluster_id)) +
# geom_point(alpha = 0.25) +
# xlab("Number of reviews") +
# ylab("Price")

km_out <- kmeans(na.omit(airbnb_2cols), centers = 5, nstart = 20)
airbnb_2cols$cluster_id <- factor(km_out$cluster)
ggplot(airbnb_2cols, aes(number_of_reviews, price, color = cluster_id)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")
