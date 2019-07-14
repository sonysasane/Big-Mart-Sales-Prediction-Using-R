train <- read.csv(file.choose())
head(train)
test <- read.csv(file.choose())
str(train)
#Let's quicky check the dimensions of our data, i.e., columns and rows.
dim(train)
dim(test)
# We will take a quick glance over the feature names of train and test datasets.
names(train)
names(test)
#Combine Train and TestTo explore data in any data science competition, 
#it is advisable to append test data to the train data. Combining train and test sets saves a 
#lot of time and effort because if we have to make any modification in the data, we would make
#the change only in the combined data and not in train and test data separately. Later we can always split the combined data back to train and test.
test$Item_Outlet_Sales <- NA
combi <- rbind(train,test)
dim(combi)
# many statistical test considers data to be normally distributed hence we remove skewness

# visualizing output variable
library(ggplot2)

ggplot(data = train, aes(x = Item_Outlet_Sales)) + geom_histogram(binwidth =100, fill = "Darkgreen") +
  xlab("Outlet_sales")
# output variable is right skewed thus we need to do some transformation
# visualizing other three numerical variables
p <- ggplot(data = combi) + geom_histogram(aes(x = Item_Weight), binwidth = 0.5 ,fill = "darkgreen") +
     xlab("Item_Weight")
q <- ggplot(data = combi) + geom_histogram(aes(x = Item_Visibility), binwidth = 0.005 ,fill = "darkgreen") +
  xlab("Item_Visibility")
r <- ggplot(data = combi) + geom_histogram(aes(x = Item_MRP), binwidth = 1 ,fill = "darkgreen") +
  xlab("Item_MRP")
install.packages("cowplot")
library(cowplot)

plot_grid(p,q,r, nrow = 1) # plots all plots in one row it is in cowplot package

