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

# EAD on categorical variables
# Item_Fat_Content variable
ggplot(data = combi, aes(x = Item_Fat_Content)) + geom_bar(fill = "red")
# from chart we know that there are only two categories low fat and regular

combi$Item_Fat_Content[combi$Item_Fat_Content == 'LF'] <- "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == 'low fat'] <- "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == 'reg'] <- "Regular"
str(combi)
ggplot(data = combi, aes(x = Item_Fat_Content)) + geom_bar(fill = "red")
combi$Item_Fat_Content
levels(combi$Item_Fat_Content)

# for other cate. variables
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cowplot")
library(dplyr)
str(combi)
a <- ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Type, Count), stat = "identity",fill = "coral1") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) + xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  ggtitle("Item_Type")
  
b <- ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "Darkgreen") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +ggtitle("Outlet_Identifier")

c <- ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +
  geom_bar(aes(x = Outlet_Size, y = Count), stat = 'identity', fill = "Red") +
  geom_label(aes(x = Outlet_Size, y = Count, label = Count), vjust = 0.5)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) + ggtitle("Outlet_Size")

second_row <- plot_grid(b,c, nrow = 1)
plot_grid(a,second_row, ncol = 1)

d <- ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n()))+
  geom_bar(aes(factor(Outlet_Establishment_Year), Count),stat = 'identity',fill = "Darkblue") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5)) + xlab("Outlet_Establishment_Year")

e <- ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +
  geom_bar(aes(Outlet_Type, Count), stat = 'identity',fill= "coral1") +
  geom_label(aes(Outlet_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))


plot_grid(d,e, ncol = 2)

train <- read.csv(file.choose())
head(train)
test <- read.csv(file.choose())
str(train)
#Let’s quicky check the dimensions of our data, i.e., columns and rows.
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
train <- combi[1:nrow(train),]
dim(train)
# BIVARIATE ANALYSIS: Exploring each independent variable
#against dependent variable
str(train)
install.packages("ggplot2")

install.packages("cowplot")
library(ggplot2)
library(cowplot)
# using scatter plots
p1 <- ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), color = "red", alpha= 0.6) + 
  theme(axis.title = element_text(size = 8.5))
                                                                                                          
p2 <- ggplot(train) + geom_point(aes(Item_Visibility, Item_Outlet_Sales), color = "Darkblue", alpha = 0.3)+
  theme(axis.title = element_text(size = 8.5))


p3<- ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), color = "Darkgreen", alpha= 0.3)+
  theme(axis.title = element_text(size = 8.5))


second_row <- plot_grid(p2,p3,nrow = 1,ncol = 2)
plot_grid(p1, second_row,nrow = 2)

# Item_Outlet_Sales is spread well across the entire range of the Item_Weight without any obvious pattern.
# In Item_Visibility vs Item_Outlet_Sales, there is a string of points at Item_Visibility = 0.0 which seems strange as item visibility cannot be completely zero. We will take note of this issue and deal with it in the later stages.
# In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 4 segments of prices that can be used in feature engineering to create a new variable.

# Target Variable vs Independent Categorical Variables
train$Item_Fat_Content[train$Item_Fat_Content == 'LF'] <- "Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content == 'low fat'] <- "Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content == 'reg'] <- "Regular"
a1 <- ggplot(train) + geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill ="magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text =  element_text(size = 6),
        axis.title = element_text(size=8.5))
a2 <- ggplot(train) + geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
a3 <- ggplot(train) + geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1), axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))


second_row2 <- plot_grid(a1,a3, ncol = 2)
plot_grid(a2,second_row2, nrow = 2)
# # Distribution of Item_Outlet_Sales across the categories of Item_Type is not very distinct and 
# same is the case with Item_Fat_Content.
# # The distribution for OUT010 and OUT019 categories of Outlet_Identifier are quite 
# similar and very much different from the rest of the categories of Outlet_Identifier
a4 <- ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
a5 <- ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
a6 <- ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1), axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

plot_grid(a5,a6, nrow = 2)














