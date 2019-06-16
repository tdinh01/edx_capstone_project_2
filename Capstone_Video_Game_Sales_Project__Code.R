# Name:  Tan Dinh
# Title:  This is code for my Capstone Video Game Sales Project
# Date: June 16, 2019
# EDX Capstone Project 2



library(tidyverse)
library(dplyr)
library(caret)



#############################################################
# Create train set and test set data file from Video Game Sales as at 22 Dec 2016.csv file
#############################################################


library(tidyverse)
library(caret)

library(readr)
Video_Games_Sales_as_at_22_Dec_2016 <- read_csv("Video_Games_Sales_as_at_22_Dec_2016.csv")


video_game_sales <- Video_Games_Sales_as_at_22_Dec_2016

video_game_sales[video_game_sales == "N/A"]  <- NA #data cleaning of N/As

video_game_sales_final <- na.omit(video_game_sales) #data cleaning of na in dataset


set.seed(1)
test_index <- createDataPartition(y = video_game_sales_final$Global_Sales, times = 1, p = 0.1, list = FALSE)  
train_set <- video_game_sales_final[-test_index,]
temp <- video_game_sales_final[test_index,]


# Make sure Genre and Publisher in test set are also in train set

test_set <- temp %>% 
  semi_join(train_set, by = "Genre") %>%
  semi_join(train_set, by = "Publisher")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#############################################################
###A3.	***Data Exploration***
#############################################################


# dimentions to show number of rows and columns
dim(train_set) 


# to look at the structure of the data features
glimpse(train_set) 

#To show the first ten rows of the data in three chunks and present it as a kable table
#chunk1
train_set1  <-  train_set %>% 
  select(1:5)
head(train_set1, n=10) %>% 
  knitr::kable()

#chunk 2
train_set1  <-  train_set %>% 
  select(6:10)
head(train_set1, n=10) %>% 
  knitr::kable()

#chunk3
train_set1  <-  train_set %>% 
  select(11:16)
head(train_set1, n=10) %>% 
  knitr::kable()


# code to calculate a five number summary
train_set2 <- train_set %>%
  select(Global_Sales)
summary(train_set2)


###A4.	***Data Visualization***
#The data was plotted to see different trends in the data.  

#Code to Figure 1
#plot by Genre
c <- video_game_sales_final %>% 
  mutate(n=1) %>% 
  group_by(Genre) %>% 
  summarize(average=mean(Global_Sales))

ggplot(c, aes(x=Genre, average)) +
  geom_point()+
  labs(y = "Average Global Sales")+
  ggtitle("Figure 1: Average Global Sales vs Genre")

#Code to Figure 2
c <- video_game_sales %>% 
  mutate(n=1) %>% 
  group_by(Publisher) %>% 
  summarize(average=mean(Global_Sales))

ggplot(c, aes(Publisher, average))+
  geom_point()+
  ggtitle("Figure 2: Average Global Sales vs Publisher")+
  labs(y = "Average Global Sales")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#code to figure 3
#An analysis of the year the video game was release compared to global sales.  
c <- video_game_sales_final %>% 
  mutate(n=1) %>% 
  group_by(Year_of_Release) %>% 
  summarize(average=mean(Global_Sales))
d <- c %>% 
  mutate(year=as.numeric(Year_of_Release))

ggplot(d, aes(Year_of_Release, average))+
  geom_point() +
  ggtitle("Figure 3: Average Global Sales vs Year of Release")+
  labs(y = "Average Global Sales", x="Year of Release")+
  scale_x_discrete(breaks = seq(1985, 2016, by = 5))

#code to figure 4
#Figure 4 below shows a histogram of global sales relative frequency. 
train_set %>%
  ggplot(aes(Global_Sales)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous() +
  scale_y_continuous() +
  xlab("Sales") +
  ylab("Relative Frequency (%)") +
  ggtitle("Figure 4: Global Sale Histogram")+
  ylim(0, 20)


#IV Results

# = -----------------------------------------------------------------------
#a. average model
# code to calculate mean of global sales 
mu <- mean(train_set$Global_Sales)  

#code to calculate first RMSE
naive_rmse <- RMSE(train_set$Global_Sales, mu) 

#code to create table for the first RMSE
rmse_results <- tibble(method = "Just the Average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

#-----------------------------------------------------------------------
# linear model
# Since Year_Of_Release is type chr, I add column that converts year to numeric.
train_set_numyr <- train_set %>% mutate(numyr = as.numeric(Year_of_Release))
test_set_numyr <- test_set %>% mutate(numyr = as.numeric(Year_of_Release))

#code to create linear model
fit_lm <- lm(Global_Sales ~ numyr, data=train_set_numyr)
lm_predict <- predict(fit_lm, test_set_numyr) #code to predict for test set
rmse_lm <- RMSE(test_set_numyr$Global_Sales, lm_predict) #calculate RMSE

rmse_results <- bind_rows(rmse_results, 
                          tibble(method="Linear Model",
                                 RMSE = rmse_lm))  # putting data into a table
rmse_results %>% knitr::kable() #put in kable table


# - -----------------------------------------------------------------------
# LOESS model

# loess degree 1
fit_loess1 <- loess(Global_Sales ~ numyr, degree=1, data=train_set_numyr)
loess1_predict <- predict(fit_loess1, test_set_numyr)
rmse_loess1 <- RMSE(test_set_numyr$Global_Sales, loess1_predict)
rmse_loess1

# putting data into a table
rmse_results <- bind_rows(rmse_results, 
                          tibble(method = "LOESS degree 1", RMSE = rmse_loess1))  

# loess degree 2
fit_loess2 <- loess(Global_Sales ~ numyr, degree=2, data=train_set_numyr)
loess2_predict <- predict(fit_loess2, test_set_numyr)
rmse_loess2 <- RMSE(test_set_numyr$Global_Sales, loess2_predict)
rmse_loess2

# putting data into a table
rmse_results <- bind_rows(rmse_results, 
                          tibble(method = "LOESS degree 2", RMSE = rmse_loess2))  

rmse_results %>% knitr::kable()

# - -----------------------------------------------------------------------

### Genre and publisher effect model
genre_avgs <- train_set %>%  #incorporating genre effect into equation, b_i is for genre
  group_by(Genre) %>% 
  summarize(b_g = mean(Global_Sales - mu))

predicted_sales <- mu + 
  test_set %>%  #testing predicted Global_Sales data to test_validation dataset
  left_join(genre_avgs, by = 'Genre') %>% 
  pull(b_g)

# model_1_rmse <- RMSE(predicted_sales, test_set$Global_Sales)  #this test genre effect

publisher_avgs <- train_set %>%
  left_join(genre_avgs, by='Genre') %>%
  group_by(Publisher) %>%
  summarize(b_publisher = mean(Global_Sales - mu - b_g))

predicted_sales_publisher <- test_set %>%   #putting both movie and user effects into model
  left_join(genre_avgs, by='Genre') %>% 
  left_join(publisher_avgs, by='Publisher') %>% 
  mutate(pred = mu +b_publisher + b_g) %>%
  #na.omit() %>% 
  pull(pred)

model_2_rmse <- RMSE(predicted_sales_publisher, test_set$Global_Sales)  
#calculate residual mean square error for the two effects
rmse_results <- bind_rows(rmse_results, 
                          tibble(method="Publisher + Genre Effects Model",
                                 RMSE = model_2_rmse))  # putting data into a table

rmse_results %>% knitr::kable() #putting into kable table


# - -----------------------------------------------------------------------

