install.packages("xlsx")
install.packages("corrplot")
install.packages("car")
install.packages("mycor")
install.packages("ggplot2")
install.packages("ggiraph")
install.packages("ppcor")
install.packages("GGally")
install.packages("DMwR")
install.packages("VIM")
install.packages("psych")
install.packages("leaps")
install.packages("stringr")
install.packages("class")
setwd("/Users/clementlaplace/Documents/Documents – Clement’s MacBook Pro (2)/Core courses/1 - Big Data Analytics /Project IMDB")
library(class)
library(DMwR)
library(VIM)
library(corrplot)
library(xlsx)
library(carData)
library(corrplot)
library(GGally)
#library(caret)
library(car)
library(mycor)
library(ggplot2)
library(ggiraph)
library(MASS)
library(ppcor)
library(GGally)
library(grid)
#library(Hmisc)
library(DMwR)
library(plyr)
library(RColorBrewer) 
library(psych)
library(leaps)
library(stringr)
library(data.table)

######LOAD DATASET :#####
setwd("/Users/susansaal/Desktop")
Dataset <- read.csv("movie_metadata.csv",sep=';')
Original=Dataset # Backup 


#####DATA DESCRIPTION :####


#####DATA EXPLORATION :####
dim(Dataset)
#Our dataset is composed with 5043 observations and 28 features
str(Dataset)
#The response variable “imdb_score” is numerical, and the predictors are mixed with numerical and categorical variables.
summary(Dataset)
#We can notice that we have Missing Values that will be handle in the next part.


#####DATA CLEANING :####
sum(duplicated(Dataset))
#We have 45 duplicated rows. Let's delete them
Dataset=Dataset[!duplicated(Dataset),]

#Remove ,,, in 'movie_facebook_likes' columns
Dataset$movie_facebook_likes... <- gsub(",", "", as.character(factor(Dataset$movie_facebook_likes...)))
str_trim(Dataset$movie_facebook_likes..., side = "right")
Dataset$movie_facebook_likes...<-as.numeric(Dataset$movie_facebook_likes...)

#Remove tidy title
Dataset$movie_title<-str_trim(Dataset$movie_title, side = "right")

###1- Handling MV:

## Exploring MV:
summary(is.na(Dataset))
colSums(sapply(Dataset, is.na))
length(which(is.na(Dataset)))
#We have Missing Values in 15 columns of our dataset. Significant amount of Missing Values are especially in 'gross', 'budget' and 'aspect ratio'
#In total, we have 20147 Missing Values in our dataset.
#We plot a heatmap to visualize missing values:
missing.values <- aggr(Dataset, sortVars=TRUE, prop=TRUE,sortCombs=TRUE,cex.lab=1.5, cex.axis=.6,cex.numbers=5,combined=FALSE, gap=-.2) 
#Heatmap confirm what we said : from our missing values visualization, we detect 3 main columns including NaN: Gross, Budget, Aspect ratio.

## Handling MV in Gross and Budget columns:
# We choose to delete all rows including NaN in our Gross and Budget column. 
Dataset=Dataset[!is.na(Dataset$gross), ]
Dataset=Dataset[!is.na(Dataset$budget), ]

length(which(is.na(Dataset)))
length(which(is.na(Dataset)))*100/nrow(Dataset)
#We now have 100 Missing Values in our data set, which represent 2.59% of our dataset.
colSums(sapply(Dataset, is.na))
#Remaining Missing Values are in 'num_critic_for_reviews', 'duration', 'actor_3_facebook_likes', 'actor_1_facebook_likes', 'facenumber_in_poster', 'actor_2_facebook_likes' and 'aspect_ratio'. Let's deal with this.

##Handling MV in aspect_ratio column:
table(Dataset$aspect_ratio)
#'aspect_ratio' corresponds to TV format. So, replace Missing Values with the mean is not significant. We’ll use the median (we can do this as the mean and the median are very close).
Dataset$aspect_ratio[is.na(Dataset$aspect_ratio)] <- median(Dataset$aspect_ratio, na.rm=TRUE)
length(which(is.na(Dataset$aspect_ratio))) 
#No more Missing Values in 'aspect_ratio'
table(Dataset$aspect_ratio)
summary(Dataset$aspect_ratio)
mean(Dataset$imdb_score[Dataset$aspect_ratio == 1.85])
mean(Dataset$imdb_score[Dataset$aspect_ratio == 1.78])
mean(Dataset$imdb_score[Dataset$aspect_ratio == 2.35])
mean(Dataset$imdb_score[Dataset$aspect_ratio != 1.85 & Dataset$aspect_ratio != 1.78 & Dataset$aspect_ratio != 2.35 ] )
#The IMDb score means for different aspect ratio fall in the range of 6.3~6.8. When choosing different aspect ratio, there is no significant difference in the IMDb score.So, we can assume that aspect_ratio is not relevant for prediciting IMDb score. Thus, we remove the column
Dataset <- subset(Dataset, select = -c(aspect_ratio))

length(which(is.na(Dataset)))
length(which(is.na(Dataset)))*100/nrow(Dataset)
#We now have 26 Missing Values in our data set, which represent 0.67% of our dataset.

##Handling MV in num_critic_for_reviews
Dataset$num_critic_for_reviews[is.na(Dataset$num_critic_for_reviews)] <- mean(Dataset$num_critic_for_reviews, na.rm = TRUE)

##Handling MV in duration
Dataset$duration[is.na(Dataset$duration)] <- round(mean(Dataset$duration, na.rm = TRUE))

##Handling MV in facenumber_in_poster
Dataset$facenumber_in_poster[is.na(Dataset$facenumber_in_poster)] <- round(mean(Dataset$facenumber_in_poster, na.rm = TRUE))

colSums(sapply(Dataset, is.na))
length(which(is.na(Dataset)))
#We now have 18 Missing Values in our data set. Missing Values are in 'actor_1_facebook_likes', 'actor_2_facebook_likes' and 'actor_3_facebook_likes'
#We observe that there are 0 values in columns 'director_faceboook_likes'(column 5), 'actor_3_facebook_likes'(column 6), 'actor_1_facebook_likes'(column 8), 'cast_total_facebook_likes'(column 14), 'movie_facebook_likes'(column 27) and 'actor_2_facebook_likes'(column 25)
#We replace the 0 values by NA in order to fill it just after.
Dataset[,c(5,6,8,14,25,27)][Dataset[,c(5,6,8,14,25,27)] == 0] <- NA
colSums(sapply(Dataset, is.na))
length(which(is.na(Dataset)))
#We now have 2533 Missing Values in our data set.

#We choose to fill the Missing Values with the Mean Algorithm.
#Mean algorithm
T1<-Sys.time() 
Dataset$actor_3_facebook_likes[is.na(Dataset$actor_3_facebook_likes)] <- round(mean(Dataset$actor_3_facebook_likes, na.rm = T))
Dataset$actor_2_facebook_likes[is.na(Dataset$actor_2_facebook_likes)] <- round(mean(Dataset$actor_2_facebook_likes, na.rm = T))
Dataset$actor_1_facebook_likes[is.na(Dataset$actor_1_facebook_likes)] <- round(mean(Dataset$actor_1_facebook_likes, na.rm = T))
Dataset$director_facebook_likes[is.na(Dataset$director_facebook_likes)] <- round(mean(Dataset$director_facebook_likes, na.rm = T))
Dataset$cast_total_facebook_likes[is.na(Dataset$cast_total_facebook_likes)] <- round(mean(Dataset$cast_total_facebook_likes, na.rm = T))
Dataset$movie_facebook_likes[is.na(Dataset$movie_facebook_likes)] <- round(mean(Dataset$movie_facebook_likes, na.rm = T))
T2<-Sys.time() 
#Running time of mean algorithm
difftime(T2, T1) 
Dataset = Dataset[,-27]

#Let's check that we don't have any NA in our dataset
sum(is.na(Dataset))

##2-Removing columns:

##Remove 'movie_idmb_link' : We choose to delete 'movie_imdb_link' column. Imdb-link is non relevant for our analysis.
Dataset <- subset(Dataset, select = -c(movie_imdb_link))

##Remove columns with names:
sum(uniqueN(Dataset$director_name))
sum(uniqueN(Dataset$actor_1_name))
sum(uniqueN(Dataset$actor_2_name))
sum(uniqueN(Dataset$actor_3_name))
#We notice that there are 1753 different names of directors and 6488 different names of actors. Thus we have decided to take instead into account the actor 1,2 & 3 facebook likes to measure the impact of actors on IMDB ratings. This solution seems logical and is more easily interpretable.
#We remove those columns.
Dataset <- subset(Dataset, select = -c(director_name,actor_1_name,actor_2_name,actor_3_name))

##Remove 'plot_keywords' column: 
# The variable 'plot_keywords' is highly diverse. Because it is too diverse for our prediction, we have decided to disregard it and thus removed the column.
Dataset <- subset(Dataset, select = -c(plot_keywords))

##3-Discretisation:
##Deal with 'color' column : let's compare imdb_score mean for movies in color, then movies in Black and White.
mean(Dataset$imdb_score[Dataset$color == "Color"])
mean(Dataset$imdb_score[Dataset$color == "Black and White"])
#From the means of imdb score for movies in color and movies in black and white, we can see there is a difference, the mean for movies in color are 6.4 and the mean for black and white movies are 7.1. So, we decide not to removing this variable, as we can think that color variable can affect a little imdb score.
#Let's create a dummy variables for color column. 1 will represent color movies and 0 black and white movies. 
Dataset$color <- as.numeric(Dataset$color=="Color")

##Deal with 'language' column:
sum(uniqueN(Dataset$language))
table(Dataset$language)
sum(Dataset$language == "English")/length(Dataset$language)*100 
boxplot(Dataset$imdb_score[Dataset$language == "English"],Dataset$imdb_score[Dataset$language != "English"])
#We notice that more than 95% of the movies are in english, meaning that this is a constant variable, so we'll set every non english language together as others.
levels(Dataset$language) <- c(levels(Dataset$language), "Others")
Dataset$language[(Dataset$language != 'English')] <- 'Others'
#Discretisation - dummy variable
Dataset$language <- as.numeric(Dataset$language=="English")

##Deal with 'country' column:
sum(uniqueN(Dataset$country))
table(Dataset$country)
sum(Dataset$country == "USA")/length(Dataset$country)*100 
#79% of movies come from USA, then we split into 2 categories.
levels(Dataset$country) <- c(levels(Dataset$country), "Others")
Dataset$country[(Dataset$country != 'USA')] <- 'Others'
#Regarding 'country', as 79% of the movies are from the USA, we have decided to regroup all non-USA movies under the new variable "Others". This will facilitate our analysis and interpratation, as we'll compare movies against the benchmark "USA".
#Discretisation - dummy variable
Dataset$country <- as.numeric(Dataset$country=="USA")

##Deal with 'content_rating'  
#Content ratings have evolved over the years, we thus adapt the old ratings to the new ones as follow:
table(Dataset$content_rating)
Dataset$content_rating[Dataset$content_rating == 'M']   <- 'PG' 
Dataset$content_rating[Dataset$content_rating == 'GP']  <- 'PG' 
Dataset$content_rating[Dataset$content_rating == 'X']   <- 'NC-17'
#Regarding the remaining  'Content_rating', we are going to group them with the largest variable (R) to reduce the complexity of the feature and facilitate the interprettion of analysis.
Dataset$content_rating[Dataset$content_rating == 'Approved']  <- 'R' 
Dataset$content_rating[Dataset$content_rating == 'Not Rated'] <- 'R' 
Dataset$content_rating[Dataset$content_rating == 'Passed']    <- 'R' 
Dataset$content_rating[Dataset$content_rating == 'Unrated']   <- 'R' 
Dataset$content_rating[Dataset$content_rating == '']   <- 'R' 
Dataset$content_rating <- factor(Dataset$content_rating)
table(Dataset$content_rating)
#Discretisation of 'content_rating' column:
Dataset$content_rating <- sapply(1:length(Dataset$content_rating), function(x) 
  if(Dataset[x,16] == "G") {0}
  else if(Dataset[x,16] == "PG") {1}
  else if(Dataset[x,16] == "PG-13") {2}
  else if(Dataset[x,16] == "R") {3}
  else if(Dataset[x,16] == "NC-17") {4})

str(Dataset)
#Now, except for 'genres' & 'movie_title', we have numerical features.

##4- Handling many genres:
#For the variable 'genres', we attempt at dealing with this variable by creating new features encompassing several movie genres (i.e. Action includes (Adventure, War, Western)). If a movie falls into one or more of those 'Big' genres, a binary indicator will indicate its belonging.
#This will hugely facilitate our analysis and interpretation, while minimizing the loss of information. And hence we remove the original genre column.
Dataset$Action <- sapply(1:length(Dataset$genres), function(x) 
  if(Dataset[x,8] %like% "Action") {1}
  else if(Dataset[x,8] %like% "Adventure") {1}
  else if(Dataset[x,8] %like% "War") {1}
  else if(Dataset[x,8] %like% "Western") {1}
  else {0})

Dataset$Animation <- sapply(1:length(Dataset$genres), function(x) 
  if(Dataset[x,8] %like% "Animation") {1}
  else if(Dataset[x,8] %like% "Comedy") {1}
  else if(Dataset[x,8] %like% "Family") {1}
  else if(Dataset[x,8] %like% "Romance") {1}
  else {0})

Dataset$Thriller <- sapply(1:length(Dataset$genres), function(x) 
  if(Dataset[x,8] %like% "Thriller") {1}
  else if(Dataset[x,8] %like% "Crime") {1}
  else if(Dataset[x,8] %like% "Mystery") {1}
  else if(Dataset[x,8] %like% "Drama") {1}
  else if(Dataset[x,8] %like% "Horror") {1}
  else if(Dataset[x,8] %like% "Film-Noir") {1}
  else {0})

Dataset$Fantasy <- sapply(1:length(Dataset$genres), function(x) 
  if(Dataset[x,8] %like% "Fantasy") {1}
  else if(Dataset[x,8] %like% "Sci-Fi") {1}
  else {0})

Dataset$Documentary <- sapply(1:length(Dataset$genres), function(x) 
  if(Dataset[x,8] %like% "Documentary") {1}
  else if(Dataset[x,8] %like% "Biography") {1}
  else if(Dataset[x,8] %like% "History") {1}
  else if(Dataset[x,8] %like% "News") {1}
  else {0})

Dataset$Other <- sapply(1:length(Dataset$genres), function(x) 
  if(Dataset[x,8] %like% "Short") {1}
  else if(Dataset[x,8] %like% "Sport") {1}
  else if(Dataset[x,8] %like% "Musical") {1}
  else {0})

#Remove 'genres' column:
Dataset <- subset(Dataset, select = -c(genres))

##Deal with 'Movie_Titles' column:
Dataset2=Dataset
#We count the number of letters in movie titles to assess wether or not, it has any impact on IMDB ratings.
Dataset2$count_movie_title<-nchar(as.character(Dataset2$movie_title))
summary(Dataset2$count_movie_title)
cor(Dataset2$count_movie_title,Dataset2$imdb_score)
#The correlation between the number of characters of a movie title and their IMDB’s rating is -0.05. Therefore due to its diversity and unsignificant correlation, we judge it is not relevant in predicting IMDB score and will only take the movie_titles for analysis interpretation.

##5-Handling features highly correlated:
#Heatmap of our dataset
ggcorr(Dataset, label=TRUE, label_round=1, label_size=2, size=1.5, hjust=.85)+
  ggtitle("Correlation Heatmap")
#Based on the heatmap we notice that Actor 1 facebook likes and cast total facebook likes are highly correlated 0.9.
#Hence,to avoid multicolinearity and keep numerical stability we remove the cast total faceook likes column.
Dataset <- subset(Dataset, select = -c(cast_total_facebook_likes))

#Based on the heatmap, we notice that num_voted_user, num_users for review and num_critic_for_reviews are highly correlated.
#We choose to take the ratio of num_user_for_reviews and num_critic_for_reviews.
Dataset$critic_review_ratio <- Dataset$num_critic_for_reviews / Dataset$num_user_for_reviews
Dataset <- subset(Dataset, select = -c(num_critic_for_reviews,num_user_for_reviews))

names(Dataset)
dim(Dataset)
sum(is.na(Dataset))

##Reorder columns:
refcols <- c("imdb_score","movie_title","title_year","language","country","color","duration","gross","budget","movie_facebook_likes","director_facebook_likes", "actor_1_facebook_likes","actor_2_facebook_likes","actor_3_facebook_likes","num_voted_users","critic_review_ratio","facenumber_in_poster","Action","Animation","Thriller","Fantasy","Documentary","Other")
Dataset <- Dataset[, c(refcols, setdiff(names(Dataset), refcols))]
head(Dataset)

#####DATA VIZUALIZATION:#####
##Vizualization of distribution IMDB score 
ggplot(Dataset, aes(x = imdb_score)) +
  geom_histogram(aes(fill = ..count..), binwidth =0.5) +
  scale_x_continuous(name = "IMDB Score",
                     breaks = seq(0,10),
                     limits=c(1, 10)) +
  ggtitle("Histogram of Movie IMDB Score") +
  scale_fill_gradient("Count", low = "blue", high = "red")

summary(Dataset$imdb_score)
#Positive skewness because mean < median

boxplot(Dataset$imdb_score, main = "IMDB notes")
boxplot(Dataset$imdb_score[Dataset$Action == 1],Dataset$imdb_score[Dataset$Animation == 1],Dataset$imdb_score[Dataset$Thriller == 1],Dataset$imdb_score[Dataset$Fantasy == 1],Dataset$imdb_score[Dataset$Documentary == 1],Dataset$imdb_score[Dataset$Other == 1], main = "IMDB repartition by films categories")


#####MODELS:#####

original_after_cleaning=Dataset
Dataset=subset(Dataset, select = -c(movie_title))

#Our goal is to build a model, which can help us predict to predict the IMDb score of a movie. We don’t really want an exact score to be predicted, we only want to know how good or how bad is the movie. Therefore, we bin the score into 8 buckets: less than 4, 4~5, 5~6, 6~6.5, 6.5~7, 7~7.5, 7.5~8 and 8~10. We believed that making a multiclassification (so binning the score) will give a more accurate score than if we decide to predict the exact score. We choose to split the score this way because we notice that most of the scores are between 5 and 8 (see Histogram of Movie IMDB Score). Split this way when the score is close to 6,7... will allow us to have a more precise score.
Dataset$imdb_binned_score <- cut(Dataset$imdb_score, breaks = c(0,4,6,8,10))
Dataset<-subset(Dataset, select = -c(imdb_score))

levels(Dataset$imdb_binned_score) <- 1:4
levels <- levels(Dataset$imdb_binned_score)
Dataset$imdb_binned_score = as.factor(Dataset$imdb_binned_score)


#Split dataset into train (80%) and test (20%)

train = Dataset[1:3085,]
test = Dataset[3086:3857,]
train.index <- sample(row.names(Dataset), dim(Dataset)[1]*0.6)
valid.index <- sample(setdiff(row.names(Dataset), train.index), dim(Dataset)[1]*0.2)
test.index <- setdiff(row.names(Dataset), union(train.index, valid.index))
train <- Dataset[train.index, ]
valid <- Dataset[valid.index, ]
test <- Dataset[test.index, ] 
#RUN METHODS !!

#random forest
install.packages("randomForest")
library(randomForest)
library(caret)
 

#Create accuracy_matrix - nrow = number of variable to select, ncol = number of trees per RF. 
accuracy_matrix = matrix(data=0, nrow=22, ncol=11)
accuracy_matrix
#Double for loop to input accuracy of random forest for each number of variables and trees. O's in the accuracy matrix are being replaced by the accuracy results of random forest models.
set.seed(34)
w=1
for (i in 1:22)
  {
  for (j in seq(100,600,by=50))
  {
    if (w<11){
      random <- randomForest(x=train[,-23],y=train$imdb_binned_score,mtry=i,importance=T,ntree = j)
      pred_imdb <-predict(random,valid[,-23])
      results <- confusionMatrix(pred_imdb, valid[,23], positive = NULL)
      overall <- results$overall
      overall.accuracy <- overall['Accuracy']
      accuracy_matrix[i,w]<-round(overall.accuracy, digits = 4)
      print(accuracy_matrix)
      
      w<-w+1
    }
    else{
      random <- randomForest(x=train[,-23],y=train$imdb_binned_score,mtry=i,importance=T,ntree = j)
      pred_imdb <-predict(random,valid[,-23])
      results <- confusionMatrix(pred_imdb, valid[,23], positive = NULL)
      overall <- results$overall
      overall.accuracy <- overall['Accuracy']
      accuracy_matrix[i,w]<-overall.accuracy
      print(accuracy_matrix)
      w=1
    }
  }
}

#Accuracy Heatmap  of number of features with number of trees.
library(reshape2)
heat_table=melt(accuracy_matrix)
library(scales) # for muted function
ggplot(heat_table, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = heat_table$value, label = round(heat_table$value, 3))) + # write the values
  scale_fill_gradient2(low = muted("red4"),
                       mid = "white",
                       high = muted("midnightblue"), 
                       midpoint = mean(x = heat_table$value)) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold", colour = "black"),
        plot.title = element_text(size=20,face="bold"),
        axis.text.y = element_text(size = 12,face = "bold")) + 
  ggtitle("Accuracy Plot") + 
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_x_discrete(name=" number of features") +
  scale_y_discrete(name=" number of Trees") +
  labs(fill="Accuracy")


train$imdb_binned_score = as.factor(train$imdb_binned_score)
valid$imdb_binned_score = as.factor(valid$imdb_binned_score)
test$imdb_binned_score = as.factor(test$imdb_binned_score) 


#Plot number of features vs. Accuracy
matplot(accuracy_matrix, type = "l", lty = 1)

accuracy_matrix
which(accuracy_matrix==max(accuracy_matrix),arr.ind=TRUE)
random <- randomForest(x=train[,-23],y=train$imdb_binned_score,mtry=15,importance=T,ntree = 400)
pred_imdb <-predict(random,test[,-23])
results <- confusionMatrix(pred_imdb, test[,23], positive = NULL)
results


