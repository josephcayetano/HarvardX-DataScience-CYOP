#Title: Chocolate Bar Rating Class Project
#Subtitle: HarvardX Data Science Capstone
#Author: Joseph Cayetano
#Date: 2022-10-10

#---------------Methods/Analysis---------------#

# We're going to install all the required libraries for this project.
# The if statements tell us that the packages won't install if you have them already.
# Note: this process could take a couple of minutes.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("viridis", repos = "http://cran.us.r-project.org")


# Adding the required libraries.
library(tidyverse)
library(caret)
library(data.table)
library(knitr)
library(kableExtra)
library(dplyr)
library(viridis)

# Defining the URL for the Chocolate Bar Ratings dataset.
choco_data_raw <- "https://raw.githubusercontent.com/josephcayetano/HarvardX-DataScience-CYOP/main/flavors_of_cacao.csv"

# Downloading the Chocolate Bar Ratings dataset and removing non printable characters.
choco <- read_csv(gsub("[^[:print:]]","",choco_data_raw),
                           na = c(""," ","NA"))

# Pulling a few rows of the dataset.
head(choco)

# Pulling the names of the variables.
names(choco)

# Displaying the dataset's total number of rows and columns. 
dim(choco)

# Changing column names for consistency.
colNames <- c("CompanyMaker",
                  "ChocoName",
                  "Reference",
                  "ReviewDate",
                  "CocoaPercent",
                  "CompanyLocation",
                  "Rating",
                  "BeanType",
                  "BroadBeanOrigin")

names(choco) <- colNames

# Fixed column names.
head(choco)
names(choco)

# Removing extra whitespaces.
choco <- choco %>% 
  mutate(across(where(is.character), str_trim))

# Replacing all empty values with NA.
choco <- choco %>% 
  mutate_all(na_if, "")

# Counting the number of missing values for each column.
missVals <- tibble("Column Name" = c("CompanyMaker",
                                                   "ChocoName",
                                                   "Reference",
                                                   "ReviewDate",
                                                   "CocoaPercent",
                                                   "CompanyLocation",
                                                   "Rating",
                                                   "BeanType",
                                                   "BroadBeanOrigin"),
                                 "Num of Missing Values" = c(sum(is.na(choco$CompanyMaker)),
                                                      sum(is.na(choco$ChocoName)),
                                                      sum(is.na(choco$Reference)),
                                                      sum(is.na(choco$ReviewDate)),
                                                      sum(is.na(choco$CocoaPercent)),
                                                      sum(is.na(choco$CompanyLocation)),
                                                      sum(is.na(choco$Rating)),
                                                      sum(is.na(choco$BeanType)),
                                                      sum(is.na(choco$BroadBeanOrigin))))

# Displaying the number of missing values for each column.
missVals %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Displaying a list of unique values for the column CompanyLocation.
uniqueCompanyLocation <- unique(choco$CompanyLocation) %>% sort()

uniqueCompanyLocation

# Fixing the inconsistencies in the CompanyLocation column
choco$CompanyLocation[choco$CompanyLocation == "Domincan Republic"] <- "Dominican Republic"
choco$CompanyLocation[choco$CompanyLocation == "Eucador"] <- "Ecuador"
choco$CompanyLocation[choco$CompanyLocation == "Niacragua"] <- "Nicaragua"
choco$CompanyLocation[choco$CompanyLocation == "Amsterdam"] <- "Netherlands"
choco$CompanyLocation[choco$CompanyLocation == "St. Lucia"] <- "Saint Lucia"
choco$CompanyLocation[choco$CompanyLocation == "Sao Tome"] <- "Sao Tome and Principe"
choco$CompanyLocation[choco$CompanyLocation == "Scotland"] <- "United Kingdom"
choco$CompanyLocation[choco$CompanyLocation == "U.K."] <- "United Kingdom"
choco$CompanyLocation[choco$CompanyLocation == "U.S.A."] <- "United States of America"
choco$CompanyLocation[choco$CompanyLocation == "Wales"] <- "United Kingdom"
choco$CompanyLocation[choco$CompanyLocation == "Martinique"] <- "France"

# Displaying a list of unique values for the column BroadBeanOrigin.
uniqueBroadBeanOrigin <- unique(choco$BroadBeanOrigin) %>% sort()

uniqueBroadBeanOrigin

# Fixing the inconsistencies in the BroadBeanOrigin column.
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Africa, Carribean, C. Am."] <- "Western Africa|Caribbean|Meso-America"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Burma"] <- "Myanmar"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Carribean"] <- "Caribbean"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Carribean(DR/Jam/Tri)"] <- "Dominican Republic|Jamaica|Trinidad and Tobago"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Central and S. America"] <- "Meso-America|South America"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Colombia, Ecuador"] <- "Colombia|Ecuador"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Congo"] <- "Republic of the Congo"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Cost Rica, Ven"] <- "Costa Rica|Venezuela"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Dom. Rep., Madagascar"] <- "Dominican Republic|Madagascar"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Dominican Rep., Bali"] <- "Dominican Republic|Indonesia"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "DR, Ecuador, Peru"] <- "Dominican Republic|Ecuador|Peru"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ecuador, Costa Rica"] <- "Ecuador|Costa Rica"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ecuador, Mad., PNG"] <- "Ecuador|Madagascar|Papua New Guinea"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ghana & Madagascar"] <- "Ghana|Madagascar"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ghana, Domin. Rep"] <- "Ghana|Dominican Republic"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ghana, Panama, Ecuador"] <- "Ghana|Panama|Ecuador"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Gre., PNG, Haw., Haiti, Mad"] <- "Grenada|Papua New Guinea|South Pacific|Haiti|Madagascar"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Guat., D.R., Peru, Mad., PNG"] <- "Guatemala|Dominican Republic|Peru|Madagascar|Papua New Guinea"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Indonesia, Ghana"] <- "Indonesia|Ghana"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Mad., Java, PNG"] <- "Madagascar|Indonesia|Papua New Guinea"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Madagascar & Ecuador"] <- "Madagascar|Ecuador"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Peru(SMartin,Pangoa,nacional)"] <- "Peru"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Peru, Belize"] <- "Peru|Belize"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Peru, Dom. Rep"] <- "Peru|Dominican Republic"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Peru, Ecuador"] <- "Peru|Ecuador"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Peru, Ecuador, Venezuela"] <- "Peru|Ecuador|Venezuela"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Peru, Mad., Dom. Rep."] <- "Peru|Madagascar|Dominican Republic"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Peru, Madagascar"] <- "Peru|Madagascar"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "PNG, Vanuatu, Mad"] <- "Papua New Guinea|Vanuatu|Madagascar"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Trinidad, Ecuador"] <- "Trinidad and Tobago|Ecuador"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ven, Bolivia, D.R."] <- "Venezuela|Bolivia|Dominican Republic"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ven, Trinidad, Ecuador"] <- "Venezuela|Trinidad and Tobago|Ecuador"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ven., Indonesia, Ecuad."] <- "Venezuela|Indonesia|Ecuador"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ven., Trinidad, Mad."] <- "Venezuela|Trinidad and Tobago|Madagascar"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Ven.,Ecu.,Peru,Nic."] <- "Venezuela|Ecuador|Peru|Nicaragua"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venez,Africa,Brasil,Peru,Mex"] <- "Venezuela|Western Africa|Brazil|Peru|Mexico"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venezuela, Carribean"] <- "Venezuela|Caribbean"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venezuela, Dom. Rep."] <- "Venezuela|Dominican Republic"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venezuela, Ghana"] <- "Venezuela|Ghana"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venezuela, Java"] <- "Venezuela|Indonesia"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venezuela, Trinidad"] <- "Venezuela|Trinidad and Tobago"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venezuela/ Ghana"] <- "Venezuela|Ghana"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Domincan Republic"] <- "Dominican Republic"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Hawaii"] <- "South Pacific"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Principe"] <- "Sao Tome and Principe"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Sao Tome"] <- "Sao Tome and Principe"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Sao Tome & Principe"] <- "Sao Tome and Principe"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "St. Lucia"] <- "Saint Lucia"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "South America, Africa"] <- "South America|Western Africa"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Tanzania"] <- "United Republic of Tanzania"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Tobago"] <- "Trinidad and Tobago"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Trinidad"] <- "Trinidad and Tobago"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Trinidad, Tobago"] <- "Trinidad and Tobago"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Trinidad-Tobago"] <- "Trinidad and Tobago"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Venezuela"] <- "Venezuela"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Vietnam"] <- "Vietnam"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "Martinique"] <- "Caribbean"
choco$BroadBeanOrigin[choco$BroadBeanOrigin == "West Africa"] <- "Western Africa"

# Displaying a list of unique values for the column BeanType.
uniqueBeanType <- unique(choco$BeanType) %>%
  sort()

uniqueBeanType

# Grouping the values from the BeanType column by main bean variety.
forastero <- c("Forastero",
                     "Forastero (Amelonado)",
                     "Forastero (Arriba)",
                     "Forastero (Arriba) ASS",
                     "Forastero (Arriba) ASSS",
                     "Forastero (Catongo)",
                     "Forastero (Parazinho)")

criollo <- c("Criollo",
             "Criollo (Amarru)",
             "Criollo (Ocumare)",
             "Criollo (Ocumare 61)",
             "Criollo (Ocumare 67)",
             "Criollo (Ocumare 77)",
             "Criollo (Porcelana)",
             "Criollo (Wild)")

trinitario <- c("Trinitario",
                "Trinitario (Amelonado)",
                "Trinitario (Scavina)")

nacional <- c("Forastero (Nacional)",
                    "Nacional",
                    "Nacional (Arriba)")

blend <- c("Amazon",
                 "Amazon mix",
                 "Amazon, ICS",
                 "Blend",
                 "Blend-Forastero,Criollo",
                 "Criollo, +",
                 "Criollo, Forastero",
                 "Criollo, Trinitario",
                 "Forastero, Trinitario",
                 "Forastero(Arriba, CCN)",
                 "Trinitario (85% Criollo)",
                 "Trinitario, Criollo",
                 "Trinitario, Forastero",
                 "Trinitario, Nacional",
                 "Trinitario, TCGA",
                 "Beniano",
                 "CCN51",
                 "EET",
                 "Matina")

choco$BeanType[which(choco$BeanType %in% forastero)] <- "Forastero"
choco$BeanType[which(choco$BeanType %in% criollo)] <- "Criollo"
choco$BeanType[which(choco$BeanType %in% trinitario)] <- "Trinitario"
choco$BeanType[which(choco$BeanType %in% nacional)] <- "Nacional"
choco$BeanType[which(choco$BeanType %in% blend)] <- "Blend"


# Inserting the value "Blend" into rows that have n/a values
# in the BeanType column only if
#     - More than one country in the BroadBeanOrigin column
#     - Name of the chocolate bar contains "Blend", "blend" or ",".
choco$BeanType[is.na(choco$BeanType) & str_detect(choco$BroadBeanOrigin, "\\|")] <- "Blend"
choco$BeanType[is.na(choco$BeanType) & str_detect(choco$ChocoName, "blend")] <- "Blend"
choco$BeanType[is.na(choco$BeanType) & str_detect(choco$ChocoName, "Blend")] <- "Blend"
choco$BeanType[is.na(choco$BeanType) & str_detect(choco$ChocoName, "\\,")] <- "Blend"

# Creating a new variable called RatingClass, as it will be used in the visualization section.
choco <- choco %>%
  mutate(RatingClass = case_when(Rating >= 1.00 & Rating <= 1.75  ~ "1-Unpleasant",
                                 Rating >= 2.00 & Rating <= 2.75  ~ "2-Disappointing",
                                 Rating >= 3.00 & Rating <= 3.75  ~ "3-Satisfactory",
                                 Rating >= 4.00 & Rating <= 4.75  ~ "4-Premium",
                                 Rating > 4.75  ~ "5-Elite"))


# Separating multiple bean origin values using the separate_rows function.
choco <- choco %>%
  separate_rows(BroadBeanOrigin,
                sep = "\\|",
                convert = FALSE)

# Removing the percent (%) sign from the CocoaPercent column
# and converting the column to numeric type.
# Percentages will be rounded to the nearest integer.
choco$CocoaPercent <- as.numeric(sub("%", "", choco$CocoaPercent, fixed = TRUE))
choco$CocoaPercent <- round(choco$CocoaPercent, digits = 0)

# Converting the Rating column to numeric type.
choco$Rating <- as.numeric(choco$Rating)

# Converting the rest of the columns to factor type.
choco$CompanyMaker <- as.factor(choco$CompanyMaker)
choco$CompanyLocation <- as.factor(choco$CompanyLocation)
choco$BeanType <- as.factor(choco$BeanType)
choco$BroadBeanOrigin <- as.factor(choco$BroadBeanOrigin)
choco$ReviewDate <- as.factor(choco$ReviewDate)
choco$RatingClass <- as.factor(choco$RatingClass)

# Looking at the dataset after the cleaning process.
head(choco) %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

###Data Visualizations###

# Top 5 companies with highest average rating (at least 10 ratings).

choco %>%
  group_by(CompanyMaker) %>%
  summarize(NumRating = n(),
            AvgRating = mean(Rating)) %>%
  filter(NumRating >= 10) %>%
  arrange(desc(AvgRating)) %>%
  head(5) %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Average rating of company locations (location must appear at least 10 times).

choco %>%
  group_by(CompanyLocation) %>% 
  filter(n() > 10) %>% 
  mutate(AvgRating = mean(Rating)) %>%
  ggplot() + 
  geom_boxplot(aes(reorder(CompanyLocation, AvgRating), Rating, fill = AvgRating)) + 
  scale_fill_continuous(low = '#ffffcc', high = '#fc4e2a', name = "Avg Rating") + 
  coord_flip() + 
  ggtitle("Average Rating of Company Locations (n > 10)") +
  xlab("Company Location") + ylab("Average Rating") +
  expand_limits(y = c(0,5))
  
# Cocoa Percentage vs. rating.

choco %>%
  ggplot(aes(x = CocoaPercent, y = Rating)) +
  geom_jitter(alpha = .75) + 
  coord_cartesian(ylim = c(0,5)) +
  ggtitle("Cocoa Percentage vs. Chocolate Bar Rating") +
  xlab("Cocoa Percentage") + ylab("Rating") +
  geom_smooth(method = 'lm', se = FALSE, col = 'red')

# Frequency of bean types being used in the chocolate bars.
choco %>%
  filter(!is.na(BeanType)) %>%
  group_by(BeanType) %>%
  summarize(NumRating = n(),
            AvgRating = mean(Rating)) %>%
  ggplot(aes(x = reorder(BeanType, NumRating),
             y = NumRating)) +
  geom_bar(stat = "identity",
           color = 'black',
           fill = '#fc4e2a') +
  ggtitle("Frequency of Bean Types Being Used in the Chocolate Bars") +
  xlab("Bean Type") + ylab("Frequency")

# Average rating of the broad bean origins of the chocolate bars (origin must appear at least 10 times).
choco %>%
  group_by(BroadBeanOrigin) %>% 
  filter(n() > 10) %>% 
  mutate(AvgRating = mean(Rating)) %>%
  ggplot() + 
  geom_boxplot(aes(reorder(BroadBeanOrigin, AvgRating), Rating, fill = AvgRating)) + 
  scale_fill_continuous(low = '#ffffcc', high = '#fc4e2a', name = "Avg Rating") + 
  coord_flip() + 
  ggtitle("Average Rating of the Broad Bean Origins of the Chocolate Bars (n > 10)") +
  xlab("Broad Bean Origins") + ylab("Average Rating")

# How chocolate bars are rated annually (2006-2017).
choco %>% 
  group_by(ReviewDate, Rating) %>% 
  summarise(
    NumReviews = n(),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = Rating, y = NumReviews)) +
  geom_bar(aes(fill = ReviewDate), stat = "identity") +
  scale_fill_viridis(discrete = T) +
  facet_wrap(~ReviewDate) +
  ggtitle("How Chocolate Bars are Rated Annually (2006-2017)") +
  xlab("Rating") + ylab("Number of Reviews")

# Frequency of the chocolate bar rating class.
choco %>%
  ggplot(aes(RatingClass)) +
  geom_bar(color = 'black',
           fill = '#fc4e2a') +
  coord_flip() +
  ggtitle("Frequency of the Chocolate Bar Rating Class") +
  xlab("Rating Class") + ylab("Frequency")


#---------------Results---------------#

# Selecting the features to use for the machine learning models and
# Removing rows with NAs.
choco_feat <- choco %>%
  select(CompanyLocation,
         CocoaPercent,
         BeanType,
         BroadBeanOrigin,
         ReviewDate,
         RatingClass) %>%
  drop_na()

# Splitting the data into training and testing sets.
# The training set will make up about 80% of the data, while the testing set will make up about 20% of the data.

set.seed(1111)
train_index <- createDataPartition(y = choco_feat$RatingClass,
                                      times = 1,
                                      p = 0.8,
                                      list = FALSE)

# Creating the training set.
train <- choco_feat[train_index, ]

# Creating the testing set.
test <- choco_feat[-train_index, ]

# Modifying the resampling method to "repeatedcv", which is a repeated
# K-fold cross-validation with 10 folds and 3 repetitions.
ctr <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

###-1st Model: Support Vector Machine (SVM)-###
set.seed(1111)

# Training the SVM model on the train set.
svm_fit <- train(RatingClass ~ .,
                 data = train,
                 trControl = ctr,
                 method = "svmRadial")

# Obtaining and displaying the performance results of the SVM model on the training set.
svm_fit_results <- svm_fit$results

svm_fit_results %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Obtaining the highest accuracy score on the training set.
svm_fit_acc <- max(svm_fit$results["Accuracy"])

# Predicting the SVM model on the test set
svm_pred <- predict(svm_fit,
                           newdata = test)

# Defining Confusion Matrix
svm_cmatrix <- confusionMatrix(svm_pred, test$RatingClass)

# Obtaining and displaying the performance results of the SVM model on the test set.
svm_pred_results <- svm_cmatrix$overall

svm_pred_results %>%
  kable(col.names = c("Value")) %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Obtaining the accuracy score on the testing set.
svm_pred_acc <- svm_pred_results["Accuracy"]

# Creating a table to record the train and test accuracy scores for each model.
svm_results <- tibble(ML_Model = "Support Vector Machine (SVM)",
                      Train_Acc_Score = svm_fit_acc,
                      Test_Acc_Score = svm_pred_acc)

# Putting the model into a list to compare with later models.
report <- svm_results

report %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)



###-2nd Model: K-Nearest Neighbors (KNN)-###
set.seed(1111)

# Training the KNN model on the train set.
knn_fit <- train(RatingClass ~ .,
                 data = train,
                 trControl = ctr,
                 method = "knn")

# Obtaining and displaying the performance results of the KNN model on the training set.
knn_fit_results <- knn_fit$results

knn_fit_results %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Obtaining the highest accuracy score on the training set.
knn_fit_acc <- max(knn_fit$results["Accuracy"])

# Predicting the KNN model on the test set
knn_pred <- predict(knn_fit,
                    newdata = test)

# Defining Confusion Matrix
knn_cmatrix <- confusionMatrix(knn_pred, test$RatingClass)

# Obtaining and displaying the performance results of the KNN model on the test set.
knn_pred_results <- knn_cmatrix$overall

knn_pred_results %>%
  kable(col.names = c("Value")) %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Obtaining the accuracy score on the testing set.
knn_pred_acc <- knn_pred_results["Accuracy"]

# Creating a table to record the train and test accuracy scores for each model.
knn_results <- tibble(ML_Model = "K-Nearest Neighbors (KNN)",
                      Train_Acc_Score = knn_fit_acc,
                      Test_Acc_Score = knn_pred_acc)

# Putting the model into a list to compare with later models.
report <- bind_rows(report,
                    knn_results)

report %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)



###-3rd Model: Random Forest (RF)-###
set.seed(1111)

# Training the RF model on the train set.
rf_fit <- train(RatingClass ~ .,
                 data = train,
                 trControl = ctr,
                 method = "rf")

# Obtaining and displaying the performance results of the RF model on the training set.
rf_fit_results <- rf_fit$results

rf_fit_results %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Obtaining the highest accuracy score on the training set.
rf_fit_acc <- max(rf_fit$results["Accuracy"])

# Predicting the RF model on the test set
rf_pred <- predict(rf_fit,
                    newdata = test)

# Defining Confusion Matrix
rf_cmatrix <- confusionMatrix(rf_pred, test$RatingClass)

# Obtaining and displaying the performance results of the RF model on the test set.
rf_pred_results <- rf_cmatrix$overall

rf_pred_results %>%
  kable(col.names = c("Value")) %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

# Obtaining the accuracy score on the testing set.
rf_pred_acc <- rf_pred_results["Accuracy"]

# Creating a table to record the train and test accuracy scores for each model.
rf_results <- tibble(ML_Model = "Random Forest (RF)",
                      Train_Acc_Score = rf_fit_acc,
                      Test_Acc_Score = rf_pred_acc)

# Putting the model into a list to compare with later models.
report <- bind_rows(report,
                    rf_results)

report %>%
  kable() %>%
  kable_styling(font_size = 11,
                full_width = FALSE)

###-Compare Results-###

# Comparing the results of the models.
clearResults <- resamples(list(SVM = svm_fit,
                                     KNN = knn_fit,
                                     RF  = rf_fit
))

# Using Dot Plot to compare the results of the models.
dotplot(clearResults)