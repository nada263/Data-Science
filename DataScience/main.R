#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("reshape2")
library(data.table)
library(ggplot2)
library(lattice)
library(reshape2)
######## [SECTION 1] > Review of Big Data Analytic Methods ##########

# step 1:

 # load dataframe 
  Zeta <- read.csv("Datasets/zeta.csv")
 
 # [1.1] > Analyze the zeta table
  summary(Zeta)
  # Record col names :
  ColNames <- colnames(Zeta)
  cat("Names of Cols : \n")
  cat(ColNames)

 # [1.2] > Record num of rows 
   NumOfRows <- nrow(Zeta)
   cat("Num Of Rows : \n")
   cat(NumOfRows)
 
 # [1.3] & [1.4] & [1.5] > Check Duplication & remove duplication & save the new table
   if( sum(duplicated(Zeta)) == 0 ){
      cat("No duplicate rows \n")
     }else{
     cat("There are duplicate rows \n")
     distinct(Zeta)
     write.csv(Zeta,"zeta_nodupes.csv", row.names = FALSE)
    }

# Step 2:
   
 # [2.1] > Load the text file of income data
  zipIncome <- read.table("Datasets/zipIncome.txt" , sep = ",", header = TRUE)

 # [2.2] > Change the column names
  colnames(Zeta)[ColNames %in% c("zcta" ,"meanhouseholdincome")]<- c("zipCode","income")

 # [2.3] > Analyze summary of income col
  cat("summary of income in zeta table \n")
  summary(Zeta$income)

 # [2.4] > Plot a scatter of data 
  Zeta_no_sex <- Zeta[,sapply(Zeta, is.numeric)] # remove sex col
  LongZeta <- melt (Zeta_no_sex)
  ggplot(LongZeta , aes(variable , value)) + geom_point()

 # [2.5] > omit outliers of income col
  Zeta_No_Outliers<- subset(Zeta_no_sex, Zeta_no_sex$income>7000 & Zeta_no_sex$income<200000)
 
 # [2.6] > summary of income after omitting outliers
  cat("summary of income in zeta table after omitting outliers \n")
  summary(Zeta_No_Outliers$income)
  # scatter of income col after omitting outliers  
  Long_Zeta_No_Outliers <- melt (Zeta_No_Outliers) 
  ggplot(Long_Zeta_No_Outliers , aes(variable , value)) + geom_point()
   
# Step 3:
  
 # [3.1] > create simple boxplot of data
  bwplot(LongZeta$value ~ LongZeta$variable ,ylab = "values",main="Boxplot of Data")

 # [3.2] > create boxplot after log data
  LogOfZeta = log(Zeta_no_sex)
  LongOfLogZeta <- melt(LogOfZeta)
  bwplot(LongOfLogZeta$value ~ LongOfLogZeta$variable ,ylab = "Log(values)",main="Boxplot of Log(Data)")
  
`######## [SECTION 2] > Advanced Analytics/Methods (K-means) ##########

# [1] > Access the census data
  census <- read.csv("Datasets/income_elec_state.csv")
  # convert dataframe to table with new names of cols
  census_table <- as.data.table(census)
  colnames(census_table) <- c("state","mean_household_income","mean_electricity_usage")
  census_table<- dplyr::select(census_table, -c(state))
  ggplot(census_table, aes(x =mean_household_income, y =mean_electricity_usage)) +geom_point()
  
# [2] Apply Kmeans on data with K =10 
  census_matrix <- as.matrix(census_table)
  summary(census_matrix)
  
  KmeansAlgorithm(census_matrix , 10 , "Kmeans of Data With K=10")
  
# [3] > Determine a reasonable value of k using elbow
  wss <- numeric(20)
  ElbowGraph(wss, census_matrix, "Elbow of Data")
  
  # from graph suitable K = 3
  KmeansAlgorithm(census_matrix , 3 , "Kmeans of Data With K=3")
  
# [4] > apply Kmeans on log10(census_table) 
  census_Log_table <- log10(census_table)
  census_Log <- as.matrix(census_Log_table)
  summary(census_Log)
  
  KmeansAlgorithm(census_Log,10,"Kmeans of Log10(Data) With K=10")

#[5] > Reevaluate choice of k of log10(census_table)
  wss_Log <- numeric(20)
  ElbowGraph(wss_Log , census_Log ,"Elbow of Log10(Data)" )
  
  # from graph suitable K =5 
  KmeansAlgorithm(census_Log , 3 , "Kmeans of Log10(Data) With K=3")
  
#[6] > check and remove outliers & reevaluate choice of k
  #check outliers
  Long_Census = melt(census_table)
  bwplot(Long_Census$value ~ Long_Census$variable ,ylab = "values", main="Boxplot of census with outliers")
  #remove outliers 
  Q1 <- quantile(census_table$mean_household_income, .25)
  Q3 <- quantile(census_table$mean_household_income, .75)
  IQR <- IQR(census_table$mean_household_income)
  no_outliers <- subset(census_table, census_table$mean_household_income > (Q1 - 1.5*IQR) & census_table$mean_household_income < (Q3 + 1.5*IQR))

  Long_Census_noOutliers = melt(no_outliers)
  bwplot(Long_Census_noOutliers$value ~ Long_Census_noOutliers$variable ,ylab = "values", main="Boxplot of census without outliers")
  # apply Kmeans on no_outliers 
  census_No_outliers <- as.matrix(no_outliers)
  summary(census_No_outliers)
  
  KmeansAlgorithm(census_No_outliers,10,"Kmeans of census without outliers With K=10")

  # Reevaluate choice of k after removing outliers
  wss_no_outliers <- numeric(20)
  ElbowGraph(wss_no_outliers,census_No_outliers,"Elbow of census without outliers") 

  # from graph suitable K = 2
  KmeansAlgorithm(census_No_outliers , 2 , "Kmeans of Data without outliers With K=2")
  
  #Kmeans Algorithm
  KmeansAlgorithm <- function(data , K , msg) {
    km <- kmeans( data, K , 15)
    cat("Km clusters are :  \n")
    km$cluster
    cat("Km centers are :  \n")
    km$centers
    #plot clusters
    plot(data ,col= km$cluster , main = msg)
    #Add centers
    points( km$centers , col =1:K ,pch=8)
  }
  #Elbow graph fn 
  ElbowGraph <- function(wss ,data , msg) {
    for(i in 1:20) wss[i] <- sum(kmeans(data , centers = i) $withinss)
    plot(1:20, wss, tybe ="b" , xlab = "number Of Clusters" , ylab = "sum of squares" , main = msg)
    lines(wss)  
  }
  