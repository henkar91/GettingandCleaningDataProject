merge_data <- function()
{
  x_train <- read.table('cleaning_data/UCI HAR Dataset/train/X_train.txt')
  y_train <- read.table('cleaning_data/UCI HAR Dataset/train/y_train.txt')
  subject_train <- read.table('cleaning_data/UCI HAR Dataset/train/subject_train.txt')
  
  x_features <- read.table('cleaning_data/UCI HAR Dataset/features.txt')
  cn <- sapply(x_features[2] , function(x){as.character(x)})
  colnames(x_train) <- cn
  
  x_train <- cbind(x_train,y_train)
  colnames(x_train)[ncol(x_train)] <- 'activityLabel'
  x_train <- cbind(x_train,subject_train)
  colnames(x_train)[ncol(x_train)] <- 'subjectLabel'
  
  x_test <- read.table('cleaning_data/UCI HAR Dataset/test/X_test.txt')
  y_test <- read.table('cleaning_data/UCI HAR Dataset/test/y_test.txt')
  subject_test <- read.table('cleaning_data/UCI HAR Dataset/test/subject_test.txt')
  
  x_test <- cbind(x_test,y_test)
  x_test <- cbind(x_test,subject_test)
  
  colnames(x_test) <- colnames(x_train)
  x <- rbind(x_train, x_test)
  
  valid_column_names <- make.names(names=names(x), unique=TRUE, allow_ = TRUE)
  names(x) <- valid_column_names
  
  return(x)
}

extract_measurements <- function(y)
{
  y <- select(y,contains("mean"), contains("std"), activityLabel, subjectLabel)
  return(y)
}

name_activityLabel <- function(z)
{
  activityNames <- read.table('cleaning_data/UCI HAR Dataset/activity_labels.txt')
  activityNames <- as.character(activityNames$V2)
  z <- mutate(z, activityName = activityNames[activityLabel])
  return(z)
}

arrange_data <- function(x)
{
  activityNames <- read.table('cleaning_data/UCI HAR Dataset/activity_labels.txt')
  activityNames <- as.character(activityNames$V2)
  x <- arrange(x, activityLabel, subjectLabel)
  c <- ncol(x) - 3
  cn <- 1:c
  m <- x[1, ]
  for(i in 1:6)
  {
    mi <- filter(x, activityLabel == i)
    mi <- as.data.frame(sapply(cn, function(i){tapply(mi[,i], mi$subjectLabel, mean)}))
    colnames(mi) <- colnames(m)[1:c]
    mi <- mutate(mi, activityLabel = rep.int(i, 30), subjectLabel = seq(1,30), activityName = rep(activityNames[i], times = 30))
    m <- rbind(m, mi)
  }
  m <- m[2: nrow(m),]
  return(m)
}
