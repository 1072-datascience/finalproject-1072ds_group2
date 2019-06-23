source("code/model.R")
source("code/CVgroup.R")

seed = 700
set.seed(seed)

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript main.R --fold n --train data/features.csv --report result/performance.csv", call.=FALSE)
} else {
  # Read arguments and store they in variables
  target <- input <- output <- c()
  for(arg in args){
    if(arg == "--fold" || arg == "--train" || arg == "--report") {
      curr_type <- arg
    } else {
      if(curr_type == "--fold") fold <- as.numeric(arg)
      else if(curr_type == "--train") train_path <- arg
      else if(curr_type == "--report") report_path <- arg
    }
  }
}

# Load Data
train_data <- read.csv(train_path, stringsAsFactors=F)

for(i in colnames(train_data)) {
  train_data[, i] <- as.numeric(train_data[, i])
}

# Random Shuffle Data
row_count = nrow(train_data)
train_data <- train_data[sample(1:row_count), ]

#Random number list of k-fold
cvlist <- CVgroup(k = fold,datasize = row_count,seed = seed)

model1_train_result <- c()
model1_validation_result <- c()
model1_test_result <- c()

model2_train_result <- c()
model2_validation_result <- c()
model2_test_result <- c()

model3_train_result <- c()
model3_validation_result <- c()
model3_test_result <- c()

model4_train_result <- c()
model4_validation_result <- c()
model4_test_result <- c()

#k-fold
for(i in c(1:fold)){
  temp1 <- as.numeric(as.character(unlist(cvlist[[i]]))) 
  temp2 <- as.numeric(as.character(unlist(cvlist[[((i%%fold)+1)]])))
  temp3 <- c(temp1,temp2)
  k_train_data <- train_data[-temp3,]
  k_validation_data <- train_data[temp1,]
  k_test_data <- train_data[temp2,]
  
  model1_train_result <- c(model1_train_result, model1_loss(k_train_data,k_train_data))
  model1_validation_result <- c(model1_validation_result, model1_loss(k_train_data,k_validation_data))
  model1_test_result <- c(model1_test_result, model1_loss(k_train_data,k_test_data))
  
  model2_train_result <- c(model2_train_result, model2_loss(k_train_data,k_train_data))
  model2_validation_result <- c(model2_validation_result, model2_loss(k_train_data,k_validation_data))
  model2_test_result <- c(model2_test_result, model2_loss(k_train_data,k_test_data))
  
  model3_train_result <- c(model3_train_result, model3_loss(k_train_data,k_train_data))
  model3_validation_result <- c(model3_validation_result, model3_loss(k_train_data,k_validation_data))
  model3_test_result <- c(model3_test_result, model3_loss(k_train_data,k_test_data))

  model4_train_result <- c(model4_train_result, model4_loss(k_train_data,k_train_data))
  model4_validation_result <- c(model4_validation_result, model4_loss(k_train_data,k_validation_data))
  model4_test_result <- c(model4_test_result, model4_loss(k_train_data,k_test_data))
}

print("model1")
print(mean(model1_train_result))
print(mean(model1_validation_result))
print(mean(model1_test_result))

print("model2")
print(mean(model2_train_result))
print(mean(model2_validation_result))
print(mean(model2_test_result))

print("model3")
print(mean(model3_train_result))
print(mean(model3_validation_result))
print(mean(model3_test_result))

print("model4")
print(mean(model4_train_result))
print(mean(model4_validation_result))
print(mean(model4_test_result))
