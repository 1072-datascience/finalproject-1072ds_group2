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

model1_result = c()
model2_result = c()
model3_result = c()
model4_result = c()

#k-fold
for(i in c(1:fold)){
  temp <- as.numeric(as.character(unlist(cvlist[[i]]))) 
  k_train_data <- train_data[temp,]
  
  model1_result <- c(model1_result, model1_loss(k_train_data))
  model2_result <- c(model2_result, model2_loss(k_train_data))
  model3_result <- c(model3_result, model3_loss(k_train_data))
  model4_result <- c(model4_result, model4_loss(k_train_data))
}

print(mean(model1_result))
print(mean(model2_result))
print(mean(model3_result))
print(mean(model4_result))


