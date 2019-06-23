source("code/model.R")
source("code/CVgroup.R")

seed = 700
set.seed(seed)

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript main.R --fold n --train data/features.csv --report performance.csv", call.=FALSE)
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

print(model1_loss(train_data))
print(model2_loss(train_data))
print(model3_loss(train_data))
print(model4_loss(train_data))

# datasize <- nrow(iris)
# cvlist <- CVgroup(k = fold,datasize = datasize,seed = seed)
# print(cvlist)
