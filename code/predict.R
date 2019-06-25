source("model.R")

seed = 700
set.seed(seed)

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript predict.R --train ../data/features.csv --predict ../results/predict.csv", call.=FALSE)
} else {
  # Read arguments and store they in variables
  target <- input <- output <- c()
  for(arg in args){
    if(arg == "--train" || arg == "--predict") {
      curr_type <- arg
    } else {
      if(curr_type == "--train") train_path <- arg
      else if(curr_type == "--predict") predict_path <- arg
    }
  }
}

# Load Data
train_data <- read.csv(train_path, stringsAsFactors=F)

row_count <- nrow(train_data)
train_idx = 1:floor(row_count*0.7)
train <- train_data[train_idx, ]
test <- train_data[-train_idx, ]

predict <- final_model(train, train_data)
out_data <- cbind(predict, train_data)
write.csv(out_data, predict_path, quote=F, row.names=F)