data<-read.table("dataset.txt")
rand_df <- data[sample(nrow(data), size=6), ]
write.table(rand_df,"Group1.txt")
