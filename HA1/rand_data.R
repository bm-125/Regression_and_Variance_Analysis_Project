data<-read.table("dataset.txt")
head(data)
library(dplyr)
sampled_data <- data %>%
  group_by(central_air, kitchen_quali) %>%
  sample_n(size = 6, replace = FALSE)


# Write the sampled data to a .txt file
write.table(sampled_data,"Group1.txt", sep = "\t", row.names = FALSE)

