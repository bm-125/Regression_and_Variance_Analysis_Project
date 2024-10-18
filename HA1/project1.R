data<-read.table("Group1.txt",header = TRUE)
head(data)
#Ver o tipo de variável e as estatísticas da variável quantitativa
summary(data)

Central_Air<-data$central_air
Kitchen_Quali<-data$kitchen_quali
Sale_Price<-data$sale_price
#Boxplot do Sale Price sozinho
boxplot(Sale_Price,horizontal = TRUE)
library(ggplot2)
# boxplots do sale price em função de central air e kitchen quali e em função dos dois
ggplot(data, aes(x = central_air, y = sale_price, col = as.factor(central_air), fill = as.factor(central_air))) +
  geom_boxplot() + 
  scale_fill_manual(breaks = levels(as.factor(data$central_air)),
                    values = c("#EE0000", "#00EE00")) +
  labs(x = "Central Air Availability", y = "Sale Price", title = "Boxplot of Sale Price by Central Air",fill = "Central Air", color = "Central Air") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#EEE9E9", linetype = "solid"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"))

ggplot(data, aes(x = kitchen_quali, y = sale_price, col = as.factor(kitchen_quali), fill = as.factor(kitchen_quali))) +
  geom_boxplot() + 
  scale_fill_manual(breaks = levels(as.factor(data$kitchen_quali)),
                    values = c("#EE0000", "#0000EE","#FFD700")) +
  labs(x = "Kitchen Quality", y = "Sale Price", title = "Boxplot of Sale Price by Kitchen Quality",fill="Kitchen Quality",color="Kitchen Quality") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#EEE9E9", linetype = "solid"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "black","black"))

ggplot(data, aes(x = kitchen_quali, y = sale_price, col = as.factor(central_air), fill = as.factor(central_air))) +
  geom_boxplot() + 
  scale_fill_manual(breaks = levels(as.factor(data$central_air)),
                    values = c("#EE0000", "#00EE00")) +
  labs(x = "Kitchen Quality", y = "Sale Price", title = "Boxplot of Sale Price by Central Air and Kitchen Quality",fill="Central Air",color="Central Air") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#EEE9E9", linetype = "solid"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("black", "black"))

#Teste para avaliar se existem diferenças significarivas nos grupos

