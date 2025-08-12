library(MASS)
str(survey)
height_data<- survey$Height
mean_height <- mean(height_data, na.rm = TRUE)
print(mean_height)
std_error <- sd(height_data, na.rm = TRUE)/sqrt(length(height_data))
print(std_error)

margin_of_error<-qt(0.95,df=length(height_data)-1)*std_error
print(margin_of_error)
lower_bound<-mean_height-margin_of_error
upper_bound<-mean_height+margin_of_error
print(lower_bound)
print(upper_bound)
