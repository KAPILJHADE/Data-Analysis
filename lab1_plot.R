data <- read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/data.csv", sep = ",", header = TRUE)
data
# Find Min 
min(data$nve_l)
min(data$nve_s)
min(data$nve_d)
min(data$c_b)
min(data$c_d)
min(data$per_b)
min(data$per_d)

# Find Max
max(data$nve_l)
max(data$nve_s)
max(data$nve_d)
max(data$c_b)
max(data$c_d)
max(data$per_b)
max(data$per_d)

# Find Mean.
mean(data$nve_l)
mean(data$nve_s)
mean(data$nve_d)
mean(data$c_b)
mean(data$c_d)
mean(data$per_b)
mean(data$per_d)

#Find Median
median(data$nve_l)
median(data$nve_s)
median(data$nve_d)
median(data$c_b)
median(data$c_d)
median(data$per_b)
median(data$per_d)

#Find Mode
mode(data$nve_l)
mode(data$nve_s)
mode(data$nve_d)
mode(data$c_b)
mode(data$c_d)
mode(data$per_b)
mode(data$per_d)

#Find Variance
var(data$nve_l)
var(data$nve_s)
var(data$nve_d)
var(data$c_b)
var(data$c_d)
var(data$per_b)
var(data$per_d)

#Find Standard Deviation
sd(data$nve_l)
sd(data$nve_s)
sd(data$nve_d)
sd(data$c_b)
sd(data$c_d)
sd(data$per_b)
sd(data$per_d)

#Find IQR
IQR(data$nve_l)
IQR(data$nve_s)
IQR(data$nve_d)
IQR(data$c_b)
IQR(data$c_d)
IQR(data$per_b)
IQR(data$per_d)

#Detecting outliers in data
#data <- read.table("data.csv", sep = ",", header = TRUE)
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  dev.new(width=5, height=4, unit="in")
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}
outlierKD(data,data$nve_l)
outlierKD(data,data$nve_s)
outlierKD(data,data$nve_d)
outlierKD(data,data$c_b)
outlierKD(data,data$c_d)
outlierKD(data,data$per_b)
outlierKD(data,data$per_d)

#Plot 1
data <- read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/data.csv", sep = ",", header = TRUE)
dev.new(width=5, height=4, unit="in")
plot(data$year,data$nve_l, type="l", col="green", lwd=5, xlab="years", ylab="No. of Births")
#lines(data$year,data$nve_d, col="red", lwd=5)
title("No. of Live Births from 2011 to 2016")
#legend("topright",c("Births","nve_ds"), lwd=c(5,2), col=c("green","red"), y.intersp=1.5)

#Plot 2
data <- read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/data.csv", sep = ",", header = TRUE)
dev.new(width=5, height=4, unit="in")
plot(data$year,data$c_b, type="b", col="green", lwd=5, xlab="years", ylab="birth rate/nve_d rate",ylim=range(data$c_d,data$c_b))
lines(data$year,data$c_d,type="b",col="red", lwd=5)
title("Birth and nve_d rates from 2011 to 2016")
legend("topright",c("Birth rate","nve_d rate"), lwd=c(5,2), col=c("green","red"), y.intersp=1.5)

#Plot 3
data <- read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/data.csv", sep = ",", header = TRUE)
dev.new(width=5, height=4, unit="in")
plot(data$year,data$per_b, type="b", col="green", lwd=5, xlab="years", ylab="birth %/nve_d % ",ylim=range(data$per_d,data$per_b))
lines(data$year,data$per_d,type="b",col="red", lwd=5)
title("Birth and nve_d percentage from 2011 to 2016")
legend("topright",c("Birth %","nve_d %"), lwd=c(5,2), col=c("green","red"), y.intersp=1.5)

#Plot 4
data <- read.table("/home/kapil/desktop/study material notes/6TH SEM/Data Analysis/data.csv", sep = ",", header = TRUE)
dev.new(width=5, height=4, unit="in")
boxplot(data$nve_s ~ data$year, xlab = "years",ylab = "No.of still Births", main = "still birth data")
