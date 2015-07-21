#get my databases

setwd("D:/Ying/coding/RStudio/outskewer")

mydataset <- local({
  data <- read.csv(file="data/my_results.txt", sep = "", stringsAsFactors = FALSE, header = TRUE)
  
  data$x <- as.numeric(data$x)
  data$yes <- as.numeric(data$yes)
  data$maybe <- as.numeric(data$maybe)
  data$no <- as.numeric(data$no)
  data$unknown <- as.numeric(data$unknown)
  
  return(data)
  
  
})

get_marks <- function(result_df){
  
  yes <- subset(result_df, yes == 1, select = "x")
  maybe <- subset(result_df, maybe == 1, select = "x")
  unknown <- subset(result_df, unknown == 1, select = "x")
  
  return(list(yes = yes, maybe = maybe, unknow = unknown))
  
}

get_points <- function(marks_list){
  
  outliers.x <- rownames(marks_list$yes)
  outliers.y <- marks_list$yes$x
  
  maybe.x <- rownames(marks_list$maybe)
  maybe.y <- marks_list$maybe$x
  
  unknown.x <- rownames(marks_list$unknown)
  unknown.y <- marks_list$unknown$x
  
  return(list(outliers.x = outliers.x, outliers.y = outliers.y, maybe.x = maybe.x, maybe.y = maybe.y, unknown.x = unknown.x, unknown.y = unknown.y))
}

results <- get_marks(mydataset)

points_pos <- get_points(results)

plot(mydataset$x, type = "l", col = "red")

points(points_pos$outliers.x, points_pos$outliers.y, pch = 21, bg = "green")

points(points_pos$maybe.x, points_pos$maybe.y, pch = 25, bg = "blue")

legendNames = c("Outliers", "May be")
legend('topright', legendNames, col = c('green', 'blue'), cex = 1.5, bty="n", pch = c(21, 25), pt.bg = c("green", "blue"))