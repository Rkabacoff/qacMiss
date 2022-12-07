#' Plot missing data by row
#'
#' @param data
#'
#' @description
#' Plotting the amount of missing data by observation
#'
#' @return ggplot2 graph
#' @export
#' @import ggplot2
#' @examples
#' data(mtcars2)
#' row_miss(mtcars2)

row_miss <- function(data){
  if(!any(is.na(data)))retun(cat("No missing data."))
  rn <- rownames(data)
  cn <- colnames(data)
  mydf <- data.frame()
  for(row in rn){
    num <- 0
    for(column in cn){
      if(is.na(data[row,column])){
        num <- num+1
      }
    }
    mydf<- rbind(mydf, num)
  }
  mytable <- as.data.frame(table(mydf))
  mytable[, "cu"] <- cumsum(mytable$Freq)
  names(mytable) <- c("Nmiss", "Freq", "cumFreq")
  ggplot(mytable)+
    geom_bar(aes(x= Nmiss,
                 y = Freq),
             stat= "Identity",
             fill = "steelblue")+
    geom_point(aes(x= Nmiss,
                   y = cumFreq),
               stat="Identity",
               color = "lightgrey")+
    geom_line(aes(x= Nmiss,
                  y = cumFreq,
                  group = 1),
              stat="Identity",
              linetype = "dashed",
              color = "grey")+
    theme_minimal()+
    labs(
      title = "Missing Values per Case",
      x = "Number of Missing Values",
      y = "Number of Cases",
      caption = "Line is cumulative frequency.")
}
