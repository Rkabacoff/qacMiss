#' Plot missing data by variable
#'
#' Plot the amount of missing data by variable
#'
#' @param data A dataframe
#' @import ggplot2 dplyr
#'
#' @return ggplot2 graph
#' @examples
#' data(mtcars2)
#' col_miss(mtcars2)
#' @export
#'
col_miss <- function(data){
  len = nrow(data)
  if(!any(is.na(data)))return(cat("No missing data"))

  columnSums <- colSums(is.na(data))
  missing_df  = data.frame(missing_vals = columnSums,
                           total = len)

  missing_df = missing_df %>%
    mutate(proportion = 100*(missing_vals/total)) %>%
    arrange(desc(proportion))
  missing_df['vars'] = rownames(missing_df)

  ggplot(missing_df, aes(y = proportion, x = reorder(vars, proportion))) +
    geom_bar(stat="identity", fill="cornflower blue") +
    coord_flip() +
    scale_y_continuous(limits=c(0,100), breaks=seq(0.0, 100.0, by=10.0)) +
    theme_minimal() +
    labs(
      title = "Missing Values By Variable",
      y = "Percent",
      x = "Variable"
    )
}

