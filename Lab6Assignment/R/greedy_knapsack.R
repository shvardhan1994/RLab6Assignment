#'Greedy algorithm for the knapsack problem
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@details This algorithm gives the approximate answer to our problem,it is able to show at least 50% of the true maximum values. And the value given by this algorithm has low computational time.
#'@examples
#'\dontrun{
#'greedy_knapsack(knapsack_objects[1:800], 3500)
#'greedy_knapsack(knapsack_objects[1:1200], 2000)
#'}
#'@return A list with maximum value and weights adding up to the maximum value
#'@export

greedy_knapsack <- function(x,W)
{
stopifnot(is.data.frame(x) & is.numeric(W) & W > 0 & colnames(x[1]) == "w" & colnames(x[2]) == 'v')
{
x$per_item <- x$v / x$w
x$rows <- row(x)
x <- x[order(x$per_item, decreasing = TRUE),]
i <- 1
temp_weight <- 0
op_list <- list(value = 0)

repeat{
  if(temp_weight <= W)
  {
  temp_weight <- temp_weight + x$w[i]
  op_list$value <- op_list$value + x$v[i]
  op_list$elements[i] <- x$rows[i]
  i <- i + 1
  if(i > nrow(x) | temp_weight + x$w[i] > W )
  {
    break()
    }
 
  }
}
return(op_list)
} 
}



