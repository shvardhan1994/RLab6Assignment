#'Brute force algorithm for the knapsack problem
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@details This algorithm gives all possbile values with good accuracy , and also gives the maximum value for the knapsack
#'@examples
#'\dontrun{
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#'brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#'brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#'}
#'@return A list with maximum value and weights adding up to the maximum value
#'@export
brute_force_knapsack <- function(x,W){
  len <- nrow(x)
  temp_value <- 0
  temp_weight <- 0
  
  stopifnot(is.data.frame(x) & is.numeric(W) & W > 0 & colnames(x[1]) == "w" & colnames(x[2]) == 'v')
{
for(i in 1:(2^(len) -1))
{
    bin <- intToBits(i)
    index <- which(bin == 1)
    weight <- 0
    value <- 0

    #for(j in 1:length(index))
    #{
      
      #weight <- weight + x$w[index[j]]
      weight <- sum(x[index,"w"])
      #value <- value + x$v[index[j]]
      value <- sum(x[index,"v"])
    
      
    #}
    
    if(weight <= W && temp_value < value)
    {
          temp_weight <- weight
          temp_value <- value
          op_list <- list( "value" = temp_value, "elements" = index)
        
    }
} 
  return(op_list)
 
} 
  
}
#system.time(t <-brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000, parallel = FALSE))
#system.time(t1 <- brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000))
