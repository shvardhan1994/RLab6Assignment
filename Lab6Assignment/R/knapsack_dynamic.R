#'Dynamic programming algorithm for the knapsack problem
#'@param x is a data.frame containing variables 'w' and 'v', weights and values
#'@param W is the total size of the knapsack
#'@param fast is set to TRUE when running Rcpp method to optimize the code
#'@details An algorithm that can solve the knapsack problem exact by iterating over all possible values of w all possbile values with good accuracy , and also gives the maximum value for the knapsack
#'@examples
#'\dontrun{
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#'knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#'knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#'}
#'@return A list with maximum value and weights adding up to the maximum value
#'@useDynLib Lab6Assignment, .registration = TRUE
#'@importFrom Rcpp sourceCpp
#'@export


knapsack_dynamic <- function(x,W,fast=FALSE)
{
  my_mat <- matrix(NA,nrow=nrow(x)+1,ncol=W+1)
  my_mat[,1] <- 0
  my_mat[1,] <- 0
  colnames(my_mat) <- c(0,1:W)
  rownames(my_mat) <- c(0,1:nrow(x))
  stopifnot(is.data.frame(x) & is.numeric(W) & W > 0 & colnames(x[1]) == "w" & colnames(x[2]) == 'v')
  {
    if(fast == FALSE){
      
      for(i in 2:(nrow(x)+1))
      {
        for(j in 1:W+1)
        {
          if(x$w[i-1] > j)
          {
            my_mat[i,j] <- my_mat[i-1,j]
          }
          else 
          {
            my_mat[i,j] <- max(my_mat[i-1,j],my_mat[i-1, j-x$w[i-1]] + x$v[i-1])
          }
        }
      }
    } else
    {
      
      my_mat <-knapSackdynamic_cpp(W = W,wt = x$w, val = x$v, n = nrow(x))
    }
  }
  i <- (nrow(x)+1)
  max_value <- max(my_mat[i,])
  temp_max_value <- max_value
  elements <- vector()
  j <- 1
  while(i > 1) {
    
    if(temp_max_value %in% my_mat[i-1,])
    {
      i <- i - 1
    } else
    {
      elements[j] <- i-1
      j <- j + 1
      i <- i - 1
      temp_max_value <- temp_max_value - x$v[i]
    }
  }
  
  op_list <- list( "value" = max_value, "elements" = sort(elements))
  return(op_list)
} 


