info.gain.rpart <- function(fit1, to_plot = T, ylab = "sum of all the
improvement (in fit$split[, 'improve'])",
                            main = "Information per variable" ,..., sort = T, col)
{
  info_gain <- tapply(cart_model$splits[, "improve"], rownames(cart_model$splits), sum)
  
  # let's order info_gain according to the original order of the letters in the data.frame
  # needed function:
  order.x.by.y <- function(x,y) order(match(x, y)) # this function gets x/y and returns the order of x so it will be like y
  x_names <- names(attr(cart_model, "xlevels")) # the original names of the elements
  info_gain_order <- order.x.by.y(names(info_gain),x_names) # the needed neworder.
  info_gain <- info_gain[info_gain_order]
  
  length_info_gain <- length(info_gain)
  # info.gain <- info.gain[c(8,1:7)]
  if(missing(col)) col <- rep("grey", length_info_gain)
  if(length(col) < length_info_gain) col <- rep(col, length_info_gain)
  if(sort) {
    ss <- order(info_gain,decreasing = T)
    info_gain <- info_gain[ss]
    col <- col[ss] # this way we can notice which belongs to which stem...
  }
  if(to_plot) barplot(info_gain, ylab = ylab, main = main,col =col,...)
  return(info_gain)
}
