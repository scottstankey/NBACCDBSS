dateconv = function(x) {
  if (length(x) > 0 && !is.na(x)){
    monthvec = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    return(((x - (x %% 1e4))*(1e-4) - 2000)*365 + 
             (sum(monthvec[1:((((x %% 1e4) - (x %% 1e2))*(1e-2)) )]) - monthvec[((((x %% 1e4) - (x %% 1e2))*(1e-2)) ):((((x %% 1e4) - (x %% 1e2))*(1e-2)) )]) +
             (x %% 1e2))
  } else {return(0)}
}

