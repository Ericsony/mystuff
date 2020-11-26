inspect <- function(data, variable) {
  N <- length(variable)
  CI_lower <- round(CI(variable)["lower"], digits = 2)
  CI_upper <- round(CI(variable)["upper"], digits = 2)
  Min <- round(summary(variable)["Min."], digits = 2)
  Max <- round(summary(variable)["Max."], digits = 2)
  Mean <- round(summary(variable)["Mean"], digits = 2)
  print(str(data))
  print("------------------------------------------------------------------------------------------------------------")
  print("Missing data per variable:")
  print(sort(colSums(is.na(data))))
  print("------------------------------------------------------------------------------------------------------------")
  print(densityplot(variable)) 
  skew <- t(Skew(variable, method=1, conf.level = .95))
  print("Skew:")
  print(skew)
  skewness <- if (skew[2] > 0 & skew[3] > 0) {
    skewness = "positively skewed"
                } else if (skew[2] < 0 & skew[3] < 0) {
                   skewness = "negatively skewed"
                }  else {
                   skewness = "not skewed"
                }
  print(skewness)
  print("------------------------------------------------------------------------------------------------------------")
  kurt <- t(Kurt(variable, method=1, conf.level = .95))
  print("Kurtosis:")
  print(kurt)
  kurtosis <- if (kurt[2] > 0 & kurt[3] > 0) {
    kurtosis = "leptokurtotic"
  } else if (kurt[2] < 0 & kurt[3] < 0) {
    kurtosis = "platykurtotic"
  }  else {
    kurtosis = "no significant kurtosis"
  }
  print(kurtosis)
  print("------------------------------------------------------------------------------------------------------------")
  print("Observations more than 3 SDs from the mean:")
  print(which(abs(scale(variable))>=3))
  print("------------------------------------------------------------------------------------------------------------")
  return(paste0("A total of ",
                N," participants provided data on this variable. The scores ranged from ",
                Min," to ",
                Max,", M = ",
                Mean,", 95% CIs [",
                CI_lower,", ",
                CI_upper,"]."))
}
