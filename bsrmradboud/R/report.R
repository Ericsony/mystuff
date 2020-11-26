# has only been tested on lm() with 1 IV and 1 DV

report <- function(model) {
  modelname_short <- paste(strsplit(as.character(model["call"]), "")[[1]][c(1,2)], collapse="")
  modelname <- ifelse(modelname_short == "lm", "linear regression analysis", "[statistical analysis]")
  IV <- trimws(sub(",.*", "", sub(".*~", "", as.character(model["call"]))))
  DV <- trimws(sub(".*=", "", sub("~.*", "", as.character(model["call"]))))
  R2 <- round(as.numeric(summary(model)["r.squared"]), digits = 3) 
  numdf <- summary(model)$fstatistic[2]
  dendf <- summary(model)$fstatistic[3]
  Fstatistic <- round(summary(model)$fstatistic[1], digits = 2)
  p_forIV <- ifelse(round(summary(model)$coefficients[2,4], digits= 24) < 0.001 , "< .001", round(summary(model)$coefficients[2,4], digits=2))
  b_forIV <- round(summary(model)$coefficients[2,1], digits=2)
  SE_forIV <- round(summary(model)$coefficients[2,2], digits=2)
  pos_or_neg <- ifelse(b_forIV > 0, "positively", "negatively")
  equalssign <- ifelse(p_forIV == "< .001", "", "=")
  return(paste0("A ",modelname," was performed to examine whether ",DV," was predicted by ", IV ,
                ". The analysis explained ", R2*100 ,"% of the variance of ", DV ,
                ", F(", numdf ,", ", dendf ,") = ", Fstatistic ,", p",equalssign," ",p_forIV,". ",
                DV ," was ",pos_or_neg," associated with ", IV ,
                " (b = ",b_forIV,", SE = ",SE_forIV,", p",equalssign," ",p_forIV,
                "), meaning that... (describe in your own words what that means)."))
}  
