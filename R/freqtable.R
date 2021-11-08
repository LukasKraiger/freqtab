
#Frequency Tables like SPSS
#F1 is Variable Name
#Var is the actual Variable


freqtable <- function(Var) {
  #'@import knitr
  #'@import kableExtra
  abs.Haeufigkeit <- table(Var)
  Prozent <- round(prop.table(abs.Haeufigkeit)*100, digits = 2)
  Skala <- unique(sort(Var))
  kum.Prozent <- round((cumsum(Prozent)/sum(Prozent))*100, digits = 2)
  kum.Haeufigkeit <- cumsum(abs.Haeufigkeit)
  tab <- cbind(Skala, abs.Haeufigkeit, kum.Haeufigkeit,Prozent,kum.Prozent)
  #rownames(tab) <- row
  knitr::kable(head(tab), row.names = ("")) #|>
   # kableExtra::add_footnote(foot, notation = "alphabet")
}
