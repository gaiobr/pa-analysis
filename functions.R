# ---- Head ------------------- 
# Project: Protected Areas Social Interest
#
# Functions used in project
#
# Created in 2019-06-19
#
# Guedes-Santos, J
#
#______________________________

# ---- Return Brazilian Protected Area Original Name ----
pa_name <- function(dataset, cod_cnuc) {
  # This function is based in PA_Dataset: BrazilianProtectedAreas_[date]>_reviewed.csv
  name <- dataset %>%
    filter(codCnuc == cod_cnuc) %>%
    select(nomeUC)
  return(name$nomeUC)
}

# ---- Return List of Brazilian PAs Name ----
pa_list_names <- function(dataset, pa_list) {
  pa_names <- NA
  for (i in 1:length(pa_list)) {
    print(i)
    print(pa_name(pa_dataset, pa_list[i]))
    print(pa_list[i])
    pa_names[i] <- pa_name(pa_dataset, pa_list[i])
  }
  return(pa_names)
}
month.list <- NA
# ---- Return English month name from Portuguese
translate_month <- function(month.pt) {
  month.list <- matrix(
    c(01:12, 
      "janeiro", "fevereiro", "março", "abril", "maio", "junho", 
      "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"),
      ncol = 2
      )
  colnames(month.list) <- c("month.n", "month.t")
  #month.list <- as.table(month.list)

  month.pt <- month.list[which(month.list[,2] == month.pt), ]
  
  return(as.numeric(month.pt[1]))
}

