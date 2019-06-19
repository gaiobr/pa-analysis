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

