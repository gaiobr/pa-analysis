rm(list = ls())

pa_dataset <- read_csv("./data/BrazilianProtectedAreas_2019-05-13.csv")
pa_means <- read_csv("./data/BPA_Means_2019-06-21.csv")

federal_pa <- pa_dataset %>%
  filter(!is.na(pa_dataset$enWikiPage)) %>%
#  filter(!is.na(pa_dataset$ptWikiPage)) %>%
  filter(esfera == "Federal")

out <- table(federal_pa$cat)
out

out <- capture.output(out)
cat("Output", out, file='output.txt', sep = '\n', append = FALSE)

state_pa <- pa_dataset %>%
  filter(!is.na(pa_dataset$enWikiPage)) %>%
#  filter(!is.na(pa_dataset$ptWikiPage)) %>%
  filter(esfera == "Estadual")

out <- table(state_pa$cat)
out

out <- capture.output(out)
cat("Output", out, file='output.txt', sep = '\n', append = FALSE)

municipality_pa <- pa_dataset %>%
  filter(!is.na(pa_dataset$enWikiPage)) %>%
#  filter(!is.na(pa_dataset$ptWikiPage)) %>%
  filter(esfera == "Municipal")

out <- table(municipality_pa$cat)
out

out <- capture.output(out)
cat("Output", out, file='output.txt', sep = '\n', append = FALSE)


pa_merge <- inner_join(pa_means,
                       pa_dataset,
                       by = c("cod_cnuc" = "codCnuc")
)

names(pa_merge)

state_pa <- pa_merge %>%
  filter(!is.na(pa_merge$enWikiPage)) %>%
  filter(esfera.x == "Federal")

head(pa_means)
sum(pa_means$mean.eng > 10)
sum(pa_means$mean.pt > 10)
(16/389)*100
(81/389)*100

rm(list = ls())

test_pt <- read_csv("./data/BPA_Wiki_Pt_2019-06-21.csv")
test_eng <- read_csv("./data/BPA_Wiki_Eng_2019-06-21.csv")
tail(test_pt)
tail(test_eng)

test <- read_csv("./data/BPA_Means_2019-06-21.csv")
head(test)
