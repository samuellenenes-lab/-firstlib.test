library(readr)
library(usethis)

df_velo <- readr::read_delim(
  "244400404_comptages-velo-nantes-metropole.csv",
  delim = ";"
)

names(df_velo)[names(df_velo) == "\ufeffNuméro de boucle"] <- "Numéro de boucle"

usethis::use_data(df_velo, overwrite = TRUE)
