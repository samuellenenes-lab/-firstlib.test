library(readr)
library(usethis)

df_velo <- readr::read_delim(
  "C:/Users/samue/OneDrive/Bureau/244400404_comptages-velo-nantes-metropole.csv",
  delim = ";"
)

usethis::use_data(df_velo, overwrite = TRUE)
