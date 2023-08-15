
needs(rvest, tidyverse)


ajps_urls <- str_c("https://dataverse.harvard.edu/dataverse/ajps?q=&types=dataverses%3Adatasets&sort=dateSort&order=desc&page=", 1:65)

extract_ajps <- function(url) {
  cat(url)
  page <- read_html(url)
  
  tibble(
    article = html_elements(page, ".card-title-icon-block") |>
      html_text(),
    date =
      html_elements(page, ".text-center+ .text-muted") |>
      html_text()
  ) |>
    mutate(year = str_extract(date, "\\d{4}"))
}

ajps <- map(ajps_urls, safely(extract_ajps), .progress = TRUE)

ajps_bind <- ajps |> 
  map('result') |> 
  compact() |> 
  bind_rows()|> 
  mutate(journal = "ajps")

|> 
  write_csv("ajps_replication.csv")



apsr_urls <- str_c("https://dataverse.harvard.edu/dataverse/the_review?q=&types=dataverses%3Adatasets&sort=dateSort&order=desc&page=", 1:54)
apsr <- map(apsr_urls, safely(extract_ajps), .progress = TRUE)

apsr_bind <- apsr |> 
  map('result') |> 
  compact() |> 
  bind_rows() |> 
  mutate(journal = "apsr")

apsr_bind
apsr |> count(year)

# JOP

jop_urls <- str_c("https://dataverse.harvard.edu/dataverse/jop?q=&types=dataverses%3Adatasets&sort=dateSort&order=desc&page=", 1:76
)
jop <- map(jop_urls, safely(extract_ajps), .progress = TRUE)

jop_bind <- jop |> 
  map('result') |> 
  compact() |> 
  bind_rows() |> 
  mutate(journal = "jop")


replication <- bind_rows(ajps_bind, apsr_bind, jop_bind)

view(replication)
replication |> 
  filter(year < 2022) |> 
  group_by(year, journal) |> 
  count() |> 
  ggplot(aes(year, n, color = journal, group = journal)) + 
  geom_point() +
  geom_line() +
  theme_light() + 
  scale_color_viridis_d()
