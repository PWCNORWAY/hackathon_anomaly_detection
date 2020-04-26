library(tidyverse)
library(vroom)
library(lubridate)
library(readxl)

df <- vroom::vroom("data/aml_trans.csv")

# Filtrer data til kun 2018-2019
df <- df %>% 
  filter(year(valuteringsdato) %in% c(2018, 2019))

# Lag anonymisert kunde-id
df <- df %>% 
  group_by(kundenummer) %>% 
  mutate(kunde_id = group_indices()) %>% 
  ungroup()

df %>% 
  select(-kundenummer) %>% 
  vroom::vroom_write("transaksjonsdata.csv", delim = ";")


# Lag fasit ---------------------------------------------------------------


# Lagre kundemapping
kundemapping <- df %>% 
  select(kundenummer, kunde_id) %>% 
  distinct(kundenummer, kunde_id)

# Finn fasit
path <- "data/Flagg 2017-2019.xlsx"

df_flagg <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_dfr(read_excel, path = path, .id = "år", skip = 2) %>% 
  filter(år == 2019)

kundenr_til_orgnr <- function(kundenr) {
  orgnummer <- as.character(as.numeric(str_remove(kundenr, fixed(" "))))
}

# Fjern PM fra flaggdata
df_flagg <- df_flagg %>% 
  mutate(Organisasjonsnummer = kundenr_til_orgnr(Kundenr),
         Dato = as.Date(Dato, format = "%d.%m.%Y")) %>% 
  filter(nchar(Organisasjonsnummer) == 9) %>% 
  mutate(Kundenr = str_remove(Kundenr, " "))

rapportert_status <- c("Etterforskes Økokrim", "Rapporteres", "Sendt Økokrim")

df_flagg_aggr <- df_flagg %>% 
  group_by(Kundenr) %>% 
  summarise(er_rapportert = any(Status %in% rapportert_status))

fasit <- kundemapping %>% 
  left_join(df_flagg_aggr, by = c("kundenummer" = "Kundenr")) %>% 
  replace_na(list(er_rapportert = FALSE))

# Skriv ut fasit
fasit %>% 
  mutate(er_rapportert = as.numeric(er_rapportert)) %>% 
  vroom::vroom_write("kundemapping_med_fasit.csv", delim = ";")

# Lag tulleinnlevering
test1_innlevering <- tibble(kunde_id = fasit$kunde_id,
                          risk_score = rnorm(nrow(fasit), 50, 5))

test2_innlevering <- tibble(kunde_id = fasit$kunde_id,
                          risk_score = rnorm(nrow(fasit), 50, 10))

test1_innlevering %>% vroom::vroom_write("innlevering/test1_kunde.csv", delim = ";")
test2_innlevering %>% vroom::vroom_write("innlevering/test2_kunde.csv", delim = ";")


