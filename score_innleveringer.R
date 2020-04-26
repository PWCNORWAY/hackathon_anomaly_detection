# Score innleveringer
library(vroom)
library(tidyverse)
library(yardstick)

# Steg 1: Legg alle kunde.csv-filer i innlevering-folderen

# Les alle innleveringer
files <- fs::dir_ls(path = "innlevering")
df_list <- vroom::vroom(files, id = "fil")

fasit <- vroom::vroom("kundemapping_med_fasit.csv")

# Legg på fasit
df_list <- df_list %>% 
  full_join(fasit)

# Sjekk at alle innleveringene er fornuftig
# De som ikke er det blir diskvalifisert :3
df_list %>% 
  group_by(fil) %>% 
  summarise(antall = n(),
            snittscore = mean(risk_score),
            max_score = max(risk_score),
            min_score = min(risk_score))

# Score innleveringer
df_list %>% 
  group_by(fil) %>% 
  mutate(er_rapportert = as.factor(er_rapportert)) %>% 
  yardstick::roc_auc(er_rapportert, risk_score) %>% 
  arrange(-.estimate)
