
library(solitude)
library(tidyverse)
library(vroom)

# Les inn data
df <- vroom::vroom("data/transaksjonsdata.csv")

# Velger variabler vi skal bruke...
# Kan være lurt å velge litt flere her 8) 
train <- df %>% 
  select(belop_NOK, tekstkode)

# Lag modell
# Tips: Øk antall trær når du har funnet noe som virker fornuftig
isf <- isolationForest$new(num_trees = 10, 
                           mtry = 1, 
                           sample_size = 1e5, 
                           nproc = 3)
isf$fit(train)

df$anomaly_score <- isf$scores$anomaly_score

# Lag plott
test %>% 
  mutate(outlier = if_else(anomaly_score > 0.5, "outlier", "normal")) %>% 
  ggplot(aes(belop_NOK, tekstkode, color = outlier)) + 
  geom_point()

# Aggreger data til kundenivå
# Er "max" et veldig godt valg av summary-funksjon? Nah. 
df %>% 
  group_by(kunde_id) %>% 
  summarise(risk_score = max(anomaly_score, na.rm = TRUE))

# Lag score fra 0-100
normaliser_score <- function(score, maksverdi) {
    n_score <-
      (score - min(score, na.rm = TRUE)) / (max(score, na.rm = TRUE) - min(score, na.rm = TRUE))
    out <- round(n_score * maksverdi)
    out
}

df <- df %>% 
  mutate(risk_score = normaliser_score(risk_score, 100))