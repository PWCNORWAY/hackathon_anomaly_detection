
library(solitude)
library(tidyverse)
library(vroom)

# Les inn data
df <- vroom::vroom("data/transaksjonsdata.csv")

# Velger variabler vi skal bruke...
# Kan være lurt å velge litt flere her 8) 
df <- df %>% 
  select(kunde_id, belop_NOK, tekstkode)

# Lag modell
# Tips: Øk antall trær når du har funnet noe som virker fornuftig
isf <- isolationForest$new(num_trees = 10, 
                           mtry = 1, 
                           sample_size = 1e5, 
                           nproc = 3)

isf$fit(df %>% select(-kunde_id))

df$anomaly_score <- isf$scores$anomaly_score

# Lag plott
df %>% 
  sample_frac(0.05) %>% 
  mutate(outlier = if_else(anomaly_score > 0.5, "outlier", "normal")) %>% 
  ggplot(aes(y = belop_NOK, x = tekstkode, color = outlier)) + 
  geom_point() +
  theme_minimal() +
  labs(title = "Unormale transaksjoner per tekstkode",
       subtitle = "Modellen beregner hva som er normal størrelse per tekstkode, og identifiserer avvik.")

# Aggreger data til kundenivå
# Er "max" et optimalt valg av summary-funksjon her? Tviler :)
kunde <- df %>% 
  group_by(kunde_id) %>% 
  summarise(risk_score = max(anomaly_score, na.rm = TRUE))

# Lag score fra 0-100
normaliser_score <- function(score, maksverdi) {
    n_score <-
      (score - min(score, na.rm = TRUE)) / (max(score, na.rm = TRUE) - min(score, na.rm = TRUE))
    out <- round(n_score * maksverdi)
    out
}

kunde <- kunde %>% 
  mutate(risk_score = normaliser_score(risk_score, 100))

# Lag innlevering i formatet "lagnavn_*.csv"
kunde %>% 
  vroom_write("innlevering/epo_kunde.csv", delim = ";")

df %>% 
  vroom_write("epo_transaksjoner.csv", delim = ";")