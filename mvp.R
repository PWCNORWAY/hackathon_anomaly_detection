
library(solitude)
library(tidyverse)
library(vroom)


# Tren modell -------------------------------------------------------------

# Les inn data
train <- vroom::vroom("data/transaksjonsdata_train.csv")

# Velger variabler vi skal bruke
# Da det gjerne tar en del tid å kjøre mange variabler, så kan det være lurt å tenke godt
# gjennom hvilke variabler du ønsker å bruke og hvordan du skal bearbeide dem.
# Husk at modellen ikke tåler for mange nivåer i faktor-variabler!
# En enkel løsning kan være å bruke fct_lump for faktorer med for mange nivåer.

train <- train %>% 
  select(kunde_id, belop_NOK, tekstkode)

# Lag modell
# Tips: Øk antall trær og/eller sample-size når du har funnet noe som virker fornuftig
# Merk: Modellen tåler ikke missing data, så bruk tidyr::replace_na f.eks. på forhånd
isf <- isolationForest$new(num_trees = 10, 
                           mtry = 1, 
                           sample_size = 1e5, 
                           nproc = 3)

isf$fit(train %>% select(-kunde_id))

train$anomaly_score <- isf$scores$anomaly_score

# Lag plott
train %>% 
  sample_frac(0.1) %>% 
  mutate(outlier = if_else(anomaly_score > 0.5, "outlier", "normal")) %>% 
  ggplot(aes(y = belop_NOK, x = tekstkode, color = outlier)) + 
  geom_point() +
  theme_minimal() +
  labs(title = "Unormale transaksjoner per tekstkode",
       subtitle = "Modellen beregner hva som er normal størrelse per tekstkode, og identifiserer avvik.")


# Score test data ---------------------------------------------------------

test <- vroom::vroom("data/transaksjonsdata_test.csv")

# Merk: Prediction datasett må ha samme antall kolonner som treningsdata
# Her kan det være lurt å bruke recipes-pakken, pass hvert fall på at test
# behandles likt som train. 
test <- test %>% 
  select(kunde_id, belop_NOK, tekstkode)

preds <- isf$predict(test %>% select(-kunde_id))

test$anomaly_score <- preds$anomaly_score

# Aggreger data til kundenivå
# Er "max" et optimalt valg av summary-funksjon her? Tviler :)
kunde <- test %>% 
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

test %>% 
  vroom_write("epo_transaksjoner.csv", delim = ";")