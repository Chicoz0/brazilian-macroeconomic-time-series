# ============================================================
# Inclinação da curva de juro real e prêmio de 10 anos
# Script de ANÁLISE (lê o painel gerado pelo script de coleta)
# ============================================================

library(tidyverse)
library(forecast)

# --- Leitura do painel cru ---
painel <- read_csv("data/painel_final.csv")
glimpse(painel)

# ------------------------------------------------------------
# Exploração que justifica as escolhas (roda uma vez)
# ------------------------------------------------------------
# Quão completo é cada vértice da curva?
painel %>%
  pivot_longer(starts_with("premio_"), names_to = "vertice", values_to = "v") %>%
  group_by(vertice) %>%
  summarise(
    inicio    = min(data[!is.na(v)]),
    fim       = max(data[!is.na(v)]),
    n_validos = sum(!is.na(v)),
    buracos   = sum(is.na(v) & data > min(data[!is.na(v)]) & data < max(data[!is.na(v)])),
    .groups = "drop"
  )
# Conclusões da exploração:
# - premio_10: completo e contíguo  -> ALVO da previsão.
# - premio_2 : poucos dados (61 buracos) -> descartado.
# - premio_5 e premio_20: boa cobertura -> inclinacao = premio_20 - premio_5.
# - Os buracos do premio_20 (2015-17 e 2019) só afetam a regressão,
#   que tolera linhas faltantes; a ARIMA roda no premio_10 (sem buracos).

# ------------------------------------------------------------
# Dataset de trabalho
# ------------------------------------------------------------
analise <- painel %>%
  filter(data >= as.Date("2010-05-01")) %>%
  mutate(inclinacao = premio_20 - premio_5) %>%
  select(data, ipca, selic, cambio,
         premio_5, premio_10, premio_20, inclinacao)

# ------------------------------------------------------------
# EDA inicial
# ------------------------------------------------------------
# Alvo: prêmio real de 10 anos
ggplot(analise, aes(data, premio_10)) +
  geom_line(color = "#1b9e77", linewidth = 0.6) +
  labs(title = "Prêmio de juro real de 10 anos (alvo)",
       x = NULL, y = "% a.a.") +
  theme_minimal()

# Inclinação da curva (20a - 5a)
ggplot(analise, aes(data, inclinacao)) +
  geom_line(color = "#7570b3", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Inclinação da curva real (20a - 5a)",
       x = NULL, y = "pontos percentuais") +
  theme_minimal()