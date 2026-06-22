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

# Série temporal do alvo (mensal, contígua a partir de 2010-05)
premio10_ts <- ts(analise$premio_10, frequency = 12, start = c(2010, 5))

# Decomposição: tendência / sazonalidade / restante
autoplot(mstl(premio10_ts))

# Autocorrelação e relação com as defasagens
acf(premio10_ts)
lag.plot(premio10_ts, lags = 9)

library(tseries)

# Teste formal de estacionariedade (H0: NÃO é estacionária)
adf.test(premio10_ts)

# Quantas diferenças o R recomenda?
ndiffs(premio10_ts)

# Diferencia uma vez e re-examina
dif1 <- diff(premio10_ts)
autoplot(dif1) +
  labs(title = "Prêmio 10 anos — 1ª diferença", x = NULL, y = NULL) +
  theme_minimal()
acf(dif1)
adf.test(dif1)

# PACF da série diferenciada — ajuda a sugerir a ordem AR (p)
pacf(dif1)

# Deixa o R escolher a ordem (sem sazonalidade, como já justificamos)
mod_auto <- auto.arima(premio10_ts, seasonal = FALSE)
summary(mod_auto)

# Diagnóstico dos resíduos do modelo escolhido
checkresiduals(mod_auto)

# --- Treino (até dez/2024) e teste (2025, 12 meses) ---
treino <- window(premio10_ts, end = c(2024, 12))
teste  <- window(premio10_ts, start = c(2025, 1))

# --- ARIMA automático (reescolhe só com o treino) ---
mod_auto_tr <- auto.arima(treino, seasonal = FALSE)
summary(mod_auto_tr)
prev_auto <- forecast(mod_auto_tr, h = length(teste))

# --- ARIMA manual com d = 1 (o concorrente do impasse) ---
mod_d1 <- Arima(treino, order = c(1, 1, 1))
summary(mod_d1)
prev_d1 <- forecast(mod_d1, h = length(teste))

# --- Visualiza as duas previsões contra o real ---
autoplot(prev_auto) +
  autolayer(teste, series = "Real") +
  labs(title = "ARIMA automático vs real (2025)", x = NULL, y = "% a.a.") +
  theme_minimal()

# --- Concorrentes de suavização exponencial ---
mod_holt <- holt(treino, h = length(teste))      # tendência, sem sazonalidade
mod_ets  <- forecast(ets(treino), h = length(teste))  # ETS automático
prev_holt <- mod_holt
prev_ets  <- mod_ets

# --- Junta tudo num data frame: real + cada previsão ---
comp <- tibble(
  data  = seq(as.Date("2025-01-01"), by = "month", length.out = length(teste)),
  Real  = as.numeric(teste),
  ARIMA = as.numeric(prev_auto$mean),
  Holt  = as.numeric(prev_holt$mean),
  ETS   = as.numeric(prev_ets$mean)
)

# --- EAM/MAE de cada modelo (função do prof) ---
eam <- function(yreal, yprev) mean(abs(yreal - yprev))

resultados <- tibble(
  Modelo = c("ARIMA(1,1,0)", "Holt", "ETS"),
  MAE = c(eam(comp$Real, comp$ARIMA),
          eam(comp$Real, comp$Holt),
          eam(comp$Real, comp$ETS))
) %>% arrange(MAE)

resultados

# --- Gráfico: real vs todas as previsões (2025) ---
comp %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  ggplot(aes(data, valor, color = serie)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Real vs previsões — teste 2025",
       x = NULL, y = "% a.a.", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

comp %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  ggplot(aes(data, valor, color = serie)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Real vs previsões — teste 2025",
       x = NULL, y = "% a.a.", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# -----------------------------
# Recorta treino/teste no MESMO data frame (pra ter as variáveis macro alinhadas)
analise_tr <- analise %>% filter(data <= as.Date("2024-12-01"))
analise_te <- analise %>% filter(data >= as.Date("2025-01-01"))

# --- Modelo de regressão: prêmio 10a explicado por macro + inclinação ---
mod_reg <- lm(premio_10 ~ selic + ipca + cambio + inclinacao, data = analise_tr)
summary(mod_reg)

# Significância global das variáveis (Anova type II, estilo do prof)
library(car)
Anova(mod_reg)

# Diagnóstico dos resíduos
par(mfrow = c(2, 2)); plot(mod_reg); par(mfrow = c(1, 1))

# O teste que falta: autocorrelação dos resíduos da regressão
library(forecast)
checkresiduals(mod_reg)

# ---------------
# Previsão da regressão no teste de 2025
prev_reg <- predict(mod_reg, newdata = analise_te)

# MAE da regressão (alinha com o real de 2025)
eam(analise_te$premio_10, prev_reg)