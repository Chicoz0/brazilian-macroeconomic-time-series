# ============================================================
# Projeto: IPCA, Selic, Câmbio USD/BRL e Prêmio de Juro Real (Tesouro IPCA+)
# Parte 1 — Coleta e tratamento dos dados
# ============================================================

# --- 0. Instalação dos pacotes (roda só o que estiver faltando) ---
pacotes  <- c("rbcb", "tidyverse", "GetTDData")
faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(faltando) > 0) install.packages(faltando)
if (!"patchwork" %in% rownames(installed.packages())) install.packages("patchwork")

# Obs. p/ quem usa Ubuntu/Linux: se a instalação tentar compilar e falhar
# por bibliotecas de sistema, rode no TERMINAL antes:
#   sudo apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev \
#        libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev
# Alternativa (Ubuntu 24.04 "noble") - usar binários e evitar compilar:
#   options(repos = c(
#     P3M  = "https://packagemanager.posit.co/cran/__linux__/noble/latest",
#     CRAN = "https://cloud.r-project.org"))

# --- 1. Pacotes ---
library(patchwork)
library(rbcb)
library(tidyverse)   # dplyr, tidyr, lubridate, ggplot2, ...
library(GetTDData)

# --- 2. Parâmetros ---
ini <- "2005-01-01"
fim <- "2025-12-31"

# --- 3. Dados macro do Banco Central (SGS) ---
# 433  = IPCA, variação mensal (%)
# 4390 = Selic acumulada no mês (%)
# 3698 = Câmbio USD/BRL (PTAX venda), média mensal
ipca   <- get_series(c(ipca   = 433),  start_date = ini, end_date = fim)
selic  <- get_series(c(selic  = 4390), start_date = ini, end_date = fim)
cambio <- get_series(c(cambio = 3698), start_date = ini, end_date = fim)

# padroniza cada série para o 1º dia do mês
prep <- function(serie, nome) {
  serie %>%
    rename(valor = 2) %>%
    mutate(data = floor_date(date, "month")) %>%
    select(data, !!nome := valor)
}

macro <- prep(ipca, "ipca") %>%
  inner_join(prep(selic,  "selic"),  by = "data") %>%
  inner_join(prep(cambio, "cambio"), by = "data") %>%
  arrange(data)

# --- 4. Prêmio de juro real (Tesouro IPCA+ / NTN-B Principal) ---
dados_td <- td_get(asset_codes = "NTN-B Principal",
                   first_year  = 2005,
                   last_year   = 2025) %>%
  mutate(prazo_anos = as.numeric(matur_date - ref_date) / 365.25)

# interpola a curva em vértices fixos (maturidade constante)
vertices <- c(2, 5, 10, 20)

premio_cm <- dados_td %>%
  group_by(ref_date) %>%
  filter(n() >= 2) %>%                       # precisa de >=2 títulos no dia
  reframe(
    prazo       = vertices,
    premio_real = approx(prazo_anos, yield_bid,
                         xout = vertices, rule = 2)$y * 100
  ) %>%
  mutate(data = floor_date(ref_date, "month")) %>%
  group_by(data, prazo) %>%
  summarise(premio_real = mean(premio_real, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = prazo, values_from = premio_real,
              names_prefix = "premio_")

# --- 5. Painel final mensal ---
painel <- macro %>%
  left_join(premio_cm, by = "data") %>%
  arrange(data) %>%
  drop_na()

glimpse(painel)
# premio_* fica NA antes de ~2005 (início da série das NTN-B) — é esperado

# 1. Câmbio (nível)
g1 <- ggplot(painel, aes(data, cambio)) +
  geom_line(color = "#1b9e77", linewidth = 0.6) +
  labs(title = "Câmbio USD/BRL", x = NULL, y = "R$/US$") +
  theme_minimal()

# 2. IPCA e Selic mensais
g2 <- painel %>%
  select(data, ipca, selic) %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  ggplot(aes(data, valor, color = serie)) +
  geom_line(linewidth = 0.6) +
  labs(title = "IPCA e Selic (% a.m.)", x = NULL, y = "% ao mês", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# 3. Prêmios de juro real por vértice
g3 <- painel %>%
  select(data, starts_with("premio_")) %>%
  pivot_longer(-data, names_to = "vertice", values_to = "premio") %>%
  ggplot(aes(data, premio, color = vertice)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Prêmio de juro real (Tesouro IPCA+)",
       x = NULL, y = "% a.a.", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")

# 4. Câmbio vs juro real de 10 anos
g4 <- ggplot(painel, aes(premio_10, cambio)) +
  geom_point(alpha = 0.5, color = "#7570b3") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  labs(title = "Câmbio vs juro real 10 anos",
       x = "Prêmio 10a (% a.a.)", y = "R$/US$") +
  theme_minimal()

# Monta o painel 2x2
(g1 | g2) / (g3 | g4)
