# ============================================================
# IPCA, Selic, Câmbio USD/BRL e Prêmio de Juro Real (Tesouro IPCA+)
# Coleta dos dados, montagem do painel e exportação em CSV
# ============================================================

# --- Pacotes ---
pacotes  <- c("rbcb", "tidyverse", "GetTDData")
faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(faltando) > 0) install.packages(faltando)

library(rbcb)
library(tidyverse)
library(GetTDData)

# --- Parâmetros ---
ini <- "2005-01-01"
fim <- "2025-12-31"

# --- Dados macro do BCB (SGS): 433=IPCA, 4390=Selic, 3698=Câmbio PTAX ---
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

# --- Prêmio de juro real (NTN-B Principal), interpolado em maturidade constante ---
dados_td <- td_get(asset_codes = "NTN-B Principal",
                   first_year  = 2005,
                   last_year   = 2025) %>%
  mutate(prazo_anos = as.numeric(matur_date - ref_date) / 365.25)

vertices <- c(2, 5, 10, 20)

premio_cm <- dados_td %>%
  group_by(ref_date) %>%
  filter(n() >= 2) %>%
  reframe(
    prazo       = vertices,
    premio_real = approx(prazo_anos, yield_bid,
                         xout = vertices, rule = 1)$y * 100
  ) %>%
  mutate(data = floor_date(ref_date, "month")) %>%
  group_by(data, prazo) %>%
  summarise(premio_real = mean(premio_real, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = prazo, values_from = premio_real,
              names_prefix = "premio_")

# --- Painel final mensal ---
painel <- macro %>%
  left_join(premio_cm, by = "data") %>%
  arrange(data)

glimpse(painel)

# --- Exporta em CSV ---
dir.create("data", showWarnings = FALSE)
write_csv(painel,     "data/painel_final.csv")          # painel tratado
write_csv(macro,      "data/macro_bcb.csv")             # IPCA, Selic, câmbio
write_csv(dados_td,   "data/tesouro_ntnb_bruto.csv")    # NTN-B sem interpolar
write_csv(premio_cm,  "data/premio_real_vertices.csv")  # prêmios por vértice (2, 5, 10 e 20 anos)