# ============================================================
# A FORMA DA CURVA DE JURO REAL E O PRÊMIO DE 10 ANOS (INE5649)
# Alvo: premio_10 = juro real de 10 anos (% a.a.).
# Dataset mensal (jan/2005 a dez/2025): data, ipca, selic, cambio,
# premio_2/5/10/20 (juro real % a.a. em prazos constantes, interpolados).
# ============================================================

library(tidyverse)
library(forecast)

# Lê o painel cru.
painel <- read_csv("../data/painel_final.csv")
glimpse(painel)


# ############################################################
#                PARTE I — DADOS E EXPLORAÇÃO
# ############################################################

# Cobertura de cada vértice (início, fim, válidos, buracos).
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
# ACHADO: premio_10 completo (246 meses, 0 buracos); premio_2 ralo (91, inviável); premio_5 e premio_20 boa cobertura.

# Recorta a janela (>= mai/2010) e cria inclinacao = premio_20 - premio_5.
analise <- painel %>%
  filter(data >= as.Date("2010-05-01")) %>%
  mutate(inclinacao = premio_20 - premio_5) %>%
  select(data, ipca, selic, cambio,
         premio_5, premio_10, premio_20, inclinacao)

# Resumo numérico das variáveis.
analise %>%
  select(premio_10, inclinacao, selic, ipca, cambio) %>%
  pivot_longer(everything(), names_to = "var", values_to = "v") %>%
  group_by(var) %>%
  summarise(
    n = sum(!is.na(v)), media = mean(v, na.rm = TRUE),
    mediana = median(v, na.rm = TRUE), dp = sd(v, na.rm = TRUE),
    min = min(v, na.rm = TRUE), max = max(v, na.rm = TRUE),
    .groups = "drop"
  )
# ACHADO: premio_10 média 5,4% (mediana 5,6%; 2,7–7,6%); inclinacao média 0,41 (-0,76 a 1,88); cambio 1,56–6,10; n=188 (inclinacao 166).

# Histograma + densidade de cada variável.
analise %>%
  select(premio_10, inclinacao, selic, ipca, cambio) %>%
  pivot_longer(everything(), names_to = "var", values_to = "valor") %>%
  ggplot(aes(valor)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#7570b3", alpha = 0.5) +
  geom_density(color = "#1b9e77", linewidth = 0.8) +
  facet_wrap(~ var, scales = "free") +
  labs(title = "Distribuição de cada variável", x = NULL, y = NULL) +
  theme_minimal()
# ACHADO: premio_10 multimodal (massa ~4% e pico ~6%).

# Correlação em nível.
library(corrplot)
analise %>%
  select(premio_10, inclinacao, selic, ipca, cambio) %>%
  drop_na() %>%
  cor() %>%
  corrplot(method = "color", addCoef.col = "black",
           tl.col = "black", type = "upper")
# ACHADO: premio_10, selic e cambio têm tendência; correlação em nível pode ser espúria.

# Correlação em nível vs em diferenças mensais.
cols <- c("premio_10", "inclinacao", "selic", "ipca", "cambio")

analise %>% select(all_of(cols)) %>% drop_na() %>% cor() %>% round(2)

analise %>%
  select(all_of(cols)) %>%
  mutate(across(everything(), ~ . - lag(.))) %>%
  drop_na() %>%
  cor() %>% round(2)
# ACHADO (linha premio_10): selic 0,83->0,02 (espúria); cambio 0,01->0,42 (aparece); inclinacao -0,81->-0,38 (sobrevive).

# Dispersão do premio_10 contra cada variável.
analise %>%
  select(premio_10, selic, inclinacao, ipca, cambio) %>%
  pivot_longer(-premio_10, names_to = "var", values_to = "valor") %>%
  ggplot(aes(valor, premio_10)) +
  geom_point(alpha = 0.4, color = "#7570b3") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  facet_wrap(~ var, scales = "free_x") +
  labs(title = "Prêmio de 10 anos vs cada variável", y = "premio_10", x = NULL) +
  theme_minimal()
# ACHADO: selic positiva forte; inclinacao negativa nítida; ipca fraca; cambio sem relação em nível.

# Variação da Selic vs variação de cada vértice.
analise %>%
  select(selic, premio_5, premio_10, premio_20) %>%
  mutate(across(everything(), ~ . - lag(.))) %>%
  drop_na() %>%
  cor() %>% round(2)
# ACHADO: Selic ~0 com todos vértices (0,04/0,02/0,04); vértices entre si 0,93 (5-10) e 0,95 (10-20).

# Série do alvo no tempo.
ggplot(analise, aes(data, premio_10)) +
  geom_line(color = "#1b9e77", linewidth = 0.6) +
  labs(title = "Prêmio de juro real de 10 anos (alvo)",
       x = NULL, y = "% a.a.") +
  theme_minimal()
# ACHADO: ondas longas (cai ~3%, sobe ~7,5%); alta persistência.

# Inclinação no tempo.
ggplot(analise, aes(data, inclinacao)) +
  geom_line(color = "#7570b3", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Inclinação da curva real (20a - 5a)",
       x = NULL, y = "pontos percentuais") +
  theme_minimal()
# ACHADO: cruza o zero várias vezes; premio_10 alto <-> curva invertida.

# Percentil do valor mais recente (dez/2025) na amostra 2010-2025.
ult <- function(x) tail(na.omit(x), 1)
analise %>%
  reframe(
    variavel   = c("premio_10", "inclinacao", "selic", "cambio"),
    valor_hoje = round(c(ult(premio_10), ult(inclinacao),
                         ult(selic), ult(cambio)), 3),
    percentil  = round(100 * c(
      mean(premio_10  <= ult(premio_10),  na.rm = TRUE),
      mean(inclinacao <= ult(inclinacao), na.rm = TRUE),
      mean(selic      <= ult(selic),      na.rm = TRUE),
      mean(cambio     <= ult(cambio),     na.rm = TRUE)
    ), 1)
  )
# ACHADO: premio_10 percentil 92,6; inclinacao 1,8; selic 98,9. Dez/2025 = extremo (juro alto + curva muito invertida).

# Objeto de série temporal mensal do alvo.
premio10_ts <- ts(analise$premio_10, frequency = 12, start = c(2010, 5))

# Decomposição tendência/sazonalidade/restante.
autoplot(mstl(premio10_ts))
# ACHADO: tendência domina (~4,5); sazonalidade ~0,3 -> ARIMA, não SARIMA.

# Autocorrelação e lag.plot.
acf(premio10_ts)
lag.plot(premio10_ts, lags = 9)
# ACHADO: ACF começa ~0,97 e cai devagar -> não-estacionária; lag 1 cola na diagonal.


# ############################################################
#              PARTE II — MODELAGEM E PREVISÃO
# ############################################################

# Testes de estacionariedade (ADF vs KPSS via ndiffs).
library(tseries)
adf.test(premio10_ts)     # H0: NÃO é estacionária
ndiffs(premio10_ts)       # usa KPSS -> H0 oposta: É estacionária
# ACHADO: ADF p=0,58 (não rejeita); ndiffs=0. Série borderline (quase raiz unitária).

# Primeira diferença e seus diagnósticos.
dif1 <- diff(premio10_ts)

autoplot(dif1) +
  labs(title = "Prêmio 10 anos — 1ª diferença", x = NULL, y = NULL) +
  theme_minimal()

acf(dif1)

adf.test(dif1)
# ACHADO: diferenciar 1x resolve; ADF p=0,01 -> estacionária.

# PACF da série diferenciada (ordens do ARIMA).
pacf(dif1)
# ACHADO: só lag 1 fura -> ~1 termo AR; palpite ARIMA(1,1,1).

# auto.arima na série inteira + checagem de resíduos.
mod_auto <- auto.arima(premio10_ts, seasonal = FALSE)
summary(mod_auto)
checkresiduals(mod_auto)   # Ljung-Box
# ACHADO: ARIMA(1,0,2), ar1=0,96; Ljung-Box p=0,61 -> resíduos OK.

# Split treino (<= dez/2024) / teste (2025) e ARIMA só no treino.
treino <- window(premio10_ts, end = c(2024, 12))
teste  <- window(premio10_ts, start = c(2025, 1))

mod_auto_tr <- auto.arima(treino, seasonal = FALSE)
summary(mod_auto_tr)
prev_auto <- forecast(mod_auto_tr, h = length(teste))
# ACHADO: no treino escolhe ARIMA(1,1,0) (d=1); muda de ideia -> confirma borderline.

# ARIMA(1,1,1) manual.
mod_d1 <- Arima(treino, order = c(1, 1, 1))
summary(mod_d1)
prev_d1 <- forecast(mod_d1, h = length(teste))
# ACHADO: ma1=-0,11 não-significativo; equivale ao (1,1,0) automático.

# ARIMA automático vs real (2025).
autoplot(prev_auto) +
  autolayer(teste, series = "Real") +
  labs(title = "ARIMA automático vs real (2025)", x = NULL, y = "% a.a.") +
  theme_minimal()
# ACHADO: previsão ~reta em 7,2; real 2025 colado; leque de confiança abre (5 a 9).

# Concorrentes de suavização exponencial.
mod_holt <- holt(treino, h = length(teste))
mod_ets  <- forecast(ets(treino), h = length(teste))
prev_holt <- mod_holt
prev_ets  <- mod_ets

# Tabela de erros (MAE no teste de 2025).
comp <- tibble(
  data  = seq(as.Date("2025-01-01"), by = "month", length.out = length(teste)),
  Real  = as.numeric(teste),
  ARIMA = as.numeric(prev_auto$mean),
  Holt  = as.numeric(prev_holt$mean),
  ETS   = as.numeric(prev_ets$mean)
)

eam <- function(yreal, yprev) mean(abs(yreal - yprev))

resultados <- tibble(
  Modelo = c("ARIMA(1,1,0)", "Holt", "ETS"),
  MAE = c(eam(comp$Real, comp$ARIMA),
          eam(comp$Real, comp$Holt),
          eam(comp$Real, comp$ETS))
) %>% arrange(MAE)
resultados
# ACHADO: ARIMA 0,18 < ETS 0,30 < Holt 0,67.

# Real vs todas as previsões.
comp %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  ggplot(aes(data, valor, color = serie)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Real vs previsões — teste 2025",
       x = NULL, y = "% a.a.", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
# ACHADO: Holt dispara (~8,7); ETS sobe amortecido; ARIMA ~reta em 7,2; ninguém capturou os solavancos.

# Regressão concorrente com a inclinação.
analise_tr <- analise %>% filter(data <= as.Date("2024-12-01"))
analise_te <- analise %>% filter(data >= as.Date("2025-01-01"))

mod_reg <- lm(premio_10 ~ selic + ipca + cambio + inclinacao, data = analise_tr)
summary(mod_reg)
library(car)
Anova(mod_reg)
# ACHADO: inclinacao -0,77 (F=42, 2ª mais forte); selic +2,17 (F=70); ipca e cambio não-sig em nível; R²aj=0,72.

# Diagnóstico dos resíduos.
par(mfrow = c(2, 2)); plot(mod_reg); par(mfrow = c(1, 1))
checkresiduals(mod_reg)
# ACHADO: Breusch-Godfrey p<2e-16 -> autocorrelação fortíssima; p-valores do OLS otimistas.

# Previsão da regressão em 2025.
prev_reg <- predict(mod_reg, newdata = analise_te)
eam(analise_te$premio_10, prev_reg)
# ACHADO: MAE regressão = 0,68 (empata Holt, 3,7x pior que ARIMA 0,18). Explicar != prever.


# ############################################################
#                  PARTE III — CONCLUSÃO
# ############################################################
# - Inclinação ajuda a EXPLICAR (2ª variável mais forte, sobrevive nas variações), mas não a PREVER: ARIMA simples venceu.
# - dez/2025 = extremo da amostra (juro real no topo, curva fortemente invertida); leitura "vai cair" é hipótese, não fato.