# ============================================================
# A FORMA DA CURVA DE JURO REAL E O PRÊMIO DE 10 ANOS
# Uma análise de séries temporais (INE5649)
# ============================================================
#
# A HISTÓRIA QUE ESTE TRABALHO CONTA
# ----------------------------------
# Quando o governo brasileiro emite um título Tesouro IPCA+ (NTN-B), ele
# promete pagar a inflação MAIS um juro real fixo. Esse "juro real" é o
# prêmio que o investidor exige para emprestar ao governo por um prazo.
# Ele muda todo dia e muda de forma diferente para cada prazo: o juro real
# de 2 anos não é o mesmo que o de 20 anos. A "curva" formada por esses
# prêmios tem uma FORMA — às vezes sobe com o prazo (curva normal - onde
# títulos de curto prazo pagam menos que os de longo prazo), às vezes
# inverte (títulos de curto prazo pagam mais que os de longo).
#
# A PERGUNTA CENTRAL: a FORMA da curva (sua inclinação) ajuda a entender e
# a prever o nível do juro real de 10 anos? E um modelo com variáveis
# macro (Selic, inflação, câmbio) + essa inclinação prevê melhor que um
# modelo de série temporal que só olha o passado do próprio juro?
#
# UMA SEGUNDA CAMADA (leitura econômica): no fim de 2025 o juro real de 10
# anos está alto E a curva está invertida. Uma leitura clássica diz que
# curva invertida reflete a EXPECTATIVA do mercado de que os juros não vão
# durar. Este trabalho investiga, com ceticismo, se a configuração atual é
# de fato um extremo histórico — e o que os dados HONESTAMENTE permitem (e
# não permitem) concluir sobre isso.
#
# O ALVO: premio_10 = juro real de 10 anos (% ao ano).
#
# O SPOILER (que vamos demonstrar): a inclinação É relevante para explicar
# o prêmio — mas a regressão, apesar de explicar bem (R²=0,72), PREVÊ pior
# que um ARIMA simples. A moral: explicar e prever são coisas diferentes.
#
# O DATASET  é assim (mês a mês, jan/2005 a dez/2025):
#   data       -> primeiro dia de cada mês
#   ipca       -> inflação mensal medida pelo IPCA (% no mês)
#   selic      -> taxa básica de juros acumulada no mês (% no mês)
#   cambio     -> dólar/real (PTAX venda, média do mês)
#   premio_2/5/10/20 -> juro real (% a.a.) do Tesouro IPCA+ em prazos fixos
#                       de 2, 5, 10 e 20 anos.
#                       Valores interpolados = estimados de valores vizinhos
#                       que temos.
#                       POR QUE INTERPOLAR? Cada título tem uma data de
#                       vencimento fixa (ex.: 2035, 2045). Num dia qualquer,
#                       o prazo que falta até o vencimento é um número
#                       quebrado — títulos com prazo de 9,8 anos ou 9,3 anos;
#                       raramente existe um título exatamente com prazo de
#                       2, 5, 10 ou 20 anos exatos. Além disso, o título
#                       mais próximo de "10 anos" muda com o tempo (conforme os
#                       títulos encurtam e novos são emitidos). Para ter uma
#                       série comparável ao longo dos anos, interpolamos a
#                       curva em PRAZOS CONSTANTES: a cada mês, estimamos qual
#                       seria o juro de um título de exatamente 2, 5, 10 e 20
#                       anos, a partir dos títulos que de fato existem naquele
#                       dia.
# ============================================================

library(tidyverse)
library(forecast)

# --- Leitura do painel cru (gerado pelo script de coleta) ---
painel <- read_csv("data/painel_final.csv")
glimpse(painel)

# ------------------------------------------------------------
# 1. PRIMEIRA INVESTIGAÇÃO: o dado existe de verdade?
# ------------------------------------------------------------
# Antes de escolher variáveis, vemos quanto de cada vértice temos de fato.
# A interpolação só é honesta quando há títulos perto do prazo; quando não
# há, o valor seria inventado (por isso, na coleta, viram NA em vez de chute).
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
# ACHADO (lendo a tabela acima):
# - premio_10: completo e contíguo (246 meses, 0 buracos) -> candidato natural a ALVO.
# - premio_2 : ralo demais (91 meses, com muitos buracos no meio) -> inviável.
# - premio_5 e premio_20: boa cobertura, com alguns buracos no premio_20.
# Conclusão parcial: analisar o premio de 2 anos não se sustenta; a inclinação terá de
# ser construída a partir dos vértices bem cobertos (5 e 20 anos).

# ------------------------------------------------------------
# 2. DATASET DE TRABALHO
# ------------------------------------------------------------
# A inclinação só existe a partir de mai/2010, então recortamos a janela.
# A inclinacao resume a FORMA da curva num número (positivo = curva normal,
# negativo = curva invertida).
analise <- painel %>%
  filter(data >= as.Date("2010-05-01")) %>%
  mutate(inclinacao = premio_20 - premio_5) %>%
  select(data, ipca, selic, cambio,
         premio_5, premio_10, premio_20, inclinacao)

# Resumo numérico para o texto.
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
# ACHADO:
# - premio_10: média 5,4% (mediana 5,6%), variando de 2,7% a 7,6% no período.
#   A mediana acima da média sugere leve assimetria à esquerda (alguns meses
#   de juro baixo "puxam" a média para baixo).
# - inclinacao: média 0,41, mas com amplitude grande (-0,76 a 1,88) — ou seja,
#   passou tanto por curva invertida quanto bem inclinada. Em média positiva
#   (curva normal foi o estado mais comum no período).
# - selic e ipca: as de MENOR dispersão relativa, oscilando em faixas estreitas.
# - cambio: o maior espalhamento (1,56 a 6,10) — reflexo da desvalorização do
#   real ao longo dos 15 anos.
# - n: premio_10/selic/ipca/cambio têm 188 meses; a inclinacao só 166 (herda
#   os buracos do premio_20), como esperado.

# ------------------------------------------------------------
# 3. CONHECENDO CADA VARIÁVEL (distribuições)
# ------------------------------------------------------------
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
# A distribuição do premio_10 não é um sino único: aparece mais de um
# aglomerado (massa perto de ~4% e pico em ~6%), sugerindo que a série
# passou por patamares distintos ao longo do período.

# ------------------------------------------------------------
# 4. QUEM ANDA COM QUEM (correlações)
# ------------------------------------------------------------
# Matriz de correlação em NÍVEL
library(corrplot)
analise %>%
  select(premio_10, inclinacao, selic, ipca, cambio) %>%
  drop_na() %>%
  cor() %>%
  corrplot(method = "color", addCoef.col = "black",
           tl.col = "black", type = "upper")
# Atenção a uma armadilha. premio_10, selic e cambio têm tendência: sobem e
# descem em ondas longas. Duas séries assim acusam correlação alta só por
# andarem na mesma direção no tempo, mesmo sem relação de verdade entre elas
# — é o que se chama de correlação ESPÚRIA (falsa, enganosa).
# Exemplo clássico: o número de afogamentos e a venda de sorvete sobem juntos
# no verão, mas um não causa o outro — quem move os dois é o calor.
# Para separar o real do espúrio, olhamos as VARIAÇÕES mês a mês (próximo
# bloco): se a correlação sobrevive nas variações, é relação genuína; se
# desaparece, era só a tendência comum enganando.

cols <- c("premio_10", "inclinacao", "selic", "ipca", "cambio")

# (a) Em níveis — pode estar contaminada pela tendência comum
analise %>% select(all_of(cols)) %>% drop_na() %>% cor() %>% round(2)

# (b) Em diferenças mensais — relação entre as VARIAÇÕES
analise %>%
  select(all_of(cols)) %>%
  mutate(across(everything(), ~ . - lag(.))) %>%
  drop_na() %>%
  cor() %>% round(2)
# ACHADO (um dos centrais do trabalho) — olhando a linha do premio_10:
# - selic:      0,83 -> 0,02. A correlação forte EVAPORA nas variações. Era
#               ESPÚRIA: premio e Selic só pareciam andar juntos por terem
#               tendências de longo prazo parecidas. Mês a mês, não casam.
#               (Se tivéssemos confiado no nível, contaríamos história errada.)
# - cambio:     0,01 -> 0,42. O OPOSTO: uma relação INVISÍVEL em nível APARECE
#               nas variações. Quando o real apanha num mês, o juro real
#               exigido sobe naquele mês (reação de curto prazo a choque).
#               Isso contradiz a regressão em nível (onde câmbio deu p=0,77).
# - inclinacao: -0,81 -> -0,38. ENCOLHE mas SOBREVIVE. Parte era tendência
#               comum, mas resta relação real. A associação que sustenta a
#               tese central NÃO é miragem — é mais modesta, e genuína.

# Dispersão do alvo contra cada variável (em nível, para visualização).
analise %>%
  select(premio_10, selic, inclinacao, ipca, cambio) %>%
  pivot_longer(-premio_10, names_to = "var", values_to = "valor") %>%
  ggplot(aes(valor, premio_10)) +
  geom_point(alpha = 0.4, color = "#7570b3") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.6) +
  facet_wrap(~ var, scales = "free_x") +
  labs(title = "Prêmio de 10 anos vs cada variável", y = "premio_10", x = NULL) +
  theme_minimal()

# ACHADOS:
# Corrobora com os anteriores. 
# Cada reta preta é o ajuste linear do prêmio contra a variável.
# - selic: nuvem apertada e inclinada -> relação positiva forte (a mais clara).
# - inclinacao: reta descendente nítida -> relação negativa (a tese principal que temos).
# - ipca: reta quase plana, pontos espalhados -> relação fraca.
# - cambio: reta horizontal, sem padrão -> sem relação linear em nível.
# Prévia do que a regressão vai testar: selic e inclinacao puxam a curva do juro real; ipca e cambio não.


# Por que a curva inverte? Cada vértice vs a Selic (em variações).
# Hipótese testada: a Selic mexeria MAIS no premio_5 do que no premio_20.
analise %>%
  select(selic, premio_5, premio_10, premio_20) %>%
  mutate(across(everything(), ~ . - lag(.))) %>%
  drop_na() %>%
  cor() %>% round(2)
# ACHADO: a hipótese não colou. A variação da Selic quase não se mexe junto
# com a de nenhum vértice (0,04 / 0,02 / 0,04), e não cai conforme vai do
# premio_5 ao premio_20 — ou seja, ela não "bate mais" no de 5 anos.
# Em compensação, apareceu outra coisa: premio_5, premio_10 e premio_20 sobem
# e descem quase juntos (0,93 entre 5 e 10; 0,95 entre 10 e 20). A curva se
# move em bloco, e a inclinação é só a sobrinha que sobra entre as pontas —
# daí ela ser uma variável tão sutil.
# Um palpite pra explicar (não testado aqui): a Selic é definida pelo Copom
# em saltos, em datas certas, enquanto os prêmios são de mercado e se mexem
# todo dia, antecipando o Copom. Por isso as variações mês a mês não casam.

# ------------------------------------------------------------
# 5. A SÉRIE NO TEMPO + ONDE ESTAMOS HOJE (leitura da curva)
# ------------------------------------------------------------
ggplot(analise, aes(data, premio_10)) +
  geom_line(color = "#1b9e77", linewidth = 0.6) +
  labs(title = "Prêmio de juro real de 10 anos (alvo)",
       x = NULL, y = "% a.a.") +
  theme_minimal()
# ACHADO: não há UMA tendência, e sim ONDAS LONGAS de vários anos — cai a
# ~3% (2012-13, 2019-20), sobe a ~7,5% (2015-16, 2024-25), em ciclos de
# política monetária e risco fiscal. A linha é "lisa": cada mês é quase igual
# ao anterior e a série leva anos para mudar de patamar. Isso se chama alta
# PERSISTÊNCIA — e vai reaparecer na ACF e no ARIMA mais à frente.

ggplot(analise, aes(data, inclinacao)) +
  geom_line(color = "#7570b3", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Inclinação da curva real (20a - 5a)",
       x = NULL, y = "pontos percentuais") +
  theme_minimal()
# ACHADO: a inclinacao cruza o zero várias vezes. E contraste com o gráfico
# anterior: quando o premio_10 está BAIXO, a inclinação tende a POSITIVA;
# quando o premio_10 está ALTO (2024-25), a curva INVERTE. Nível e forma se
# movem em sentidos opostos — coerente com a correlação negativa (-0,38).

# Onde estamos HOJE, na história da AMOSTRA (2010-2025)?
# Percentil = fração de meses com valor <= o valor mais recente (dez/2025).
# IMPORTANTE: "história" = nossa janela 2010-2025, NÃO "todos os tempos".
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
# ACHADO ROBUSTO:
# - premio_10 no percentil 92,6 (juro real alto vs os últimos 15 anos).
# - inclinacao no percentil 1,8 (curva mais invertida que em 98% da amostra).
# - selic no percentil 98,9 (aperto monetário quase máximo).
# Dez/2025 é um EXTREMO CONJUNTO RARO: juro alto + curva muito invertida.
#
# LEITURA DA CURVA:
# - O que é MEDIÇÃO: estamos num extremo histórico da amostra, e a
#   inclinação tem associação negativa real (porém modesta) com o nível.
# - O que é INTERPRETAÇÃO (hipótese, não medida): a curva invertida é lida
#   como o mercado ESPERANDO que o juro alto não perdure. Mas isso supõe a
#   hipótese das expectativas e ignora o prêmio de prazo — é leitura, não fato.
# - HUMILDADE NECESSÁRIA: expectativa do mercado NÃO é certeza. E, como o
#   ARIMA vai mostrar, o nível futuro é largamente imprevisível (quase
#   passeio aleatório). Logo, o robusto é só "estamos num extremo"; qualquer
#   afirmação sobre "vai cair" extrapolaria o que os dados sustentam.

# Objeto de série temporal mensal do alvo.
premio10_ts <- ts(analise$premio_10, frequency = 12, start = c(2010, 5))

# Decomposição em tendência / sazonalidade / restante.
autoplot(mstl(premio10_ts))
# ACHADO CRUCIAL: a tendência domina (~4,5 pontos); a "sazonalidade" é
# minúscula (~0,3) e sem padrão de calendário aparente.
# CONSEQUÊNCIA: usaremos ARIMA (não SARIMA) e Holt (não Holt-Winters).
# Juro de mercado não tem "mês de alta".

# Autocorrelação: o passado prevê o presente?
acf(premio10_ts)
lag.plot(premio10_ts, lags = 9)
# ACHADO: 
# a ACF começa em ~0,97 e CAI DEVAGAR -> assinatura de série
# NÃO-ESTACIONÁRIA (o nível vagueia). 
# O lag.plot confirma: no lag 1 os pontos quase colam na diagonal
# (persistência altíssima).

# ------------------------------------------------------------
# 6. A SÉRIE É ESTACIONÁRIA? (o ponto sutil)
# ------------------------------------------------------------
# (estacionária = série estável, que oscila em torno de uma média fixa em vez
#  de vagar entre patamares ao longo do tempo)
library(tseries)
adf.test(premio10_ts)     # H0: NÃO é estacionária
ndiffs(premio10_ts)       # usa KPSS -> H0 oposta: É estacionária
# ACHADO (contradição reveladora):
# - ADF: p=0,58 -> não rejeita H0 -> "parece NÃO-estacionária".
# - ndiffs: 0   -> "não precisa diferenciar" (diferenciar = olhar a variação
#   de um mês pro outro, em vez do valor absoluto).
# Eles divergem porque têm hipóteses nulas OPOSTAS e cada um falhou em
# rejeitar a sua. A série é BORDERLINE — quase raiz unitária (= não tem um
# nível fixo pra onde voltar; vai ficando onde o último empurrão deixou...
# por exemplo, a temperatura de uma cidade tende a orbitar uma média;
# já nossa situação é como o saldo de uma conta, que parte
# sempre do último valor e não desfaz os movimentos passados).
# Faz sentido: o juro real é limitado (não vai a infinito), mas em 15 anos se
# comporta quase como passeio aleatório (= cada valor é o anterior + um
# empurrão aleatório, então prever pra onde vai é quase impossível).

# A série original vagava (não-estacionária), e isso é um problema: o ARIMA
# precisa de uma série estável pra modelar. A saída é DIFERENCIAR — em vez do
# valor cheio de cada mês (6,7%, 7,0%...), passar a olhar a VARIAÇÃO de um mês
# pro outro (+0,2, -0,1, +0,4...). Esses valores ficam sempre perto de zero,
# então não "vagam" como os valores cheios. Aqui testamos se diferenciar UMA
# vez já resolve.
dif1 <- diff(premio10_ts)

# Gráfico: antes a série passeava entre 3% e 7%; agora oscila em torno de
# ZERO e sempre volta pra lá. Cara de série estável.
autoplot(dif1) +
  labs(title = "Prêmio 10 anos — 1ª diferença", x = NULL, y = NULL) +
  theme_minimal()

# ACF: na série original ela caía devagar (a tal "memória longa"); agora
# DESPENCA — só o lag 1 se destaca, o resto entra nas bandas. A persistência
# sumiu.
acf(dif1)

# ADF: p=0,01 -> agora REJEITA "não-estacionária" -> a série diferenciada É
# estacionária, com confiança estatística.
adf.test(dif1)

# ACHADO: diferenciar uma vez resolveu. A série da VARIAÇÃO mensal é estável,
# então é nela que os modelos vão trabalhar (o "d=1" do ARIMA, mais à frente,
# é exatamente isso: o modelo diferenciando a série por dentro).

# PACF da série diferenciada: ajuda a estimar quantos termos AR o modelo
# precisa. A regra é contar quantos "palitos" furam a linha azul no começo —
# aqui só o lag 1 fura, sugerindo ~1 termo AR. Junto com a ACF (que apontava
# ~1 termo MA), o palpite manual fica perto de ARIMA(1,1,1) — bem na vizinhança
# do que o auto.arima vai escolher sozinho na próxima seção.
pacf(dif1)

# ------------------------------------------------------------
# 7. MODELANDO O FUTURO (séries temporais)
# ------------------------------------------------------------
# Deixamos o auto.arima escolher sozinho a melhor combinação de termos.
mod_auto <- auto.arima(premio10_ts, seasonal = FALSE)
summary(mod_auto)
checkresiduals(mod_auto)   # Ljung-Box: testa se sobrou padrão nos resíduos
# (H0: resíduos são ruído aleatório, sem estrutura)
# ACHADO: o modelo escolhido foi ARIMA(1,0,2). Curioso que ele NÃO diferenciou
# (o "0" do meio), apesar de termos visto que a série vaga. Em vez disso, pôs
# um ar1 = 0,96 (quase 1) — um termo que "puxa" cada mês quase totalmente do
# anterior, o que na prática imita o efeito de diferenciar. É o caráter
# borderline de novo: a série está no limiar, e o modelo lidou com isso por
# outro caminho.
# E os resíduos? O Ljung-Box deu p=0,61 (>0,05): NÃO rejeitamos "resíduos
# aleatórios" -> o modelo capturou a estrutura da série, não sobrou padrão.
# Confirmação no gráfico: a ACF dos resíduos fica dentro das bandas. Modelo OK.

# PREVISÃO fora da amostra: treino até dez/2024, teste = 2025 (12 meses).
treino <- window(premio10_ts, end = c(2024, 12))
teste  <- window(premio10_ts, start = c(2025, 1))

# ARIMA reescolhido SÓ com o treino (até dez/2024).
mod_auto_tr <- auto.arima(treino, seasonal = FALSE)
summary(mod_auto_tr)
prev_auto <- forecast(mod_auto_tr, h = length(teste))
# ACHADO (a virada): com a série INTEIRA (seção 7), o auto.arima tinha
# escolhido d=0 (não diferenciar). Agora, vendo só o treino, ele escolhe
# ARIMA(1,1,0) -> d=1, decide DIFERENCIAR. O MESMO método mudou de ideia só
# por enxergar uma amostra um pouco menor. Isso não é defeito: é a prova
# concreta de que a série está no LIMIAR entre estável e "vaga" — tão em cima
# do muro que basta tirar 2025 pra resposta virar.

# E se a gente montar um ARIMA na mão, em vez de aceitar o automático?
# Testamos o (1,1,1) — o palpite que a ACF+PACF sugeriram lá atrás — pra ver
# se um modelo um pouco mais rico se sai diferente.
mod_d1 <- Arima(treino, order = c(1, 1, 1))
summary(mod_d1)
prev_d1 <- forecast(mod_d1, h = length(teste))
# ACHADO: deu quase no mesmo. O termo extra do (1,1,1) é a parte "MA", que usa
# os ERROS de previsão dos meses anteriores pra corrigir o palpite atual. Aqui
# esse termo (ma1 = -0,11) saiu minúsculo e não-significativo (erro-padrão
# maior que o próprio coeficiente -> o efeito pode ser zero), e os erros de
# treino ficaram idênticos aos do automático. Na prática, o (1,1,1) manual e o
# (1,1,0) automático são o mesmo modelo — sinal de robustez: dois caminhos
# diferentes chegam ao mesmo lugar.

autoplot(prev_auto) +
  autolayer(teste, series = "Real") +
  labs(title = "ARIMA automático vs real (2025)", x = NULL, y = "% a.a.") +
  theme_minimal()
# ACHADO: a previsão central (linha escura) é quase uma RETA em ~7,2 — a série
# é tão persistente que o melhor palpite é "o futuro parece o último valor". O
# real de 2025 (laranja) fica colado nela: foi um ano de juro "de lado".
# Mas repare no LEQUE azul que abre: são as faixas de 80% e 95% de confiança.
# O modelo não promete 7,2 cravado — ele diz que em 12 meses o valor pode estar
# em qualquer lugar entre ~5 e ~9. Quanto mais longe projeta, mais largo o
# leque: o modelo admitindo que sabe cada vez menos. Coerente com a série
# quase-passeio-aleatório, e um lembrete honesto de que "acertar" aqui é, em
# parte, mérito de 2025 ter ficado dentro dessa faixa larga.

# Concorrentes de suavização exponencial (sem sazonalidade).
mod_holt <- holt(treino, h = length(teste))
mod_ets  <- forecast(ets(treino), h = length(teste))
prev_holt <- mod_holt
prev_ets  <- mod_ets

# ------------------------------------------------------------
# 8. QUEM PREVÊ MELHOR? (comparação por MAE no teste de 2025)
# ------------------------------------------------------------
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
# ACHADO (ranking): ARIMA 0,18  <  ETS 0,30  <  Holt 0,67.
# - ARIMA venceu por ser HUMILDE: projeta o último nível (~7,2), e como 2025
#   ficou de lado, acertou.
# - HOLT foi o PIOR porque EXTRAPOLA a tendência: pegou a alta de 2023-24 e
#   projetou a série continuando a subir — mas ela estancou, e ele foi se
#   descolando do real mês a mês.
# - ETS ficou no meio: também sobe, mas de forma amortecida (menos agressivo
#   que o Holt), então erra menos que ele e mais que o ARIMA.

comp %>%
  pivot_longer(-data, names_to = "serie", values_to = "valor") %>%
  ggplot(aes(data, valor, color = serie)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Real vs previsões — teste 2025",
       x = NULL, y = "% a.a.", color = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
# O gráfico mostra com os olhos o que o MAE mediu: o HOLT (azul) dispara em
# diagonal, indo a ~8,7 enquanto o real ficou perto de 7,3 — o erro de quem
# apostou na tendência. O ETS (verde) sobe igual, mas mais devagar. O ARIMA
# (vermelho) é a linha quase plana em ~7,2, ignorando os zigue-zagues do real,
# mas orbitando o nível certo. O Real (roxo) serpenteia sem direção: 2025 de
# lado. Detalhe honesto: NINGUÉM capturou os solavancos do real — o ARIMA
# venceu por ser uma reta na ALTURA certa, não por adivinhar os movimentos.

# ------------------------------------------------------------
# 9. A IDEIA ORIGINAL: a inclinação ajuda? (regressão concorrente)
# ------------------------------------------------------------
analise_tr <- analise %>% filter(data <= as.Date("2024-12-01"))
analise_te <- analise %>% filter(data >= as.Date("2025-01-01"))

mod_reg <- lm(premio_10 ~ selic + ipca + cambio + inclinacao, data = analise_tr)
summary(mod_reg)
library(car)
Anova(mod_reg)
# ACHADO (a resposta à pergunta central):
# - inclinacao: coef = -0,77, e no Anova é a 2ª variável mais forte (F=42),
#   atrás só da Selic. A intuição original estava certa: a forma da curva
#   carrega informação sobre o nível. O sinal negativo bate com a leitura
#   (curva inverte <-> juro alto). IMPORTANTE: o p-valor cru (p≈0) é otimista,
#   porque os resíduos são autocorrelacionados (vamos ver já abaixo) — mas a
#   relação SOBREVIVE nas variações (-0,38), e é esse o apoio robusto de que
#   ela é real, não só o p-valor.
# - selic: coef = +2,17, F=70 -> a força dominante, como esperado.
# - ipca e cambio: não-significativos EM NÍVEL. Faz sentido econômico: o prêmio
#   já é juro REAL (descontada a inflação), então o ipca spot não agrega; e o
#   efeito do câmbio provavelmente já vem embutido na Selic. (Lembrar: em
#   VARIAÇÕES o câmbio apareceu com 0,42 — a regressão em nível não pega esse
#   efeito de curto prazo.)
# - R² ajustado = 0,72: bom ajuste.

# Diagnóstico de resíduos: os 4 gráficos clássicos parecem OK...
par(mfrow = c(2, 2)); plot(mod_reg); par(mfrow = c(1, 1))
# ...mas eles NÃO testam autocorrelação, o pressuposto que mais falha em
# série temporal. Rodamos o teste certo (o mesmo do exercício 5):
checkresiduals(mod_reg)
# ACHADO DECISIVO: Breusch-Godfrey p < 2e-16 -> autocorrelação FORTÍSSIMA, e
# dá pra VER isso no gráfico: os resíduos (painel de cima) fazem ondas longas,
# meses seguidos do mesmo sinal, em vez de pular aleatoriamente em torno de
# zero; e a ACF (painel de baixo) decai devagar, com vários lags fora das
# bandas — a "memória" que sobrou e o modelo não capturou.
# O que isso significa: a regressão acerta a relação CONTEMPORÂNEA (no mesmo
# mês, Selic e inclinação explicam bem o prêmio), mas viola a independência dos
# resíduos por ignorar a dinâmica temporal. CONSEQUÊNCIA HONESTA: sob
# autocorrelação, os erros-padrão do OLS são subestimados, então os p-valores
# da regressão são otimistas (menos confiáveis do que parecem). A relação da
# inclinação é grande e econômica, mas a PRECISÃO estatística é menor que o
# "p≈0" sugere — e é exatamente a lacuna que a família ARIMA existe para preencher.

# E na previsão de 2025?
prev_reg <- predict(mod_reg, newdata = analise_te)
eam(analise_te$premio_10, prev_reg)
# ACHADO FINAL: MAE da regressão = 0,68 — empatada com o PIOR modelo (Holt) e
# 3,7x pior que o ARIMA (0,18). O modelo mais rico em variáveis (R²=0,72,
# inclinação relevante) PREVÊ PIOR que um ARIMA que só olha o passado da série.
# POR QUÊ esse paradoxo? Porque explicar e prever são coisas diferentes:
#  (1) a regressão é estática — fotografa a relação de um mês, mas ignora a
#      dinâmica temporal (foi o que a autocorrelação dos resíduos denunciou),
#      então erra em sequência ao projetar;
#  (2) a regressão ainda jogou COM VANTAGEM: para prever 2025 ela usou os
#      valores REAIS de Selic/IPCA/câmbio/inclinação de 2025 — informação que,
#      na vida real, você não teria de antemão (teria que prevê-los antes). E
#      mesmo com essa vantagem, perdeu pro ARIMA.
# EXPLICAR != PREVER: um bom ajuste no passado não garante boa previsão no futuro.
# RESSALVA: ARIMA e regressão tiveram treinos um pouco diferentes (a regressão
# perdeu 22 meses pelos buracos do premio_20). Mas o teste (2025) é idêntico e a
# margem do ARIMA é grande demais (3,7x) para isso explicar o resultado.

# ------------------------------------------------------------
# 10. CONCLUSÃO
# ------------------------------------------------------------
# FECHANDO O ARCO: a pergunta que abriu o trabalho era se a forma da curva
# (a inclinação) ajuda a entender o juro de 10 anos. A resposta tem duas
# camadas: SIM para EXPLICAR (a inclinação é a 2ª variável mais forte na
# regressão, e a relação sobrevive até nas variações), mas isso NÃO se
# converteu em previsão melhor — o ARIMA simples ganhou. A inclinação é
# informativa sobre o presente; não é uma bola de cristal sobre o futuro.
#
# MORAL TÉCNICA: para prever este juro real, um ARIMA simples (que só usa o
# passado da própria série) bateu uma regressão cheia de variáveis macro. Em
# uma série quase-passeio-aleatório e "de lado" no teste, a humildade venceu.
#
# MORAL ECONÔMICA (leitura da curva, com o devido ceticismo):
# - ROBUSTO (medição): dez/2025 é um extremo histórico da amostra — juro real
#   no topo e curva fortemente invertida; e a inclinação tem associação real,
#   ainda que modesta, com o nível do juro.
# - INTERPRETAÇÃO (hipótese, não fato): a curva invertida é lida como o
#   mercado esperando que o juro alto não perdure. Isso supõe a hipótese das
#   expectativas e ignora o prêmio de prazo.
# - LIMITE: a própria modelagem mostra que o nível futuro é largamente
#   imprevisível. Portanto, a curva informa uma EXPECTATIVA do mercado, não um
#   destino. O que os dados sustentam com segurança é apenas que estamos num
#   ponto extremo e incomum — não uma previsão confiável sobre para onde vai.