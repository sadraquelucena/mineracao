# Parte 1: Carregamento de Pacotes ----
# -------------------------------------------------------------------
library(duckdb)     # Para processamento 'out-of-core' (ler arquivos grandes)
library(DBI)        # Interface de banco de dados
library(tidyverse)  # Para manipulação (dplyr, tidyr, readr)
library(tidymodels) # Para pré-processamento (recipe)
library(readxl)     # Para ler o .xlsx do IBGE
library(GGally)     # Para exploração (ggpairs)
library(skimr)      # Para diagnósticos (skim)


# Parte 2: Conexão e Consulta 'Out-of-Core' (DATASUS) ----
# -------------------------------------------------------------------
# Esta é a etapa mais importante para "Big Data".
# Não vamos carregar o .csv de 2GB na memória.
# Vamos usar o DuckDB para *consultar* o arquivo como se fosse um SQL.

# 1. Preparar a conexão (em memória, sem salvar em disco)
con <- dbConnect(
  duckdb::duckdb(),
  dbdir = ":memory:" # Processamento 100% em memória RAM
)

# Caminho para o arquivo gigante para não precisarmos escrever em toda consulta
caminho_datasus <- "/home/sadraque/Documentos/UFS/Disciplinas/2025.2/mineracao de dados em estatistica/slides/04-processamento_out-of-core/DO24OPEN.csv"
caminho_ibge <- "/home/sadraque/Documentos/UFS/Disciplinas/2025.2/mineracao de dados em estatistica/slides/04-processamento_out-of-core/pop2024.xlsx"

# 3. Criar a consulta SQL
# Objetivo: Agregar óbitos por *causa básica* e *município de residência*
consulta <- sprintf("
  SELECT
    CODMUNRES AS cod_mun_res_datasus,
    COUNT(*) AS total_obitos,
    SUM(CASE WHEN CAUSABAS BETWEEN 'C00' AND 'D48' THEN 1 ELSE 0 END) AS obitos_neoplasias,
    SUM(CASE WHEN CAUSABAS BETWEEN 'I00' AND 'I99' THEN 1 ELSE 0 END) AS obitos_circulatorio,
    SUM(CASE WHEN CAUSABAS BETWEEN 'J00' AND 'J99' THEN 1 ELSE 0 END) AS obitos_respiratorio,
    SUM(CASE WHEN CAUSABAS BETWEEN 'V01' AND 'V99' THEN 1 ELSE 0 END) AS obitos_transporte
  FROM '%s'
  WHERE CODMUNRES IS NOT NULL
  GROUP BY cod_mun_res_datasus", caminho_datasus)

# 4. Executar a consulta e trazer o resultado para o R
# O R só receberá o *resultado* (5570 linhas), não o arquivo original.
obitos_agregados_mun <- dbGetQuery(con, consulta)

# 5. Encerrar a conexão
dbDisconnect(con, shutdown = TRUE)


# Parte 3: Carregamento de dados e Limpeza (IBGE População) ----
# -------------------------------------------------------------------
# Os dados de população vêm em um .xlsx "sujo" (padrão SIDRA).
pop_ibge_2024_bruto <- read_excel(
  caminho_ibge,
  skip = 4, # Pula as 4 linhas de cabeçalho
  col_names = c("MUN", "cod_ibge_7d", "nome_municipio", "pop2024")
)


# Parte 4: O Desafio da Integração (Chave 6 vs 7 dígitos) ----
# -------------------------------------------------------------------
# O DATASUS usa código de 6 dígitos (CODMUNRES).
# O IBGE (SIDRA) usa código de 7 dígitos (com dígito verificador).
# Precisamos criar uma chave compatível.
# Obtido de http://sidra.ibge.gov.br/tabela/6579
pop_ibge_2024_limpo <- pop_ibge_2024_bruto |>
  # Cria o código de 6 dígitos removendo o último caractere
  mutate(
    cod_mun_6d = as.numeric(str_sub(cod_ibge_7d, start = 1, end = -2))
  )


# Parte 5: Integração (Join) e Feature Engineering (Taxas) ----
# -------------------------------------------------------------------

dados_taxas <- pop_ibge_2024_limpo |>
  # 1. INTEGRAÇÃO: Juntar IBGE + DATASUS pela chave de 6 dígitos
  left_join(
    obitos_agregados_mun,
    by = c("cod_mun_6d" = "cod_mun_res_datasus")
  ) |>
  # 2. LIMPEZA PÓS-JOIN: Municípios sem óbitos (do join) ficam com NA.
  # Vamos substituir NAs de contagem por 0.
  mutate(
    across(starts_with("obitos_"), ~ if_else(is.na(.), 0, .))
  ) |>
  # 3. FEATURE ENGINEERING: Cálculo das taxas (por 100.000 habitantes)
  mutate(
    taxa_mort_geral = (total_obitos / pop2024) * 100000,
    taxa_mort_neoplasias = (obitos_neoplasias / pop2024) * 100000,
    taxa_mort_circulatorio = (obitos_circulatorio / pop2024) * 100000,
    taxa_mort_respiratorio = (obitos_respiratorio / pop2024) * 100000,
    taxa_mort_transporte = (obitos_transporte / pop2024) * 100000
  ) |>
  # 4. LIMPEZA (TEXTO): Extrair a UF do nome
  tidyr::separate(
    nome_municipio,
    into = c("municipio", "uf"),
    sep = " \\(", # Separador é " ("
    remove = FALSE # Mantém a coluna original
  ) |>
  mutate(
    uf = str_remove(uf, "\\)") # Remove o ")" final
  ) |>
  # 5. SELEÇÃO FINAL: Manter apenas as colunas úteis
  select(
    cod_ibge_7d,
    cod_mun_6d,
    municipio,
    uf,
    pop2024,
    starts_with("taxa_") # Pega todas as colunas de taxas
  ) |>
  # 6. FILTRAGEM: Remove NAs (municípios que podem ter falhado no join)
  drop_na(taxa_mort_geral)


# Parte 6: Diagnóstico (AED) ----
# -------------------------------------------------------------------
# Agora que os dados estão limpos e integrados, vamos explorá-los.
summary(dados_taxas)
skim(dados_taxas)

# Vamos visualizar as correlações e distribuições
# ggpairs(dados_taxas |> select(starts_with("taxa_"))) # Demora muito para executar


# Parte 7: Padronização (Z-Score) via `recipes` ----
# -------------------------------------------------------------------
# Usamos `recipes` para:
# 1. Definir a "receita" (os passos de pré-processamento).
# 2. "Preparar" (prep) a receita (calcular média e devio padrão de 2024).
# 3. "Aplicar" (bake) a receita (usar os parâmetros de 2024
#    para transformar 2024 e qualquer dado futuro).
#

# 1. Definir a Receita
receita_padronizacao <- recipe(dados_taxas) |>
  
  # Identifica quais colunas são IDs (não devem ser padronizadas)
  update_role(
    cod_ibge_7d, cod_mun_6d, municipio, uf, pop2024,
    new_role = "ID"
  ) |>
  
  # Aplica a padronização Z-score (média 0, var 1)
  # em todas as colunas numéricas que NÃO são "ID".
  step_normalize(all_numeric_predictors())

# 2. Preparar a Receita
# 'prep()' calcula a média e o SD dos dados de treinamento
receita_preparada <- prep(receita_padronizacao, training = dados_taxas)

# 3. Aplicar a Receita
# 'bake()' aplica os parâmetros calculados no 'prep()'
dados_padronizados_final <- bake(receita_preparada, new_data = dados_taxas)

# 4. Verificar o Resultado
glimpse(dados_padronizados_final)

# Verificando se as taxas estão com média 0 e variância 1
dados_padronizados_final |>
  select(starts_with("taxa_")) |>
  skim()

