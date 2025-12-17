################################################################################
#  Código para baixar dados do Projeto 5                                       #
#  Mini Projeto P1: Predição de Baixo Peso ao Nascer                           #                                                                             #
################################################################################

library(microdatasus)

# Lista de UFs por região
ufs_norte <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
ufs_nordeste <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
ufs_sudeste <- c("ES", "MG", "RJ", "SP")
ufs_centro_oeste <- c("DF", "GO", "MT", "MS")
ufs_sul <- c("PR", "RS", "SC")

# Download
dados_nordeste <- fetch_datasus(
  year_start = 2024,
  year_end = 2024,
  uf = ufs_nordeste, # substitua pela região escolhida
  information_system = "SINASC"
)
