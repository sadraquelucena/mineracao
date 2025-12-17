################################################################################
#  Código para baixar dados do Projeto 5                                       #
#  Mini Projeto P2: Predição de Baixo Peso ao Nascer                           #                                                                             #
################################################################################

library(microdatasus)

# Download
dados_chik <- fetch_datasus(
  year_start = 2024,
  year_end = 2024,
  information_system = "SINAN-CHIKUNGUNYA"
)

# Lista de UFs por região
ufs_norte <- c("11", "12", "13", "14", "15", "16", "17") # 11=RO, 12=AC, 13=AM, 14=RR, 15=PA, 16=AP, 17=TO
ufs_nordeste <- c("21", "22", "23", "24", "25", "26", "27", "28", "29") # 21=MA, 22=PI, 23=CE, 24=RN, 25=PB, 26=PE, 27=AL, 28=SE, 29=BA
ufs_sudeste <- c("31", "32", "33", "35") # 31=MG, 32=ES, 33=RJ, 35=SP
ufs_sul <- c("41", "42", "43") # 41=PR, 42=SC, 43=RS
ufs_centro_oeste <- c("50", "51", "52", "53") # 50=MS, 51=MT, 52=GO, 53=DF


dados_chik <- dados_chik |>
  dplyr::filter(SG_UF_NOT %in% ufs_nordeste) |> # substitua pela região escolhida
  dplyr::filter(CLASSI_FIN == 13) |>  # casos de chikungunya confirmados
  dplyr::mutate(
    # (1=Sim, 0=Não)
    houve_hospitalizacao = case_when(
      HOSPITALIZ == "1" ~ 1,
      TRUE ~ 0 # Assume não hospitalizado se 2 ou 9 (Ignorado)
    )
  )
