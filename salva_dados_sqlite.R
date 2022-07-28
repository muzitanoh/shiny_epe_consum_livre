library(DBI)
library(janitor)
library(magrittr)
library(lubridate)
library(tidyverse)
library(stringr)
library(readxl)
library(dbplyr, warn.conflicts = FALSE)


#### Leitura e tratamendo p/ o SQL ####

dim_cnae_ramo_atividade <- read_xlsx(path = "dados/cnae/CNAE para CCEE.xlsx") %>%
  rename(col2 = 2) %>% 
  filter(
    !is.na(col2)
  ) %>% 
  row_to_names(
    row_number = 1
  ) %>% 
  clean_names() %>%
  select(
    cnae = matches("codigo"),
    setor = matches("ccee")
  ) %>% 
  mutate(
    cnae4 = str_remove_all(cnae, pattern = "[.]"),
    cnae4 = str_sub(cnae4, 1, 4)
  )

# write_rds(
#   categorias,
#   "dados/dim_cnae_ramo_atividade.rds"
# )


cadastro_unidades_2018 <- read_rds("dados/ciclo2018/cadastro_unidades.rds")
cadastro_unidades_2019 <- read_rds("dados/ciclo2019/cadastro_unidades.rds")
cadastro_unidades_2020 <- read_rds("dados/ciclo2020/cadastro_unidades.rds")

mensal_unidade_2018 <- read_rds("dados/ciclo2018/mensal_unidades.rds")
mensal_unidade_2019 <- read_rds("dados/ciclo2019/mensal_unidades.rds") %>% 
  rename(consumo_total_na_rede = consumo_da_rede) %>% 
  mutate_at(
    vars(consumo_total_na_rede), as.numeric
  )
mensal_unidade_2020 <- read_rds("dados/ciclo2020/mensais_unidades.rds")



#CCEE
consumo_ccee_2017_e_2018 <- read_rds("dados/ccee/consumo_ccee_com_cnae_e_ramo.rds") %>% 
  select(consumo, data, ramo_atividade, empresa = nome_ccee)
consumo_ccee_2019 <- read_rds("dados/ccee/consumo_ccee_2019.rds") %>% 
  select(consumo, data, ramo_atividade, empresa)

consumo_ccee <- bind_rows(consumo_ccee_2017_e_2018, consumo_ccee_2019)



#### Criação do Banco de Dados SQL ####

con <- dbConnect(RSQLite::SQLite(), "dados/consumidores-livres.sqlite")


dbWriteTable(con, "dim_cnae_ramo_atividade", dim_cnae_ramo_atividade, overwrite = TRUE)

dbWriteTable(con, "cadastro_unidades_epe_2018", cadastro_unidades_2018, overwrite = TRUE)
dbWriteTable(con, "mensal_unidade_epe_2018", mensal_unidade_2018, overwrite = TRUE)

dbWriteTable(con, "cadastro_unidades_epe_2019", cadastro_unidades_2019, overwrite = TRUE)
dbWriteTable(con, "mensal_unidade_epe_2019", mensal_unidade_2019, overwrite = TRUE)

dbWriteTable(con, "cadastro_unidades_epe_2020", cadastro_unidades_2020, overwrite = TRUE)
dbWriteTable(con, "mensal_unidade_epe_2020", mensal_unidade_2020, overwrite = TRUE)

dbWriteTable(con, "consumo_ccee", consumo_ccee, overwrite = TRUE)


dbListTables(con)


dbDisconnect(con)



