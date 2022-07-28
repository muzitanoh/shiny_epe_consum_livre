

source("util.R", encoding = "UTF-8")


#### Coleta dos dados SQL ####

con <- dbConnect(RSQLite::SQLite(), "dados/consumidores-livres.sqlite")


dim_cnae_ramo_atividade <- tbl(con, "dim_cnae_ramo_atividade") %>% collect()

# para funcionar a conexão deve estar aberta
faz_consumo_mensal_por_setor_epe <- function(cadastro_unidades, mensal_unidade, ano_base){
  
  cadastro <- tbl(con, cadastro_unidades) %>% 
    select(
      id_arquivo, numero_unidade, cnae
    )
  
  consumo <- tbl(con, mensal_unidade) %>%
    select(
      id_arquivo, numero_unidade, data, consumo_total_na_rede
    ) %>%
    left_join(
      cadastro,
      by = c("id_arquivo", "numero_unidade")
    ) %>%
    filter(
      !is.na(consumo_total_na_rede)
    ) %>%
    collect() %>%
    mutate(
      cnae4 = str_remove_all(cnae, pattern = "[.]"),
      cnae4 = str_sub(cnae4, 1, 4)
    ) %>%
    left_join(
      dim_cnae_ramo_atividade,
      by = "cnae4"
    ) %>%
    mutate(
      data = as_date(data),
      ano = year(data),
      mes = month(data)
    ) %>%
    filter(
      ano == ano_base
    ) %>%
    group_by(
      ano, mes, setor
    ) %>%
    summarise(
      consumo_total = sum(consumo_total_na_rede, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      setor = if_else(is.na(setor), "DESCONHECIDO", setor)
    )

  return(consumo)
  
}


consumo_forms_2018_epe <- faz_consumo_mensal_por_setor_epe("cadastro_unidades_epe_2018", "mensal_unidade_epe_2018", 2017)
consumo_forms_2019_epe <- faz_consumo_mensal_por_setor_epe("cadastro_unidades_epe_2019", "mensal_unidade_epe_2019", 2018)
# consumo_forms_2020_epe <- faz_consumo_mensal_por_setor_epe("cadastro_unidades_epe_2020", "mensal_unidade_epe_2020", 2019)

consumo_ccee <- tbl(con, "consumo_ccee") %>%
  collect() %>% 
  mutate(
    data = as_date(data),
    ano = year(data),
    mes = month(data)
  ) %>% 
  rename(
    setor = ramo_atividade
  )


dbDisconnect(con)



consumo_consolidado_mensal_epe <- bind_rows(consumo_forms_2018_epe, consumo_forms_2019_epe) %>% 
  rename(
    consumo_epe = consumo_total
  )

comparacao_ccee_epe <- consumo_ccee %>%
  group_by(
    ano, mes, setor
  ) %>%
  summarise(
    consumo_ccee = sum(consumo, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  inner_join(
    consumo_consolidado_mensal_epe, by = c("ano", "mes", "setor")
  ) 







