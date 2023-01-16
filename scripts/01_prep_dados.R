# Bibliotecas ----
{
  library(tidyverse)
  library(sf)
  library(png)
  library(ggnewscale)
  library(patchwork)
}

# Leitura dos arquivos ----

crsutm <- 32723
crsutm_sirgas <- 31983
crsdegrees <- 4326

## grid - base para acessibilidade e população ----
grid <- read_sf('data/shp/grid_hex_MSP_short.shp', crs=crsutm)

### acessibilidade ----
acc17 <-  read_delim('data/acc_2017.csv', delim=';') %>% 
  select(-c(acc, threshold, accessibility, year)) %>% 
  left_join(grid, by = c('origin'='id')) %>% 
  st_as_sf()

brks <- quantile(acc17$perc, probs = seq(0,1,0.2), names = F)

acc17 <- acc17 %>% 
  mutate(acc_range =  as.numeric(cut(perc,
                                     breaks = brks,
                                     labels = 1:5,
                                     include.lowest=T,
                                     right=F))-.5)

write_sf(acc17, 'data/nt_pde_access/grid_acessibilidade17.shp')

### população ----
pop <- read_csv('data/population_micro_grid2.csv') %>% 
  mutate(predom = case_when(g1 > g2 & g1 > g3 ~ 'g1',
                            g2 > g1 & g2 > g3 ~ 'g2',
                            T ~ 'g3')) %>% 
  left_join(grid, by = c('id_grid'='id')) %>% 
  st_as_sf()

write_sf(pop, 'data/nt_pde_access/grid_familias_por_renda_familiar.shp')

## ZEIS ----
# arquivos obtidos pelo Geosampa e tratados 

zeis02 <- read_sf('data/ZEIS/zeis_2002_valid.shp') %>% 
  mutate(zeis = as.numeric(str_extract(Layer, '\\d')),
         LPUOS = 2004) %>% 
  select(zeis, LPUOS)  

zeis14 <- read_sf('data/ZEIS/zeis_2014_valid.shp')%>% 
  mutate(zeis = as.numeric(str_extract(ZONA,'\\d')),
         LPUOS = 2016) %>% 
  select(zeis, LPUOS)

zeis <- rbind(zeis02, zeis14) %>% filter(!is.na(zeis))

write_sf(zeis, 'data/nt_pde_access/zeis.shp')

## EETU ----
# arquivos disponíveis no Geosampa

# eixos de estruturação - proposto existente
ex_iex <- sf::read_sf('data/eixos/sirgas_PDE_3-Eixos-EETU.shp',crs=crsutm_sirgas) %>% 
  st_transform(crs=crsutm) %>% 
  mutate(infraestrutura = 'existente',
         var = factor('propostos', levels = c('propostos','ativados'))) %>% 
  select(infraestrutura, var)

# eixos de estruturação - proposto previsto
ex_iprev <- sf::read_sf('data/eixos/sirgas_PDE_3A-EixosPrevistos.shp', crs=crsutm_sirgas) %>% 
  st_transform(crs=crsutm) %>% 
  mutate(infraestrutura = 'prevista',
         var = factor('propostos', levels = c('propostos','ativados')))%>% 
  select(infraestrutura, var)

# eixos de estruturação - ativos
eixos_ativos <- sf::read_sf('data/eixos/SIRGAS_SHP_eixoativodecreto_polygon.shp', crs=crsutm_sirgas) %>% 
  st_transform(crs=crsutm) %>% 
  mutate(infraestrutura = 'prevista e ativada',
         var = factor('ativados', levels = c('propostos','ativados')))%>% 
  select(infraestrutura, var)

ex_propostos <- rbind(ex_iex, ex_iprev) 
ex_propostos$area <- units::drop_units(sf::st_area(ex_propostos))

eixos <- rbind(select(ex_propostos,-area), eixos_ativos)

write_sf(eixos, 'data/nt_pde_access/eetu.shp')

# a partir do quadro 1 https://gestaourbana.prefeitura.sp.gov.br/arquivos/PDE_lei_final_aprovada/QUADROS/PDF/PDE2013_SUBST2_Quadro_1_CA_Definicoes.pdf
# Habitação de Interesse Social – no máximo um sanitário e uma vaga de garagem
## HIS 1: até 3 SM - MCMV valor máx 96k 2020
## HIS 2: 3 a 6 SM - MCMV valor máx 240k 2020  
# Habitação de Mercado Popular – até dois sanitários e até uma vaga de garagem
## entre 6 a 10 SM - MCMV valor máx 300k 2020  

# referência do IGP-DI para correção (o mesmo usado em Hoyler, 2016)
igpdi <- read_delim('data/igp_di_12-2013_07-2021.csv', 
                    delim = ';', col_types = "cd",
                    locale=locale(decimal_mark = ",")) %>%
  mutate(date = lubridate::parse_date_time(Data, 
                                           orders = 'dmy'),
         acum = lag(cumsum(`IGP-DI`)),
         acum = if_else(is.na(acum), 0, acum),
         total = sum(`IGP-DI`),
         corr = 1+(total - acum)/100) %>% 
  select(date, corr)

# ano de referência (base é a partir de 1985)
ano <- 2008

private <- sf::read_sf('data/lancamentos/residencial/EMPRES_MSP_85_18_CEM_V2.shp') %>% 
  st_transform(crs=crsutm) %>% 
  rename_all(tolower) %>% 
  filter(ano_lan>=ano, !str_detect(nome_emp, 'HOTEL')) %>% # a partir de 2014 e não seja hotel
  select(banh_uni, gar_unid, p_tt_atu, p_tt_un, tt_unid, mes_lan, ano_lan, ar_ut_un, ar_tt_un, nome_emp) 

private$date <- as.Date(as.numeric(private$mes_lan), origin = "1900-01-01")
lubridate::day(private$date) <- 1

private <- left_join(private, igpdi) %>% 
  mutate(p_tt_atu = p_tt_un*corr,
         classe = case_when(banh_uni <= 1 & gar_unid <= 1 & ( # em todos os períodos tem esses parametros
           (ano_lan  <= 2011 & p_tt_un <= 52000 & ar_ut_un <= 50) | # fase 1
             ((ano_lan > 2011 & ano_lan  <= 2015) & p_tt_un <= 76000 & ar_ut_un <= 50) | # fase 2
             (ano_lan  > 2015 & p_tt_un <= 96000 & ar_ut_un <= 70) # fase 3
         ) ~ 'HIS',
         banh_uni <= 2 & gar_unid <= 1 & ar_ut_un <= 70 & ( # em todos os períodos tem esses parametros
           (ano_lan <= 2011 & (p_tt_un > 52000 & p_tt_un <= 130000)) | # fase 1
             ((ano_lan > 2011 & ano_lan  <= 2015) & (p_tt_un > 76000 & p_tt_un <= 190000)) | # fase 2
             (ano_lan  > 2015 & (p_tt_un > 96000 & p_tt_un <= 225000)) # fase 3
         ) ~ 'HMP',
         T ~ 'alto padrão'))

# interseção dos eixos com a base de lançamentos
private_eixos <- eixos %>% 
  filter(infraestrutura != 'prevista') %>% 
  sf::st_intersection(acc17) %>% 
  sf::st_intersection(private) %>% 
  sf::st_drop_geometry()

write_csv(private_eixos, 'data/nt_pde_access/lancamentos_eetus.csv')

# cálculo das áreas das ZEIS em km2
zeis$area <- units::drop_units(st_area(zeis))/10^6

# interseção entre zeis e o grid
zeis_acc <- st_intersection(zeis, acc17)
zeis_acc$area <- units::drop_units(st_area(zeis_acc))

# dos lançamentos, quais estão nas ZEIS de vazios?
priv_zeis <- st_intersection(private, filter(zeis_acc, zeis %in% c(2,3) & LPUOS == 2016)) %>% 
  st_drop_geometry()

write_csv(priv_zeis, 'data/nt_pde_access/lancamentos_zeis.csv')
