# Bibliotecas ----
{
    library(tidyverse)
    library(sf)
    library(png)
    library(ggnewscale)
    library(patchwork)
}

# Configurações dos mapas e gráficos ----
crsutm <- 32723
crsutm_sirgas <- 31983
crsdegrees <- 4326

## tamanho padrão  ----
base <- 210 # largura das imagens
# tamanho da fonte e da linha serão proporcionais ao tamanho da figura
font.size <- base/ggplot2::.pt*1.414286 # multiplicado pela relação entre a altura e largura de uma A4
line.size <- base/ggplot2::.stroke*1.414286

# limites dos mapas
coord_lim_short <- coord_sf(xlim = c(-46.826081, -46.365377), 
                            ylim = c(-23.716492, -23.384091),
                            crs = crsdegrees)

## opções de fundo dos mapas ----
basemap <- png::readPNG('data/basemap_short_esri_light_gray_12.png')
ggbasemap_short <- grid::rasterGrob(basemap, interpolate = T)
basemap <- png::readPNG('data/basemap_esri_light_gray_12.png')
ggbasemap <- grid::rasterGrob(basemap, interpolate = T)
rm(basemap)
gc()

## temas ----
theme_chart <- theme_bw() +
  theme(panel.border = element_blank(),
        panel.spacing = unit(2, 'mm'),
        # axis
        axis.line = element_line(color='grey30'),
        axis.ticks = element_line(color='grey30'),
        axis.text = element_text(size = font.size*0.1),
        axis.title = element_text(size = font.size*0.12),
        # legend
        legend.position = 'bottom',
        legend.text = element_text(size=font.size*0.1),
        legend.title = element_text(size=font.size*0.12),
        legend.spacing.x = unit(.01,"cm"),
        # strip
        strip.background = element_blank(),
        strip.text = element_text(size = font.size*0.12),
        # margin
        plot.margin = margin(2, 2, 2, 2, 'mm'))


theme_map <- theme(
  # axis
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  # facet grid
  panel.grid = element_line(color="#CFCED3"),
  panel.spacing = unit(2,'mm'),
  # legend
  legend.position = "bottom",
  legend.box = "horizontal",
  legend.direction = "horizontal",
  legend.spacing = unit(base*0.1, "mm"),
  legend.text = element_text(size=font.size*0.1),
  legend.title = element_text(size=font.size*0.12),
  # facet
  strip.background = element_blank(), 
  strip.text = element_text(size = font.size*0.12),
  # background
  panel.background = element_rect(fill="#CFCED3"),
  plot.background = element_blank(),
  # caption
  plot.caption = element_text(size = font.size*0.08,
                              hjust = 1),
  # margin
  plot.margin = margin(1, 1, 1, 1, 'mm'))


# Carregando os dados ----

## máscaras ----
muni <- read_sf("data/shp/Outros_muni-MSP.shp", quiet=T, crs=crsutm) %>% 
  sf::st_transform(crs = crsdegrees) 

## ZEIS ----

zeis <- read_sf('data/nt_pde_access/zeis.shp')

## EETU ----

eixos <- read_sf('data/nt_pde_access/eetu.shp')

## acessibilidade ----
acc17 <- read_sf('data/nt_pde_access/grid_acessibilidade17.shp')

## população ----
pop <- read_sf('data/nt_pde_access/grid_familias_por_renda_familiar.shp')

## lançamentos dos empreendimentos ----
# disponibilizado pela Embraesp e organizado pelo CEM

# a partir do quadro 1 https://gestaourbana.prefeitura.sp.gov.br/arquivos/PDE_lei_final_aprovada/QUADROS/PDF/PDE2013_SUBST2_Quadro_1_CA_Definicoes.pdf
# Habitação de Interesse Social – no máximo um sanitário e uma vaga de garagem
## HIS 1: até 3 SM - MCMV valor máx 96k 2020
## HIS 2: 3 a 6 SM - MCMV valor máx 240k 2020  
# Habitação de Mercado Popular – até dois sanitários e até uma vaga de garagem
## entre 6 a 10 SM - MCMV valor máx 300k 2020  

# interseção entre EETU e os empreendimentos
emp_eixos <- read_csv('data/nt_pde_access/lancamentos_eetus.csv')

# interseção entre ZEIS e os empreendimentos
emp_zeis <- read_csv('data/nt_pde_access/lancamentos_zeis.csv')

# Figura 1: Predominância de famílias por classe e acessibilidade a empregos----

## preparação dos dados ----

df1 <- pop %>% 
  mutate(var = factor('famílias', levels = c('famílias','acessibilidade'),
                      labels = c('famílias','acessibilidade'))) %>% 
  rename(value = predom,
         id = id_grid) %>% 
  select(-c(g1, g2, g3)) %>% 
  group_by(value, var) %>% 
  summarise()

brks <- quantile(acc17$perc, probs = seq(0,1,0.2), names = F)

df2 <- acc17 %>% 
  mutate(var = factor('acessibilidade', levels = c('famílias','acessibilidade'),
                      labels = c('famílias','acessibilidade'))) %>% 
  rename(value = acc_range,
         id = origin) %>% 
  group_by(value,var) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(value = as.numeric(as.character(cut(value, 
                                             breaks = 0:5,
                                             labels = brks[2:6]*100-.5))))
## plot do mapa ----
p <- df1 %>% 
  ggplot() +
  # bg
  annotation_custom(ggbasemap_short, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # pop
  geom_sf(data = df1, aes(fill = value), color = NA) +
  viridis::scale_fill_viridis('predominante',
                              labels = c('baixa\nrenda', 'média\nrenda', 'alta\nrenda'),
                              option='C', 
                              discrete = T,
                              begin = 0, end = .85,
                              guide = guide_legend(title.position = 'top',
                                                   order = 1))+
  new_scale_fill()+
  # acessibilidade
  geom_sf(data = df2, aes(fill = value), color = NA) +
  scale_fill_stepsn('% de postos de trabalho',
                    colors = viridis::viridis(5),
                    values = round(brks*100,0)/100,
                    breaks = round(brks*100,0),
                    limits = round(brks[c(1,6)]*100,0),
                    guide = guide_colorsteps(show.limits=T,
                                             title.position = 'top',
                                             order = 2))+
  # máscaras
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
  facet_grid(.~var)+
  labs(title = 'Predominância de famílias por classe e acessibilidade a empregos') +
  coord_lim_short +
  theme_map 

ggsave('figuras/familias_acessibilidade.png', 
       p, dpi = 500, width = 7, height = 4, 
       units = "in", type = 'cairo-png')

p

# Figura 2: EETU propostos no PDE e ativados ----

## plot do mapa ----
p <- ggplot() +
  # bg
  annotation_custom(ggbasemap_short, xmin=-Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  # EETU propostos
  geom_sf(data = eixos, aes(fill = infrstr), color = NA) +
  scale_fill_manual('infraestrutura',
                    values = c("#0D0887FF", "#FEBA2CFF", "#D6556DFF"),
                    labels = c('existente', 'prevista', 'prevista e ativada'),
                    guide = guide_legend(title.position = 'top',
                                         order = 1))+
  #base maps
  geom_sf(data=muni, fill = 'grey40', color = 'transparent', alpha = 0.25) +
  labs(title = 'Localização dos EETUs') +
  coord_lim_short +
  theme_map 

ggsave('figuras/EETUs.png', 
       p, dpi = 500, width = 7, height = 4, 
       units = "in", type = 'cairo-png')

p

# Figura 3: # empreendimentos que atenderam o mínimo de UH ----

## preparação dos dados ----

# os empreendimentos atingiram o mínimo?
aux <- emp_eixos %>% 
  mutate(min = round(tt_unid*ar_tt_un/(80),0),
         min_round = round(min/10,0)*10,
         dif = tt_unid/min-1,
         dif_round = tt_unid/min_round-1,
         bate = dif>=0,
         bate_round = dif_round>=0)%>%
  select(bate, min, dif, tt_unid, 
         ano_lan, ar_ut_un) %>% 
  group_by(bate, ano_lan) %>% 
  summarise(n=n()) %>% 
  mutate(n=if_else(bate, n, -n)) 

p <- aux %>% 
  ggplot() +
  geom_bar(aes(ano_lan, n, fill = bate), stat = 'identity') +
  geom_text(data = subset(aux, n > 0), aes(ano_lan, n, label = n), vjust = -.2)+
  geom_text(data = subset(aux, n < 0), aes(ano_lan, n, label = -n), vjust = 1)+
  geom_vline(aes(xintercept=2014), color = 'grey50', linetype = 'dashed')+
  geom_text(aes(2013.75, y = 32),
            label = 'PDE',
            size = font.size*0.03,
            color = 'grey50',
            angle = 90,
            hjust = 1) +
  geom_vline(aes(xintercept=2016), color = 'grey50', linetype = 'dashed')+
  geom_text(aes(2015.75, y = 32),
            size = font.size*0.03,
            label = 'LPUOS',
            color = 'grey50',
            angle = 90,
            hjust = 1) +
  scale_fill_manual('',
                    values = c("#D6556DFF", "#0D0887FF"),
                    labels = c('não atende o mínimo', 'atende o mínimo')) +
  scale_x_continuous(breaks = 2008:2018)+
  scale_y_continuous(labels = rev(c(25, 0 , 25, 50, 75)))+
  labs(x='', y = 'número de empreendimentos',
       title = 'Empreendimentos que atenderiam ou não o mínimo de UHs esperado')+
  coord_cartesian(ylim = c(-80, 35), expand = F)+
  theme_chart

ggsave('figuras/EETUs_areaminima.png', 
       p, dpi = 500, width = 7, height = 4, 
       units = "in", type = 'cairo-png')

p

# Figura 7: Empreendimentos lançados em perímetros das ZEIS ----

## preparação dos dados ----

aux <- emp_zeis %>% 
  group_by(ano_lan, classe) %>% 
  summarise(n = n(),
            uhs = sum(tt_unid, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(ano_lan) %>% 
  mutate(n_ano = sum(n, na.rm=T),
         perc = n/n_ano) %>% 
  ungroup() 

## plot do gráfico ----

p <- aux %>% 
  ggplot(aes(ano_lan, n))+
  geom_col(aes(fill = classe)) +
  geom_text(aes(label = scales::percent(perc, accuracy = 1), group = classe), 
            color = 'grey90',
            size = font.size/35,
            position = position_stack(vjust = 0.5)) +
  geom_text(aes(y=n_ano, label = n_ano), vjust = -0.2) +
  geom_vline(aes(xintercept=2014), color = 'grey50', linetype = 'dashed')+
  geom_text(aes(2013.75, y = 44),
            label = 'PDE',
            size = font.size*0.03,
            color = 'grey50',
            angle = 90,
            hjust = 1) +
  geom_vline(aes(xintercept=2016), color = 'grey50', linetype = 'dashed')+
  geom_text(aes(2015.75, y = 44),
            size = font.size*0.03,
            label = 'LPUOS',
            color = 'grey50',
            angle = 90,
            hjust = 1) +
  scale_fill_manual('',
                    values = c("#D6556DFF", "#0D0887FF"),
                    labels = c('alto padrão', 'EHMP'))+
  scale_x_continuous(breaks = 2008:2018)+
  scale_y_continuous(name = 'número de empreendimentos') +
  labs(x='',
       title = 'Empreendimentos construídos dentro do perímetro das ZEIS')+
  coord_cartesian(ylim = c(0, 45), expand = F)+
  theme_chart

ggsave('figuras/ZEIS_uso.png', 
       p, dpi = 500, width = 7, height = 4, 
       units = "in", type = 'cairo-png')

p
