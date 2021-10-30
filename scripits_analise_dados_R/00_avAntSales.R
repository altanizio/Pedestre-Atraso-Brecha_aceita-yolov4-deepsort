# Análise da Av. Ant Sales - 01/04/2019 13:00:04 - 

# Biblotecas
library(tidyverse)
library(lubridate)
library(plotly)
library(fitdistrplus) 
library(factoextra)
library(ggforce)
library(concaveman)
library(scales)

# Leitura dos dados
avAntSales_00_30_atraso <- read_csv('../outputs/avAntSales/avAntSales_00_30/atrasos.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 13:00:04') + seconds(time_start))
avAntSales_30_60_atraso <- read_csv('../outputs/avAntSales/avAntSales_30_60/atrasos.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 13:30:06') + seconds(time_start))
avAntSales_60_90_atraso <- read_csv('../outputs/avAntSales/avAntSales_60_90/atrasos.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 14:00:10') + seconds(time_start))
avAntSales_90_120_atraso <- read_csv('../outputs/avAntSales/avAntSales_90_120/atrasos.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 14:30:14') + seconds(time_start))
avAntSales_120_150_atraso <- read_csv('../outputs/avAntSales/avAntSales_120_150/atrasos.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 15:00:14') + seconds(time_start))
avAntSales_150_180_atraso <- read_csv('../outputs/avAntSales/avAntSales_150_180/atrasos.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 15:30:14') + seconds(time_start))

atrasos <- bind_rows(avAntSales_00_30_atraso,avAntSales_30_60_atraso,avAntSales_60_90_atraso,
                    avAntSales_90_120_atraso,avAntSales_120_150_atraso,avAntSales_150_180_atraso)
remove(avAntSales_00_30_atraso,avAntSales_30_60_atraso,avAntSales_60_90_atraso,
          avAntSales_90_120_atraso,avAntSales_120_150_atraso,avAntSales_150_180_atraso)

avAntSales_00_30_carros <- read_csv('../outputs/avAntSales/avAntSales_00_30/carros.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 13:00:04') + seconds(time_start))
avAntSales_30_60_carros <- read_csv('../outputs/avAntSales/avAntSales_30_60/carros.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 13:30:06') + seconds(time_start))
avAntSales_60_90_carros <- read_csv('../outputs/avAntSales/avAntSales_60_90/carros.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 14:00:10') + seconds(time_start))
avAntSales_90_120_carros <- read_csv('../outputs/avAntSales/avAntSales_90_120/carros.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 14:30:14') + seconds(time_start))
avAntSales_120_150_carros <- read_csv('../outputs/avAntSales/avAntSales_120_150/carros.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 15:00:14') + seconds(time_start))
avAntSales_150_180_carros <- read_csv('../outputs/avAntSales/avAntSales_150_180/carros.csv') %>% mutate(tempo_real = dmy_hms('01/04/2019 15:30:14') + seconds(time_start))

carros <- bind_rows(avAntSales_00_30_carros,avAntSales_30_60_carros,avAntSales_60_90_carros,
                    avAntSales_90_120_carros,avAntSales_120_150_carros,avAntSales_150_180_carros)
remove(avAntSales_00_30_carros,avAntSales_30_60_carros,avAntSales_60_90_carros,
  avAntSales_90_120_carros,avAntSales_120_150_carros,avAntSales_150_180_carros)

# Tratamento
primeiro_quartil  <-  quantile(atrasos$time, probs = 0.25, na.rm = TRUE)
atrasos <- atrasos %>% mutate(atraso = case_when(
  (inicio_linha_1 != inicio_linha_2) & (!is.na(time)) ~ NA_real_ ,
  (!is.na(time)) ~ time,
  TRUE ~ NA_real_
), atraso_trat = case_when(
  (inicio_linha_1 != inicio_linha_2) & (!is.na(time)) ~ as.numeric(primeiro_quartil) ,
  (!is.na(time)) ~ time,
  TRUE ~ NA_real_
))


table(is.na(atrasos$atraso))
table(is.na(atrasos$atraso_trat))

atrasos <- atrasos %>% mutate(atraso_trat = atraso)

carros <- carros %>% filter(!is.na(classe))

#Analises
carros_only <- carros %>% filter(classe != 'bicycle') %>% mutate(diff = tempo_real - lag(tempo_real)) %>% 
  mutate(diff = ifelse(is.na(diff) | diff < 0,quantile(diff, probs = 0.25, na.rm = TRUE),diff))

brechas <- as.numeric(carros_only$diff)
g <- 128-66#mean(brechas[brechas>quantile(brechas, probs = 0.99, na.rm = TRUE)])
ciclo <- 128 
gwalk  <-  g + 4
dp_HCM <-  (ciclo - gwalk)^2 / (2*ciclo)

atrasos_only <- atrasos %>% filter(!is.na(atraso_trat))
atrasos_only <- atrasos_only %>% mutate(tempo_real_final = tempo_real + seconds(atraso_trat))

tempo_inicial <- min(c(min(carros_only$tempo_real),min(atrasos$tempo_real)))
tempo_final <-  max(c(max(carros_only$tempo_real),max(atrasos_only$tempo_real_final))) + minutes(15)


fluxo <- carros_only %>% 
  mutate(tempo_real_cut = cut(tempo_real,seq(tempo_inicial,tempo_final,by = '15 min'))) %>%
  group_by(tempo_real_cut,classe) %>% summarise(fluxo = n())

brechas <- carros_only %>% 
  mutate( brecha = lead(diff),
    tempo_real_cut = cut(tempo_real,seq(tempo_inicial,tempo_final,by = '15 min'))) %>%
  group_by(tempo_real_cut) %>% summarise(fluxo_carros = n(),brecha_media = mean(brecha, na.rm = TRUE))


atrasos_only <- atrasos %>% filter(!is.na(atraso_trat))
atrasos_only <- atrasos_only %>% mutate(tempo_real_final = tempo_real + seconds(atraso_trat))

fluxo_atrasos <- atrasos_only %>% 
  mutate(tempo_real_cut = cut(tempo_real_final,seq(tempo_inicial,tempo_final,by = '15 min'))) %>%
  group_by(tempo_real_cut) %>% summarise(fluxo_pedestres = n(), atraso_medio = mean(atraso_trat, na.rm = TRUE))

dados  <-  left_join(brechas,fluxo_atrasos,by = 'tempo_real_cut')
dados  <-  dados[1:12,]

ggplot(dados,aes(x = ymd_hms(tempo_real_cut))) + 
  geom_line(aes(y = (fluxo_carros), color = 'Fluxo_carros')) +
  geom_line(aes(y = (fluxo_pedestres), color = 'fluxo_pedestres'))+
  geom_line(aes(y = (brecha_media), color = 'brecha_media'))+
  geom_line(aes(y = (atraso_medio), color = 'atraso_medio'))

ggplot(dados,aes(x = ymd_hms(tempo_real_cut))) + 
  geom_line(aes(y = (brecha_media), color = 'brecha_media'))+
  geom_line(aes(y = (atraso_medio), color = 'atraso_medio'))

ggplot(dados,aes(x = ymd_hms(tempo_real_cut))) + 
  geom_line(aes(y = (fluxo_carros), color = 'Fluxo_carros'))+
  geom_line(aes(y = (atraso_medio), color = 'atraso_medio'))

grafico <- ggplot() + 
  geom_line(data = carros_only, aes(x = ymd_hms(tempo_real),y = (diff), color = 'Brecha'))+
  geom_point(data = atrasos_only,aes(x = (ymd_hms(tempo_real_final)),y = (atraso_trat), color = 'atraso'))
ggplotly(grafico)


write.csv2(atrasos_only,'avAntSales_atrasos_bruto.csv')
write.csv2(carros_only,'avAntSales_veiculos_bruto.csv')

# Atraso


atrasos_only %>% 
  mutate(tempo_real_cut = cut(tempo_real_final,seq(tempo_inicial,tempo_final,by = '15 min'))) %>%
  group_by(tempo_real_cut) %>% 
  summarise(fluxo_pedestres = n(), 
            atraso_medio = mean(atraso_trat, na.rm = TRUE), 
            inferior = t.test(atraso_trat)$conf.int[1], 
            superior = t.test(atraso_trat)$conf.int[2])  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = atraso_medio)) + geom_col(fill = 'steelblue') +
  xlab('Tempo') + ggtitle('') + ylab('Atraso médio (s) a cada 15 min')  +
  geom_hline(aes(yintercept = dp_HCM, color = 'HCM'), size = 2)+
  geom_errorbar(
    aes(ymin = inferior, 
        ymax = superior), 
    width=200, alpha = 0.8, colour="orange", size=1.3
  )  +  theme_Publication()   + theme(legend.title=element_blank())
#ggsave("graficos/atraso_HCM.png", width = 8, height = 4, dpi=600)

carros_only %>% 
  mutate( brecha = lead(diff),
          tempo_real_cut = cut(tempo_real,seq(tempo_inicial,tempo_final,by = '15 min'))) %>%
  group_by(tempo_real_cut) %>% 
  summarise(fluxo_carros = n(),
            brecha_media = mean(brecha, na.rm = TRUE), 
            inferior = t.test(brecha)$conf.int[1], 
            superior = t.test(brecha)$conf.int[2]) %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = brecha_media)) + geom_col(fill = 'steelblue') +
  xlab('Tempo') + ggtitle('') + ylab('Intervalo de tempo entre veículos (s)\n a cada 15 minutos')  +
  geom_errorbar(
    aes(ymin = inferior, 
        ymax = superior), 
    width=200, alpha = 0.8, colour="orange", size=1.3
  )  +  theme_Publication()   + theme(legend.title=element_blank())
#ggsave("graficos/brecha_media.png", width = 8, height = 4, dpi=600)



dados  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = fluxo_pedestres)) + geom_col(fill = 'steelblue') +
  xlab('Tempo') + ggtitle('') + ylab('Fluxo de pedestre em ped/15min') +  theme_Publication()  
#ggsave("graficos/fluxo_pedestres_cor.png", width = 8, height = 4, dpi=600)

fluxo  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = fluxo)) + geom_col(aes(fill = classe)) +  theme_minimal() +
  xlab('Tempo') + ggtitle('') + ylab('Fluxo de veículos a cada 15 minutos')  + labs(fill='Veículo') 

dados  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = brecha_media)) + geom_col(fill = 'steelblue') +  theme_Publication() +
  xlab('Tempo') + ggtitle('') + ylab('Intervalo de tempo entre veículos (s)\n a cada 15 minutos') 
#ggsave("graficos/IT_carro.png", width = 8, height = 4, dpi=600)

brechas_2 <- carros_only %>% 
  mutate( brecha = lead(diff),
          tempo_real_cut = cut(tempo_real,seq(tempo_inicial,tempo_final,by = '5 min'))) %>%
  group_by(tempo_real_cut) %>% summarise(fluxo_carros = n(),brecha_media = mean(brecha, na.rm = TRUE))

fluxo_atrasos_2 <- atrasos_only %>% 
  mutate(tempo_real_cut = cut(tempo_real_final,seq(tempo_inicial,tempo_final,by = '5 min'))) %>%
  group_by(tempo_real_cut) %>% summarise(fluxo_pedestres = n(), atraso_medio = mean(atraso_trat, na.rm = TRUE))

dados_2  <-  left_join(brechas_2,fluxo_atrasos_2,by = 'tempo_real_cut')
dados_2  <-  dados_2[1:12,]

dados_2  %>%
  ggplot(aes(x = fluxo_carros, y = atraso_medio)) + geom_point(color = 'steelblue',size = 3) +  theme_minimal() +
  xlab('Fluxo carros') + ggtitle('') + ylab('Atraso médio (s)') 


descdist(atrasos_only$atraso_trat, boot = 1000)
fit_gamma <- fitdist(atrasos_only$atraso_trat, "gamma", discrete = F)
fit_lnorm <- fitdist(atrasos_only$atraso_trat, "lnorm", discrete = F)
fit_weibull <- fitdist(atrasos_only$atraso_trat, "weibull", discrete = F)
fit <- fitdist(atrasos_only$atraso_trat, "exp", discrete = F)
summary(fit)
plot(fit, histo = TRUE, demp = TRUE)
denscomp(fit, addlegend=TRUE, demp = TRUE)
gofstat(list(fit,fit_gamma,fit_lnorm,fit_weibull), discrete = F)
gofstat(list(fit,fit_gamma,fit_lnorm,fit_weibull), discrete = T)
denscomp(list(fit,fit_gamma,fit_lnorm,fit_weibull), demp = TRUE)


brechas <-  carros_only %>% dplyr::select(tempo = tempo_real, diff)
brechas <-  brechas %>% mutate(brecha = lead(diff))
brechas_aux <- seq(min(brechas$tempo),max(brechas$tempo),by = '1 sec')
brechas_aux <- tibble(tempo = brechas_aux)
brechas <- brechas %>% mutate(tempo = as.character(tempo))
brechas_aux <- brechas_aux %>% mutate(tempo = as.character(tempo))
brechas_join <- left_join(brechas_aux,brechas, by = 'tempo')
brechas_join <- brechas_join %>% group_by(tempo) %>% summarise(brecha = min(brecha))
brechas_final <-  brechas_join

aux_calc <- 0
for(i in seq(1, length(brechas_join$brecha),1)){
  time  <-  brechas_join$tempo[i]
  valor <-  brechas_join$brecha[i]
  
  if(!is.na(valor)){
    aux_calc <- valor
    brechas_final[i,2]  <- valor
  }else{
    aux_calc = aux_calc# - 1
    if(aux_calc<0){
      brechas_final[i,2]  <- 0.5
    }else{
      brechas_final[i,2]  <- aux_calc
    }
  }
}

atrasos_final  <-  atrasos_only %>% dplyr::select(ID = id,tempo_inicial = tempo_real, tempo_final = tempo_real_final, atraso = atraso_trat)
atrasos_final <- atrasos_final %>% mutate(tempo_final = as.character(tempo_final))
atrasos_final <- left_join(atrasos_final, brechas_final, by = c('tempo_final' = 'tempo'))
atrasos_final <- atrasos_final %>% filter_all(~!is.na(.))

#theme_Publication() +
#  xlab('Tempo') + ggtitle('') + ylab('Headway médio (s) a cada 15 minutos') 
#ggsave("graficos/headway_carro.png", width = 8, height = 4, dpi=600)

atrasos_final  %>%
  ggplot(aes(x = atraso)) + 
  geom_histogram(fill = 'steelblue',breaks = seq(0,80,5)) +
  scale_x_continuous(breaks = seq(0,80,10), minor_breaks = seq(0,80,5)) +
  xlab('Atraso (s)') + ylab('Frequência') +  theme_Publication() 
#ggsave("graficos/histograma_atrasos.png", width = 6, height = 4, dpi=600)

atrasos_final  %>%
  ggplot(aes(y = atraso)) + geom_boxplot(fill = 'steelblue') +
  xlab('') + ylab('Atraso (s)')  +  theme_Publication() 
#ggsave("graficos/boxplot_atrasos.png", width = 4, height = 4, dpi=600)

atrasos_final  %>%
  ggplot(aes(x = brecha)) + geom_histogram(fill = 'steelblue',breaks = seq(0,80,5))+
  scale_x_continuous(breaks = seq(0,80,10), minor_breaks = seq(0,80,5)) +
  xlab('Brecha aceita (s)') + ylab('Frequência') + geom_vline(xintercept = 60, color = 'red', size = 2) +  theme_Publication() 
#ggsave("graficos/histograma_brecha.png", width = 6, height = 4, dpi=600)

atrasos_final  %>%
  ggplot(aes(y = brecha)) + geom_boxplot(fill = 'steelblue')+
  xlab('') + ylab('Brecha aceita (s)') +  theme_Publication() 
#ggsave("graficos/boxplot_brecha.png", width = 4, height = 4, dpi=600)

tempo_final- minutes(15)
tempo_inicial 

quantile(atrasos_final$atraso)
quantile(atrasos_final$brecha)

t.test(atrasos_final$atraso)
t.test(atrasos_final$brecha)

t.test(dados$brecha_media)#headway

max(dados$fluxo_carros)*4 # VPH
max(dados$fluxo_pedestres)*4 # PPH

sum(dados$fluxo_carros)*4 # VPH
max(dados$fluxo_pedestres)*4 # PPH

length(atrasos_final$atraso)

atrasos_final %>% write_csv2('avAntSales_atrasos.csv')


descdist(atrasos_final$brecha, boot = 1000)
fit <- fitdist(atrasos_final$brecha, "exp", discrete = F)
summary(fit)
plot(fit, histo = TRUE, demp = TRUE)
denscomp(fit, addlegend=TRUE, histo = TRUE, demp = TRUE)
gofstat(fit, discrete = F)
gofstat(fit, discrete = T)



fviz_nbclust(atrasos_final %>% dplyr::select(atraso, brecha), kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

set.seed(100)

atrasos_final %>% dplyr::select(atraso, brecha)
km.res=kmeans(atrasos_final %>% dplyr::select(atraso, brecha), 5, iter.max = 10000, nstart = 25)

atrasos_final_cluster_id = bind_cols(atrasos_final , cluster=km.res$cluster)

atrasos_final_cluster= bind_cols(atrasos_final %>% dplyr::select(atraso, brecha), cluster=km.res$cluster)


atrasos_final_cluster %>%
  mutate(cluster = factor(cluster)) %>%
  ggplot(aes(brecha, atraso)) +
  geom_point() +
  scale_x_continuous(name = 'Brecha aceita (s)') + 
  scale_y_continuous(name = 'Atraso na travessia (s)') + 
  theme_bw() +
  scale_x_continuous(name = 'Brecha aceita (s)') + 
  scale_y_continuous(name = 'Atraso na travessia (s)') + 
  theme_bw() +  theme_minimal() + theme(text = element_text(size=20))

atrasos_final_cluster %>%
  mutate(cluster = factor(cluster, label = c('D','B','C','A','E'))) %>%
  ggplot(aes(brecha, atraso)) +
  geom_point(aes(color = cluster,shape = cluster), size = 2) +
  geom_mark_hull(
    concavity = 10,
    expand = 0,
    radius = 0.03,
    aes(fill = cluster)
  ) +
  geom_text(label = 'A', x = 8, y = 15, size = 7)+
  geom_text(label = 'B', x = 8, y = 60, size = 7)+
  geom_text(label = 'C', x = 58, y = 10, size = 7)+
  geom_text(label = 'D', x = 58, y = 40, size = 7)+
  geom_text(label = 'E', x = 40, y = 10, size = 7)+
  scale_x_continuous(name = 'Brecha aceita (s)') + 
  scale_y_continuous(name = 'Atraso na travessia (s)') + 
  geom_vline(xintercept = 60, color = 'red', size = 1) +
  theme_Publication() +
  theme(legend.position = "none")
#theme(legend.title=element_blank())
#ggsave("graficos/chushes_2_sales.png", width = 6, height = 4, dpi=600)

atrasos_final_cluster %>% 
  group_by(cluster) %>% 
  summarise(freq = n(), atraso_medio = mean(atraso), brecha_media = mean(brecha)) %>% 
  mutate(prop = freq/sum(freq)) %>% 
  mutate_all(~round(.,2)) %>%
  mutate(prop = scales::label_percent()(prop)) %>% 
  kableExtra::kable() %>% kableExtra::kable_styling()
  #DT::datatable()

