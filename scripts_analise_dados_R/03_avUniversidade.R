##########################################
# Criador: Francisco Altanizio (2021)    #
##########################################

# Análise da Ab Universidade - 29/03/2019 13:00:04 - 

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
avUniversidade_00_30_atraso <- read_csv('../outputs/avUniversidade/avUniversidade_00_30/atrasos.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 13:00:04') + seconds(time_start))
avUniversidade_30_60_atraso <- read_csv('../outputs/avUniversidade/avUniversidade_30_60/atrasos.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 13:30:06') + seconds(time_start))
avUniversidade_60_90_atraso <- read_csv('../outputs/avUniversidade/avUniversidade_60_90/atrasos.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 14:00:06') + seconds(time_start))
avUniversidade_90_120_atraso <- read_csv('../outputs/avUniversidade/avUniversidade_90_120/atrasos.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 14:30:03') + seconds(time_start))
avUniversidade_120_150_atraso <- read_csv('../outputs/avUniversidade/avUniversidade_120_150/atrasos.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 15:00:03') + seconds(time_start))
avUniversidade_150_180_atraso <- read_csv('../outputs/avUniversidade/avUniversidade_150_180/atrasos.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 15:30:11') + seconds(time_start))

atrasos <- bind_rows(avUniversidade_00_30_atraso,avUniversidade_30_60_atraso,avUniversidade_60_90_atraso,
                    avUniversidade_90_120_atraso,avUniversidade_120_150_atraso,avUniversidade_150_180_atraso)
remove(avUniversidade_00_30_atraso,avUniversidade_30_60_atraso,avUniversidade_60_90_atraso,
          avUniversidade_90_120_atraso,avUniversidade_120_150_atraso,avUniversidade_150_180_atraso)

avUniversidade_00_30_carros <- read_csv('../outputs/avUniversidade/avUniversidade_00_30/carros.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 13:00:04') + seconds(time_start))
avUniversidade_30_60_carros <- read_csv('../outputs/avUniversidade/avUniversidade_30_60/carros.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 13:30:06') + seconds(time_start))
avUniversidade_60_90_carros <- read_csv('../outputs/avUniversidade/avUniversidade_60_90/carros.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 14:00:06') + seconds(time_start))
avUniversidade_90_120_carros <- read_csv('../outputs/avUniversidade/avUniversidade_90_120/carros.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 14:30:03') + seconds(time_start))
avUniversidade_120_150_carros <- read_csv('../outputs/avUniversidade/avUniversidade_120_150/carros.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 15:00:03') + seconds(time_start))
avUniversidade_150_180_carros <- read_csv('../outputs/avUniversidade/avUniversidade_150_180/carros.csv') %>% mutate(tempo_real = dmy_hms('29/03/2019 15:30:11') + seconds(time_start))

carros <- bind_rows(avUniversidade_00_30_carros,avUniversidade_30_60_carros,avUniversidade_60_90_carros,
                    avUniversidade_90_120_carros,avUniversidade_120_150_carros,avUniversidade_150_180_carros)
remove(avUniversidade_00_30_carros,avUniversidade_30_60_carros,avUniversidade_60_90_carros,
  avUniversidade_90_120_carros,avUniversidade_120_150_carros,avUniversidade_150_180_carros)

# Tratamento
primeiro_quartil  <-  quantile(atrasos$time, probs = 0.25, na.rm = TRUE)
atrasos <- atrasos %>% mutate(atraso = case_when(
  (inicio_linha_1 != inicio_linha_2) & (!is.na(time)) ~ NA_real_ ,
  (!is.na(time)) ~ time,
  TRUE ~ NA_real_
), atraso_trat = case_when(
  (inicio_linha_1 != inicio_linha_2) & (!is.na(time)) ~ primeiro_quartil ,
  (!is.na(time)) ~ time,
  TRUE ~ NA_real_
))
atrasos <- atrasos %>% mutate(atraso_trat = atraso)
carros <- carros %>% filter(!is.na(classe))

#Analises
carros_only <- carros %>% filter(classe != 'bicycle') %>% mutate(diff = tempo_real - lag(tempo_real)) %>% 
  mutate(diff = ifelse(is.na(diff) | diff < 0,quantile(diff, probs = 0.25, na.rm = TRUE),diff))

brechas <- as.numeric(carros_only$diff)
plot(quantile(brechas, probs = seq(0,1,0.01), na.rm = TRUE))
g <- 145-46#mean(brechas[brechas>quantile(brechas, probs = 0.99, na.rm = TRUE)])
ciclo <- 145
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

write.csv2(atrasos_only,'avUniversidade_atrasos_bruto.csv')
write.csv2(carros_only,'avUniversidade_veiculos_bruto.csv')



# Atraso

dados  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = atraso_medio)) + geom_col(fill = 'steelblue') +  theme_minimal() +
  xlab('Tempo') + ggtitle('') + ylab('Atraso médio (s) a cada 15 min')  +
  geom_hline(aes(yintercept = dp_HCM, color = 'HCM'), size = 2) + theme(legend.title=element_blank())

dados  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = fluxo_pedestres)) + geom_col(fill = 'steelblue') +  theme_minimal() +
  xlab('Tempo') + ggtitle('') + ylab('Fluxo de pedestres a cada 15 minutos')  

fluxo  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = fluxo)) + geom_col(aes(fill = classe)) +  theme_minimal() +
  xlab('Tempo') + ggtitle('') + ylab('Fluxo de veículos a cada 15 minutos')  + labs(fill='Veículo') 

dados  %>%
  ggplot(aes(x = ymd_hms(tempo_real_cut), y = brecha_media)) + geom_col(fill = 'steelblue') +  theme_minimal() +
  xlab('Tempo') + ggtitle('') + ylab('Headway médio (s) a cada 15 minutos') 





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
    aux_calc = aux_calc #- 1
    if(aux_calc<0){
      brechas_final[i,2]  <- 0.5
    }else{
      brechas_final[i,2]  <- aux_calc
    }
  }
}

atrasos_final  <-  atrasos_only %>% dplyr::select(tempo_inicial = tempo_real, tempo_final = tempo_real_final, atraso = atraso_trat)
atrasos_final <- atrasos_final %>% mutate(tempo_final = as.character(tempo_final))
atrasos_final <- left_join(atrasos_final, brechas_final, by = c('tempo_final' = 'tempo'))
atrasos_final <- atrasos_final %>% filter_all(~!is.na(.))

atrasos_final  %>%
  ggplot(aes(x = atraso)) + 
  geom_histogram(fill = 'steelblue',binwidth = 10) +
  xlab('Atraso (s)') + ylab('Freq')

atrasos_final  %>%
  ggplot(aes(y = atraso)) + geom_boxplot(fill = 'steelblue') +
  xlab('') + ylab('Atraso (s)')

atrasos_final  %>%
  ggplot(aes(x = brecha)) + geom_histogram(fill = 'steelblue',binwidth = 5)+
  xlab('Brecha aceita (s)') + ylab('Freq')

atrasos_final  %>%
  ggplot(aes(y = brecha)) + geom_boxplot(fill = 'steelblue')+
  xlab('') + ylab('Brecha aceita (s)')


tempo_inicial
tempo_final- minutes(15)

quantile(atrasos_final$atraso)
quantile(atrasos_final$brecha)

t.test(atrasos_final$atraso)
t.test(atrasos_final$brecha)

t.test(dados$brecha_media)#headway

max(dados$fluxo_carros)*4 # VPH
max(dados$fluxo_pedestres)*4 # PPH

length(atrasos_final$atraso)
atrasos_final %>% write_csv2('avUniversidade_atrasos.csv')






fviz_nbclust(atrasos_final %>% dplyr::select(atraso, brecha), kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

set.seed(153)
km.res=kmeans(atrasos_final %>% dplyr::select(atraso, brecha), 5, iter.max = 100000, nstart = 25)

atrasos_final_cluster= bind_cols(atrasos_final %>% dplyr::select(atraso, brecha), cluster=km.res$cluster)


atrasos_final_cluster %>%
  mutate(cluster = factor(cluster)) %>%
  ggplot(aes(brecha, atraso)) +
  geom_point() +
  scale_x_continuous(name = 'Brecha aceita (s)') + 
  scale_y_continuous(name = 'Atraso na travessia (s)') + 
  theme_bw()

atrasos_final_cluster %>%
  mutate(cluster = factor(cluster)) %>%
  ggplot(aes(brecha, atraso)) +
  geom_point(aes(color = cluster,shape = cluster), size = 2) +
  geom_mark_hull(
    concavity = 10,
    expand = 0,
    radius = 0.03,
    aes(fill = cluster)
  ) +
  scale_x_continuous(name = 'Brecha aceita (s)') + 
  scale_y_continuous(name = 'Atraso na travessia (s)') + 
  theme_bw()


