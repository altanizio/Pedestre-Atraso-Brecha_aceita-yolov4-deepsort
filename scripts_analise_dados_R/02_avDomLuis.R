##########################################
# Criador: Francisco Altanizio (2021)    #
##########################################

# Análise da Av. Dom Luis - 03/04/2019 11:09:03 - 

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
avDomLuis_02_32_atraso <- read_csv('../outputs/avDomLuis/avDomLuis_02_32/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:09:03') + seconds(time_start))
avDomLuis_32_62_atraso <- read_csv('../outputs/avDomLuis/avDomLuis_32_62/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:39:06') + seconds(time_start))
avDomLuis_62_92_atraso <- read_csv('../outputs/avDomLuis/avDomLuis_62_92/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:09:10') + seconds(time_start))
avDomLuis_92_122_atraso <- read_csv('../outputs/avDomLuis/avDomLuis_92_122/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:39:15') + seconds(time_start))
avDomLuis_122_152_atraso <- read_csv('../outputs/avDomLuis/avDomLuis_122_152/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:09:43') + seconds(time_start))
avDomLuis_152_182_atraso <- read_csv('../outputs/avDomLuis/avDomLuis_152_182/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:39:46') + seconds(time_start))

atrasos <- bind_rows(avDomLuis_02_32_atraso,avDomLuis_32_62_atraso,avDomLuis_62_92_atraso,
                    avDomLuis_92_122_atraso,avDomLuis_122_152_atraso,avDomLuis_152_182_atraso)
remove(avDomLuis_02_32_atraso,avDomLuis_32_62_atraso,avDomLuis_62_92_atraso,
          avDomLuis_92_122_atraso,avDomLuis_122_152_atraso,avDomLuis_152_182_atraso)

avDomLuis_02_32_carros <- read_csv('../outputs/avDomLuis/avDomLuis_02_32/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:09:03') + seconds(time_start))
avDomLuis_32_62_carros <- read_csv('../outputs/avDomLuis/avDomLuis_32_62/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:39:06') + seconds(time_start))
avDomLuis_62_92_carros <- read_csv('../outputs/avDomLuis/avDomLuis_62_92/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:09:10') + seconds(time_start))
avDomLuis_92_122_carros <- read_csv('../outputs/avDomLuis/avDomLuis_92_122/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:39:15') + seconds(time_start))
avDomLuis_122_152_carros <- read_csv('../outputs/avDomLuis/avDomLuis_122_152/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:09:43') + seconds(time_start))
avDomLuis_152_182_carros <- read_csv('../outputs/avDomLuis/avDomLuis_152_182/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:39:46') + seconds(time_start))

carros <- bind_rows(avDomLuis_02_32_carros,avDomLuis_32_62_carros,avDomLuis_62_92_carros,
                    avDomLuis_92_122_carros,avDomLuis_122_152_carros,avDomLuis_152_182_carros)
remove(avDomLuis_02_32_carros,avDomLuis_32_62_carros,avDomLuis_62_92_carros,
  avDomLuis_92_122_carros,avDomLuis_122_152_carros,avDomLuis_152_182_carros)

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
g <- 145-80#mean(brechas[brechas>quantile(brechas, probs = 0.99, na.rm = TRUE)])
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

write.csv2(atrasos_only,'avDomLuis_atrasos_bruto.csv')
write.csv2(carros_only,'avDomLuis_veiculos_bruto.csv')



grafico <- ggplot() + 
  geom_line(data = carros_only, aes(x = ymd_hms(tempo_real),y = (diff), color = 'Brecha'))+
  geom_point(data = atrasos_only,aes(x = (ymd_hms(tempo_real_final)),y = (atraso_trat), color = 'atraso'))
ggplotly(grafico)

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
tempo_final - minutes(15)


quantile(atrasos_final$atraso)
quantile(atrasos_final$brecha)

t.test(atrasos_final$atraso)
t.test(atrasos_final$brecha)

t.test(dados$brecha_media)#headway

max(dados$fluxo_carros)*4 # VPH
max(dados$fluxo_pedestres)*4 # PPH

length(atrasos_final$atraso)
atrasos_final %>% write_csv2('avDomLuis_atrasos.csv')




fviz_nbclust(atrasos_final %>% dplyr::select(atraso, brecha), kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res=kmeans(atrasos_final %>% dplyr::select(atraso, brecha), 4, iter.max = 1000, nstart = 25)

atrasos_final_cluster= bind_cols(atrasos_final %>% dplyr::select(atraso, brecha), cluster=km.res$cluster)


atrasos_final_cluster %>%
  mutate(cluster = factor(cluster)) %>%
  ggplot(aes(brecha, atraso)) +
  geom_point() +
  scale_x_continuous(name = 'Brecha aceita (s)') + 
  scale_y_continuous(name = 'Atraso na travessia (s)') + 
  theme_bw()

atrasos_final_cluster %>%
  mutate(cluster = factor(cluster, label = c('D','B','C','A'))) %>%
  ggplot(aes(brecha, atraso)) +
  geom_point(aes(color = cluster,shape = cluster), size = 2) +
  geom_mark_hull(
    concavity = 10,
    expand = 0,
    radius = 0.03,
    aes(fill = cluster)
  ) +
  geom_text(label = 'A', x = 20, y = 20, size = 7)+
  geom_text(label = 'B', x = 20, y = 50, size = 7)+
  geom_text(label = 'C', x = 80, y = 10, size = 7)+
  geom_text(label = 'D', x = 80, y = 75, size = 7)+
  scale_x_continuous(name = 'Brecha aceita (s)') + 
  scale_y_continuous(name = 'Atraso na travessia (s)') + 
  geom_vline(xintercept = 60, color = 'red', size = 1) +
  theme_Publication() +
  theme(legend.position = "none")
  #theme(legend.title=element_blank())
#ggsave("graficos/chushes_2_2.png", width = 6, height = 4, dpi=600)


