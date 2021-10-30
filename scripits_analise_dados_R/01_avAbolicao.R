# Análise da Av Abolicao - 03/04/2019 11:10:02 - 

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
avAbolicao_03_33_atraso <- read_csv('../outputs/avAbolicao/avAbolicao_03_33/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:10:02') + seconds(time_start)) 
avAbolicao_33_63_atraso <- read_csv('../outputs/avAbolicao/avAbolicao_33_63/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:39:59') + seconds(time_start))
avAbolicao_63_93_atraso <- read_csv('../outputs/avAbolicao/avAbolicao_63_93/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:09:59') + seconds(time_start))
avAbolicao_93_123_atraso <- read_csv('../outputs/avAbolicao/avAbolicao_93_123/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:39:59') + seconds(time_start))
avAbolicao_123_153_atraso <- read_csv('../outputs/avAbolicao/avAbolicao_123_153/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:09:59') + seconds(time_start))
avAbolicao_153_183_atraso <- read_csv('../outputs/avAbolicao/avAbolicao_153_183/atrasos.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:39:59') + seconds(time_start))

atrasos <- bind_rows(avAbolicao_03_33_atraso,avAbolicao_33_63_atraso,avAbolicao_63_93_atraso,
                    avAbolicao_93_123_atraso,avAbolicao_123_153_atraso,avAbolicao_153_183_atraso)
remove(avAbolicao_03_33_atraso,avAbolicao_33_63_atraso,avAbolicao_63_93_atraso,
          avAbolicao_93_123_atraso,avAbolicao_123_153_atraso,avAbolicao_153_183_atraso)

avAbolicao_03_33_carros <- read_csv('../outputs/avAbolicao/avAbolicao_03_33/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:10:02') + seconds(time_start))
avAbolicao_33_63_carros <- read_csv('../outputs/avAbolicao/avAbolicao_33_63/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 11:39:59') + seconds(time_start))
avAbolicao_63_93_carros <- read_csv('../outputs/avAbolicao/avAbolicao_63_93/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:09:59') + seconds(time_start))
avAbolicao_93_123_carros <- read_csv('../outputs/avAbolicao/avAbolicao_93_123/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 12:39:59') + seconds(time_start))
avAbolicao_123_153_carros <- read_csv('../outputs/avAbolicao/avAbolicao_123_153/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:09:59') + seconds(time_start))
avAbolicao_153_183_carros <- read_csv('../outputs/avAbolicao/avAbolicao_153_183/carros.csv') %>% mutate(tempo_real = dmy_hms('03/04/2019 13:39:59') + seconds(time_start))

carros <- bind_rows(avAbolicao_03_33_carros,avAbolicao_33_63_carros,avAbolicao_63_93_carros,
                    avAbolicao_93_123_carros,avAbolicao_123_153_carros,avAbolicao_153_183_carros)
remove(avAbolicao_33_33_carros,avAbolicao_33_63_carros,avAbolicao_63_93_carros,
  avAbolicao_93_123_carros,avAbolicao_123_153_carros,avAbolicao_153_183_carros)

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

table(is.na(atrasos$atraso))
table(is.na(atrasos$atraso_trat))
atrasos <- atrasos %>% mutate(atraso_trat = atraso)

carros <- carros %>% filter(!is.na(classe))

#Analises
carros_only <- carros %>% filter(classe != 'bicycle') %>% mutate(diff = tempo_real - lag(tempo_real)) %>% 
  mutate(diff = ifelse(is.na(diff) | diff < 0,quantile(diff, probs = 0.25, na.rm = TRUE),diff))

brechas <- as.numeric(carros_only$diff)
plot(quantile(brechas, probs = seq(0,1,0.01), na.rm = TRUE))
g <- 127-54#mean(brechas[brechas>quantile(brechas, probs = 0.98, na.rm = TRUE)])
ciclo <- 127
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


fluxo_atrasos <- atrasos_only %>% 
  mutate(tempo_real_cut = cut(tempo_real_final,seq(tempo_inicial,tempo_final,by = '15 min'))) %>%
  group_by(tempo_real_cut) %>% summarise(fluxo_pedestres = n(), atraso_medio = mean(atraso_trat, na.rm = TRUE))

dados  <-  left_join(brechas,fluxo_atrasos,by = 'tempo_real_cut')

write.csv2(atrasos_only,'avAbolicao_atrasos_bruto.csv')
write.csv2(carros_only,'avAbolicao_veiculos_bruto.csv')

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

atrasos_only  %>%
  ggplot(aes(x = atraso_trat)) + geom_histogram(fill = 'steelblue',binwidth = 10) + xlab('Atraso (s)') + ylab('Freq')

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
  ggplot(aes(x = atraso)) + geom_histogram(fill = 'steelblue',binwidth = 10)

atrasos_final  %>%
  ggplot(aes(y = atraso)) + geom_boxplot(fill = 'steelblue')

atrasos_final  %>%
  ggplot(aes(x = brecha)) + geom_histogram(fill = 'steelblue',binwidth = 5)

atrasos_final  %>%
  ggplot(aes(y = brecha)) + geom_boxplot(fill = 'steelblue')

quantile(atrasos_final$atraso)
quantile(atrasos_final$brecha)

tempo_inicial
tempo_final - minutes(15)

t.test(atrasos_final$atraso)
t.test(atrasos_final$brecha)

t.test(dados$brecha_media)#headway

max(dados$fluxo_carros)*4 # VPH
max(dados$fluxo_pedestres)*4 # PPH

length(atrasos_final$atraso)

atrasos_final %>% write_csv2('avAbolicao_atrasos.csv')

descdist(atrasos_final$brecha, boot = 1000)
fit <- fitdist(atrasos_final$brecha, "exp", discrete = F)
summary(fit)
plot(fit, histo = TRUE, demp = TRUE)
denscomp(fit, addlegend=TRUE, histo = TRUE, demp = TRUE)
gofstat(fit, discrete = F)
gofstat(fit, discrete = T)



fviz_nbclust(atrasos_final %>% dplyr::select(atraso, brecha), kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res=kmeans(atrasos_final %>% dplyr::select(atraso, brecha), 5, iter.max = 1000, nstart = 1)

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

atrasos_final_cluster %>% 
  group_by(cluster) %>% 
  summarise(freq = n(), atraso_medio = mean(atraso), brecha_media = mean(brecha)) %>% 
  mutate(prop = freq/sum(freq)) %>% 
  mutate_all(~round(.,2)) %>%
  mutate(prop = scales::label_percent()(prop)) %>% 
  kableExtra::kable() %>% kableExtra::kable_styling()
  #DT::datatable()
