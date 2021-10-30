library(tidyverse)
dados <- read_csv2('hcm.csv')

dados %>%
  ggplot(aes(x = Local, y = Atraso,fill = 'Estimado (IC 95%)')) + geom_col() +
  xlab('Local') + ggtitle('') + ylab('Atraso médio (s)')  +
  geom_errorbar(
    aes(ymin = II, 
        ymax = IS), 
    width=0.2, alpha = 0.7, size=1.3, linetype="solid"
  )+   
  geom_point(aes(y = HCM, color = 'HCM',fill = NULL), size = 3, stroke = 1.7,shape=3)+
  scale_fill_manual(values = c("Estimado (IC 95%)" = "steelblue")) + 
  scale_color_manual(values = c("HCM" = "orange"))  +
  theme_Publication()   + theme(legend.title=element_blank())
#ggsave("graficos/atraso_HCM_global_cor.png", width = 8, height = 4, dpi=600)