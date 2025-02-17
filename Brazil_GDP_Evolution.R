library(lubridate)
library(tidyverse)
library(sidrar)
library(zoo)
library(scales)

########### Taxa trimestre contra trimestre imdediatamente anterior#####################
#Dados

br_dados_01 = get_sidra(api = '/t/5932/n1/all/v/6564/p/last%2029/c11255/90687,90691,90696/d/v6564%201')

br_dados_01 = br_dados_01 %>% mutate( Data = as.yearqtr(`Trimestre (Código)`, format = '%Y%q')) %>% 
  select(Valor, Data, `Setores e subsetores`)

br_dados_01 = br_dados_01 %>% pivot_wider(id_cols = Data, names_from = `Setores e subsetores`,
                                          values_from = Valor)
colnames(br_dados_01) = c("Período", "Agropecuária","Indústria","Serviços")

#Gráfico linha
br_dados_01_long = br_dados_01 %>%
  gather(Setores, Valor, -Período)

gr1.1 = br_dados_01_long %>% filter(Período >= '2018 Q1') %>% 
  ggplot(aes(Período, Valor, color = Setores)) + geom_line(size=.6) +
  geom_hline(yintercept = 0,color = "black", linetype = "dashed") +
  scale_x_yearqtr(breaks = pretty_breaks(n=10), format = '%yT%q') +
  scale_fill_manual(values = c("dark green", "dark red","dark blue")) +
  scale_color_manual(values = c("dark green", "dark red","dark blue"))+ 
  labs(x = "Trimestres", y = "Variação (%)",
       title = "Evolução PIB trimestral [Quarterly GDP Evolution]",
       subtitle = "Taxa trimestre contra trimestre imdediatamente anterior [Quarter-on-quarter growth rate]") 

gr1.1

view(br_dados_01_long %>% arrange(Período))

#Gráfico BARRA

gr3.1 = br_dados_01_long %>%  filter(Período >= '2020 Q1')    %>% 
  ggplot(aes(x=Período, y=Valor, colour=Setores))+ 
  geom_bar(aes(fill=Setores, colour=Setores), stat='identity') +
  geom_hline(yintercept=0, colour='black', linetype='dashed')+
  scale_fill_manual(values = c("dark green", "dark red","dark blue")) +
  scale_color_manual(values = c("dark green", "dark red","dark blue"))+
  scale_x_yearqtr(breaks = pretty_breaks(n=8), format = '%YQ%q')+
  labs(x = "Trimestres", y = "Variação")

gr3.1
