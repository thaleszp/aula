# RAIS 1985 do RS = 2.370.043 obs (txt com 400 mega)
#excel tem limite de 1 milhão de linhas. 

################## Código que usei para montar o arquivo RaisSM1995


library(tidyverse)
RaisSM1995 <- RS1995 %>% filter (`Município` == 431690)
  
write.csv(RaisSM1995,'RaisSM1995.csv')


#clean_names ---> pacote janitor para simplificar o nome das variáveis (e tira acentos)
library(janitor)  
RaisSM1995 %>% clean_names()

  
################## AULA  
  
## Excel ----> R  
  
# Pipes: 
#Escrito como %>% , permite encadear uma série de funções em uma base de dados.


################## 6 principais funções do pacote dplyr -> usar library(tidyverse)
#VER https://r4ds.had.co.nz/tidy-data.html


#SELECT ----> seleciona um conjunto de variáveis
  
DataFrame %>% 
  select(x)

Dataframe %>% select(-variável1, -variável2) #escolher todas variáveis menos as especificadas

DF2 <- RaisSM1995 %>% select(Tamanho.Estabelecimento:Tipo.Vínculo)




#########################################################################################

#MUTATE ----> cria / transforma uma variável 
# ifelse cria condições lógicas no R

DF %>%
  mutate(nome = X * 100) 

RaisSM1995 <- RaisSM1995 %>%
              mutate(faixa_etaria = ifelse(Idade <= 18, "Jovem",
                                  ifelse(Idade %in% 18:25, "jovem_adulto",
                                         ifelse(Idade %in% 25:50, "adulto",
                                                ifelse(Idade > 50, "senior", "no"))))) #sempre tem que terminar com o "else" = "no"

#operador %in% é utilizado para idenficar um elemento em um vetor
# será que existe o CBO 99920 na base?

99920 %in% RaisSM1995$CBO.94.Ocupação


# grepl (base R) permite usar vetores com caracteres no mutate

teste <- grepl("adulto", RaisSM1995$faixa_etaria)
sum(teste, na.rm = TRUE)

#compare com o resultado acima. Qual a diferença entre os dois códigos?
RaisSM1995 %>% filter(faixa_etaria %in% c("adulto")) %>%
  count(faixa_etaria)

RaisSM1995 <- mutate(RaisSM1995, classificando = ifelse(grepl("adul", faixa_etaria), "Tudo Adulto", "piá"))


                                      
#########################################################################################

#FILTER ----> filtra informações baseado em uma ou mais condições

DF %>% 
  filter(x > 100) 

remuneracao_homens <- RaisSM1995 %>% 
                      filter(Sexo.Trabalhador == 1) %>% 
                      filter(Vl.Remun.Média..SM. >= 10) ##seleciona homens que ganham 10 SM ou mais.

RaisIdades <- RaisSM1995 %>%
          filter(Idade %in% 20:30) #selecionar idades de 20 até 30. c("BB", "AA") para caracteres


#########################################################################################

#SUMMARIZE ----> resume uma informação 

DF %>% 
  summarize(nome = mean(x, na.rm = TRUE)) #na.rm retira os NA da base

RaisSM1995 %>%
  filter(Sexo.Trabalhador == 2) %>% 
  summarize(salariomulher = mean(Vl.Remun.Média..SM., na.rm = TRUE)) #qual a remuneracao media das mulheres 
                                                                     #(em salários mínmos)



#########################################################################################

#GROUP_BY ----> junta informações (semelhante ao Pivot Table no Excel)

DF %>% 
  group_by(x) %>% 
  summarize(mean_height = mean(height, na.rm = TRUE))

RaisSM1995 %>%
  group_by(Sexo.Trabalhador) %>% 
  summarize(salarios = mean(Vl.Remun.Média..SM., na.rm = TRUE)) #comparar rendas por categorias


#########################################################################################

#ARRANGE ----> ordena as informações

DF %>% 
  arrange(X) %>% 
  select(X)


#########################################################################################

library(ggplot2)
theme_set(theme_classic())


#  Plot Densidade
g <- ggplot(RaisSM1995, aes(`Vl.Remun.Média..SM.`))
g + geom_density(aes(fill=factor(Sexo.Trabalhador)), alpha=0.8) + 
  labs(title="Distribuição de salários em 1995", 
       subtitle="preços nominais",
       caption="Fonte: Rais",
       x="faixa de salários",
       fill="Gênero") + scale_x_continuous(limits = c(0, 10))


#########################################################################################

#LUBRIDATE ----> ORGANIZAR DATAS 
#COMO TEMOS 1 VARIÁVEL COM MÊS, PRECISAMOS CONSTRUIR COLUNAS COM ANO E DIA. 
#lembrando que RAIS tem "mês.admissão" = 0, ou seja, a pessoa já está no emprego.
#vamos usar valor da remuneração média (que está em salários mínimos). 
#vamos criar uma data média (junho de 1995) para ajustar o valor dos salários. 

RaisSM1995 <- RaisSM1995 %>% mutate(remu_media = Vl.Remun.Média..SM. * 100) #100 é o salário minimo em junho de 1995


library(lubridate)

RaisSM1995$ano <- 1995
RaisSM1995$mes <- 6
RaisSM1995$dia <- 1


RaisSM1995 <- RaisSM1995 %>% 
  mutate(Datas = make_date(year=ano, month=mes, day=dia))


library(deflateBR) #deflacionar dados. opção padrão é o IPCA

RaisSM1995$salario <- deflate(RaisSM1995$remu_media, RaisSM1995$Datas, "01/2019")



#quanto recebia um professor de ensino médio em 1995?
#CBO 1-41

CBO <- as.character(RaisSM1995$'CBO.94.Ocupação')
CBO3 <- substr(CBO, 1, 3)
RaisSM1995$CBO <- as.numeric(CBO3)

prof_medio <- RaisSM1995 %>%
              filter(CBO == 141)
#ou 
RaisSM1995 %>%
  filter(CBO == 141) %>%
  summarise(mean(salario, na.rm = TRUE))

#tirar desligados
RaisSM1995 %>%
  filter(CBO == 141) %>%
  summarise(mean(salario, na.rm = TRUE))

############################################################################

#SEPARATE ----> 1900/07 EM DUAS COLUNAS: 1900 E 07

DF %>% 
separate(variavel_original, into = c("nova_var 1", "nova_var 2"), sep = "/", convert = TRUE)
#convert = true é para converter automático chr em number.



# UNITE ----> INVERSO DE SEPARATE

unite(new, VAR1, VAR2, sep = "") #sep é para estabelecer o separador


############################################################################
#vamos pegar as 10 profissões que mais aparecem no CBO


RaisSM1995$CBO <- as.character(RaisSM1995$CBO.94.Ocupação) #porque variável original está como fator

categorias <- RaisSM1995 %>% count(CBO.94.Ocupação)

categorias %>% 
top_n(10) %>%
  mutate(CBO.94.Ocupação = reorder(CBO.94.Ocupação, n)) %>%  
  ggplot(aes(CBO.94.Ocupação, n)) +
    geom_col(show.legend = FALSE, fill = "#7288B9") +
    coord_flip() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_discrete(breaks = c("45130", "95932", "39310"),
    labels = c("Vendedor de comércio varejista","Servente de obras","Auxiliar de escritório")) +
    labs(y = "Número de empregos",
       x = NULL,
       title = "Principais categorias de empregos com carteira assinada em Santa Maria",
       subtitle = "CBO, fonte Rais") +
       theme_light()
    





#########################################################################################
library(BETS) #pacote da FGV para pegar dados do IPEA, BC, etc.

##https://cran.r-project.org/web/packages/BETS/vignettes/BETS_basic_usage.html 

BETSsearch()


BETS.chart(ts = 'iie_br', file = "iie_br", open = TRUE)





