library(wooldridge)
options(scipen=999)

# como fazer o pooling dos dados?

#library(plm)
#poolmod <- plm(modelo, base de dados, model="pooling")



#Exercicio C1

data("fertil1")
?fertil1

eq13.1 <- lm(kids ~ educ + age + agesq + black + east + 
               northcen + west + farm + othrural +
               town + smcity + y74 + y76 + y78 + y80 + y82 + y84, data=fertil1)

summary(eq13.1)

#iii - Suponha que voce acredite que que a variacao de u mude só com o tempo

u <- resid(eq13.1)
fertil1$u2 <- u^2
 
eq13.1b <- lm(u2 ~ y74 + y76 + y78 + y80 + y82 + y84, data=fertil1)
summary(eq13.1b)
#evidencia de heterosc.

#___________________________________________________________________________________
#Exercicio C2

data("cps78_85")
?cps78_85

#como você interpreta o coeficiente de y85 na eq 13.2?

eq13.2 <- lm(lwage ~ y85 + educ + y85*educ + 
               exper + expersq + union + female + y85*female, data = cps78_85)
summary(eq13.2)

#coef de y85 é a mudança proporcional no salário de um homem com zero anos de educ.

#Mantendo outros fatores fixos, qual o aumento percentual estimado do salario 
#nominal de um homem com 12 anos de escolaridade? Proponha uma reg para obter o iC 
#para essa estimativa.

#queremos estimar coef de y85 + coef de y85*educ, com educ = 10.
#mudança no intercepto para um homem com 12 anos de educ. 
#com y85*(educ - 12), coef e e.p está em y85.

0.11780622 + (0.01846053*12)

#para o erro padrão.
cps78_85$educb <- cps78_85$educ - 12
eq13.2b <- lm(lwage ~ y85 + educ + y85*educb + 
               exper + expersq + union + female + y85*female, data = cps78_85)
summary(eq13.2b)

ic_inf <- 0.339-1.96*0.034
ic_sup <- 0.339+1.96*0.034

#faça nova estimativa de 13.2, mas com todos os salarios medidos em dolares de 78

cps78_85$wage <- exp(cps78_85$lwage)

library(dplyr)

y85 <- cps78_85 %>% 
       filter(y85==1) %>%
       mutate(rwage = wage/1.65)          
  

index <- cps78_85$y85 == 1
cps78_85$rwage[index] <- (cps78_85$wage[index])/1.65 

eq13.2c <- lm(lrwage ~ y85 + educ + y85*educ + 
                exper + expersq + union + female + y85*female, data = cps78_85)
summary(eq13.2c)

#___________________________________________________________________________________
#Exercicio C3

data("kielmc")
?kielmc

#o que significa se  coef de log(dist) é >0?

c3 <- lm(lprice ~ y81 + ldist + y81*ldist, data=kielmc)
summary(c3)

#i, se log(dist) é >0, incinerador é mais distante de casas mais caras.

#coef de y81*ldist=0.04 não é significativo (mas tem sinal correto)

c3_3 <- lm(lprice ~ y81 + ldist + y81*ldist +
             age + agesq + lintstsq + lland + larea , data=kielmc)
summary(c3_3)
#que caracteristicas das casas explicam a dif de precos, não a dist do incinerador

#___________________________________________________________________________________
#Exercicio C5
data("rental")
?rental
c5 <- lm(lrent ~ y90 + lpop + lavginc + pctstu, data=rental)
summary(c5)

# em termos nominais, alugueis amentaram 26% em 10 anos. 
# pctstu 1 ponto percentual aumenta o alguel em 0.5%

#ii erros padrao sao validos? ai são efeitos fixos.

#_________________________________________________________

#p.512 1.4
data("jtrain")
library(tidyverse)
jtrain2 <- jtrain %>% filter(year %in% c(1987, 1988)) %>%
                      select(scrap, year, grant) %>%
                      na.omit()

library(plm)

m_dif <- plm(scrap ~ grant, data=jtrain2, 
              index = c("year"), model="fd")
summary(m_dif)


m_dif <- plm(diff(scrap, 1) ~ diff(grant, 1), data=jtrain2, index = c("year"), model="pooling")
summary(m_dif)




#separar o data frame por ano
teste <- jtrain2 %>%
  split(.$year) %>% # from base R
  map(~ lm(scrap ~ grant, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

##########################################################################
#AULA PAINEL EFEITOS FIXOS 

#ler clipboard
dadosprova <- read.delim("clipboard")


#MODELOS DE PAINEL: EFEITO FIXO E ALEATORIO

coplot(Nota ~ T_Estudo|Aluno, type="l", data=dadosprova)
coplot(Nota ~ T_Estudo|Aluno, type="b", data=dadosprova)

library(gplots)

plotmeans(Nota ~ Aluno, main="Notas dos alunos", data=dadosprova)
abline(h=6)
plotmeans(Nota ~ Prova, main="Notas das provas", data=dadosprova)


#sera que foi dificuldade de entender o conteudo ou falta de estudo?

#Estimativa em MQO, nao considera heterogeneidade entre alunos e provas.
m1 <- lm(Nota ~ T_Estudo, data=dadosprova)
summary(m1)

#qual a nota de quem estudou zero horas? 
#qual observação o modelo mais superestimou a nota?
#Qual a nota esperada de quem estudou 5 horas para a prova?

2.0901*(0.7035*5)

m2 <- lm(Nota ~ T_Estudo + Aluno, data=dadosprova)
summary(m2)

#R escolhe o primeiro nome em ordem alfabetica como referencia.
# Qual a nota esperada ddo Jose se ele estudou 5 horas?

(2.57925 - 0.98868)*(5*0.68679)



#Modelo de efeitos fixos com tempo
#Prova eh um numero, tem que ser categoria

dadosprova$Prova <- as.factor(dadosprova$Prova)
m3 <- lm(Nota ~ T_Estudo + Aluno + Prova, data=dadosprova)
summary(m3)

#pode ver se media dos testes foi maior ou menor.
#atencao para a reducao dos graus de liberdade.

#estimar direto
library(plm)

m_fixo <- plm(Nota ~ T_Estudo, data=dadosprova, 
              index = c("Aluno", "Prova"), model="within")
summary(m_fixo)

#model = "within" eh sem efeito fixo de tempo

m_fixo_tempo <- plm(Nota ~ T_Estudo + Prova, data=dadosprova, 
                    index = c("Aluno"), model="within")
summary(m_fixo_tempo)

#mas e os efeitos individuais?

fixef(m_fixo)
fixef(m_fixo_tempo)

#modelo de efeitos aleatorios
m_aleatorio <- plm(Nota ~ T_Estudo, data=dadosprova, 
                   index = c("Aluno", "Prova"), model="random")

summary(m_aleatorio)

#com erro padrao robusto

summary(m_fixo, vcov = vcovHC(m_fixo))
summary(m_fixo, vcov = vcovHC)
summary(m_fixo, vcov = function(x) vcovHC(x, method = "white2"))

#teste de hausman

phtest(m_fixo, m_aleatorio)
#hipotese nula eh que efeitos aleatorios sao melhores
#nao precisa adicionar efeito fixo.

#teste para efeitos fixos de tempo.

#H0 = efeito de tempo nao eh necessario
pFtest(m_fixo, m_fixo_tempo)


##----------------- comparar modelos


ychapeu_ols <- predict(m1)

theme_set(theme_bw())

ggplot(dadosprova, aes(T_Estudo, Nota)) + 
                            geom_point(size=4, alpha=0.5) + 
                            geom_line(aes(y = ychapeu_ols), size = 1, color="gray40") +
                            geom_abline(intercept = (2.57925-0.98868), slope = 0.68679)





#####----------------
# https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html

library(fixest)

m3f <- feols(Nota ~ T_Estudo | Aluno+Prova, data = dadosprova)
summary(m3f)

#mostrar tabela
esttable(m3f, se = "twoway", titles = c("Gaussian"))

#para extrair os efeitos fixos
efeitosfixos <- fixef(m3f)
summary(efeitosfixos)

plot(efeitosfixos)

# O que significa o "twoway" nos modelos?
#estimar o modelo com dummies individuais e de tempo (em OLS)

