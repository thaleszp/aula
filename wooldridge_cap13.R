library(wooldridge)
options(scipen=999)

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

