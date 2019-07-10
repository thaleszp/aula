#termos de interacao e variaveis categoricas

library(wooldridge)
data(wage1)
library(tidyverse)

?wage1 #ver nome das variaveis

ggplot(wage1, aes(exper, wage)) + 
  geom_point() + 
  geom_smooth() +
  theme_bw() + 
  labs (title = "", subtitle = "Fonte: Wooldridge, dados 'wage1'", caption = "", x = "experiência", y = "salário") + 
  theme(text = element_text(size=18))




#relação quadrática entre salário e experiência.

modelo1 <- lm(wage ~ exper, data=wage1)
summary(modelo1)

modelo2 <- lm(wage ~ exper + expersq, data=wage1)
summary(modelo2)




#qual o impacto de 1 ano a mais de experiencia no modelo 2? depende do período.

0.298 + (2*-0.0061)*13 #13 para 14 anos de experiencia

0.298 + (2*-0.0061)*25 #25 para 26 anos o efeito da experiência já é negativo




# Comparar soma quadrado dos residuos (SQR) dos modelos

residuo_mod1 <- resid(modelo1)
residuo_mod2 <- resid(modelo2)

sum(residuo_mod1^2)
sum(residuo_mod2^2)

#qual a relacao desse resultado com o R^2 ?




######### VARIAVEIS CATEGORICAS


#qual a diferenca de media de salario entre homens e mulheres?

wage1 %>% filter(female == 0) %>%
  summarise(mean(wage))

wage1 %>% filter(female == 1) %>%
  summarise(mean(wage))

4.587659 - 7.099489 




# o que signica o coeficiente the female na regressao abaixo?

modelo1 <- lm(wage ~ female, data=wage1)
summary(modelo1)

plot(y = wage1$wage, x = wage1$female)




# funcao abline(a=NULL, b=NULL, h=NULL, v=NULL, ...)

abline(a= 7.09-0, b= 0)
abline(a= 7.09-2.27, b= 0)

mean(wage1$wage)
abline(a= 5.89, b=0)




# criar duas variaveis categoricas para homem e mulher

wage1 <- wage1 %>% 
  mutate(homem = ifelse(female ==0, "1", "0"))

wage1 <- wage1 %>% 
  mutate(novadummy = ifelse(female ==1, "2", "1"))

teste1<- lm(wage ~ homem + educ, data=wage1)
summary(teste1)

teste2<- lm(wage ~ novadummy + educ, data=wage1)
summary(teste2)
 



# Separar base entre homem e mulher

homem <- wage1 %>% filter(female == 0) #comparar a media de salario com o coeficiente linear do modelo 1
mulher <- wage1 %>% filter(female == 1)




#termos de interação com variáveis categoricas = desloca o intercepto

modelo3 <- lm(wage ~ married*female, data=wage1) #esse comando já adiciona as três variáveis, ver no summary
summary(modelo3) #interação entre casado e mulher é significativo. 

5.16 + 2.81 + (-2.86) + (-0.55*1) #média salarial = ser mulher e ser casada  

5.16 + 2.81 + (-2.86*0) + (-0.55*0) #média salarial = ser homem e ser casado 

5.16 + 2.81*0 + (-2.86*0) + (-0.55*1) #média salarial = ser mulher e solteira no salário


-0.55 - 2.86 #mulheres casadas ganham em média 3.4 a menos que o grupo controle (homens casados)


#fazer a media simples pela base de dados

wage1 %>% filter(female == 1) %>%
          filter(married == 1) %>%
          summarise(mean(wage))

wage1 %>% filter(female == 0) %>%
  filter(married == 1) %>%
  summarise(mean(wage))

7.98 - 4.56 #essa comparacao mostra que o mesmo resultado. mulheres casadas ganham em media 3.4 a menos que homens casados

#outras comparacoes

4.565909 - 4.611583 #mulheres casada - mulher solteira

7.983032 - 5.168023 #homem casado - homem solteiro


# dataframe para fazer um grafico  
valores <- c(4.565909, 4.611583, 7.983032, 5.168023)
civil <- c("Mulher casada", "Mulher solteira", "Homem casado", "Homem solteiro")
civil <- as.factor(civil)
DF <- data.frame(civil, valores)

ggplot(DF, aes(x=civil, y=valores)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Comparacao entre salarios medios", 
       subtitle="solteiro e casados", 
       caption="fonte: wooldrige") +
      theme_bw()




# como ler termos de interacao com variaveis continuas
modelo4 <- lm(wage ~ educ*exper, data=wage1)
summary(modelo4) #assuma que todas as variaveis sejam significativas.

0.601 + 0.002*(20) #efeito parcial da educacao no salario para individuos com 20 anos de experiencia

0.045 + 0.002*(12) #efeito parcial da experiencia no salario para individuos com 12 anos de educacao
#perdemos o efeito parcial de cada variavel. agora o efeito de uma sempre depende do valor da outra.

