# Aula Modelo de Escolha Discreta - Logit

# CASE PROPENSAO A COMPRA - LISTERINE (PAINEL KANTAR) ---------------------------------
library(tidyverse)
library(readxl)
library(sjmisc)
library(Hmisc)
listerine <- read_excel("Kantar Antiseptic DS 2021.xlsx", sheet = "BD")
View(listerine)
listerine <- Kantar_Antiseptic_DS_2021
# Ajustes  -----------------------------------------------------------
freq <- listerine %>% group_by(idconsumer, channel) %>%
  summarise(freq = n()) %>% 
  arrange(-freq) 
View(freq)

listerine <- listerine %>% 
  mutate(size = as.numeric(gsub("ML ","", size)),
         sizel = size/1000,
         volume = sizel*quantity,
         pricel = price/sizel) %>% 
  filter(pricel > 0)


listerine <- merge(listerine, freq)
# Clientes que mais compram?
listerine %>% group_by(idconsumer) %>% 
  summarise(freq = freq) %>% 
  arrange(-freq)

# Por volume?
listerine %>%
  group_by(idconsumer) %>% 
  summarise(qtdL = sum(volume)) %>%
  arrange(-qtdL)


listerine %>%
  filter(brand == "LISTERINE") %>% 
  group_by(channel) %>% 
  summarise(qtdL = sum(quantity)) %>%
  arrange(-qtdL)
# Existe uma estratégia de preço por canal?

listerine %>%
  filter(brand == "LISTERINE") %>% 
  group_by(channel) %>% 
  summarise(mean = mean(pricel)) %>% 
  arrange(-mean)

# Existe uma estratégia de preço por classe social?

listerine %>%
   group_by(socialclass) %>% 
  summarise(mean = mean(size)) %>% 
  arrange(-mean)

listerine %>% filter(socialclass == "DE") %>% select(size) %>% boxplot()

# Criando dummies (PARA LOGIT)

listerine <- listerine %>% 
  mutate(promotion1 = ifelse(promotion == "SEM PROMOÇÃO", 0, 1)) %>% 
  fastDummies::dummy_cols(c("channel", "payment","package","socialclass","brand", "month"))

# Modelo de Escolha Discreta (Logit) ----------------------------------------------------------------------

# escolher as variaveis com base na inteligencia do negocio
# existe algum problema de multicolinearidade?

varcorsig<- listerine %>% 
  select(brand_LISTERINE , pricel , sizel , volume , freq , promotion1 ,
                                       package_Unitário , channel_ASSAI , channel_CARREFOUR ,
                                       `channel_DROGARIA SAO PAULO` , channel_MERCADINHO ,
                                       payment_Dinheiro , `payment_Cartão de Débito` , 
                                       `payment_Cartão de Crédito` , socialclass_AB1 , socialclass_B2 , 
                                       socialclass_C1 , socialclass_C2 ,
                                       month_1 , month_2 , month_3 , month_4 , month_5 , month_6 , 
                                       month_7 , month_8 , month_9 , month_10 , month_11) %>% 
  as.matrix() %>%
  rcorr() 
  
View(varcorsig$r) # package unitário & promotion1; cuidado nas interpretações de variáveis dummies 
# volume e size estao capturando um comportamento muito parecido (nao sao as mesmas coisas)


# Logit - estimação -------------------------------------------------------

# estimar o modelo de escolha discreta para Listerine
# VARIÁVEL DEPENDENTE BINÁRIA - ESCOLHA DA MARCA LISTERINE
logit <- glm(brand_LISTERINE ~ pricel + sizel + volume + freq + promotion1 +
                   package_Unitário + channel_ASSAI + channel_CARREFOUR +
                   `channel_DROGARIA SAO PAULO` + channel_MERCADINHO +
                   payment_Dinheiro + `payment_Cartão de Débito` + 
                   `payment_Cartão de Crédito` + socialclass_AB1 + socialclass_B2 + 
                   socialclass_C1 + socialclass_C2 +
                   month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + 
                   month_7 + month_8 + month_9 + month_10 + month_11
                  , data = listerine, family = "binomial"(link="logit"))

summary(logit)

# veja que volume tem efeito significativo
# vamos separar os efeitos entao: sizel e quantity

logit <- glm(brand_LISTERINE ~ pricel + sizel + quantity + freq + promotion1 +
                    package_Unitário + channel_ASSAI + channel_CARREFOUR +
                    `channel_DROGARIA SAO PAULO` + channel_MERCADINHO +
                    payment_Dinheiro + `payment_Cartão de Débito` + 
                    `payment_Cartão de Crédito` + socialclass_AB1 + socialclass_B2 + 
                    socialclass_C1 + socialclass_C2 +
                    month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + 
                    month_7 + month_8 + month_9 + month_10 + month_11
                  , data = listerine, family = "binomial"(link="logit"))

summary(logit)

# qual foi o percentual de acertos?

library(caret)

problist <- predict(logit, newdata=listerine, type="response")
predictprob <- ifelse(problist > 0.5, "1", "0")
round(mean(predictprob == listerine$brand_LISTERINE), 2)

# onde erramos mais (falsos positivos?)

pdata <- predict(logit, newdata = listerine, type = "response")
listerine$brand_LISTERINE <- as.factor(listerine$brand_LISTERINE)
confusionMatrix(data = as.factor(as.numeric(pdata>0.5))
                , reference = listerine$brand_LISTERINE)

#estimar a probabilidade de retenção de cada cliente da base ---------------------

listerine$problist = predict(logit, newdata=listerine, type="response")


# 17 visualizar efeito de pre?o por litro na probabilidade de compra de listerine --------------------

library(magrittr)
listerine %>%
  ggplot(aes(pricel , problist)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Modelo de Propensão a Compra de Listerine", 
       x = "Preço por litro",
       y = "Probabilidade de Compra de Listerine")

 # quer dizer que tenho que aumentar o pre?o para aumentar a probabilidade de compra?
 # veja o que acontece com Plax (#20)

# 18 visualizar efeito do tamanho da embalagem na probabilidade de compra de listerine ------

library(magrittr)
listerine %>%
  ggplot(aes(sizel , problist)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Modelo de Propens?o a Compra de Listerine", 
       x = "Tamanho da embalagem (litros)",
       y = "Probabilidade de Compra de Listerine" )

# 19 visualizar efeito do Volume comprado na probabilidade de compra de listerine  ------

library(magrittr)
listerine %>%
  ggplot(aes(volumeml , problist)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Modelo de Propens?o a Compra de Listerine", 
       x = "Volume em ml (quantidade x embalagem)",
       y = "Probabilidade de Compra de Listerine" )

# 20 Modelo de Escolha Discreta Plax (Logit) ----------------------------------------------------------------------

logitplax <- glm(`brand_COLGATE PLAX` ~ pricel + sizel + volume + freq + promotion1 +
                   package_Unitário + channel_ASSAI + channel_CARREFOUR +
                   `channel_DROGARIA SAO PAULO` + channel_MERCADINHO +
                   payment_Dinheiro + `payment_Cartão de Débito` + 
                   `payment_Cartão de Crédito` + socialclass_AB1 + socialclass_B2 + 
                   socialclass_C1 + socialclass_C2 +
                   month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + 
                   month_7 + month_8 + month_9 + month_10 + month_11
                 , data = listerine, family = "binomial"(link="logit"))

summary(logitplax)

listerine$probplax = predict(logitplax, newdata=listerine, type="response")

#write.csv(listerine, file = 'listerineprob.csv') # salvar os dados em excel

library(magrittr)
listerine %>%
  ggplot(aes(pricel , probplax)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(title = "Modelo de Propensão a Compra de Colgate Plax", 
       x = "Preço por litro",
       y = "Probabilidade de Compra de Plax")
