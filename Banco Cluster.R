# Aula Banco - Cluster

# Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(Hmisc)
library(PerformanceAnalytics)

## DESCRITIVAS ----------------------------------------------------------------

load("banco1.RData")

# 2 Histogramas: transacao, volume, tempo e rentabilidade 

ggptrans <- ggplot(banco, aes(x = transac)) + geom_histogram(binwidth = 1)
ggpvol <- ggplot(banco, aes(x = vol)) + geom_histogram(binwidth = 1)
ggptemp <- ggplot(banco, aes(x = tempo)) + geom_histogram(binwidth = 1)
ggprent <- ggplot(banco, aes(x = rent)) + geom_histogram(binwidth = 1)

grid.arrange(ggptrans, ggpvol, ggptemp, ggprent, ncol = 2)  

# 3 media: transacao, volume, tempo e rentabilidade


vars <- banco %>% 
  select(transac, vol, tempo, rent, educ, renda, idade)

summary(vars)

sapply(vars, mean, na.rm=TRUE) # media de todas as vars 
sapply(vars, quantile, na.rm=TRUE) # media de todas as vars

## PREPARAR PARA SEGMENTACAO ----------------------------------------------------------------
# 1 retirar NAs
banco <- na.omit(banco) # remover missing values

# 2 padronizar as variaveis de critrio de segmentacao---------------------------------------------------------------- 
#(apenas se estiverem em escalas diferentes)
banco1 <- banco %>% 
  select(transac, rent, vol, tempo, internet1) %>%
  mutate(across(everything(), scale,  .names = "{col}z"))

# 3 Correlação  ----------------------------------------------------------------
# avaliar se variaveis de criterio de segmentacao sao correlacionadas

banco1 %>% select(ends_with("z")) %>% 
  chart.Correlation()

# matriz de correlacao
varcritcor = banco1 %>% select(ends_with("z")) %>% as.matrix() %>% rcorr()
View(varcritcor$r)
View(varcritcor$P)

# 4 Por que é recomendavel retirar volume ou rentabilidade? ----------------------------------------------------------------
# como volume e rentabilidade tem correlacao alta, deixar as 2 
# no modelo irá viesar minha análise.(vamos ficar com rentabilidade)

banco1 %>% select(ends_with("z") ,-volz)


# SEGMENTACAO: WARDSLINKAGE & DENDROGRAMA ----


# 1 Calcular distancias euclidianas e wardslinkage 
euclid <- banco1 %>% 
  select(ends_with("z") ,-volz) %>% dist("euclidean")# distancia euclidiana

ward <- euclid %>% hclust(method="ward.D2") # procedimento ao quadrado - ajuda a suavizar

wardg1 <- plot(ward, labels = NULL, hang = -1, cex = 0.6,
     main = "Cluster Dendrograma", sub = NULL,
     xlab = NULL, ylab = "Dissimilaridade")

# 2 avaliar o dendrograma: quantos cluster vamos selecionar? 
# poderiam ser entre 2-6. Vamos selecionar 3:

ward3 <- cutree(ward, k=3)
table(ward3) # numero de clientes por cluster
rect.hclust(ward, k=3, border=2:5)

# 3 identificar o cluster de cada cliente na base
banco[ , "cluster3"] <- ward3

# 4 Visualizar clusters

factoextra::fviz_cluster(list(data = banco, cluster = ward3))

# 5 calcular as centroides dos clusters (descrever perfil tipico do cluster) 

clus3media <- banco %>%
  group_by(cluster3) %>%
  summarise_at(.vars = c("transac", "rent", "vol", "tempo", "educ", "idade", "mulher1",
                         "carro", "telefone1", "internet1", "computador1", "renda",
                         "sobras", "cartcred", "cxelet", "chequeesp", "pagautom", 
                         "financ", "finimob", "parcel", "fundoinv", "cdb", "previd", 
                         "segurocarro", "segurocasa", "segurovida"),
              .funs = mean)

View(clus3media)

# 6 identificar os limites (range) de cada var por cluster  

percentil <- c(0.2, 0.8)

percrotulo <- map_chr(percentil, ~paste0(.x*100, "%"))

range <- map(percentil, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = percrotulo )

varclusrange <- banco %>% 
  group_by(cluster3) %>% 
  summarize_at(vars("transac", "rent", "vol", "tempo", "educ", "idade", "mulher1",
                    "carro", "telefone1", "internet1", "computador1", "renda",
                    "sobras", "cartcred", "cxelet", "chequeesp", "pagautom", 
                    "financ", "finimob", "parcel", "fundoinv", "cdb", "previd", 
                    "segurocarro", "segurocasa", "segurovida"), funs(!!!range)) %>% 
  select(cluster3, contains("transac"), contains( "rent"), contains( "vol"), 
         contains( "tempo"), contains( "educ"), contains( "idade"), contains( "mulher1"), 
         contains( "telefone1"), contains( "internet1"), contains( "computador1"), 
         contains( "renda"), contains( "cartcred"), contains( "cxelet"), 
         contains( "chequeesp"), contains( "pagautom"), contains( "finimob"), 
         contains( "parcel"), contains( "fundoinv"), contains( "cdb"), 
         contains( "previd"), contains( "segurocasa"), contains( "segurovida"))

write.csv(varclusrange, file = 'bancoclusterrange.csv') # salvar os dados em excel
