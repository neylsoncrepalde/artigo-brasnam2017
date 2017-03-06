# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde

#library(readr)
#dados <- read_csv("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/dados_rede_teste.csv")

rede3 <- dados[,c(3,24)]
names(rede3) <- c("nome","indicacao")
#View(rede3)

####################################################################
# Limpando os dados
rede3$indicacao[rede3$indicacao=="nenhum"] = "none"
rede3$indicacao[rede3$indicacao=="Não se aplica"] = "none"
rede3$indicacao[rede3$indicacao=="Não houve"] = "none"
rede3$indicacao[rede3$indicacao=="Ninguém"] = "none"
rede3$indicacao[rede3$indicacao=="nenhum destes"] = "none"
rede3$indicacao[rede3$indicacao=="Nenhum"] = "none"
rede3$indicacao[rede3$indicacao=="Nennhum"] = "none"
rede3$indicacao[rede3$indicacao=="nunca escrevi ou publiquei artigo com nenhum dos colegas acima"] = "none"
rede3$indicacao[rede3$indicacao=="Nenhum desses"] = "none"
rede3$indicacao[rede3$indicacao=="Nenhum destes"] = "none"
rede3$indicacao[rede3$indicacao=="NENHUM"] = "none"
rede3$indicacao[rede3$indicacao=="sozinha"] = "none"
rede3$indicacao[rede3$indicacao=="x"] = "none"
rede3$indicacao[rede3$indicacao=="Nao publiquei com os colegas"] = "none"
rede3$indicacao[rede3$indicacao=="."] = "none"
rede3$indicacao[rede3$indicacao=="0"] = "none"
rede3$indicacao[rede3$indicacao=="Nunca pedi revisão"] = "none"
rede3$indicacao[rede3$indicacao=="Não sei..."] = "none"
############################################################################

#####################################################################
# Limpando acentos e colocando letras minúsculas
rede3 <- sapply(rede3[,c(1,2)], tolower)
rede3 <- gsub("ã","a", rede3[,c(1,2)])
rede3 <- gsub("á","a", rede3[,c(1,2)])
rede3 <- gsub("é","e", rede3[,c(1,2)])
rede3 <- gsub("í","i", rede3[,c(1,2)])
rede3 <- gsub("ó","o", rede3[,c(1,2)])
rede3 <- gsub("ú","u", rede3[,c(1,2)])
###################################################################

###################################################################
# Separando os nomes e montando edgelist com o comando unnest()
library(tidyr)
rede3 <- as.data.frame(rede3, stringsAsFactors = F)

teorica <- rede3 %>% unnest(indicacao=strsplit(indicacao,", "))
#View(teorica)
##################################################################

###################################################################
# Preparando a rede
library(igraph)
teorica <- as.matrix(teorica)
teorica <- graph_from_edgelist(teorica, directed = T)
teorica <- delete_vertices(teorica, "none")
teorica <- simplify(teorica)

#plot(teorica, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3)
#plot(teorica, vertex.label=NA, vertex.size=8, edge.arrow.size=0.3,
#     edge.color="black",main="Theoretical Prestige", xlab="Density = 0.05")
