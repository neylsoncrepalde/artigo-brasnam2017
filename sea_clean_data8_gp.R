# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde
# REDES BIPARTIDAS GRUPOS DE PESQUISA - rodar apos sea_clean_data.R

rede.gp <- dados[,c(3,16)]
names(rede.gp) <- c("nome","afiliacao")

#View(rede.gp)

# Limpando os dados da participacao em grupos de estudo/pesquisa

#View(rede.gp)
rede.gp$afiliacao[rede.gp$afiliacao=="Não"] <- "none"
rede.gp$afiliacao[rede.gp$afiliacao=="não"] <- "none"
rede.gp$afiliacao[rede.gp$afiliacao=="nao"] <- "none"
rede.gp$afiliacao[rede.gp$afiliacao=="Não."] <- "none"
rede.gp$afiliacao[rede.gp$afiliacao=="Nao"] <- "none"
rede.gp$afiliacao[rede.gp$afiliacao=="Sim"] <- "none"
rede.gp$afiliacao[rede.gp$afiliacao=="NÃO"] <- "none"
rede.gp$afiliacao[rede.gp$afiliacao=="Grupo de Estudos em Metodologias Qualitativas (nome provisório, sob orientação da professora Yumi)"] = "GE Metodologias Qualitativas em CS"
rede.gp$afiliacao[rede.gp$afiliacao=="Sim. Grupo de estudos em metodologia qualitativa (Profa. Yumi). Grupo parado atualmente."] = "GE Metodologias Qualitativas em CS"
rede.gp$afiliacao[rede.gp$afiliacao=="Grupo de estudos de metodologia qualitativa em sociologia (o grupo da Yumi, não tem nome definido ainda)"] = "GE Metodologias Qualitativas em CS"
rede.gp$afiliacao[rede.gp$afiliacao=="Sim, Grupo de pesquisa de Metodologias Qualitativas"] = "GE Metodologias Qualitativas em CS"
rede.gp$afiliacao[rede.gp$afiliacao=="grupo de pesquisa em metodologia qualitativa"] = "GE Metodologias Qualitativas em CS"
rede.gp$afiliacao[rede.gp$afiliacao=="Giars"] = "GIARS"
rede.gp$afiliacao[rede.gp$afiliacao=="Cpeqs"] = "CPEQS"
rede.gp$afiliacao[rede.gp$afiliacao=="Incite"] = "INCITE"
rede.gp$afiliacao[rede.gp$afiliacao=="InCiTe"] = "INCITE"
rede.gp$afiliacao[rede.gp$afiliacao=="Sim Crisp"] = "CRISP"
rede.gp$afiliacao[rede.gp$afiliacao=="CRISP-UFMG"] = "CRISP"
rede.gp$afiliacao[rede.gp$afiliacao=="Centro de Estudos em Criminalidade e Segurança Pública (CRISP)"] = "CRISP"

names(dados)[16]="gp"
dados$gp[dados$gp=="Não"] <- NA
dados$gp[dados$gp=="não"] <- NA
dados$gp[dados$gp=="nao"] <- NA
dados$gp[dados$gp=="Não."] <- NA
dados$gp[dados$gp=="Nao"] <- NA
dados$gp[dados$gp=="Sim"] <- NA
dados$gp[dados$gp=="NÃO"] <- NA
dados$gp[dados$gp=="Grupo de Estudos em Metodologias Qualitativas (nome provisório, sob orientação da professora Yumi)"] = "GE Metodologias Qualitativas em CS"
dados$gp[dados$gp=="Sim. Grupo de estudos em metodologia qualitativa (Profa. Yumi). Grupo parado atualmente."] = "GE Metodologias Qualitativas em CS"
dados$gp[dados$gp=="Grupo de estudos de metodologia qualitativa em sociologia (o grupo da Yumi, não tem nome definido ainda)"] = "GE Metodologias Qualitativas em CS"
dados$gp[dados$gp=="Sim, Grupo de pesquisa de Metodologias Qualitativas"] = "GE Metodologias Qualitativas em CS"
dados$gp[dados$gp=="grupo de pesquisa em metodologia qualitativa"] = "GE Metodologias Qualitativas em CS"
dados$gp[dados$gp=="Giars"] = "GIARS"
dados$gp[dados$gp=="Cpeqs"] = "CPEQS"
dados$gp[dados$gp=="Incite"] = "INCITE"
dados$gp[dados$gp=="InCiTe"] = "INCITE"
dados$gp[dados$gp=="Sim Crisp"] = "CRISP"
dados$gp[dados$gp=="CRISP-UFMG"] = "CRISP"
dados$gp[dados$gp=="Centro de Estudos em Criminalidade e Segurança Pública (CRISP)"] = "CRISP"

########################################################################

#Limpando os nomes
# Limpando acentos e colocando letras minúsculas
rede.gp$nome <- sapply(rede.gp$nome, tolower)
rede.gp$nome <- gsub("ã","a", rede.gp$nome)
rede.gp$nome <- gsub("á","a", rede.gp$nome)
rede.gp$nome <- gsub("é","e", rede.gp$nome)
rede.gp$nome <- gsub("í","i", rede.gp$nome)
rede.gp$nome <- gsub("ó","o", rede.gp$nome)
rede.gp$nome <- gsub("ú","u", rede.gp$nome)

library(tidyr)
rede.gp <- as.data.frame(rede.gp, stringsAsFactors = F)

GP <- rede.gp %>% unnest(afiliacao=strsplit(afiliacao," / "))
#View(GP)
#####################################################################

# Montando a rede
library(igraph)
GP <- as.matrix(GP)
GP <- graph_from_edgelist(GP, directed = F)
type <- bipartite_mapping(GP)$type
V(GP)$type = type
GP <- delete_vertices(GP, "none")
GP <- simplify(GP)

#plot(GP, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3, vertex.color=as.numeric(V(GP)$type)+1)
