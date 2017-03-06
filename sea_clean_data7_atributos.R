# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde
# ATRIBUTOS - rodar apos sea_clean_data.R

library(descr)
library(magrittr)
#names(dados)

# Limpando e transformando variáveis
# SEXO
dados$Sexo %<>% as.factor

# COR/RAÇA
dados[[6]] %<>% as.factor

# TRABALHA FORMALMENTE?
dados[[7]] %<>% as.factor
#freq(dados$`Você trabalha formalmente?`)
#freq(dados$Sexo[dados$`Você trabalha formalmente?`=="Sim"])

# OCUPAÇÃO PRINCIPAL
#freq(dados$ocupacao)
names(dados)[8] <- "ocupacao"
dados$ocupacao[dados$ocupacao=="Doutorando"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="estudante"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Estudante de mestrado"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Estudante de pos graduação"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Estudante de pós graduação"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Estudante doutorado"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Estudante/Mestrado"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Estudante pós"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Mestranda"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Mestranda em Sociologia"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Não estou trabalhando no momento (bolsista)"] <- "Estudante"
dados$ocupacao[dados$ocupacao=="Pesquisadora"] <- "Pesquisador"
dados$ocupacao[dados$ocupacao=="professora"] <- "Professor"
dados$ocupacao[dados$ocupacao=="Professora"] <- "Professor"
dados$ocupacao[dados$ocupacao=="Professora de Educação Básica"] <- "Professor"
dados$ocupacao[dados$ocupacao=="Professor universitário"] <- "Professor"
dados$ocupacao[dados$ocupacao=="servidor público estadual"] <- "Servidor Público"
dados$ocupacao[dados$ocupacao=="Servidor público SUS BH"] <- "Servidor Público"
dados$ocupacao[dados$ocupacao=="Socióloga"] <- "Sociólogo"
dados$ocupacao[dados$ocupacao=="SOCIÓLOGO/ANALISTA"] <- "Sociólogo"
dados$ocupacao <- as.factor(dados$ocupacao)

# SITUACAO ACADEMICA
dados[[10]] %<>% as.factor
#freq(dados$`Qual é a sua situação acadêmica neste momento?`)

# ANO DO CURSO
dados[[11]] %<>% as.factor
#freq(dados$`Em que ano do curso você se encontra?`[dados$`Qual é a sua situação acadêmica neste momento?`=="Doutorando"])

# BOLSISTA
dados[[12]] %<>% as.factor

# Consertando os cursos
names(dados)[21] <- "cursos"
dados$cursos[5] <- 6
