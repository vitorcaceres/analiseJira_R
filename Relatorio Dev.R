#Carregando biblitoecas necessárias
rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(data.table)

#Setando diretório
my.dir <- "/Users/vitorcaceres/Desktop/R"
setwd(my.dir)

#Carregando dados
my.database <- read_csv2("DataBaseNov.csv") #Carrega arquivo CSV
my.database$Performance <- my.database$`Σ de Tempo Gasto` / my.database$`Σ da Estimativa Original` #Cria coluna de Performance
my.database$`Registro de Trabalho.updated` <- substr(my.database$`Registro de Trabalho.updated`,0,11) #Extrai a data da coluna de registro de trabalho
my.database$`Registro de Trabalho.updated` <- parse_date_time(my.database$`Registro de Trabalho.updated`, orders = c("ymd", "dmy", "mdy")) #Transforma a data em formato de data

#criando coluna para Dia da Semana, Mês e Ano para ser filtrado depois
my.database$Mes <- month(my.database$`Registro de Trabalho.updated`) #Coluna Mês
my.database$DiaSemana <- weekdays(my.database$`Registro de Trabalho.updated`) #Coluna Dia da Semana
my.database$Ano <- year(my.database$`Registro de Trabalho.updated`) #Coluna Ano

#Criando coluna para verificar se existem duplicados baseado na chave
my.database$duplicado <- duplicated(my.database$Chave)

#Limpando os Dados
my.database <- my.database %>% filter(!grepl('Cenários', Resumo)) %>%
  filter(!grepl('Testes', Resumo)) %>%
  filter(!grepl('Cenários de Testes', Resumo)) %>%
  filter(Responsável!= "Graziela Gomes") %>%
  filter(Responsável!= "Priscilla Souza Alves") %>%
  filter(Responsável!= "Cássio Schroeder") %>%
  filter(Responsável!= "rodrigo freitas") %>%
  filter(Responsável!= "Julianne Glicerio") %>%
  filter(Responsável!= "Vítor Cáceres") %>%
  filter(Responsável!= "Daniel Barnasque") %>%
  filter(Responsável!= "Matheus Martins") %>%
  filter(Responsável!= "Luiza Fajardo Schneider") %>%
  filter(duplicado== "FALSE") # Ao pedir pro jira o worklog.updated, ele puxa um registro para cada dia registrados, portanto podem ter linhas duplicadas

#Criando database filtrado só com tasks com estimativas
my.databasePerf <- filter(my.database, `Σ da Estimativa Original`!=0)

##############################
####  Criando as Funções  ####
##############################

#Performance Mes - Dando diferença entre dentro e fora da função.
#Acredito que esta dando diferença pois no monthlyreport eu utilizo o summarize e nessa opção estou fazendo o mean direto na estimativa.

PerformanceMes <- function(Desenvolvedor, Mes) {
  my.temp_database <- my.databasePerf %>% filter(Ano=="2020") %>%
                                      filter(Mes==Mes) %>%
                                      filter(Responsável==Desenvolvedor)
  output <- mean(my.temp_database$Performance)
 return(output)
}
PerformanceMes("Ana Coimbra", "11")

#Performance Ano - Provavelmente dando diferença entre dentro e fora da função que nem o PerformanceMes
PerformanceAno <- function(Desenvolvedor, Ano) {
  my.temp_database <- my.databasePerf %>% filter(Ano==Ano) %>% 
        filter(Responsável==Desenvolvedor)
  output <- mean(my.temp_database$Performance)
  return(output)
}
PerformanceMes("Ana Coimbra", "2020")

#Grafico de performance Ano - Grafico ta lendo anos que eu não sei daonde
GraficoPerformanceAno <- function(Desenvolvedor, Ano) {
  my.temp_database <- my.databasePerf %>% filter(Ano==Ano) %>% 
                                          filter(Responsável==Desenvolvedor) %>%
                                          select(`Registro de Trabalho.updated`, Performance)
            plot(my.temp_database)
}
GraficoPerformanceAno("Ana Coimbra", "2020")

# #Distribuição Performance Mes e Ano - Rolando algum problema com os histogramas. Buscar se ta tudo completo.
# DistribuicaoDev <- function(Desenvolvedor, Mes, Ano) {
#   my.temp_database <- my.databasePerf %>% filter(Ano==Ano) %>%
#                                           filter(Responsável==Desenvolvedor)
#   output1 <- hist(my.temp_database$Performance)
#   my.temp_database2 <- my.temp_database %>% filter(Mes==Mes)
#   output2 <- hist(my.temp_database2$Performance)
#   return(output1, output2)
# }
# DistribuicaoDev("Thomas Delgado", "11", "2020")

#Lista Atividades Mes - Ver um jeito mais bonito de mostrar a lista

AtividadesMes <- function(Desenvolvedor, Mes) {
  my.temp_database <- my.database %>% filter(Ano=="2020") %>%
    filter(Mes==Mes) %>%
    filter(Responsável==Desenvolvedor) %>%
    select(Chave, `Tipo de item`, Resumo, Performance)
  return(my.temp_database)
  }
AtividadesMes("Ana Coimbra", "11")

#Grafico de Tempo estimado e Logado no Ano - Configrurar certinho o grafico. Tabela esta pronta.
GraficoLogadoAno <- function(Desenvolvedor, Ano) {
  my.temp_database <- my.database %>% filter(Ano==Ano) %>%
    filter(Responsável==Desenvolvedor) %>%
    select(`Σ da Estimativa Original`, `Σ de Tempo Gasto`, Mes, `Registro de Trabalho.updated`)
  plot(my.temp_database$`Σ de Tempo Gasto`)
}
GraficoLogadoAno("Ana Coimbra", "2020")


