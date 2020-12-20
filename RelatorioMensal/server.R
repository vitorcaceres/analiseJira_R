#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(shiny)
#Setando diretório
my.dir <- "/Users/vitorcaceres/Desktop/R"
setwd(my.dir)

# create the server functions for the dashboard  
server <- function(input, output) { 

#Carregando dados
my.database <- read_csv2("DataBaseNov.csv")#Carrega arquivo CSV
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
    filter(Ano=="2020") %>%
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

#Criando o databasePerf, que servirá para calcuar a performance. O database ficará intacto para calcular o tempo gasto.

my.databasePerf <- filter(my.database, `Σ da Estimativa Original`!=0)

#Ranking médio de Perfoirmance
my.MonthlyReport <- reactive({my.databasePerf %>% filter(Mes==input$InputMes) %>%
    group_by(my.databasePerf, Responsável) %>% 
    summarize(Performance=mean(Performance)) %>%
    arrange(Responsável)
})

#Tempo Gasto total (contando bugs)
my.SumTimeSpent <- reactive({my.database %>% filter(Mes==input$InputMes) %>%
    group_by_(Responsável) %>%
    summarise(HorasLogadas = sum(`Σ de Tempo Gasto`)/3600) %>%
    arrange(Responsável)
})


#Tempo Estimado total
my.SumEstimated <- reactive({my.databasePerf %>% filter(Mes==input$InputMes) %>%
    group_by_(Responsável) %>%
    summarise(EstimadoTotal = sum(`Σ da Estimativa Original`)/3600) %>%
    arrange(Responsável)
})


#Numero de tasks trabalhadas
my.SumIssues <- reactive({my.database %>% filter(Mes==input$InputMes) %>% 
    group_by_(Responsável) %>%
    summarise(Issues=n()) %>%
    arrange(Responsável)
})


#Responsável por história
my.DicionarioHistoria <- my.database %>% filter(`Tipo de item` == "História") %>%
    select(Chave, Responsável)

#Número de sub bugs por história no mes
my.bugsporHist <- reactive({my.database %>% filter(Mes==input$InputMes) %>% 
    filter(`Tipo de item` == "Sub-Bug") %>%
    group_by_(Pai.undefined) %>%
    summarise(Numero=n()) %>%
    rename(Chave = Pai.undefined)
})

#Relacionando o número de subbugs ao Dev

my.DicionarioHistoria2 <- reactive({my.bugsporHist %>% left_join(my.DicionarioHistoria, by="Chave") %>%#Unindo tabelas
                                                        group_by_(Responsável) %>% # Somando sub bugs por responsável
                                                        summarize(BugsGerados = sum(Numero))
})


#Tempo gasto em bugs ou subbugs
Bugs <- c("Sub-Bug", "Bug") #Criando lista do que será filtrado
my.TempoBug <- reactive({my.database %>% filter(Mes==input$InputMes) %>%
    filter(`Tipo de item` %in% Bugs) %>% #Filtro por tipo de item
    group_by_(Responsável) %>% #Agrupar por responsável
    summarise(HorasLogadasBugs=sum(`Σ de Tempo Gasto`)/3600) #Somando o tempo gasto dos itens filtrados
})




#Reactive
my.MonthlyReport <- reactive({my.MonthlyReport %>% filter(Responsável==input$Desenvolvedor) %>%
left_join(my.TempoBug, by="Responsável") %>%#Adicionando na tabela princiapl
left_join(my.DicionarioHistoria2, by="Responsável") %>%#Adicionando na tabela princiapl
left_join(my.SumIssues, by="Responsável") %>%#Adicionando na tabela princiapl
left_join(my.SumEstimated, by="Responsável") %>%#Adicionando na tabela princiapl
left_join(my.SumTimeSpent, by="Responsável")#Adicionando na tabela princiapl
})

#Quais dados devem ser enviados para cada box
output$value1 <- reactive({renderValueBox({
    valueBox(
        formatC(my.MonthlyReport$Performance)
        ,"Performance"
        ,icon = icon("stats",lib='glyphicon')
        ,color = "blue")  
})
})

output$value2 <- reactive({renderValueBox({ 
    valueBox(
        formatC(my.MonthlyReport$HorasLogadas, format="d", big.mark=',')
        ,'Horas Logadas'
        ,icon = icon("pulse",lib='glyphicon')
        ,color = "green")  
})
})
output$value3 <- reactive({renderValueBox({
    valueBox(
        formatC(my.MonthlyReport$EstimadoTotal, format="d", big.mark=',')
        ,"Estimativa Total"
        ,icon = icon("stats",lib='glyphicon')
        ,color = "green")
})
})
output$value4 <- reactive({renderValueBox({    
    valueBox(
        formatC(my.MonthlyReport$Issues)
        ,"Issues Totais"
        ,icon = icon("stats",lib='glyphicon')
        ,color = "yellow")  
})
})
output$value5 <- reactive({renderValueBox({
    valueBox(
        formatC(my.MonthlyReport$BugsGerados, format="d", big.mark=',')
        ,'Bugs Gerados'
        ,icon = icon("pulse",lib='glyphicon')
        ,color = "red")  
})
})
output$value6 <- reactive({renderValueBox({
    valueBox(
        formatC(my.MonthlyReport$HorasLogadasBugs, format="d", big.mark=',')
        ,"Horas trabalhadas em Bugs"
        ,icon = icon("stats",lib='glyphicon')
        ,color = "red")  
})
})

}
