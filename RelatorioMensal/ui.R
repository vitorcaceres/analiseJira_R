#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)


#Primeiro criamos os agrupamentos que serão mostrados pela UI. Acredito que sejam pre definidos na biblioteca do shinydashboards.
ui <- dashboardPage( #A Ui será a dashboardpage, que conterá:
    dashboardHeader(), # O que deve vir no header
    dashboardSidebar(),# O que deve vir na barra lateral
    dashboardBody() # O que deve ir no corpo do dashboard.
)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "MVP - Análises Gerenciamento")  #Aplicar Titulo no header

#Sidebar content of the dashboard
sidebar <- dashboardSidebar( #Indica o que terá na barra lateral
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), #Cria menu de dashboard
        menuItem("Visit-us", icon = icon("send",lib='glyphicon'), #Cria botão de visitar
                 href = "https://kobe.io")
    )
)

frow1 <- fluidRow(selectInput("InputDev", "Desenvolvedor", list("Alcides Junior",
                                                                "Alexandre Henrique Cavalcanti",
                                                                "Alice Abreu",
                                                                "Ana Coimbra",
                                                                "Antônio Vidal",
                                                                "Arthur Piccoli",
                                                                "Christy Hauschild Schott",
                                                                "Douglas Gubert",
                                                                "Evandro Terra",
                                                                "Fábio Hideki Lagôa",
                                                                "Felipe Campara Silva",
                                                                "Fernando Donini Ramos",
                                                                "Gabriel Machado",
                                                                "Jonathas Hernandes",
                                                                "Livia Vasconcelos",
                                                                "Luis Filipe Almeida",
                                                                "Marcella Barros",
                                                                "Marcelo Alves",
                                                                "Natanael Fernandes Toscano Araujo",
                                                                "Pedro Lucas de Oliveira Cabral",
                                                                "Raphael Sousa",
                                                                "Tauã Silva Rodrigues",
                                                                "thomas delgado",
                                                                "Victor Leal",
                                                                "Willian Anderson de Almeida")),
                  textOutput("Desenvolvedor"),
                  selectInput("InputMes", "Mês", list("1","2", "3","4","5","6","7","8","9","10","11","12")),
                  textOutput("Mes")
                  )

frow2 <- fluidRow(#Aplicar na primeira linha
    valueBoxOutput("value1") #Uma caixa de indicador que receberá o value1
    ,valueBoxOutput("value2")
    ,valueBoxOutput("value3")
    ,valueBoxOutput("value4") 
    ,valueBoxOutput("value5")
    ,valueBoxOutput("value6")
)

# frow3 <- fluidRow(  #Na segunda linha fluida, adiciona duas caixas:
#     box(
#         title = "Revenue per Account"
#         ,status = "primary"
#         ,solidHeader = TRUE 
#         ,collapsible = TRUE 
#         ,plotOutput("revenuebyPrd", height = "300px")
#     )
#     ,box(
#         title = "Revenue per Product"
#         ,status = "primary"
#         ,solidHeader = TRUE 
#         ,collapsible = TRUE 
#         ,plotOutput("revenuebyRegion", height = "300px")
#     ) 
# )
# torna o body do dashboard as duas linhas fluidas
body <-dashboardBody(frow1, frow2)

ui <- dashboardPage(title = 'MVP - Análises Gerenciamento', header, sidebar, body, skin='blue')

