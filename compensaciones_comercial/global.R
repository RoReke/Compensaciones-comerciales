## librerias ----
library(openxlsx)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
require(DT)
library(writexl)
library(shinyalert)
library(ECharts2Shiny)
require(tidyverse)

## funciones ----

functionread.a <-  function(x) {
  z <- paste("data/", mesanio, "/A",x, ".rds", sep ="")
  if(file.exists(z))
    y <-  (readRDS(z))
  
}


functionread.c <- function(x) {
  z <- paste("data/", mesanio, "/C",x, ".rds", sep ="")
  if(file.exists(z))
    y <-  (readRDS(z))
  
}

functionrds.a <-  function(x) {
  z <- paste("data/", mesanio, "/A",x, ".rds", sep ="")
  if(!file.exists(z))
  saveRDS(data %>% filter(Ejecutivo.de.Cuenta == x),z)


}


functionrds.c <-  function(x) {
  z <- paste("data/", mesanio, "/C",x, ".rds", sep ="")
  if(!file.exists(z))
  saveRDS(data1 %>% filter(Ejecutivo.de.Cuenta == x),z)


} # estas funciones sirven para leer la informacion por ejecutivos


'%ni%' <- Negate('%in%')

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

## construccion  del dataframe ----

useShinyalert()
data.compensa.aum.B <- readRDS("data/data.compensa.aum.B.Rds")
data.compensa.sinaum.B <- readRDS("data/data.compensa.sinaum.B.Rds")
mesanio <- format(Sys.Date(),"%m-%Y")
dir.create(file.path(main_dir = "data", sub_dir_new = mesanio),showWarnings = F)


data <- data.compensa.aum.B %>% 
                  mutate (
                     pct_aumento = round((`Vble con aumento`/`Vble Actual`)-1,2),
                    `% aumento original` = pct_aumento,
                     masa_salarial_12m = round(masa_salarial_12m,0),
                     Aumento.pesos = round(masa_salarial_12m*(`Vble Actual`/100)*pct_aumento,0),
                     Vble_Actual = percent(`Vble Actual`/100),
                     `Vble con aumento` = `Vble con aumento`/100,
                     Modificado = "NO",
                     Comentarios = ""
                    ) %>% 
                    rename (
                     `Razón Social` = `Razon Social`,
                     `Grupo Económico` = grupo_economico,
                     `% de Aumento` = pct_aumento,
                     `$ de aumento` = Aumento.pesos)
data1 <- data.compensa.sinaum.B %>% 
                  mutate (
                      pct_aumento = 0, 
                      Aumento.pesos = 0,
                      pct_aumento = round(pct_aumento,2),
                      masa_salarial_12m = round(masa_salarial_12m,0),
                      Aumento.pesos = round(Aumento.pesos,0),
                      Vble_Actual = percent(`Vble Actual`/100),
                      Comentarios = ""
                        ) %>% 
                   rename (
                      `Razón Social` = `Razon Social`,
                      `Grupo Económico` = grupo_economico,
                      `% de Aumento` = pct_aumento,
                      `$ de aumento` = Aumento.pesos )

## ordeno y selecciono las variables 

data <- data %>%
          select(
              Contrato,
              `Razón Social`,
              `Vble Actual`,
               Vble_Actual,
               `Vble con aumento`,
               `% de Aumento`,
               `% aumento original`,
               `$ de aumento`,
                Segmento,
               `Grupo Económico`,
                Ejecutivo.de.Cuenta,
                masa_salarial_12m,
                Organizador,
                Productor,
                Modificado,
                Comentarios
                    )

data1 <- data1 %>% 
            select(
              Contrato,
              `Razón Social`,
              `Vble Actual`,
               Vble_Actual,
              `% de Aumento`,
              `$ de aumento`,
               Segmento,
              `Grupo Económico`,
              `Score Contrato`,
               Ejecutivo.de.Cuenta,
               masa_salarial_12m,
               Organizador,
               Productor,
               mintopefinal,
               Comentarios
                  )
login1 <- c("Ejecutivo", "Asociart01")
login2 <- c("Supervisor", "Geracapo")

## inputs  ----

list = unique(data$Ejecutivo.de.Cuenta)
# guardo datos agrupados por ejecutivo ejecutivos 

sapply(list, functionrds.a) 
sapply(list, functionrds.c) 

## interface para el ejecutivo ----
header <- dashboardHeader(title = "Feedback Comercial")  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
     div(style="text-align: center;",downloadButton("downloadData", "Download")),
     useShinyalert(),
     div(style="text-align: center;", actionButton(inputId = "Updated_trich",label = "Enviar ",width = 100,icon("paper-plane"), 
     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  ) ,width= 130
 )


frow1 <- fluidRow(
  valueBoxOutput("value1"),
  valueBoxOutput("value2")
)

frow3 <- fluidPage(
  tabsetPanel(
    tabPanel("Aumentos", "", DTOutput("aumentos")),
    tabPanel("Compensaciones", "", DTOutput("compensa")),
    type = c("pills")
  )
)

body <- dashboardBody(frow1,frow3)

ui1 <- dashboardPage(title = 'Compensaciones Comerciales', header, sidebar, body, skin='red')

## para el supervisor ----

ecdels1 <- c("CHACON SABRINA","MARENGO JULIETA MARIA","DIEZ JUAN")
header <- dashboardHeader(title = "Feedback Comercial")  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    div(style="text-align: center;",downloadButton("downloadDatas", "Download")),
    useShinyalert(),
    div(style="text-align: center;", actionButton(inputId = "Updated_trichs",label = "Enviar ",width = 100, icon("paper-plane"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  ),width= 130
)

frow1 <- fluidRow(
  valueBoxOutput("value1s")
  ,valueBoxOutput("value2s")
)

frow3 <- fluidPage(
  tabsetPanel(
    tabPanel("Aumentos", "", DTOutput("aumentoss")),
    tabPanel("Compensaciones", "", DTOutput("compensas")),
    type = c("pills")
  )
)
body <- dashboardBody(frow1,frow3)

ui2 <- dashboardPage(title = 'Compensaciones Comerciales', header, sidebar, body, skin='red')

### salida de datos para interface administrador ----

data.model1 <- readRDS("data/model.rds") 
data.model <- data.model1 %>% 
                   select(
                     contrato_id,
                     sucursal_desc,
                     rango_capitas,
                     razon_social,
                     provincia,
                     sector,
                     alicuota_variable,
                     alicuota_modelo,
                     pct_aumento_modelo,
                     pct_aumento,
                     fl_filter_propension
                         )


## interface del administrador ----

headeradmin <- dashboardHeader(title = "Feedback Comercial")  
sidebaradmin <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    div(style="text-align: center;",downloadButton("downloadDataadmin", "Download")),
    div(style="text-align: center;",downloadButton("downloadDataadmin1", "Compensaciones")),
    useShinyalert()
  ),width= 130
)

 frow1 <- fluidRow(
   valueBoxOutput("value1admin"),
   valueBoxOutput("value2admin")
 )

 frow2 <- fluidPage(tabsetPanel(
   tabPanel("Aumentos", "", DTOutput("aumentosadmin")),
   tabPanel("Compensaciones", "", DTOutput("compensaadmin")),
   tabPanel("Q Aumentos",
   loadEChartsLibrary(),
   tags$div(id="test", style="width:40%;height:300px;"),
   deliverChart(div_id = "test")
   ),
    tabPanel("Premio", 
            loadEChartsLibrary(),
            tags$div(id="test1", style="width:50%;height:400px;"),
            deliverChart(div_id = "test1")
   ),type = c("pills")
  )
)

bodyadmin <- dashboardBody(frow1,frow2)

ui3 <- dashboardPage(title = 'Compensaciones Comerciales', headeradmin, sidebaradmin, bodyadmin, skin='red')