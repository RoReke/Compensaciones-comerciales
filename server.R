server <- function(input, output, session) { 
  ## logica de log in ----
  
  logged <- reactiveValues(logged = FALSE, pas ="")
  ejecutivo <- reactive({ 
    ejecutivo = "JUAN DIEZ"
    if(!is.null(input$ejecutivo)) 
    ejecutivo = input$ejecutivo               
    return(ejecutivo)
    })
  
  observeEvent(input$signin, {
    if( input$pw == "1") {
      logged$logged <- TRUE
      logged$pas <- "1"
    } else if (input$pw == "Bona99") {
      logged$logged <- TRUE
      logged$pas <- "Bona99"
    } else if (input$pw == "ChaconElli99") {
      logged$logged <- TRUE
      logged$pas <- "ChaconElli99"
    } else if (input$pw == "Blasco99") {
      logged$logged <- TRUE
      logged$pas <- "Blasco99"
    } else if (input$pw == "Tenorio99") {
      logged$logged <- TRUE
      logged$pas <- "Tenorio99"
    } else if (input$pw == "admin") {
        logged$logged <- TRUE
        logged$pas <- "admin"
    } else {}
  })
  ## logica de ingreso segun clase: ejecutivo, supervisor o administrador ----
    output$ui <- renderUI({
        if(logged$logged == FALSE) {
          return(
            shinyUI(fluidPage(
              sidebarPanel(
              selectInput("ejecutivo","Seleccionar", choices = c(sort(c(unique(data$Ejecutivo.de.Cuenta),"0.Admin","1.Supervisor")))),
              passwordInput("pw", "Contraseña"),
              actionButton("signin", "Ingresar")
            ), mainPanel(img(src='feedback.jpg',height = 400, weight = 400)))))
        } else if(logged$logged == TRUE & logged$pas == "1") {
              return(
                ui1
                )
              
        } else if(logged$logged == TRUE & logged$pas == "Bona99" | logged$pas =="ChaconElli99"| logged$pas =="Blasco99"| logged$pas =="Tenorio99") {
               return(
                ui2
                  )
        } else if(logged$logged == TRUE & logged$pas == "admin") {
                return(
                ui3
                )  
  
        } else {}
      })
    

  ## lectura de archivos ----
  dfa <- reactive({ 
          z <- paste("data/", mesanio, "/A",ejecutivo(), ".rds", sep ="")
          a <- data %>% filter(Ejecutivo.de.Cuenta == ejecutivo())
          if(file.exists(z))
          a <-  (readRDS(z))
          return(a) 
          })
  
  observe({
    x$df <- dfa()
    })

  x <- reactiveValues(
    df = data
  )
  
  dfa1 <- reactive({
    z <- paste("data/", mesanio, "/C",ejecutivo(), ".rds", sep ="")
    a <- data1 %>% filter(Ejecutivo.de.Cuenta == ejecutivo())
    if(file.exists(z))
      a <-  (readRDS(z))
    return(a) 
  })
  
  observe({
    y$df <- dfa1()
    
  })
  
  y <- reactiveValues(
    df = data1
  )
  
  ## visualizacion de aumentos ----
  proxy0 = DT::dataTableProxy("aumentos")
  output$aumentos = DT::renderDT({
      datatable(
          x$df %>% 
            filter(Ejecutivo.de.Cuenta == ejecutivo()),
          selection = 'none', editable = TRUE,
          options = list(pageLength = 100, scrollX = TRUE,scrollY = "500px",
          columnDefs = list(list(visible=FALSE, targets=c(0,3,11,12,15))),
          initComplete = JS("function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"))
          )%>%
          formatStyle('% de Aumento',  color = 'black', backgroundColor = 'orange', 
                      fontWeight = 900,textAlign = 'center') %>%
          formatPercentage('% aumento original', 2) %>%
          formatPercentage('% de Aumento', 2) %>% 
          formatPercentage('Vble con aumento', 2) %>%
          formatCurrency('$ de aumento') %>% 
          formatStyle(
            'Modificado',
            target = 'row',
            backgroundColor = styleEqual(c("SÍ", "NO"), c('silver', 'white'))) %>% 
            formatStyle(columns = colnames(data), fontSize = '120%')
        })
  ## logica de edicion de aumentos por los usuarios ----
 
   observeEvent(input$aumentos_cell_edit, {
    info = input$aumentos_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    isolate(
      if (j %in% match(c("Comentarios"), names(x$df))) {
        print(match(c("Comentarios"), names(x$df)))
        x$df[i, j]  <<- DT::coerceValue(v, x$df[i, j])
      } else {
      if (is.na(as.numeric(v))) {showNotification("Sólo se permiten valores numéricos", type = "warning")
      } else {
      if (as.numeric(v) > x$df[i,"% aumento original"] & j %in% match(c("% de Aumento"), names(x$df))) {
        showNotification(paste("No superar el % de aumento", x$df[i,"% aumento original"]),type = "warning") 
      } else {
      if (j %in% match(c("% de Aumento"), names(x$df))) {
        print(match(c("% de Aumento"), names(x$df)))

        x$df[i, j]  <<- DT::coerceValue(v, x$df[i, j])
        x$df$`$ de aumento` <- round(x$df$`% de Aumento` * x$df$masa_salarial_12m * (x$df$`Vble Actual`)/100,0)
        x$df$Modificado = if_else(x$df$`% de Aumento` < x$df$`% aumento original`, "SÍ", "NO")

      } else { showNotification("No cambiar esta columna",type = "warning")}
      }}
    })
    replaceData(proxy0, x$df, resetPaging = FALSE, clearSelection = FALSE)  

  })

  ## logica de edicion de compensaciones por los usuarios ----

  proxy1 = DT::dataTableProxy("compensa")
  output$compensa = DT::renderDT({
                      datatable(y$df %>% 
                                  filter(Ejecutivo.de.Cuenta == ejecutivo()),
                       selection = 'none', editable = TRUE,
                       options = list(pageLength = 100, scrollX = TRUE,scrollY = "500px",
                       columnDefs = list(list(visible=FALSE, targets=c(0,3,10,11,14))),
                       initComplete = JS("function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                            "}")))%>%
                       formatStyle('% de Aumento',  color = 'black', backgroundColor = 'lightblue', fontWeight = 900, textAlign = 'center') %>%
                       formatPercentage('% de Aumento', 2) %>%
                       formatCurrency('$ de aumento') %>% 
                       formatStyle(columns = colnames(data1), fontSize = '120%')})

  observeEvent(input$compensa_cell_edit, {
    info = input$compensa_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value

    isolate(
      if (j %in% match(c("Comentarios"), names(y$df))) {
        print(match(c("Comentarios"), names(y$df)))
        y$df[i, j]  <<- DT::coerceValue(v, y$df[i, j])
      } else {
      if (is.na(as.numeric(v))) {showNotification("Sólo se permiten valores numéricos", type = "warning")
      } else {
      if ((as.numeric(v) + 1 )*y$df[i,"Vble Actual"] > y$df[i,"mintopefinal"] & j %in% match(c("% de Aumento"), names(y$df))) {
        showNotification(paste("El aumento máximo es %", (round((y$df[i,"mintopefinal"]/y$df[i,"Vble Actual"]-1)*100 ,2)))
                         , type = "warning")
      } else {
      if (j %in% match(c("% de Aumento"), names(y$df))) {
          print(match(c("% de Aumento"), names(y$df)))
          y$df[i, j]  <<- DT::coerceValue(v, y$df[i, j])
          y$df$`$ de aumento` <- round(y$df$`% de Aumento` * y$df$masa_salarial_12m * (y$df$`Vble Actual`)/100,0)
      } else { showNotification("No cambiar esta columna",type = "warning")}
      }}
    })
    replaceData(proxy1, y$df, resetPaging = FALSE, clearSelection = FALSE)
  })

  
  ## Indicadores: logica ----
  
  dfinput <- reactive({
    a <-  data %>%
    filter (Ejecutivo.de.Cuenta == ejecutivo())
    total.aumento <- sum(a$`$ de aumento`)
  })


  dfinput1 <- reactive({ 
    D <- y$df %>%
    filter (Ejecutivo.de.Cuenta == ejecutivo())
    total.compensa <- sum(D$`$ de aumento`)
  })

  dfinput2 <- reactive({ 
    b <- x$df %>%
    filter (Ejecutivo.de.Cuenta == ejecutivo())
    total.a.compensar <- sum(b$`$ de aumento`)
  })

  dfinput3 <- reactive({ 
    c <- dfinput()-dfinput1()-dfinput2()
  })

  output$value1 <- renderValueBox({
    df1 <- paste("$",prettyNum(dfinput(),big.mark=",",scientific=FALSE))
    valueBox(
      formatC(df1, format="d", big.mark=',')
      ,'Total Aumento'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
  })

  output$value2 <- renderValueBox({
    df2 <-paste("$",prettyNum(dfinput3(),big.mark=",",scientific=FALSE))
    if(dfinput3() > 0) { valueBox(
      formatC(df2, format="d", big.mark=',')
      ,'Total a Compensar'
      ,icon = icon("thumbs-down",lib='glyphicon')
      ,color = "red")
   } else {
     valueBox(
       formatC(df2, format="d", big.mark=',')
       ,'Total a Compensar'
       ,icon = icon("thumbs-up",lib='glyphicon')
       ,color = "green")
  }})


  ## logica de descarga y guardado ----
  data_list <- reactive({
    list(
      Aumentos = x$df %>% filter (Ejecutivo.de.Cuenta == ejecutivo()),
      Compensaciones = y$df %>% filter (Ejecutivo.de.Cuenta == ejecutivo())
    )
  })

  output$downloadData <- downloadHandler(
    filename = function()    { paste(ejecutivo(), ".xlsx", sep = "")},
    content = function(file) {write_xlsx(data_list(), path = file)}
  )

  
  observeEvent(input$Updated_trich, {
    saveRDS(x$df, paste("data/", mesanio, "/A",ejecutivo(), ".rds", sep =""))
    saveRDS(y$df, paste("data/",mesanio,"/C",ejecutivo(), ".rds", sep =""))
    shinyalert(title = "Enviado!", type = "success")
  })


### interface para el supervisor ----
  
  dfas <- reactive({
    lists <- "SIGNORELLI, DARIO GABRIEL"
     if(logged$pas== "Bona99")
       lists = c("SANTOS MAXIMILIANO ARIEL","AVILA ROBERTO","BARETTO ANA SILVINA","GIULIANO MARTIN")
     if(logged$pas== "ChaconElli99")
       lists = c("ARTIME NATALIA","LIMA ANTONIO","MANCUSI GRISEL","MONTES DE OCA MATIAS","NARVAEZ MARCELA","PRESTA LEONARDO"
                 ,"PUGLIESE SEBASTIAN","SATELIER LORENA","SIGNORELLI, DARIO GABRIEL","TABOADA JAVIER PABLO",
                 "ULSAMER GUINDER BRUNO","ZAVAGLIA PABLO ARIEL","CASTELLO ESTELA","CHACON SABRINA","DIEZ JUAN",
                 "DOMINGUEZ WALTER","LASTRA EMANUEL","FANJUL RODRIGO","PADIN MARIA CRISTINA")
     if(logged$pas== "Blasco99")
       lists = c("PENA FERNANDO DANIEL","RAIMONDI HECTOR GABRIEL","ROLLS ALEJANDRO RENE","TORRES CAROLA NYDIA",
     "GOMEZ JUAN MANUEL","MARENGO JULIETA MARIA","MOYANO ADRIANA DELFINA","NEVILLE RODOLFO","CRISTALDO JAVIER")
     if(logged$pas== "Tenorio99")
       lists = c("CALVIGIONI NICOLAS", "CHIO GUSTAVO")
    ejecutivos.list <- lapply(lists, functionread.a)
    ejecutivos.df.aum <- do.call(rbind.data.frame, ejecutivos.list)  
    return(ejecutivos.df.aum)
  })
  
  observe({
    xs$df <- dfas()
    
  }) 
  
  
  xs <- reactiveValues(
    df = data
  )
  
  
  proxys = DT::dataTableProxy("aumentoss")
  
  output$aumentoss = DT::renderDT({datatable(xs$df ,
                                            selection = 'none', editable = TRUE, 
                                            options = list(autoWidth = TRUE,
                                                           
                                                           pageLength = 100, scrollX = TRUE,scrollY = "500px",
                                                           columnDefs = list(list(visible=FALSE, targets=c(0,3,12,14))),
                                                           initComplete = JS("function(settings, json) {",
                                                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                             "}")))%>%
      formatStyle('% de Aumento',  color = 'black', backgroundColor = 'orange', fontWeight = 900,textAlign = 'center') %>% 
      formatPercentage('% de Aumento', 2) %>% 
      formatPercentage('% aumento original', 2) %>%
      formatPercentage('Vble con aumento', 2) %>% 
      formatCurrency('$ de aumento')%>% 
      formatStyle(
        'Modificado',
        target = 'row',
        backgroundColor = styleEqual(c("SÍ", "NO"), c('silver', 'white'))) %>% 
      formatStyle(columns = colnames(data), fontSize = '120%')
    })
  
  
  
  observeEvent(input$aumentoss_cell_edit, {
    
    
    info = input$aumentoss_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    
    isolate(
      if (j %in% match(c("Comentarios"), names(xs$df))) {
        print(match(c("Comentarios"), names(xs$df)))
        
        xs$df[i, j]  <<- DT::coerceValue(v, xs$df[i, j])
      } else {
      if (is.na(as.numeric(v))) {showNotification("Sólo se permiten valores numéricos", type = "warning")
      } else {
        if (as.numeric(v) > xs$df[i,"% aumento original"] & j %in% match(c("% de Aumento"), names(xs$df))) {
          showNotification(paste("No superar el % de aumento", xs$df[i,"% aumento original"]),type = "warning") # check to stop the user from editing only few columns
        } else {
          if (j %in% match(c("% de Aumento"), names(xs$df))) {
            print(match(c("% de Aumento"), names(xs$df)))
            
            xs$df[i, j]  <<- DT::coerceValue(v, xs$df[i, j])
            
            
            xs$df$`$ de aumento` <- round(xs$df$`% de Aumento` * xs$df$masa_salarial_12m * (xs$df$`Vble Actual`)/100,0)
            xs$df$Modificado = if_else(xs$df$`% de Aumento` < xs$df$`% aumento original`, "SÍ", "NO")
            
          } else { showNotification("No cambiar esta columna",type = "warning")}
        }}})
    replaceData(proxys, xs$df, resetPaging = FALSE, clearSelection = FALSE)  # replaces data displayed by the updated table
    
  })
  
  
  dfa1s <- reactive({
    
    lists <- "SIGNORELLI, DARIO GABRIEL"
    if(logged$pas== "Bona99")
      lists = c("SANTOS MAXIMILIANO ARIEL","AVILA ROBERTO","BARETTO ANA SILVINA","GIULIANO MARTIN")
    if(logged$pas== "ChaconElli99")
      lists = c("ARTIME NATALIA","LIMA ANTONIO","MANCUSI GRISEL","MONTES DE OCA MATIAS","NARVAEZ MARCELA","PRESTA LEONARDO"
                ,"PUGLIESE SEBASTIAN","SATELIER LORENA","SIGNORELLI, DARIO GABRIEL","TABOADA JAVIER PABLO",
                "ULSAMER GUINDER BRUNO","ZAVAGLIA PABLO ARIEL","CASTELLO ESTELA","CHACON SABRINA","DIEZ JUAN",
                "DOMINGUEZ WALTER","LASTRA EMANUEL","FANJUL RODRIGO","PADIN MARIA CRISTINA")
    if(logged$pas== "Blasco99")
      lists = c("PENA FERNANDO DANIEL","RAIMONDI HECTOR GABRIEL","ROLLS ALEJANDRO RENE","TORRES CAROLA NYDIA",
                "GOMEZ JUAN MANUEL","MARENGO JULIETA MARIA","MOYANO ADRIANA DELFINA","NEVILLE RODOLFO","CRISTALDO JAVIER")
    if(logged$pas== "Tenorio99")
      lists = c("CALVIGIONI NICOLAS", "CHIO GUSTAVO")
    ejecutivos.list <- lapply(lists, functionread.c)
    ejecutivos.df.aum <- do.call(rbind.data.frame, ejecutivos.list)  
    return(ejecutivos.df.aum)
  })
  
  observe({
    ys$df <- dfa1s()
    
  }) 
  
  ys <- reactiveValues(
    df = data1
  )
  #y = reactiveValues(df = data1)
  proxy1s = DT::dataTableProxy("compensas")
  
  
  output$compensas = DT::renderDT({datatable(ys$df,
                                            selection = 'none', editable = TRUE, 
                                            options = list(pageLength = 100, scrollX = TRUE,scrollY = "500px",
                                                           columnDefs = list(list(visible=FALSE, targets=c(0,3,11,14))),
                                                           initComplete = JS("function(settings, json) {",
                                                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                             "}")))%>%
      formatStyle('% de Aumento',  color = 'black', backgroundColor = 'lightblue', fontWeight = 900, textAlign = 'center') %>% 
      formatPercentage('% de Aumento', 2) %>% 
      formatCurrency('$ de aumento') %>% 
      formatStyle(columns = colnames(data1), fontSize = '120%')
    })
  
  
  
  
  
  observeEvent(input$compensas_cell_edit, {
    info = input$compensas_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    isolate(
      if (j %in% match(c("Comentarios"), names(ys$df))) {
        print(match(c("Comentarios"), names(ys$df)))
        
        ys$df[i, j]  <<- DT::coerceValue(v, ys$df[i, j])
      } else {
      
      if (is.na(as.numeric(v))) {showNotification("Sólo se permiten valores numéricos", type = "warning")
      } else {
        if ((as.numeric(v) + 1 )*ys$df[i,"Vble Actual"] > ys$df[i,"mintopefinal"] & j %in% match(c("% de Aumento"), names(ys$df))) {
          showNotification(paste("El aumento máximo es %", (round((ys$df[i,"mintopefinal"]/ys$df[i,"Vble Actual"]-1)*100 ,2)))
                           , type = "warning") 
        } else {
          if (j %in% match(c("% de Aumento"), names(ys$df))) {
            print(match(c("% de Aumento"), names(ys$df)))
            
            ys$df[i, j]  <<- DT::coerceValue(v, ys$df[i, j])
            
            
            ys$df$`$ de aumento` <- round(ys$df$`% de Aumento` * ys$df$masa_salarial_12m * (ys$df$`Vble Actual`)/100,0)
            
            
          } else { showNotification("No cambiar esta columna",type = "warning")}
        }}})
    replaceData(proxy1s, ys$df, resetPaging = FALSE, clearSelection = FALSE)  # replaces data displayed by the updated table
    
  })
  
  
  #some data manipulation to derive the values of KPI boxes
  dfinputs <- reactive({ a <-  data %>% filter (Ejecutivo.de.Cuenta %in% xs$df$Ejecutivo.de.Cuenta)
  total.aumento <- sum(a$`$ de aumento`)  
  })
  
  
  dfinput1s <- reactive({ D <- ys$df  
  total.compensa <- sum(D$`$ de aumento`)   
  })
  
  dfinput2s <- reactive({ b <- xs$df 
  total.a.compensar <- sum(b$`$ de aumento`)  
  })
  
  dfinput3s <- reactive({ c <- dfinputs()-dfinput1s()-dfinput2s()
  })
  
  #creating the valueBoxOutput content
  output$value1s <- renderValueBox({
    df1s <- paste("$",prettyNum(dfinputs(),big.mark=",",scientific=FALSE))
    valueBox(
      formatC(df1s, format="d", big.mark=',')
      ,'Total Aumento'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
    
    
  })
  
  
  
  output$value2s <- renderValueBox({
    df2s <-paste("$",prettyNum(dfinput3s(),big.mark=",",scientific=FALSE))
    if(dfinput3s() > 0) { valueBox( 
      formatC(df2s, format="d", big.mark=',')
      ,'Total a Compensar'
      ,icon = icon("thumbs-down",lib='glyphicon')
      ,color = "red")
    } else {
      valueBox( 
        formatC(df2s, format="d", big.mark=',')
        ,'Total a Compensar'
        ,icon = icon("thumbs-up",lib='glyphicon')
        ,color = "green")
    }})
  
  
 
  data_lists <- reactive({
    list(
      Aumentos = xs$df ,
      Compensaciones = ys$df 
    )
  })
  
  output$downloadDatas <- downloadHandler(
    filename = function()    { paste(ejecutivo(), ".xlsx", sep = "")},
    content = function(file) {write_xlsx(data_lists(), path = file)}
  )
  
  
  observeEvent(input$Updated_trichs, {
    functionrdsss.a <-  function(x) {
      z <- paste("data/", mesanio, "/A",x, ".rds", sep ="")
      
      saveRDS(xs$df %>% filter(Ejecutivo.de.Cuenta == x),z)
      
      
    }
    functionrdsss.c <-  function(x) {
      z <- paste("data/", mesanio, "/C",x, ".rds", sep ="")
      
      saveRDS(ys$df %>% filter(Ejecutivo.de.Cuenta == x),z)
      
      
    }
    
 
    lists <- "SIGNORELLI, DARIO GABRIEL"
    
    if(logged$pas== "Bona99")
      lists = c("SANTOS MAXIMILIANO ARIEL","AVILA ROBERTO","BARETTO ANA SILVINA","GIULIANO MARTIN")
    if(logged$pas== "ChaconElli99")
      lists = c("ARTIME NATALIA","LIMA ANTONIO","MANCUSI GRISEL","MONTES DE OCA MATIAS","NARVAEZ MARCELA","PRESTA LEONARDO"
                ,"PUGLIESE SEBASTIAN","SATELIER LORENA","SIGNORELLI, DARIO GABRIEL","TABOADA JAVIER PABLO",
                "ULSAMER GUINDER BRUNO","ZAVAGLIA PABLO ARIEL","CASTELLO ESTELA","CHACON SABRINA","DIEZ JUAN",
                "DOMINGUEZ WALTER","LASTRA EMANUEL","FANJUL RODRIGO","PADIN MARIA CRISTINA")
    if(logged$pas== "Blasco99")
      lists = c("PENA FERNANDO DANIEL","RAIMONDI HECTOR GABRIEL","ROLLS ALEJANDRO RENE","TORRES CAROLA NYDIA",
                "GOMEZ JUAN MANUEL","MARENGO JULIETA MARIA","MOYANO ADRIANA DELFINA","NEVILLE RODOLFO","CRISTALDO JAVIER")
    if(logged$pas== "Tenorio99")
      lists = c("CALVIGIONI NICOLAS", "CHIO GUSTAVO")
    sapply(lists, functionrdsss.a)
    sapply(lists, functionrdsss.c)
    shinyalert(title = "Enviado!", type = "success")
  })
 
  
  ### salida de datos para interface administrador ----
  
  
  ejecutivos.list.a <- reactive({ 
            ejecutivos.list <- lapply(list, functionread.a)
            ejecutivos.df.aum <- do.call(rbind.data.frame, ejecutivos.list)  
            return(ejecutivos.df.aum)
  })    
  
  
  ejecutivos.list.c <- reactive({ 
            ejecutivos.list <-lapply(list, functionread.c)
            ejecutivos.df.comp <- do.call(rbind.data.frame, ejecutivos.list)
            return(ejecutivos.df.comp)
    })
  
  ejecutivoscambios <- reactive({                                              
            ejecutivos.list.a <- xa$df %>% select(Contrato,`% de Aumento`)
            ejecutivos.list.c <- ya$df %>% select(Contrato,`% de Aumento`)
            ejecutivoscambios <- rbind (ejecutivos.list.a, ejecutivos.list.c)
            return(ejecutivoscambios)     
  })
  
  ejecutivos.con.model <- reactive({ 
           model <- data.model %>% select(contrato_id,
                                          pct_aumento) %>%
                                   filter (contrato_id %ni% ejecutivoscambios()$Contrato) %>%
             rename(
               Contrato = contrato_id,
               `% de Aumento` =pct_aumento
             ) %>% rbind(ejecutivoscambios())
           model1 <- left_join(data.model,model, by = c("contrato_id" = "Contrato"))  %>% mutate (
                                      `Alicuota Final` = alicuota_variable * (1 + `% de Aumento` )) %>% 
                                                        select(
                                                             contrato_id,
                                                             sucursal_desc,
                                                             rango_capitas,
                                                             razon_social,
                                                             provincia,
                                                             sector,
                                                             Mes_Posible_Aumento,
                                                             alicuota_variable,
                                                             alicuota_modelo,
                                                             pct_aumento_modelo,
                                                             `Alicuota Final`,
                                                             `% de Aumento`,
                                                             fl_filter_propension) %>% rename(
                                                               Contrato = contrato_id,
                                                               Agencia =  sucursal_desc,
                                                               Segmento = rango_capitas,
                                                               `Razon Social` = razon_social,
                                                               Provincia =provincia,
                                                               Sector =sector,
                                                               `Alicuota Actual` = alicuota_variable,
                                                               `Alicuota Modelo` = alicuota_modelo,
                                                               `Aumento PreReglas` = pct_aumento_modelo,
                                                               `Alicuota Final` = `Alicuota Final`,
                                                               `Aumento PostReglas` = `% de Aumento`,
                                                                fl_propension =fl_filter_propension)
                                                            
                                                               return(model1)
  })

  output$downloadDataadmin <- downloadHandler(
    filename = function()    { paste("Salida final", ".xlsx", sep = "")},
    content = function(file) {write_xlsx(ejecutivos.con.model(), path = file)}
  )
  
  output$downloadDataadmin1 <- downloadHandler(
    filename = function()    { paste("lista de contratos compensados", ".xlsx", sep = "")},
    content = function(file) {write_xlsx(ya$df %>% select (Contrato), path = file)}
  )
  
  
  dfinputadmin <- reactive({ a <-  data 
  total.aumento <- sum(a$`$ de aumento`)  
  return(total.aumento)
  })
  output$value1admin <- renderValueBox({
    df1s <- paste("$",prettyNum(dfinputadmin(),big.mark=",",scientific=FALSE))
    valueBox(
      formatC(df1s, format="d", big.mark=',')
      ,'Total Aumento'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
    
    
    
  })
  
  
  
  data.con.cambios <- reactive({ 
    model <- data %>% select(Contrato,
                            `% de Aumento`) %>%
      filter (Contrato %ni% ejecutivoscambios()$Contrato)%>%
   rbind(ejecutivoscambios())
    model1 <- left_join(model, data.model1 %>% select(contrato_id,alicuota_variable,
                                                      masa_salarial_12m),
                        by = c("Contrato" = "contrato_id"))  %>% 
                        mutate (aumentos = masa_salarial_12m* (alicuota_variable/100)*`% de Aumento` )
    z <- sum(model1$aumentos)
    zz <- round(dfinputadmin() - z,2)
    return(zz)
      })
  
  dfinputzadmin<-  reactive({ a <-  data %>% filter (Contrato %in% xa$df$Contrato)
  total.aumento <- sum(a$`$ de aumento`)  
  })
  dfinput1admin <- reactive({ D <- ya$df  
  total.compensa <- sum(D$`$ de aumento`)   
  })
  
  dfinput2admin <- reactive({ b <- xa$df 
  total.a.compensar <- sum(b$`$ de aumento`)  
  })
  
  dfinput3admin <- reactive({ c <- dfinputzadmin() -dfinput1admin()-dfinput2admin()
  })
  
  output$value2admin <- renderValueBox({
    df2s <-paste("$",prettyNum(dfinput3admin(),big.mark=",",scientific=FALSE))
    if(dfinput3admin() > 0) { valueBox( 
      formatC(df2s, format="d", big.mark=',')
      ,'Total a Compensar'
      ,icon = icon("thumbs-down",lib='glyphicon')
      ,color = "red")
    } else {
      valueBox( 
        formatC(df2s, format="d", big.mark=',')
        ,'Total a Compensar'
        ,icon = icon("thumbs-up",lib='glyphicon')
        ,color = "green")
    }})

q.aumentos.modificados <- reactive({  z <- left_join(ejecutivos.list.a(),
                                         data, by = "Contrato") %>% mutate (
                                           cantidad = if_else(`% de Aumento.y` > `% de Aumento.x`, 1, 0 ))
                                      zz <- sum(z$cantidad)
                                      return(zz)})

q.aumento <-     reactive({  z <- nrow(data)    
                                 return(z)}) 

q.compensaciones.modificados <- reactive({  z <- ejecutivos.list.c() %>% 
                                                      mutate (
                                                      cantidad = if_else(`% de Aumento` > 0 , 1, 0 ))
                                        zz <- sum(z$cantidad)
                                        return(zz)
                      
})



dat <- reactive({ 
         dat <-
         c(rep("Q Aumentos sin modificar", q.aumento()-q.aumentos.modificados()),
         rep("Q modificados", q.aumentos.modificados()))
         return(dat)
})

renderPieChart(div_id = "test",
               data = dat())




dfaadmin <- reactive({ 
  dfaadmin <- ejecutivos.list.a() %>% filter (Modificado == "SÍ")
  return(dfaadmin)
  }) 
  
  observe({
    xa$df <- dfaadmin()
    
  }) 


xa <- reactiveValues(
  df = data
)
proxyadmin = DT::dataTableProxy("aumentosadmin")
output$aumentosadmin = DT::renderDT({datatable(xa$df ,
                                          selection = 'none', editable = TRUE,
                                          options = list(pageLength = 100, scrollX = TRUE,scrollY = "500px",
                                                         columnDefs = list(list(visible=FALSE, targets=c(0,3,12))),
                                                         initComplete = JS("function(settings, json) {",
                                                                           "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                           "}")))%>%
    formatStyle('% de Aumento',  color = 'black', backgroundColor = 'orange', 
                fontWeight = 900,textAlign = 'center') %>%
    formatPercentage('% aumento original', 2) %>%
    formatPercentage('% de Aumento', 2) %>% 
    formatPercentage('Vble con aumento', 2) %>%
    formatCurrency('$ de aumento') %>% 
    formatStyle(
      'Modificado',
      target = 'row',
      backgroundColor = styleEqual(c("SÍ", "NO"), c('silver', 'white'))) %>% 
      formatStyle(columns = colnames(data), fontSize = '120%')
})

observeEvent(input$aumentosadmin_cell_edit, {
  
  
  info = input$aumentosadmin_cell_edit
  str(info)
  i = info$row
  j = info$col
  v = info$value
  
  
  isolate(
    if (j %in% match(c("Comentarios"), names(xa$df))) {
      print(match(c("Comentarios"), names(xa$df)))
      
      xa$df[i, j]  <<- DT::coerceValue(v, xa$df[i, j])
    } else {
      if (is.na(as.numeric(v))) {showNotification("Sólo se permiten valores numéricos", type = "warning")
      } else {
        if (as.numeric(v) > xa$df[i,"% aumento original"] & j %in% match(c("% de Aumento"), names(xa$df))) {
          showNotification(paste("No superar el % de aumento", xa$df[i,"% aumento original"]),type = "warning") # check to stop the user from editing only few columns
        } else {
          if (j %in% match(c("% de Aumento"), names(xa$df))) {
            print(match(c("% de Aumento"), names(xa$df)))
            
            xa$df[i, j]  <<- DT::coerceValue(v, xa$df[i, j])
            
            
            xa$df$`$ de aumento` <- round(xa$df$`% de Aumento` * xa$df$masa_salarial_12m * (xa$df$`Vble Actual`)/100,0)
            xa$df$Modificado = if_else(xa$df$`% de Aumento` < xa$df$`% aumento original`, "SÍ", "NO")
            
          } else { showNotification("No cambiar esta columna",type = "warning")}
        }}})
  replaceData(proxyadmin, xa$df, resetPaging = FALSE, clearSelection = FALSE)  # replaces data displayed by the updated table
  
})

dfcadmin <- reactive({ 
  dfcadmin <- ejecutivos.list.c() %>% filter(`% de Aumento` > 0)
  return(dfcadmin)
}) 

observe({
  ya$df <- dfcadmin()
  
}) 


ya <- reactiveValues(
  df = data1
)

proxy1admin = DT::dataTableProxy("compensaadmin")
output$compensaadmin = DT::renderDT({datatable(ya$df ,
                                          selection = 'none', editable = TRUE,
                                          options = list(pageLength = 100, scrollX = TRUE,scrollY = "500px",
                                                         columnDefs = list(list(visible=FALSE, targets=c(0,3,11,14))),
                                                         initComplete = JS("function(settings, json) {",
                                                                           "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                           "}")))%>%
    formatStyle('% de Aumento',  color = 'black', backgroundColor = 'lightblue', fontWeight = 900, textAlign = 'center') %>%
    formatPercentage('% de Aumento', 2) %>%
    formatCurrency('$ de aumento') %>% 
    formatStyle(columns = colnames(data1), fontSize = '120%')
  })

observeEvent(input$compensaadmin_cell_edit, {
  info = input$compensaadmin_cell_edit
  str(info)
  i = info$row
  j = info$col
  v = info$value
  
  isolate(
    if (j %in% match(c("Comentarios"), names(ya$df))) {
      print(match(c("Comentarios"), names(ya$df)))
      
      ya$df[i, j]  <<- DT::coerceValue(v, ya$df[i, j])
    } else {
      
      if (is.na(as.numeric(v))) {showNotification("Sólo se permiten valores numéricos", type = "warning")
      } else {
        if ((as.numeric(v) + 1 )*ya$df[i,"Vble Actual"] > ya$df[i,"mintopefinal"] & j %in% match(c("% de Aumento"), names(ya$df))) {
          showNotification(paste("El aumento máximo es %", (round((ya$df[i,"mintopefinal"]/ya$df[i,"Vble Actual"]-1)*100 ,2)))
                           , type = "warning") 
        } else {
          if (j %in% match(c("% de Aumento"), names(ya$df))) {
            print(match(c("% de Aumento"), names(ya$df)))
            
            ya$df[i, j]  <<- DT::coerceValue(v, ya$df[i, j])
            
            
            ya$df$`$ de aumento` <- round(ya$df$`% de Aumento` * ya$df$masa_salarial_12m * (ya$df$`Vble Actual`)/100,0)
            
            
          } else { showNotification("No cambiar esta columna",type = "warning")}
        }}})
  replaceData(proxy1admin, ya$df, resetPaging = FALSE, clearSelection = FALSE)  
})


} 

