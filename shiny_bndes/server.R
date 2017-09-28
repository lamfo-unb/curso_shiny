require(rCharts)
require(stringr)
require(leaflet)
require(dplyr)
require(haven)
require(shiny)

shinyServer(function(input,output){
  
  dados_linha <- reactive({
    
    if(input$media_soma == "Media") {
              d1 <- dados_setor_media %>% 
                    filter (ano >= input$anos[1] & ano <= input$anos[2] )
                
              return(d1) } else 
                
    if(input$media_soma == "Soma") {
              d2 <- dados_setor_soma %>% 
                    filter (ano >= input$anos[1] & ano <= input$anos[2] )
              return(d2) }
    })
  
  dados_barra <- reactive({
    
    if(input$num_porc == "Percentual"){
          d1 <- dados_empre_porc %>% 
                filter (ano >= input$anos[1] & ano <= input$anos[2] )
                
          return(d1) } else
    
    if(input$num_porc == "Valor Absoluto"){
         d2 <- dados_empre_num %>% 
               filter (ano >= input$anos[1] & ano <= input$anos[2] )
     
         return(d2) }
            

      })
  

  
  
  output$grafico1 <- renderChart2({
    
       if(input$media_soma == "Media") {
                 
                 xx1 <- nPlot(Valor ~ ano, group = "Setor", data = dados_linha(),
                              type = 'lineChart') 
                 
             return(xx1)  }  
    
           else 
      
        if(input$media_soma == "Soma") {
      
                 xx2 <- nPlot(Valor ~ ano, group = "Setor", data = dados_linha(),
                              type = 'lineChart') 
                  
             return(xx2)  }
  
  })
  
  output$grafico2 <- renderChart2({
    
    if(input$num_porc == "Percentual") {
      
      x1 <- nPlot(freq ~ ano, group = "Setor", data = dados_barra(),
                  type = 'multiBarChart') 
      
      x1$chart(reduceXTicks = FALSE)
      
      return(x1)  }  
    
    else 
      
      if(input$num_porc == "Valor Absoluto") {
        
        x2 <- nPlot(empresas ~ ano, group = "Setor", data = dados_barra(),
                    type = 'multiBarChart') 
        
        x2$chart(reduceXTicks = FALSE)
        
        return(x2)  }
    
  })
  
  output$brasil1 <- renderLeaflet({

    pal <- colorNumeric("Blues",domain = NULL,n=5)
    
    state_popup <- paste0("<strong>Estado: </strong>", brasil$UF, 
                          "<br><strong>Numero de empresas: </strong>",brasil$empresas)
    
    leaflet(data = brasil) %>%
      addPolygons(
        data = brasil,
        color = "#BDBDC3", weight = 1, opacity = 0.5,
        fillColor = ~colorNumeric(pal, brasil$empresas)(empresas), fillOpacity = 1, 
        popup = state_popup) %>%
      addLegend("bottomleft", pal = pal, values = ~brasil$empresas,
                title = "Numero de Empresas",
                opacity = 1)
  })
  
  
   output$grafico3 <- renderChart2({
     
     p4 = nPlot(~ Financeiro2, data = dados, type = 'pieChart')
     p4
   })
  
})