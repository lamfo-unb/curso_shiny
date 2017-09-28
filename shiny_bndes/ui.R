shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML(".leaflet-container { background: white; }")) #FFFFFF
  ),
  
  fluidRow(
    #br(),
    column(3, 
           selectInput(inputId = "media_soma",
                         label = "Media ou Soma",
                        choices = c("Media","Soma"),
                       selected = "Media" , width = '200px') , offset = 1 ),
    
    column(3,  
           sliderInput(inputId = "anos" , label = "Periodo",
                       min = 2002 , max = 2016, value = c(2002,2016),
                       step = 1,sep="") , offset = 1 ),
            
    column(3,
              selectInput(  inputId = "num_porc",
                          label = "Numero ou Porcentagem",
                          choices = c("Percentual", "Valor Absoluto"),
                          selected = "Percentual" , width = '200px')  , offset = 1  )
    ),
  
    br(),
  
  fluidRow(
      column(6, showOutput("grafico1","nvd3")  ),
      column(6, showOutput("grafico2","nvd3")  )
  ),
  fluidRow(
      column(6,leafletOutput("brasil1" , height = "500px")),
      column(6,showOutput("grafico3","nvd3")))
 ))
