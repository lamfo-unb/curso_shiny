library(haven)
library(rCharts)    #library(devtools) ; install_github('rCharts', 'ramnathv')
library(stringr)
library(tidyverse)
library(rgdal)
library(leaflet)


arquivo <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path),
                     "bndes03.csv")

dados <- data.table::fread(arquivo,header = TRUE)
# usuário   sistema decorrido 
# 13.184     0.284    13.490 

dados <- dados %>% mutate( apoio            = str_to_upper(  str_trim(apoio) ),
                           Custo_Financeiro = str_trim(Custo_Financeiro),
                           setor_bndes      = str_trim(setor_bndes) )

dados <- dados %>%
  mutate( Setor =  if_else(substr(setor_bndes,1,3) == "IND" , "Industria",
                           if_else(substr(setor_bndes,1,3) == "COM", "Comercio",
                                   "Agropecuaria"))) %>%
  mutate( Financeiro2 = if_else(substr(Custo_Financeiro,1,1) == "1", "CDI",
                                if_else(substr(Custo_Financeiro,1,1) == "7" | 
                                          substr(Custo_Financeiro,1,1) == "9" , "SELIC",
                                        if_else(substr(Custo_Financeiro,1,4) == "TAXA" , 
                                                "TAXA FIXA",
                                                Custo_Financeiro))))

dados <- dados %>% mutate( modalidade = replace( modalidade , modalidade == "" , NA ) ,
                           area       = replace( area       , area == ""       , NA ) )

dados_setor_media <- dados %>% 
                     group_by(ano,Setor) %>% 
                     filter(ano >= 2002 & ano <= 2016) %>%
                     summarise(Valor = mean(Val_Contratado)/100000)

dados_setor_soma <- dados %>% 
                    group_by(ano,Setor) %>% 
                    filter(ano >= 2002 & ano <= 2016) %>%
                    summarise(Valor = sum(Val_Contratado)/100000)

dados_empre_num <- dados %>% 
                   group_by(ano,Setor) %>% 
                   filter(!is.na(modalidade)) %>%
                   summarise(empresas = n_distinct(empresa))

dados_empre_porc <- dados %>% 
                    group_by(ano,Setor) %>% 
                    filter(!is.na(modalidade)) %>%
                    summarise(empresas = n_distinct(empresa)) %>%
                    mutate( freq = empresas / sum(empresas))

# Download em: http://downloads.ibge.gov.br/downloads_geociencias.htm
# organizacao_do_territorio >>> malhas_territoriais >>> malhas_municipais 
#  >>> municipio_2015 >>> Brasil >>> BR >>> br_unidades_da_federacao.zip

arquivo2 <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path))

shp <- readOGR(dsn = arquivo2, 
               layer = "BRUFE250GC_SIR", stringsAsFactors=FALSE, encoding="UTF-8")

mapinha <- dados %>% 
  group_by(ano,UF) %>% 
  filter(ano == 2016 & UF != "IE") %>%
  summarise(empresas = n_distinct(empresa))

cd_uf <- c(11,12,13,14,15,16,17,
           21,22,23,24,25,26,27,28,29,
           31,32,33,35,41,42,43,50,51,52,53)

sig_uf <- c("RO","AC","AM","RR","PA","AP","TO",
            "MA","PI","CE","RN","PB","PE","AL","SE","BA",
            "MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF")

mapinha <- mapinha %>%
            mutate(codigo = NA_real_)

for(i in 1:nrow(mapinha)){
  posicao <- mapinha$UF[i]==sig_uf
  mapinha$codigo[i] <- cd_uf[posicao]
}

mapinha$codigo <- as.character(mapinha$codigo)

names(mapinha) <- c("ano","UF","empresas","CD_GEOCUF")

brasil <- merge(shp,mapinha, by = "CD_GEOCUF")

proj4string(brasil) <- CRS("+proj=longlat +datum=WGS84 +no_defs")



pal <- colorNumeric("Blues",domain = NULL,n=5) #cores do mapa

state_popup <- paste0("<strong>Estado: </strong>", 
                      brasil$UF, 
                      "<br><strong>Numero de empresas: </strong>", 
                      brasil$empresas)
