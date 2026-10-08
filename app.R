library(shiny)
library(leaflet)
library(tidyverse)
library(bslib)
library(geobr)
library(sf)
library(htmltools)
library(plotly)
library(googledrive)
# ==============================================================================
# 1. Carregamento e Preparação dos Dados
# ==============================================================================

# Carrega a malha municipal do Rio Grande do Sul (geobr retorna a coluna 'name_muni')
geo_rs <- read_municipality(code_muni = "RS", year = 2020, showProgress = FALSE) %>%
  st_transform(crs = 4326) %>%
  mutate(code_muni = as.character(code_muni)) # Garante tipo caractere para o join

# Carrega o dataframe 
# Desativar autenticação se o link for público
drive_deauth() 

# Baixar e ler diretamente usando o link original de compartilhamento
dados <- read.csv(drive_download(as_id("https://drive.google.com/file/d/1z8A2HmndcJY60Op1R_PzOt6FDIxf3Iwu/view?usp=sharing"), overwrite = TRUE)$local_path)
#head(dados)

# Cria um mapping único conectando Cultura + Variável -> Unidade de Medida
mapeamento_unidades <- dados %>%
  distinct(Produto.Agrícola, Variável, Unidade.de.Medida) %>%
  rename(cultura = Produto.Agrícola)

# Tratamento principal do Dataset
dados_agricolas <- dados %>%
  mutate(Código.do.Município = as.character(Código.do.Município)) %>%
  pivot_wider(
    id_cols = c(Código.do.Município, Município, Ano, Produto.Agrícola, regional),
    names_from = Variável,
    values_from = Valor,
    values_fn = list(Valor = mean) 
  ) %>%
  rename(
    code_muni = Código.do.Município,
    name_muni = Município,
    ano = Ano,
    cultura = Produto.Agrícola,
    Area = `Área plantada ou destinada à colheita`,
    Producao = `Quantidade produzida`,
    Rendimento = `Rendimento médio da produção`,
    Valor_Prod = `Valor da produção`
  )

# Listas 
culturas_disponiveis <- sort(unique(dados_agricolas$cultura))
cidades_disponiveis <- sort(unique(dados_agricolas$name_muni))
anos_disponiveis <- sort(unique(dados_agricolas$ano))

# ==============================================================================
# 2. Interface do Usuário (UI)
# ==============================================================================
ui <- page_sidebar(
  title = "Painel Agrícola: Culturas no RS",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  sidebar = sidebar(
    selectInput(
      inputId = "cultura_selecionada",
      label = "Escolha a cultura:",
      choices = culturas_disponiveis,
      selected = culturas_disponiveis[1]
    ),
    hr(),
    selectInput(
      inputId = "cidade_selecionada",
      label = "Escolher cidade ou selecionar no mapa:",
      choices = cidades_disponiveis,
      selected = cidades_disponiveis[1]
    ),
    actionButton("reset_zoom", "Ver Estado Inteiro", class = "btn-outline-secondary btn-sm w-100"),
    hr(),
    markdown("*Fonte: Tabela 5457 PAM IBGE.*")
  ),
  
  layout_columns(
    col_widths = c(5, 7), 
    
    # # LADO ESQUERDO: Mapa com controle de ano posicionado logo abaixo dele
    tags$div(
      class = "d-flex flex-column gap-2",
      style = "height: 850px; max-height: 850px;",

      card(
        height = "700px",
        full_screen = TRUE,
        leafletOutput("mapa_rs", height = "100%")
      ),

      # Seletor de Ano posicionado EXATAMENTE ABAIXO do mapa
      card(
        height = "142px",
        card_body(
          sliderInput(
            inputId = "ano_mapa_calor",
            label = "Escolha o ano para mapa da produção:",
            min = 2001,   # Será atualizado dinamicamente pelo Server baseando-se no seu arquivo
                  max = 2025,   # Será atualizado dinamicamente pelo Server
                  value = 2025, # Ano padrão inicial
                  step = 1,
                  sep = "",
                  animate = animationOptions(interval = 1500, loop = FALSE), # se loop = T adiciona botão de "Play" automático
            width = "100%"
          )
        )
      )
    ),
    
    # LADO DIREITO: Quatro Gráficos
    tags$div(
      class = "d-flex flex-column gap-2",
      style = "height: 850px; max-height: 850px; overflow-y: hidden;",
      
      card(
        height = "100%",
        card_header(textOutput("titulo_rendimento")),
        plotlyOutput("grafico_rendimento", height = "160px")
      ),
      card(
        height = "100%",
        card_header(textOutput("titulo_area")),
        plotlyOutput("grafico_area", height = "160px")
      ),
      card(
        height = "100%",
        card_header(textOutput("titulo_producao")),
        plotlyOutput("grafico_producao", height = "160px")
      ),
      card(
        height = "100%",
        card_header(textOutput("titulo_valor")),
        plotlyOutput("grafico_valor", height = "160px")
      )
    )
  )
)

# ==============================================================================
# 3. Lógica do Servidor (Server)
# ==============================================================================
server <- function(input, output, session) {
  
  cidade_ativa <- reactiveVal()
  
  observe({
    if(is.null(cidade_ativa()) && length(cidades_disponiveis) > 0) {
      cod_inicial <- dados_agricolas %>% 
        filter(name_muni == input$cidade_selecionada) %>% 
        pull(code_muni) %>% 
        first()
      cidade_ativa(cod_inicial)
    }
  })
  
  # Filtra os dados de produção do ano e cultura ativos para montar o mapa coroplético
  dados_ano_atual <- reactive({
    req(input$cultura_selecionada, input$ano_mapa_calor)
    dados_agricolas %>%
      filter(
        cultura == input$cultura_selecionada,
        ano == as.numeric(input$ano_mapa_calor)
      )
  })
  
  # Cruza limites geográficos garantindo que o 'name_muni' estrutural do geobr permaneça preservado
  mapa_data <- reactive({
    req(cidade_ativa())
    df_prod <- dados_ano_atual()
    
    geo_rs %>%
      left_join(df_prod, by = "code_muni") %>%
      mutate(
        is_selecionada = code_muni == cidade_ativa(),
        Producao_Limpa = ifelse(is.na(Producao) | Producao < 0, 0, Producao)
      )
  })
  
  # GATILHO 1: Clique no mapa
  observeEvent(input$mapa_rs_shape_click, {
    id_clicado <- input$mapa_rs_shape_click$id 
    if (!is.null(id_clicado)) {
      dados_muni <- dados_agricolas %>% filter(code_muni == id_clicado)
      if (nrow(dados_muni) > 0) {
        cidade_ativa(id_clicado)
        nome_exibicao <- dados_muni$name_muni
        updateSelectInput(session, "cidade_selecionada", selected = nome_exibicao)
      }
    }
  })
  
  # GATILHO 2: Seleção no dropdown de busca de cidades
  observeEvent(input$cidade_selecionada, {
    req(input$cidade_selecionada)
    
    cod_selecionado <- dados_agricolas %>% 
      filter(name_muni == input$cidade_selecionada) %>% 
      pull(code_muni) %>% 
      first()
    
    req(cod_selecionado)
    
    if (is.null(cidade_ativa()) || cod_selecionado != cidade_ativa()) {
      cidade_ativa(cod_selecionado)
    }
    
    muni_geo <- geo_rs %>% filter(code_muni == cod_selecionado)
    if (nrow(muni_geo) > 0) {
      bbox <- st_bbox(muni_geo)
      leafletProxy("mapa_rs") %>%
        fitBounds(
          lng1 = as.numeric(bbox[["xmin"]]), lat1 = as.numeric(bbox[["ymin"]]),
          lng2 = as.numeric(bbox[["xmax"]]), lat2 = as.numeric(bbox[["ymax"]])
        )
    }
  })
  
  # Botão de Reset do Zoom
  observeEvent(input$reset_zoom, {
    bbox_rs <- st_bbox(geo_rs)
    leafletProxy("mapa_rs") %>%
      fitBounds(
        lng1 = as.numeric(bbox_rs[["xmin"]]), lat1 = as.numeric(bbox_rs[["ymin"]]),
        lng2 = as.numeric(bbox_rs[["xmax"]]), lat2 = as.numeric(bbox_rs[["ymax"]])
      )
  })
  
  # Renderização do Mapa Leaflet Base
  output$mapa_rs <- renderLeaflet({
    bbox_rs <- st_bbox(geo_rs)
    leaflet() %>%
      addTiles() %>%
      fitBounds(
        lng1 = as.numeric(bbox_rs[["xmin"]]), lat1 = as.numeric(bbox_rs[["ymin"]]),
        lng2 = as.numeric(bbox_rs[["xmax"]]), lat2 = as.numeric(bbox_rs[["ymax"]])
      )
  })
  
  # Renderização Dinâmica Coroplética e Legenda 
  observeEvent({
    mapa_data()
    cidade_ativa()
  }, {
    sf_df <- mapa_data()
    req(nrow(sf_df) > 0)
    
    # Filtra apenas os valores que possuem produção real para basear o mínimo e máximo da legenda
    valores_reais <- sf_df %>% filter(Producao_Limpa > 0) %>% pull(Producao_Limpa)
    
    if(length(valores_reais) == 0) {
      valores_reais <- c(0, 1)
    }
    
    # Criando escala numérica contínua do mínimo ao máximo (YlOrRd: Amarelo -> Laranja -> Vermelho)
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = valores_reais,
      na.color = "#E0E0E0" # Municípios sem dados ficam em cinza neutro
    )
    
    # Montagem segura das caixas de texto explicativas utilizando a coluna estrutural nativa .x do geobr
    labels_seguros <- lapply(1:nrow(sf_df), function(i) {
      nome_muni_atual <- sf_df$name_muni.x[i]
      valor_atual <- sf_df$Producao_Limpa[i]
      
      texto <- if(valor_atual > 0) {
        paste0("<strong>", nome_muni_atual, "</strong><br/>Produção: ", format(valor_atual, big.mark = ".", decimal.mark = ","))
      } else {
        paste0("<strong>", nome_muni_atual, "</strong><br/>Sem dados / Zerado neste ano")
      }
      HTML(texto)
    })
    
    proxy <- leafletProxy("mapa_rs") %>%
      clearShapes() %>%
      clearControls() # Reseta a legenda antiga
    
    proxy %>%
      addPolygons(
        data = sf_df,
        # Se for o município selecionado, ganha destaque contorno branco
        fillColor = ~ifelse(is_selecionada, pal(Producao_Limpa), pal(Producao_Limpa)),
        weight = ~ifelse(is_selecionada, 3, 1),
        opacity = 1,
        color = ~ifelse(is_selecionada, "#FFF", "#A0A0A0"), # Borda branca no selecionado, cinza nos outros
        fillOpacity = 0.85,
        layerId = ~code_muni, 
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#FFF", 
          fillOpacity = 0.95,
          bringToFront = TRUE     
        ),
        label = labels_seguros,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      # Adiciona a legenda contínua correta no canto inferior direito
      addLegend(
        pal = pal,
        values = valores_reais,
        opacity = 0.85,
        title = paste0("Produção - ", input$cultura_selecionada),
        position = "bottomright",
        labFormat = labelFormat(big.mark = "")
      )
  }, ignoreInit = FALSE)
  nome_cidade_ativa <- reactive({
    req(cidade_ativa())
    dados_agricolas %>%
      filter(code_muni == cidade_ativa()) %>%
      pull(name_muni) %>%
      first()
  })
  obter_unidade_dinamica <- function(nome_variavel) {
    unid <- mapeamento_unidades %>%
      filter(cultura == input$cultura_selecionada, Variável == nome_variavel) %>%
      pull(Unidade.de.Medida) %>%
      first()
    if(is.null(unid) || is.na(unid) || unid == "") return("")
    return(paste0(" (", unid, ")"))
  }
  output$titulo_rendimento <- renderText({
    paste0(nome_cidade_ativa(), " - Rendimento Médio da Produção", obter_unidade_dinamica("Rendimento médio da produção"), " - ", input$cultura_selecionada)
  })
  output$titulo_area       <- renderText({
    paste0(nome_cidade_ativa(), " - Área Plantada ou Destinada à Colheita", obter_unidade_dinamica("Área plantada ou destinada à colheita"), " - ", input$cultura_selecionada)
  })
  output$titulo_producao   <- renderText({
    paste0(nome_cidade_ativa(), " - Quantidade Produzida", obter_unidade_dinamica("Quantidade produzida"), " - ", input$cultura_selecionada)
  })
  output$titulo_valor      <- renderText({
    paste0(nome_cidade_ativa(), " - Valor da Produção", obter_unidade_dinamica("Valor da produção"), " - ", input$cultura_selecionada)
  })
  df_temporal <- reactive({
    req(cidade_ativa())
    dados_agricolas %>%
      filter(code_muni == cidade_ativa(), cultura == input$cultura_selecionada) %>%
      arrange(ano)
  })
  criar_grafico_linha <- function(df, coluna_y, cor_linha) {
    if (nrow(df) == 0 || all(is.na(df[[coluna_y]]))) {
      return(plot_ly() %>% layout(title = list(text = "Sem dados para esta combinação", font = list(size = 11))))
    }
    plot_ly(data = df, x = ~ano, y = ~get(coluna_y), type = 'scatter', mode = 'lines',
            line = list(color = cor_linha, width = 3)) %>%
      layout(
        xaxis = list(
          title = "",
          tickmode = "linear",
          dtick = 1,
          tickformat = "d",
          automargin = TRUE
        ),
        yaxis = list(title = ""),
        margin = list(l = 10, r = 10, b = 10, t = 5)
      )
  }
  output$grafico_rendimento <- renderPlotly({ criar_grafico_linha(df_temporal(), "Rendimento", "#1F4E79") })
  output$grafico_area       <- renderPlotly({ criar_grafico_linha(df_temporal(), "Area", "#1F4E79") })
  output$grafico_producao   <- renderPlotly({ criar_grafico_linha(df_temporal(), "Producao", "#1F4E79") })
  output$grafico_valor      <- renderPlotly({ criar_grafico_linha(df_temporal(), "Valor_Prod", "#1F4E79") })
}
# ==============================================================================
#   4. Inicialização do Aplicativo
# ==============================================================================
  shinyApp(ui = ui, server = server)

