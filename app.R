library(shiny)
library(leaflet)
library(tidyverse)
library(bslib)
library(geobr)
library(sf)
library(htmltools)
library(plotly)
library(googledrive)
library(dplyr)
library(ggplot2)
# ==============================================================================
# 1. Carregamento e Preparação dos Dados
# ==============================================================================

# Carrega a malha municipal do Rio Grande do Sul
geo_rs <- read_municipality(code_muni = "RS", year = 2020, showProgress = FALSE) %>%
  st_transform(crs = 4326) %>%
  mutate(code_muni = as.character(code_muni)) # Garante tipo caractere para o join

# Desativar autenticação se o link for público
drive_deauth() 

# Baixar e ler diretamente usando o link original de compartilhamento
dados <- read.csv(drive_download(as_id("https://drive.google.com/file/d/1pCYmcrxdNKQvpQSWl8wFZF6SjOPwjVQ1/view?usp=sharing"), overwrite = TRUE)$local_path)
dados_caf26 <- read.csv(drive_download(as_id("https://drive.google.com/file/d/1lMmPsm6Va8ZrsQeIo57F3WIZeWKpKPzV/view?usp=sharing"), overwrite = TRUE)$local_path)
dados_demo <- read.csv(drive_download(as_id("https://drive.google.com/file/d/1xE9QcmrDoyCvTDOxJG-O93cooZL7MFJr/view?usp=sharing"), overwrite = TRUE)$local_path)
dados_idese <- read.csv(drive_download(as_id("https://drive.google.com/file/d/1dMcKOXGFktoGEIlhUJaA7Br8DtRWDEEe/view?usp=sharing"), overwrite = TRUE)$local_path,encoding = "latin1")
dados_estab <- read.csv(drive_download(as_id("https://drive.google.com/file/d/1x2HHHr82AH-IwrTR0nrMFYAL-RfsbVgs/view?usp=sharing"), overwrite = TRUE)$local_path)
dados_estab<-dados_estab[which(dados_estab$Tipologia=="Agricultura familiar - não"| dados_estab$Tipologia=="Agricultura familiar - sim"),]
atividades<-sort(unique(dados_estab$Grupos.de.atividade.econômica))

mapeamento_unidades <- dados %>%
  distinct(Produto.Agrícola, Variável, Unidade.de.Medida) %>%
  rename(cultura = Produto.Agrícola)

# Tratamento principal do Dataset Agrícola
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

# Ajuste nos dados do CAF 
dados_caf_limpo <- dados_caf26 %>%
  mutate(IBGE = as.character(IBGE)) %>%
  rename(
    code_muni = IBGE,
    name_muni = MUNICÍPIO
  )

# Ajuste nos dados demográficos 
dados_demo_limpo <- dados_demo %>%
  mutate(Código.do.Município = as.character(Código.do.Município)) %>%
  rename(
    code_muni = Código.do.Município,
    name_muni = Município
  )
dados_demo_limpo<-cbind(dados_demo_limpo,dados_demo_limpo$Valor)
colnames(dados_demo_limpo)[12]<-"Valores"
dados_demo_limpo$Valor[which(dados_demo_limpo$Sexo=="Homens")]<--dados_demo_limpo$Valor[which(dados_demo_limpo$Sexo=="Homens")]
ordem<-rev(unique(dados_demo_limpo$Grupo.de.Idade))

#Ajuste nos dados de estabelecimentos
dados_estab_limpo<-dados_estab %>%
  mutate(Município..Código. = as.character(Município..Código.)) %>%
  rename(
    code_muni = Município..Código.,
    name_muni = Município
  )

# Listas para a UI
culturas_disponiveis <- sort(unique(dados_agricolas$cultura))
cidades_disponiveis <- sort(unique(dados_agricolas$name_muni))
anos_disponiveis <- sort(unique(dados_agricolas$ano))

# ==============================================================================
# 2. Interface do Usuário (UI)
# ==============================================================================
ui <- page_sidebar(
  title = "Painel Agrícola do Rio Grande do Sul",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  fillable = FALSE,
  
  sidebar = sidebar(
    selectInput(
      inputId = "cultura_selecionada", 
      label = "Escolha a cultura:", 
      choices = culturas_disponiveis, 
      selected = culturas_disponiveis[1] # <-- Faltava o [1]
    ),
    hr(),
    selectInput(
      inputId = "cidade_selecionada", 
      label = "Escolha a cidade ou selecione no mapa:", 
      choices = cidades_disponiveis, 
      selected = cidades_disponiveis[1]   # <-- Faltava o [1]
    ),
    actionButton("reset_zoom", "Ver Estado Inteiro", class = "btn-outline-secondary btn-sm w-100"),
    hr(),
    markdown("*Fonte:*"),
    markdown("*Culturas - Tabela 5457 PAM IBGE;*"),
    markdown("*População - Tabela 10089 Censo Demográfico IBGE 2022;*"),
    markdown("*CAF - MDA setembro 2026;*"),
    markdown("*Idese (Revisão 2026) - Departamento de Economia e Estatística RS outubro 2026;*"),
    markdown("*Estabelecimentos agropecuários - Tabela 6754 Censo Agropecuário IBGE 2017.*"),
    hr(),
    markdown("*Autoria: Aline Armanini Stefanan*")
  ),
  
  layout_columns(
    col_widths = c(5, 7, 12),
    
    # 1º ELEMENTO (5 colunas - Esquerda: define a altura total)
    tags$div(
      class = "d-flex flex-column gap-2 h-100",
      card(height = "700px", full_screen = TRUE, leafletOutput("mapa_rs", height = "100%")),
      card(
        height = "142px",
        card_body(
          sliderInput(
            inputId = "ano_mapa_calor",
            label = "Escolha o ano para o mapa da produção:",
            min = 2001, max = 2025, value = 2025, step = 1, sep = "",
            animate = animationOptions(interval = 1500, loop = FALSE),
            width = "100%"
          )
        )
      ),
      card(card_header(textOutput("titulo_demo")), plotlyOutput("piramide_plot", height = "500px")),
      card(card_header(textOutput("titulo_idese")), plotlyOutput("grafico_idese", height = "400px"))
    ),
    
    # 2º ELEMENTO (7 colunas - Direita: estica cada card igualmente para acompanhar a esquerda)
    tags$div(
      class = "d-flex flex-column gap-2 h-100",
      card(class = "flex-fill", card_header(textOutput("titulo_rendimento")), plotlyOutput("grafico_rendimento", height = "100%")),
      card(class = "flex-fill", card_header(textOutput("titulo_area")), plotlyOutput("grafico_area", height = "100%")),
      card(class = "flex-fill", card_header(textOutput("titulo_producao")), plotlyOutput("grafico_producao", height = "100%")),
      card(class = "flex-fill", card_header(textOutput("titulo_valor")), plotlyOutput("grafico_valor", height = "100%")),
      card(class = "flex-fill", card_header(textOutput("titulo_caf")), plotlyOutput("plot_barras", height = "100%"))
    ),
    
    # 3º ELEMENTO (12 colunas - Linha de baixo inteira)
    card(
      full_screen = TRUE,
      card_header(textOutput("titulo_estab")),
      plotlyOutput("grafico_complexo", height = "650px")
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
  
  dados_ano_atual <- reactive({
    req(input$cultura_selecionada, input$ano_mapa_calor)
    dados_agricolas %>%
      filter(
        cultura == input$cultura_selecionada,
        ano == as.numeric(input$ano_mapa_calor)
      )
  })
  
  mapa_data <- reactive({
    req(cidade_ativa())
    df_prod <- dados_ano_atual()
    
    geo_rs %>%
      left_join(df_prod, by = "code_muni") %>%
      mutate(
        is_selecionada = (code_muni == cidade_ativa()),
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
        nome_exibicao <- dados_muni$name_muni %>% first()
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
  
  # Atualização dinâmica das camadas e cores do mapa coroplético
  observeEvent({
    mapa_data()
    cidade_ativa()
  }, {
    sf_df <- mapa_data()
    req(nrow(sf_df) > 0)
    
    valores_reais <- sf_df %>% filter(Producao_Limpa > 0) %>% pull(Producao_Limpa)
    if(length(valores_reais) == 0) valores_reais <- c(0, 1)
    
    pal <- colorNumeric(palette = "YlOrRd", domain = valores_reais, na.color = "#E0E0E0")
    
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
      clearControls()
    
    proxy %>%
      addPolygons(
        data = sf_df,
        fillColor = ~pal(Producao_Limpa),
        weight = ~ifelse(is_selecionada, 3, 1),
        opacity = 1,
        color = ~ifelse(is_selecionada,"#1F4E79", "#A0A0A0"),# "#FFF", "#A0A0A0"),
        fillOpacity = 0.85,
        layerId = ~code_muni, 
        highlightOptions = highlightOptions(weight = 3, color = "#1F4E79", fillOpacity = 0.95, bringToFront = TRUE),
        label = labels_seguros,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
      ) %>%
      addLegend(pal = pal, values = valores_reais, opacity = 0.85, title = paste0("Produção - ", input$cultura_selecionada), position = "bottomright", labFormat = labelFormat(big.mark = ""))
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
  
  output$titulo_rendimento <- renderText({ paste0(nome_cidade_ativa(), " - Rendimento Médio da Produção", obter_unidade_dinamica("Rendimento médio da produção"), " - ", input$cultura_selecionada) })
  output$titulo_area       <- renderText({ paste0(nome_cidade_ativa(), " - Área Plantada ou Destinada à Colheita", obter_unidade_dinamica("Área plantada ou destinada à colheita"), " - ", input$cultura_selecionada) })
  output$titulo_producao   <- renderText({ paste0(nome_cidade_ativa(), " - Quantidade Produzida", obter_unidade_dinamica("Quantidade produzida"), " - ", input$cultura_selecionada) })
  output$titulo_valor      <- renderText({ paste0(nome_cidade_ativa(), " - Valor da Produção", obter_unidade_dinamica("Valor da produção"), " - ", input$cultura_selecionada) })
  output$titulo_caf        <- renderText({ paste0(nome_cidade_ativa(), " - Caracterização das Unidades Familiares de Produção Agrária (UFPA) do Cadastro Nacional da Agricultura Familiar (CAF)") })
  output$titulo_demo        <- renderText({ paste0(nome_cidade_ativa(), " - População residente no meio rural") })
  output$titulo_estab        <- renderText({ paste0(nome_cidade_ativa(), " - Distribuição de Estabelecimentos Agropecuários") })
  output$titulo_idese        <- renderText({ paste0(nome_cidade_ativa(), " - Idese (Revisão 2026) referente ao período de 2022 a 2024") })
  
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
    plot_ly(data = df, x = ~ano, y = ~get(coluna_y), type = 'scatter', mode = 'lines', line = list(color = cor_linha, width = 3)) %>%
      layout(xaxis = list(title = "", tickmode = "linear", dtick = 1, tickformat = "d", automargin = TRUE), yaxis = list(title = ""), margin = list(l = 10, r = 10, b = 10, t = 5))
  }
  output$grafico_rendimento <- renderPlotly({ criar_grafico_linha(df_temporal(), "Rendimento", "#1F4E79") })
  output$grafico_area       <- renderPlotly({ criar_grafico_linha(df_temporal(), "Area", "#1F4E79") })
  output$grafico_producao   <- renderPlotly({ criar_grafico_linha(df_temporal(), "Producao", "#1F4E79") })
  output$grafico_valor      <- renderPlotly({ criar_grafico_linha(df_temporal(), "Valor_Prod", "#1F4E79") })

  #dados CAF filtrados reativamente da base limpa (formato longo)

  dados_filtrados <- reactive({
    req(cidade_ativa())
    dados_caf_limpo %>%
      filter(code_muni == cidade_ativa()) %>%
      group_by(Caracterização.da.UFPA) %>%
      summarise(Quantidade = sum(Quantidade, na.rm = TRUE)) %>%
      arrange(desc(Caracterização.da.UFPA))
  })
  output$plot_barras <- renderPlotly({
    dados <- dados_filtrados()
    req(nrow(dados) > 0)
    max_valor <- max(dados$Quantidade, na.rm = TRUE) * 1.15
    if(is.infinite(max_valor) || is.na(max_valor)) max_valor <- 10
    plot_ly(
      data = dados,
      x = ~Quantidade,
      y = ~factor(Caracterização.da.UFPA),
      type = "bar",
      orientation = "h",
      text = ~Quantidade,
      textposition = "outside",
      marker = list(color = "#1F4E79")
    ) %>%
      layout(
        xaxis = list(title = "Quantidade de unidades familiares de produção agrária", range = c(0, max_valor)),
        yaxis = list(title = "", categoryorder = "array", categoryarray = unique(dados$Caracterização.da.UFPA)),
        margin = list(t = 40, l = 180, r = 60, b = 40)
      )
  })

  dados_longo <- reactive({
    req(cidade_ativa())
    dados_demo_limpo %>%
      filter(code_muni == cidade_ativa() & Situação.de.domicílio=="Rural")%>%
      mutate(Grupo.de.Idade = factor(Grupo.de.Idade, levels = ordem))
  })
  output$piramide_plot <- renderPlotly({
    df_plot <- dados_longo()
    req(nrow(df_plot) > 0)

    # 1. Calcula o total absoluto por Sexo e cria a coluna da legenda
    df_plot <- df_plot %>%
      group_by(Sexo) %>%
      mutate(
        total_sexo = sum(abs(as.numeric(Valor)), na.rm = TRUE),
        sexo_legenda = paste0(
          Sexo, " (", format(total_sexo, big.mark = ".", decimal.mark = ","), ")"
        )
      ) %>%
      ungroup()

    # 2. Mapeia as cores fixas aos novos rótulos dinâmicos
    rotulos_unicos <- unique(df_plot$sexo_legenda)
    cores_piramide <- setNames(
      ifelse(grepl("^Homens", rotulos_unicos), "aquamarine3", "#ff7f50"),#"#00ced1", "#ff7f50"),
      rotulos_unicos
    )

    gg <- ggplot(
      df_plot,
      aes(
        x = Grupo.de.Idade,
        y = Valor,
        fill = sexo_legenda, # <-- Usa a nova coluna na legenda
        text = paste(
          "Faixa Etária:", Grupo.de.Idade,
          "<br>Gênero:", Sexo,
          "<br>Quantidade:", Valores
        )
      )
    ) +
      geom_bar(stat = "identity", width = 0.7) +
      coord_flip() +
      # Formata os números absolutos no eixo com ponto de milhar
      scale_y_continuous(
        labels = function(x) format(abs(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
      ) +
      scale_fill_manual(values = cores_piramide) +
      labs(
        x = "",
        y = "",
        fill = ""
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      )

    ggplotly(gg, tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center", # Centraliza perfeitamente a legenda
          y = -0.15
        )
      )
  })

  criar_painel_idese <- function(df, titulo_painel, mostrar_legenda = TRUE) {
    df_blocos <- df %>% filter(Categoria_Variavel %in% c("Bloco_Educacao", "Bloco_Saude", "Bloco_Renda"))
    df_geral  <- df %>% filter(Categoria_Variavel == "Idese")
    
    # Nomes das cores batendo exatamente com Categoria_Variavel
    cores_blocos <- c(
      "Bloco_Educacao" = "#4e79a7",
      "Bloco_Saude"    = "#59a14f",
      "Bloco_Renda"    = "#f28e2b"
    )
    
    # Nomes bonitos para exibir na legenda e no balão
    nomes_bonitos <- c(
      "Bloco_Educacao" = "Educação",
      "Bloco_Saude"    = "Saúde",
      "Bloco_Renda"    = "Renda"
    )
    
    p <- plot_ly()
    
    # Adiciona as colunas para cada bloco
    for (bloco in c("Bloco_Educacao", "Bloco_Saude", "Bloco_Renda")) {
      dados_b <- df_blocos %>% filter(Categoria_Variavel == bloco)
      nome_exibicao <- nomes_bonitos[[bloco]]
      
      p <- p %>% add_bars(
        data = dados_b,
        x = ~factor(Ano),
        y = ~VALOR,
        name = nome_exibicao,
        legendgroup = nome_exibicao,
        showlegend = mostrar_legenda,
        marker = list(color = unname(cores_blocos[[bloco]])),
        hovertemplate = paste0(
          "<b>", titulo_painel, "</b><br>",
          "Ano: %{x}<br>",
          nome_exibicao, ": <b>%{y:.3f}</b><extra></extra>"
        )
      )
    }
    
    # Adiciona a linha para o Idese Geral
    p <- p %>% add_trace(
      data = df_geral,
      x = ~factor(Ano),
      y = ~VALOR,
      type = "scatter",
      mode = "lines+markers",
      name = "Idese Geral",
      legendgroup = "Idese Geral",
      showlegend = mostrar_legenda,
      line = list(color = "#111111", width = 3),
      marker = list(color = "#111111", size = 8),
      hovertemplate = paste0(
        "<b>", titulo_painel, "</b><br>",
        "Ano: %{x}<br>",
        "Idese Geral: <b>%{y:.3f}</b><extra></extra>"
      )
    )
    
    p %>% layout(
      barmode = "group",
      yaxis = list(
        title = "Índice (0 a 1)",
        range = c(0, 1.05),
        tickformat = ".2f"
      ),
      xaxis = list(title = ""),
      annotations = list(
        list(
          x = 0.5, y = 1.06,
          text = paste0("<b>", titulo_painel, "</b>"),
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 14)
        )
      )
    )
  }
  
  output$grafico_idese <- renderPlotly({
    req(cidade_ativa())
    
    # Limpa colunas sem nome e padroniza nomes e tipo numérico da coluna VALOR
    df_limpo <- dados_idese[, !is.na(names(dados_idese)) & names(dados_idese) != ""] %>%
      rename(
        code_muni = COD,
        name_muni = NOME
      ) %>%
      mutate(
        code_muni = as.character(code_muni),
        VALOR = suppressWarnings(as.numeric(gsub(",", ".", as.character(VALOR))))
      )
    
    # Filtra dados do município selecionado
    df_muni <- df_limpo %>%
      filter(code_muni == as.character(cidade_ativa()))
    
    # Filtra dados do RS (verifique se na sua tabela o RS tem COD == "43" ou NOME == "Rio Grande do Sul")
    df_rs <- df_limpo %>%
      filter(code_muni == "43" | name_muni == "Rio Grande do Sul")
    
    validate(
      need(nrow(df_muni) > 0, "Dados do Idese não disponíveis para este município.")
    )
    
    # Extrai o texto do nome do município para o título do painel
    titulo_muni <- unique(df_muni$name_muni)[1]
    
    p_muni <- criar_painel_idese(df_muni, titulo_painel = titulo_muni, mostrar_legenda = TRUE)
    p_rs   <- criar_painel_idese(df_rs,   titulo_painel = "Rio Grande do Sul", mostrar_legenda = FALSE)
    subplot(
      p_muni, p_rs,
      nrows = 1,
      shareY = TRUE,
      titleX = TRUE,
      margin = 0.04
    ) %>%
      layout(
        barmode = "group",
        # Aumenta a margem inferior para caber a legenda com folga
        margin = list(t = 50, b = 90, l = 55, r = 15),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.15,
          yanchor = "top" # <-- O segredo: ancora o TOPO da legenda abaixo do eixo X
        )
      )
  })
  
  output$grafico_complexo <- renderPlotly({
    req(cidade_ativa())
    
    ordem_desejada <- unique(dados_estab_limpo$Grupos.de.área.total)
    if (length(ordem_desejada) >= 19) {
      ordem_desejada[18:19] <- c("De 10.000 ha e mais", "Produtor sem área")
    }
    
    dadosE_filtrados <- dados_estab_limpo %>%
      filter(
        code_muni == cidade_ativa()
      ) %>%
      mutate(
        Valor = suppressWarnings(as.numeric(gsub(",", ".", Valor)))
      ) %>%
      filter(!is.na(Valor))
    
    validate(
      need(nrow(dadosE_filtrados) > 0, "Nenhum dado encontrado para este município.")
    )
    
    # 1. Calcula totais por Tipologia (títulos das facetas)
    totais_tipologia <- dadosE_filtrados %>%
      group_by(Tipologia) %>%
      summarise(Total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        rotulo_faceta = paste0(
          Tipologia, " (Total: ", 
          format(Total, big.mark = ".", decimal.mark = ","), ")"
        )
      )
    novos_titulos <- setNames(totais_tipologia$rotulo_faceta, totais_tipologia$Tipologia)
    
    # 2. Cria uma coluna própria para a legenda já contendo o total
    dadosE_filtrados <- dadosE_filtrados %>%
      group_by(Grupos.de.atividade.econômica) %>%
      mutate(
        total_ativ = sum(Valor, na.rm = TRUE),
        atividade_legenda = paste0(
          Grupos.de.atividade.econômica, " (", 
          format(total_ativ, big.mark = ".", decimal.mark = ","), ")"
        )
      ) %>%
      ungroup() %>%
      mutate(
        # Mantém a mesma ordem alfabética original para preservar as cores do viridis
        atividade_legenda = factor(
          atividade_legenda, 
          levels = sort(unique(atividade_legenda))
        ),
        Grupos.de.área.total = factor(
          Grupos.de.área.total,
          levels = ordem_desejada
        ),
        texto_hover = paste0(
          "Tipologia: ", Tipologia, "<br>",
          "Área: ", Grupos.de.área.total, "<br>",
          "Atividade: ", Grupos.de.atividade.econômica, "<br>",
          "Estabelecimentos: ", format(Valor, big.mark = ".", decimal.mark = ",")
        )
      )
    
    p <- ggplot2::ggplot(
      dadosE_filtrados,
      ggplot2::aes(
        x = Grupos.de.área.total,
        y = Valor,
        fill = atividade_legenda, # <-- Usa a nova coluna diretamente aqui
        text = texto_hover
      )
    ) +
      ggplot2::geom_bar(stat = "identity", position = "stack", alpha = 0.9) +
      ggplot2::facet_wrap(
        ~ Tipologia, 
        scales = "free_y",
        labeller = ggplot2::as_labeller(novos_titulos)
      ) +
      ggplot2::scale_y_continuous(
        labels = function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE),
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::labs(
        x = "",#"Grupos de Área (Tamanho da Propriedade)",
        y = "",#"Número de Estabelecimentos",
        fill = ""#"Atividade Econômica"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
        strip.text = ggplot2::element_text(face = "bold", color = "#333333"),
        panel.spacing = grid::unit(2, "lines"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
    
    fig <- plotly::ggplotly(p, tooltip = "text")
    fig <- plotly::layout(
      fig,
      autosize = TRUE,
      margin = list(b = 150, l = 70, r = 10, t = 50),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.45
      )
    )
    fig
  })
  
  
}

#==============================================================================


#4. Inicialização do Aplicativo


#==============================================================================

shinyApp(ui = ui, server = server)

