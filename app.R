library(shiny)
library(leaflet)
library(tidyverse)
library(reactlog)
library(svglite) 
library(fontawesome)
library(flexdashboard)
library(shinyWidgets)
library(shinyjs)
library(histoslider)
library(bsicons)
library(plotly)
library(bslib)
library(htmltools)
library(rsconnect)

reactlog_enable()

# Data
hf_data_tbl_updated <- read_csv("data/updated_processed_data.csv")
no_of_clusters <- hf_data_tbl_updated |> distinct(cluster) |> nrow()
states_choices <- append(unique(hf_data_tbl_updated$state), "All",after=0)
TITLE        <- "Heifer Naija Unlock Dashboard Demo using R Shiny Web Application"


card <- function(body, title) {
    div(class = "card",
        div(icon("people-group", style = "color:#2a4153"), 
            class = "card-header bg-success text-white text-start font-weight-bold", title),
        div(class = "card-body d-flex justify-content-center", body)
    )
}


ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%} #species_list_text { padding: 20px; border: 1px solid #ededed; border-radius: 10px; margin: 10px; display: flex; flex-direction: row;flex-wrap: wrap;} .species_item{ flex-basis: 50%; } .species_list_header{font-weight:700;color: green; margin-bottom: 20px;}"),
    # Application title
    fluidRow(column(
        2, 
        tags$img(
            src = "./images/logo-navy-heifer.svg",
            width = "150px",
            height = "100px",style="margin:10px;"
        )
    ),
    column(10, h2(TITLE), style="background-color:#2b4053;color:#fff;")),
    fluidRow(style="margin:0px 10px;",
        column(3,
               uiOutput('p3')),
        column(3,
               uiOutput('p1')
        ),
        column(3,
               uiOutput('p2')
        ),
        column(3,
               uiOutput('p4')
        )
    ),
    fluidRow(style="margin:0px 10px;",
        column(3,
               #style="margin:0px 2px;",
               selectizeInput(inputId = "hf_states",
                              label = "Select State:",
                              choices = states_choices)),
        column(3, 
               #style="margin:0px 2px;",
               selectizeInput(inputId = "hf_lgas",
                              label = "Select LGA:",
                              choices = NULL)),
        column(3,
               #style="margin:0px 2px;",
               selectizeInput(inputId = "hf_gender",
                              label = "Select Gender:",
                              choices = unique(hf_data_tbl_updated$sex_of_respondent))),
        column(3, 
               #style="margin:0px 2px;",
               br(),
               actionButton(inputId = "hf_search", icon = icon("refresh"),
                            label = "Update"))
    ),
    fluidRow(style="margin:10px;",
        column(6,h5("States Covered:"),
               gaugeOutput("gauge_states",height = "100px"),
               fluidRow(
                   column(6,
                       h5("Farmers Education Distribution"),
                       plotlyOutput(outputId = "hf_edu")),
                   column(6,
                          h5("Phone Ownership by States"),
                          plotlyOutput(outputId = "hf_phone"))
               )),
        column(6,
               h4("Geographical Distribution of Farmers in Nigeria"),
               leafletOutput("map",height = "500")))
)


server <- function(input, output, session) {
    
    output$p1 <- renderUI({
        #<i class="fa-solid fa-group-arrows-rotate"></i>  
        card(tags$h2(fa("group-arrows-rotate", 
                        fill = "#2a4153"), 
                     "7,516"), 
             'Farmers Population in Naija Unlock')
    })
    #<i class="fa-solid fa-money-bill-wheat"></i>
    output$p2 <- renderUI({
        fig <- tags$h2(fa("money-bill-wheat", 
                          fill = "#2a4153"), 
                       "$879,657")
        card(fig, 
             'Value Addition')
    })
    
    
    output$p3 <- renderUI({
        fig <- tags$h2(fa("calendar-days", 
                          fill = "#2a4153"), 
                       "256")
        card(fig, 'Total farmers registered in the last 30 days')
    })
    
    output$p4 <- renderUI({
        fig <- tags$h2(fa("house", 
                          fill = "#2a4153"), 
                       "656")
        card(fig, 'Total farmers registered')
    })
    
    output$gauge_states = renderGauge({
        gauge(hf_data_tbl_updated |> distinct(state) |> nrow(), 
              min = 0, 
              max = 36, 
              sectors = gaugeSectors(success = c(18, 36), 
                                     warning = c(12, 18),
                                     danger = c(0, 12)))
    })
    
    output$hf_edu <- renderPlotly({
        
        if(input$hf_states == "All"){
        figEdu <- hf_data_tbl_updated |> 
            count(state,lga,sex_of_respondent,education) |> 
            ggplot(aes(x =state,text = str_glue("{education}: {round((n/sum(n))*100, digits=2)}%
                                        Gender: {sex_of_respondent}
                                        LGA: {lga}"), 
                       y=n, fill= factor(education), group=education)) + 
            geom_bar(stat = "identity", position = "fill")+
            labs(x="",y="")+
            scale_y_continuous(labels = scales::percent) +
            scale_fill_manual(values = c("#2a4153", "#b65e2d", "#397485","#7dbec2", "#68d4a5", "#d48668"))+
            theme(legend.position="none")+
            coord_flip()
        
        } else{
            
            figEdu <- hf_data_tbl_updated |> 
                filter(state == input$hf_states & 
                           lga == input$hf_lgas & 
                           sex_of_respondent==input$hf_gender) |> 
                count(state,lga,sex_of_respondent,education) |> 
                ggplot(aes(x =state,text = str_glue("{education}: {round((n/sum(n))*100, digits=2)}%
                                        Gender: {sex_of_respondent}
                                        LGA: {lga}"), 
                           y=n, fill= factor(education), group=education)) + 
                geom_bar(stat = "identity", position = "fill")+
                labs(x="",y="")+
                scale_y_continuous(labels = scales::percent) +
                scale_fill_manual(values = c("#2a4153", "#b65e2d", "#397485","#7dbec2", "#68d4a5", "#d48668"))+
                theme(legend.position="none")
        }
        
        ggplotly(figEdu,tooltip = "text")
        
    })
    
    output$hf_phone <- renderPlotly({
        if(input$hf_states == "All"){
            figPh <- hf_data_tbl_updated |> 
                count(state,lga,sex_of_respondent,phone_ownership) |> 
                ggplot(aes(x =state,text = str_glue("Phone Ownership
                                        {phone_ownership}: {round((n/sum(n))*100, digits=2)}%
                                        Gender: {sex_of_respondent}
                                        LGA: {lga}"), 
                           y=n, fill= factor(phone_ownership), group=phone_ownership)) + 
                geom_bar(stat = "identity", position = "fill")+
                labs(x="",y="")+
                scale_y_continuous(labels = scales::percent) +
                scale_fill_manual(values = c("#2a4153", "#b65e2d", "#397485","#7dbec2", "#68d4a5", "#d48668"))+
                theme(legend.position="none")+
                coord_flip()
            
        } else{
        figPh <- hf_data_tbl_updated |> 
            filter(state == input$hf_states & 
                       lga == input$hf_lgas & 
                       sex_of_respondent==input$hf_gender) |> 
            count(state,lga,sex_of_respondent,phone_ownership) |> 
            ggplot(aes(x =state,text = str_glue("Phone Ownership
                                        {phone_ownership}: {round((n/sum(n))*100, digits=2)}%
                                        Gender: {sex_of_respondent}
                                        LGA: {lga}"), 
                       y=n, fill= factor(phone_ownership), group=phone_ownership)) + 
            geom_bar(stat = "identity", position = "fill")+
            labs(x="",y="")+
            scale_y_continuous(labels = scales::percent) +
            scale_fill_manual(values = c("#2a4153", "#b65e2d", "#397485","#7dbec2", "#68d4a5", "#d48668"))+
            theme(legend.position="none")
        
        }
        ggplotly(figPh, tooltip = "text")
    })
    
    output$map <- renderLeaflet({
        
        labels <- sprintf(
            "<strong>%s</strong><br/>Cluster: %s",
            hf_data_tbl_updated$name,
            hf_data_tbl_updated$cluster) |>
            lapply(htmltools::HTML)
        
        leaflet() |>
            addTiles() |>
            fitBounds(lng1 = 3, 
                      lat1 = 4, 
                      lng2 = 15, 
                      lat2 = 14) |> 
            addProviderTiles(providers$Esri.WorldStreetMap) |>
            addCircleMarkers(data = hf_data_tbl_updated[hf_data_tbl_updated$farmland == 'Yes', ],
                             lng = ~long,
                             lat = ~lat,
                             radius = 1,
                             color = 'green',
                             label = ~labels,
                             # clusterOptions = markerClusterOptions(),
                             # clusterId = "cluster1",
                             popup = ~paste(name, "<br>Cluster:", cluster,
                                            "<br>Cluster Head:", cluster_head,
                                            "<br>State:", state,
                                            "<br>Sex:", sex_of_respondent),
                             # layerId = rownames(hf_data_tbl_updated),
                             group = 'Own A Farmland') |> 
            addCircleMarkers(data = hf_data_tbl_updated[hf_data_tbl_updated$farmland == 'No', ],
                             lng = ~long,
                             lat = ~lat,
                             radius = 1,
                             color = 'red',
                             label = ~labels,
                             # clusterOptions = markerClusterOptions(),
                             # clusterId = "cluster1",
                             popup = ~paste(name, "<br>Cluster:", cluster,
                                            "<br>Cluster Head:", cluster_head,
                                            "<br>State:", state,
                                            "<br>Sex:", sex_of_respondent),
                             # layerId = rownames(hf_data_tbl_updated),
                             group = 'Own No Farmland') |> 
            addLayersControl(overlayGroups = c('Own A Farmland', 'Own No Farmland'),
                             options = layersControlOptions(collapsed = FALSE),
                             position = 'bottomleft')
    })
    
    
    filteredData <- eventReactive(req(input$hf_states, input$hf_lgas),{
       
        hf_data_tbl_updated |> 
            filter(state==input$hf_states, 
                   lga ==input$hf_lgas, 
                   sex_of_respondent==input$hf_gender)
    })
    
    
    observeEvent(req(input$hf_states),{
        
        lga_choices <- hf_data_tbl_updated |> 
            filter(state==input$hf_states) |> 
            select(lga) |> pull(lga)
        
        updateSelectizeInput(session, 'hf_lgas', 
                             choices = lga_choices, 
                             server = TRUE)
        
    })
    
    observeEvent(input$hf_search,{
        
        if(input$hf_states == "All"){
            
            labels_proxy_all <- sprintf(
                "<strong>%s</strong><br/>Cluster: %s",
                hf_data_tbl_updated$name,
                hf_data_tbl_updated$cluster) |>
                lapply(htmltools::HTML)
            
            leafletProxy("map") |>
                clearMarkers() |>
                # clearMarkerClusters() |>
                # removeMarkerFromCluster(input$remove1, "cluster1") |>
                addTiles() |>
                fitBounds(lng1 = min(hf_data_tbl_updated$long), 
                          lat1 = min(hf_data_tbl_updated$lat), 
                          lng2 = max(hf_data_tbl_updated$long), 
                          lat2 = max(hf_data_tbl_updated$lat)) |>
                addProviderTiles(providers$Esri.WorldStreetMap) |>
                addCircleMarkers(data = hf_data_tbl_updated[hf_data_tbl_updated$farmland == 'Yes', ],
                                 lng = ~long,
                                 lat = ~lat,
                                 radius = 1,
                                 color = 'green',
                                 label = ~labels_proxy_all,
                                 # clusterOptions = markerClusterOptions(),
                                 # clusterId = "cluster2",
                                 popup = ~paste(name, "<br>Cluster:", cluster,
                                                "<br>Cluster Head:", cluster_head,
                                                "<br>State:", state,
                                                "<br>Sex:", sex_of_respondent),
                                 # layerId = rownames(filteredData()),
                                 group = 'Own A Farmland') |> 
                addCircleMarkers(data = hf_data_tbl_updated[hf_data_tbl_updated$farmland == 'No', ],
                                 lng = ~long,
                                 lat = ~lat,
                                 radius = 1,
                                 color = 'red',
                                 label = ~labels_proxy_all,
                                 # clusterOptions = markerClusterOptions(),
                                 # clusterId = "cluster2",
                                 popup = ~paste(name, "<br>Cluster:", cluster,
                                                "<br>Cluster Head:", cluster_head,
                                                "<br>State:", state,
                                                "<br>Sex:", sex_of_respondent),
                                 # layerId = rownames(filteredData()),
                                 group = 'Own No Farmland') |> 
                addLayersControl(overlayGroups = c('Own A Farmland', 'Own No Farmland'),
                                 options = layersControlOptions(collapsed = FALSE),
                                 position = 'topleft')
        } else{
            
            labels_proxy <- sprintf(
                "<strong>%s</strong><br/>Cluster: %s",
                filteredData()$name,
                filteredData()$cluster) |>
                lapply(htmltools::HTML)
            
            leafletProxy("map") |>
                clearMarkers() |>
                addTiles() |>
                fitBounds(lng1 = min(filteredData()$long), 
                          lat1 = min(filteredData()$lat), 
                          lng2 = max(filteredData()$long), 
                          lat2 = max(filteredData()$lat)) |>
                addProviderTiles(providers$Esri.WorldStreetMap) |>
                addCircleMarkers(data = filteredData()[filteredData()$farmland == 'Yes', ],
                                 lng = ~long,
                                 lat = ~lat,
                                 radius = 1,
                                 color = 'green',
                                 label = ~labels_proxy,
                                 # clusterOptions = markerClusterOptions(),
                                 # clusterId = "cluster2",
                                 popup = ~paste(name, "<br>Cluster:", cluster,
                                                "<br>Cluster Head:", cluster_head,
                                                "<br>State:", state,
                                                "<br>Sex:", sex_of_respondent),
                                 # layerId = rownames(filteredData()),
                                 group = 'Own A Farmland') |> 
                addCircleMarkers(data = filteredData()[filteredData()$farmland == 'No', ],
                                 lng = ~long,
                                 lat = ~lat,
                                 radius = 1,
                                 color = 'red',
                                 label = ~labels_proxy,
                                 # clusterOptions = markerClusterOptions(),
                                 # clusterId = "cluster2",
                                 popup = ~paste(name, "<br>Cluster:", cluster,
                                                "<br>Cluster Head:", cluster_head,
                                                "<br>State:", state,
                                                "<br>Sex:", sex_of_respondent),
                                 # layerId = rownames(filteredData()),
                                 group = 'Own No Farmland') |> 
                addLayersControl(overlayGroups = c('Own A Farmland', 'Own No Farmland'),
                                 options = layersControlOptions(collapsed = FALSE),
                                 position = 'topleft')
        }
        
    })
    
    
    
}

shinyApp(ui, server)

