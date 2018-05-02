shinyServer(function(input, output){
  # show bar plot
  output$pump_plot <- renderPlotly({
    if(input$selected == "basin"){
      tanz %>% group_by(basin,status_group) %>% summarise(count = n()) %>%
        group_by(basin) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~basin, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack', title = "Functionality by Basin", autosize = F, 
               width = 1000, height = 500, margin = m, xaxis = list(title = ""), yaxis = list(title = ""))
    }else if(input$selected == "region"){
      tanz %>% group_by(region,status_group) %>% summarise(count = n()) %>%
        group_by(region) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~region, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack', title = "Functionality by Region", autosize = F, 
               width = 1000, height = 500, margin = m, xaxis = list(title = ""), yaxis = list(title = ""))
    }else if(input$selected == "extraction_type"){
      tanz %>% group_by(extraction_type,status_group) %>% summarise(count = n()) %>%
        group_by(extraction_type) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~extraction_type, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack', title = "Functionality by Extraction Type",
               autosize = F, width = 1000, height = 500, margin = m, xaxis = list(title = ""), yaxis = list(title = ""))
    }else if(input$selected == "water_quality"){
      tanz %>% group_by(water_quality,status_group) %>% summarise(count = n()) %>%
        group_by(water_quality) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~water_quality, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack', title = "Functionality by Water Quality", 
               autosize = F, width = 1000, height = 500, margin = m, xaxis = list(title = ""), yaxis = list(title = ""))
    }else if(input$selected == "waterpoint_type"){
      tanz %>% group_by(waterpoint_type,status_group) %>% summarise(count = n()) %>%
        group_by(waterpoint_type) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~waterpoint_type, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack', title = "Functionality by Waterpoint Type", 
               autosize = F, width = 1000, height = 500, margin = m, xaxis = list(title = ""), yaxis = list(title = ""))
    }else if(input$selected == "payment"){
      tanz %>% group_by(payment,status_group) %>% summarise(count = n()) %>%
        group_by(payment) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~payment, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack', title = "Functionality by Payment", 
               autosize = F, width = 1000, height = 500, margin = m, xaxis = list(title = ""), yaxis = list(title = ""))
    }else if(input$selected == "construction_year"){
      tanzconst %>% group_by(construction_year,status_group) %>% summarise(count = n()) %>%
        group_by(construction_year) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~construction_year, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack', title = "Functionality by Construction Year",
               autosize = F, width = 1000, height = 500, margin = m, xaxis = list(title = ""), yaxis = list(title = ""))
    }else if(input$selected == "gps_height"){
      tanz_histo %>% plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~gps_height, color = ~status_group) %>%
        layout(barmode = "overlay", title = "Functionality by GPS Height")
    }

  

  })
  
  output$map <- renderLeaflet({
    leaflet(tanz_region) %>% setView(lng = 36, lat = -5, zoom = 5) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addWebGLHeatmap(lng=~longitude, lat=~latitude,size=3,units='px')
  })
 

  # show data using DataTable
  output$table <- DT::renderDataTable({
    datatable(tanz_sel, rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
})