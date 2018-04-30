shinyServer(function(input, output){
  # show bar plot
  output$pump_plot <- renderPlotly({
    if(input$selected == "basin"){
      tanz %>% group_by(basin,status_group) %>% summarise(count = n()) %>%
        group_by(basin) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~basin, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack')
    }else if(input$selected == "region"){
      tanz %>% group_by(region,status_group) %>% summarise(count = n()) %>%
        group_by(region) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~region, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack')
    }else if(input$selected == "extraction_type"){
      tanz %>% group_by(extraction_type,status_group) %>% summarise(count = n()) %>%
        group_by(extraction_type) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~extraction_type, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack')
    }else if(input$selected == "water_quality"){
      tanz %>% group_by(water_quality,status_group) %>% summarise(count = n()) %>%
        group_by(water_quality) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~water_quality, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack')
    }else if(input$selected == "waterpoint_type"){
      tanz %>% group_by(waterpoint_type,status_group) %>% summarise(count = n()) %>%
        group_by(waterpoint_type) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~waterpoint_type, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack')
    }else if(input$selected == "payment"){
      tanz %>% group_by(payment,status_group) %>% summarise(count = n()) %>%
        group_by(payment) %>% mutate(perc = count/sum(count)) %>% 
        plot_ly(x = ~payment, y = ~perc, color = ~status_group, type='bar') %>% 
        layout(barmode = 'stack')
    }else if(input$selected == "gps_height"){
      tanz_histo %>% plot_ly(alpha = 0.6) %>%
        add_histogram(x = ~gps_height, color = ~status_group) %>%
        layout(barmode = "overlay")
    }

  })
  
  output$map <- renderPlot({
    ggplot() +
      geom_polygon(data = tanz,
                   aes(x=longitude, y=latitude, group=group)) +
      coord_equal() + geom_point(data = tanz,
                                 aes(x=longitude, y=latitude), color="red", alpha = .2) +
      ggtitle("Pump Functionality") +
      xlab("Latitude") + ylab("Longitude") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # show data using DataTable
  output$table <- DT::renderDataTable({
    datatable(tanz_sel, rownames=FALSE) %>% 
      formatStyle(input$selected, background="skyblue", fontWeight='bold')
  })
})