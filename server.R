shinyServer(function(input, output){
  # show bar plot
  output$pump_plot <- renderPlot({
    if(input$selected == "basin"){
      tanz %>% group_by(basin,status_group) %>% summarise(count = n()) %>%
        group_by(basin) %>% mutate(perc = count/sum(count)) %>% 
        ggplot(aes(x = basin, y = perc, fill = status_group)) +
        geom_bar(stat="identity","position" = "dodge")+
        ggtitle("Basin Pump Functionality Dist") +
        xlab("Location") + ylab("Percent of Pump Function") +
        theme(plot.title = element_text(hjust = 0.5))
    }else if(input$selected == "region"){
      tanz %>% group_by(region,status_group) %>% summarise(count = n()) %>%
        group_by(region) %>% mutate(perc = count/sum(count)) %>% 
        ggplot(aes(x = region, y = perc, fill = status_group)) +
        geom_bar(stat="identity")+
        ggtitle("Region Pump Functionality Dist") +
        xlab("Location") + ylab("Percent of Pump Function") +
        theme(plot.title = element_text(hjust = 0.5))
    }else if(input$selected == "extraction_type"){
      tanz %>% group_by(extraction_type,status_group) %>% summarise(count = n()) %>%
        group_by(extraction_type) %>% mutate(perc = count/sum(count)) %>% 
        ggplot(aes(x = extraction_type, y = perc, fill = status_group)) +
        geom_bar(stat="identity","position" = "dodge")+
        ggtitle("Extraction Type Pump Functionality Dist") +
        xlab("Extraction Type") + ylab("Percent of Pump Function") +
        theme(plot.title = element_text(hjust = 0.5))
    }else if(input$selected == "water_quality"){
      tanz %>% group_by(water_quality,status_group) %>% summarise(count = n()) %>%
        group_by(water_quality) %>% mutate(perc = count/sum(count)) %>% 
        ggplot(aes(x = water_quality, y = perc, fill = status_group)) +
        geom_bar(stat="identity","position" = "dodge")+
        ggtitle("Water Quality Pump Functionality Dist") +
        xlab("Water Quality") + ylab("Percent of Pump Function") +
        theme(plot.title = element_text(hjust = 0.5))
    }else if(input$selected == "waterpoint_type"){
      tanz %>% group_by(waterpoint_type,status_group) %>% summarise(count = n()) %>%
        group_by(waterpoint_type) %>% mutate(perc = count/sum(count)) %>% 
        ggplot(aes(x = waterpoint_type, y = perc, fill = status_group)) +
        geom_bar(stat="identity","position" = "dodge")+
        ggtitle("Waterpoint Pump Functionality Dist") +
        xlab("Waterpoint Type") + ylab("Percent of Pump Function") +
        theme(plot.title = element_text(hjust = 0.5))
    }else if(input$selected == "payment"){
      tanz %>% group_by(payment,status_group) %>% summarise(count = n()) %>%
        group_by(payment) %>% mutate(perc = count/sum(count)) %>% 
        ggplot(aes(x = payment, y = perc, fill = status_group)) +
        geom_bar(stat="identity","position" = "dodge")+
        ggtitle("Waterpoint Pump Functionality Dist") +
        xlab("Payment Type") + ylab("Percent of Pump Function") +
        theme(plot.title = element_text(hjust = 0.5))
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