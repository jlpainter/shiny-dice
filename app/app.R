## ###########################################################
##
## dice/app.R
##
## Educational shiny app to teach students the ideas
## behind a chi-square distribution using the investigation
## of different dice making companies
##
## @author: Craig Lazarski & Jeffery Painter
## @modified: 2020-Dec-24
##
## ###########################################################

library(shiny)
library(ggplot2)
source("dice_functions.R")

# Load these to start
all_companies = getDiceCompanyNamesAsVector()

# store these once for all users
dice_distributions <- getMyDistributions()

ui <- navbarPage(

  title="Dice Explorer",
  
  ## ###########################################################
  ## Panel 1
  ## ###########################################################
  tabPanel("Analysis",
           
           sidebarLayout(
             
             sidebarPanel(
               em("Select a company and sample size"),
               
               # input
               selectInput(inputId = "panel1_input_company_id", label="Dice Company", all_companies, selected=1),
               
               # input
               numericInput(inputId = "panel1_input_sample_size", 
                            label="Sample Size", value = 10, min=1, max=10000),
               
               actionButton(inputId="panel1_btn_graph", label="Graph Samples"),
               
               # text output
               htmlOutput(outputId="panel1_output_dice_freq")        
               
             ),
             
             mainPanel(
               
               HTML('<div style="float: right;"><img src="color-dice.png" style="width:150px;"></div>'),
               includeHTML("www/task_01.html"),
               
               # output
               plotOutput(outputId = "panel1_histogram_plot"),
             )
           ),    
           
  ),
  
  ## ###########################################################
  ## Panel 2
  ## ###########################################################
  tabPanel("Chi Square Analysis",
           sidebarLayout(
             
             sidebarPanel(
               # input
               selectInput(inputId = "panel2_input_company_id", 
                           label="Dice Company", all_companies, selected=1),
               
               # input
               numericInput(inputId = "panel2_input_sample_size", 
                            label="Sample Size", value = 10, min=1, max=10000),
               
               actionButton(inputId="panel2_btn_graph", 
                            label="Update Sample Size"),
               
               # text output
               htmlOutput(outputId="panel2_output_dice_percents"),
               
               # text output
               verbatimTextOutput(outputId="panel2_output_test_statistic"),
             ),
             
             mainPanel(
               includeHTML("www/task_02.html"),

               # output
               fluidRow(
                 column(5, plotOutput(outputId = "panel2_graph_histogram"),),
                 column(5, plotOutput(outputId= "panel2_graph_chisquare"))
               ),
             )
           ),
  ),
  ## ###########################################################
  ## Panel 3: Power
  ## ###########################################################
  tabPanel("Power",
           sidebarLayout(
             
             sidebarPanel(
               # input
               selectInput(inputId = "panel3_input_company_id", label="Dice Company", all_companies, selected=1),
               
               # input
               numericInput(inputId = "panel3_input_sample_size", 
                            label="Sample Size", value = 10, min=1, max=10000),
               
               numericInput(inputId = "panel3_input_simulations", 
                            label="Number of simulations (500 max)", value = 1, min=1, max=500),
               
               actionButton(inputId="panel3_btn_graph", label="Update Sample Size"),
               
               hr(),                         # html - adds break line
               h4("True weight of dice"),    # html header level 4
               plotOutput(outputId = "panel3_graph_company_weights", 
                          height = "150px", width="100%")
               
             ),
             
             mainPanel(
             
               includeHTML("www/task_03.html"),
               
               plotOutput(outputId= "panel3_graph_chisquare"),
               
               # text output
               verbatimTextOutput(outputId="panel3_output_test_statistic")
             )
           )          
  ),
  tabPanel("About",
     includeHTML("www/about.html")
  )
)




server <- function(input, output) {

  ## #################################
  ## Panel 1 input/outputs
  ## #################################
  # INPUT
  #   panel1_input_company_id
  #   panel1_input_sample_size
  #   panel1_btn_graph
  ## #################################
  # OUTPUT
  #   panel1_output_dice_freq 
  ## #################################
  
  rv <- eventReactive( input$panel1_btn_graph, {
    total_samples = input$panel1_input_sample_size
    companyId = as.numeric(input$panel1_input_company_id) 
    companyWeights = dice_distributions[[companyId]]
    dice_roll = sample( companyWeights, total_samples, replace=T )
    tbl = table(dice_roll)
    dd = data.frame(tbl)
    
    # check for missing dice rolls
    all_rolls = c(1:6)
    dice_rolled = as.numeric(row.names(tbl))
    # need labels as a string to add to data frame
    missing_rolls = as.character(setdiff(all_rolls, dice_rolled))                                         
    for ( dice in missing_rolls )
    {
      newrow = data.frame("dice_roll" = dice, "Freq" = 0)
      dd = rbind(dd, newrow)  
    }
    
    dd$percent <- dd$Freq / sum(dd$Freq)
    
    # dice are factors and need this to order empty dice rolls
    dd$dice_roll <- factor( dd$dice_roll, levels = as.character(c(1:6)))
    dd = dd[order(dd$dice_roll),]
    row.names(dd) <- NULL
    results_dataframe = dd
    return(dd)
  })
  
 
  ######################################################
  # Panel 1
  ######################################################
  myTitle <- eventReactive( input$panel1_btn_graph, {
    companyName = getCompanyById(as.numeric(input$panel1_input_company_id))
    rolls = input$panel1_input_sample_size
    title = paste(rolls, " dice rolls from Company: ", companyName)
  })
  
  # ggplot our data frame    
  output$panel1_histogram_plot <- renderPlot(
    { 
      ggplot(rv())+geom_bar(aes(x=dice_roll, y=Freq, fill=dice_roll), stat='identity')+
        ggtitle(myTitle()) +
        theme(plot.title = element_text(lineheight=0.8, size=20, face="bold")) +
        labs( x = "Dice Roll", y = "Frequency")
    }
  )
  
  output$panel1_output_dice_freq <- renderPrint(
    {
      # build output as html table
      df = rv()
      
      output = "<table style='border: 1px solid black; padding: 15px; width: 100%; text-align: center'> <tr style='background-color:#FFE4C4'> <th> Dice Roll </th> <th> Freq </th> <th> Percent </th> </tr>"
      for ( row in 1:nrow(df) )
      {
        entry = df[row,]
        output = paste(output, "<tr><td>", entry$dice_roll, "</td> <td>", entry$Freq, "</td><td>", entry$percent, "</td></tr>")
      }
      output = paste(output, "</table>")
      
      # print the table
      HTML(output)    
      
    })
  
  
  ## #################################
  ## Panel 2 input/outputs
  ## #################################
  # INPUT
  #   panel2_input_company_id
  #   panel2_input_sample_size
  #   panel2_btn_graph
  ## #################################
  # OUTPUT
  #   panel2_output_dice_percents
  #   panel2_output_test_statistic
  #   panel2_graph_histogram
  #   panel2_graph_chisquare
  ## #################################                   

  #####################################################
  # Panel 2
  ######################################################
  rv2 <- eventReactive( input$panel2_btn_graph, {
    total_samples = input$panel2_input_sample_size
    companyId = as.numeric(input$panel2_input_company_id) 
    sample_dist = dice_distributions[[companyId]]
    dice_roll = sample( sample_dist, total_samples, replace=T )
    tbl = table(dice_roll)
    dd = data.frame(tbl)
    
    # check for missing dice rolls
    all_rolls = c(1:6)
    dice_rolled = as.numeric(row.names(tbl))
    # need labels as a string to add to data frame
    missing_rolls = as.character(setdiff(all_rolls, dice_rolled))                                         
    for ( dice in missing_rolls )
    {
      newrow = data.frame("dice_roll" = dice, "Freq" = 0)
      dd = rbind(dd, newrow)  
    }
    
    dd$percent <- dd$Freq / sum(dd$Freq)
    
    # dice are factors and need this to order empty dice rolls
    dd$dice_roll <- factor( dd$dice_roll, levels = as.character(c(1:6)))
    dd = dd[order(dd$dice_roll),]
    row.names(dd) <- NULL
    results_dataframe = dd
    return(dd)
  })
  
  ts <- function(dd) {
    # company ID    
    companyId = as.numeric(isolate(input$panel2_input_company_id))
    weights = getCompanyWeight(companyId)
    total_rolls = sum(dd$Freq)
    exp_counts = (1.0/6.0) * total_rolls
    test_statistic = sum((dd$Freq - exp_counts)^2 / exp_counts)
    p_value = pchisq(test_statistic, df=5, lower.tail=FALSE)
    results = data.frame("test_statistic" = test_statistic, "p-value" = round(p_value, 4)) 
    return(results)
  }
  
  myTitle2 <- eventReactive( input$panel2_btn_graph, {
    companyName = getCompanyById(as.numeric(input$panel2_input_company_id))
    rolls = input$panel2_input_sample_size
    title = paste(companyName, ":", rolls, "dice rolls")
  })
  
  # ggplot our data frame    
  output$panel2_graph_histogram <- renderPlot(
    { 
      ggplot(rv2())+geom_bar(aes(x=dice_roll, y=Freq, fill=dice_roll), stat='identity')+
        ggtitle(myTitle2()) +
        theme(plot.title = element_text(lineheight=0.8, size=20, face="bold")) +
        geom_hline(yintercept = isolate(input$panel2_input_sample_size/6), linetype="dashed", color="red") +
        annotate("text", x=3.25, y=isolate(input$panel2_input_sample_size/5.8), label = "Expected Value", color="red", size= 6) +
        labs( x = "Dice Roll", y = "Frequency")
    }
  )
  output$panel2_graph_chisquare <- renderPlot(
    {
      #create density curve
      curve(dchisq(x, df = 5), from = 0, to = 40,
            main = 'Chi-Square Distribution (df = 5)',
            ylab = 'Density',
            lwd = 2)
      
      #create vector of x values
      x_vector <- seq(11.07, 40)
      
      #create vector of chi-square density values
      p_vector <- dchisq(x_vector, df = 5)
      
      #fill in portion of the density plot from 0 to 40
      polygon(c(x_vector, rev(x_vector)), c(p_vector, rep(0, length(p_vector))),
              col = adjustcolor('red', alpha=0.5), border = NA)
      text(11.07, 0.1, "alpha =0.05", col = "red")
      text(15, 0.05, "Reject", col="red")
      text(5, 0.05, "Fail to reject", col = "blue")
      abline(v = 11.07, lwd = 3, lty=3, col = "blue")
      
      TS = ts(rv2())$test_statistic
      t_vector <- seq(TS, 40)
      tsp_vector <- dchisq(t_vector, df=5)
      polygon(c(t_vector, rev(t_vector)), c(tsp_vector, rep(0, length(tsp_vector))),
              col = adjustcolor('green', alpha=0.5), border = NA)
      
      points(TS,0.001, pch=19, lwd=5)
    }
  )
  
  output$panel2_output_dice_percents <- renderPrint(
    {
      # build output as html table
      df = rv2()
      
      output = "<table style='border: 1px solid black; padding: 15px; width: 100%; text-align: center'> <tr style='background-color:#FFE4C4'> <th> Dice Roll </th> <th> Freq </th> <th> Percent </th> </tr>"
      for ( row in 1:nrow(df) )
      {
        entry = df[row,]
        output = paste(output, "<tr><td>", entry$dice_roll, "</td> <td>", entry$Freq, "</td><td>", round(entry$percent, 2), "</td></tr>")
      }
      output = paste(output, "</table>")
      
      # print the table
      HTML(output)    
      
    })
  
  output$panel2_output_test_statistic <- renderPrint( { print(ts(rv2()), row.names = F) } )

  ## #################################
  ## Panel 3 input/outputs
  ## #################################
  # INPUT
  #   panel3_input_company_id
  #   panel3_input_sample_size
  #   panel3_input_simulations
  #   panel3_btn_graph
  ## #################################
  # OUTPUT
  #   panel3_graph_chisquare
  #   panel3_output_test_statistic
  ## #################################                
    
  ######################################################
  # Panel 3
  ######################################################
  
  panel3_ggplot_title <- eventReactive( input$panel3_btn_graph, {
    companyName = getCompanyById(as.numeric(input$panel3_input_company_id))
    rolls = input$panel3_input_sample_size
    sims = input$panel3_input_simulations
    title = paste(companyName,":", sims, "simulations of", rolls, "rolls")
  })
  
  rv3 <- eventReactive( input$panel3_btn_graph, {
    total_samples = input$panel3_input_sample_size
    companyId = as.numeric(input$panel3_input_company_id) 
    trial_size = input$panel3_input_simulations
    sample_dist = dice_distributions[[companyId]]
    
    alltstats = data.frame()
    
    for( trial in 1:trial_size){
      dice_roll = sample( sample_dist, total_samples, replace=T )
      tbl = table(dice_roll)
      dd = data.frame(tbl)
      
      # check for missing dice rolls
      all_rolls = c(1:6)
      dice_rolled = as.numeric(row.names(tbl))
      # need labels as a string to add to data frame
      missing_rolls = as.character(setdiff(all_rolls, dice_rolled))                                         
      for ( dice in missing_rolls )
      {
        newrow = data.frame("dice_roll" = dice, "Freq" = 0)
        dd = rbind(dd, newrow)  
      }
      
      dd$percent <- dd$Freq / sum(dd$Freq)
      tstats = ts(dd)
      alltstats = rbind(alltstats, tstats)
    }
    
    return(alltstats)
  })
  
  
  output$panel3_output_test_statistic <- renderPrint(
    {
      df = rv3()
      fail_to_reject = nrow(df[df$test_statistic < 11.07,])/nrow(df)
      reject = nrow(df[df$test_statistic > 11.07,])/nrow(df)
      Decision = c("Type 2 error", "Power")
      # Round to 4 decimal places each
      Rate = c( round(fail_to_reject, 4), round(reject, 4) )
      output = data.frame(Decision, Rate)
      print(output, row.names=F)
    })
  
  # ggplot our data frame    
  output$panel3_graph_chisquare <- renderPlot(
    { 
      data = rv3()
      x <- data$test_statistic
      y1 <- pchisq(seq(0,40,length=1),5)
      x1 <- seq(0,40, length=.5)
      ggplot(data, aes(x)) + 
        ggtitle(panel3_ggplot_title()) +
        theme(plot.title = element_text(lineheight=0.8, size=20, face="bold"), axis.title.y=element_blank(),
              axis.text.y=element_blank()) +
        coord_cartesian(xlim = c(0, 80), ylim=c(-.01, 0.8)) + 
        geom_dotplot(binwidth=1)+
        #geom_histogram(aes(x, y=..density..), binwidth = 1) 
        geom_vline(xintercept=11.07, linetype="dashed", color = "red", size=2) +
        annotate("text", x=22, y=.3, label = "Rejection region", color="red", size= 6) +
        annotate("text", x=22, y=.25, label = expression(chi^2 ~ "> 11.07"), color ="red", size =4) +
        labs( x = "Test Statistics", y = "Frequency")
    }
  )
  
  ## Output the true weights of a company
  output$panel3_graph_company_weights <- renderPlot(
    { 
      companyId = as.numeric(input$panel3_input_company_id)
      weights = getCompanyWeight(companyId)
      # convert to a dataframe
      df = data.frame()
      dice_roll = 1
      for ( weight in weights )
      {
        row = data.frame("dice_roll" = dice_roll, "Weight" = weight)
        df = rbind(df, row)
        dice_roll = dice_roll + 1
      }
      df$dice_roll <- factor( df$dice_roll, levels = as.character(c(1:6)))
      ggplot(df)+geom_bar(aes(x=dice_roll, y=Weight, fill=dice_roll ), stat='identity') + 
        xlab("Roll") + ylab("Percent")
    }
  )
  
}

shinyApp(ui = ui, server=server)
