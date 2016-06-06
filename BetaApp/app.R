
library(shiny)
library(MPAtools)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)

# Define UI for application that draws a histogram
ui = navbarPage("A tool to evaluate the effectiveness of Marine Reserves",
      # First tab starts here
      tabPanel("Options",
               sidebarPanel(
                 selectInput(inputId="obj",
                             label="What is the objective of this reserve?",
                             choices=c("Protect a single species",
                                       "Protect the entire ecosystem",
                                       "Increase fishing revenues"),
                             selected="Protect a single species"),
                 selectInput(inputId="si",
                             label="Select a site",
                             choices=c("Isla Natividad" = "IslaNatividad",
                                       "Isla Magdalena"="IslaMagdalena",
                                       "El Rosario"="ElRosario"),
                             selected="IslaNatividad")
                 )
               ),
      #Second tab starts here
      tabPanel("Inputs",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId="sp",
                               label="Select a species",
                               choices=c("Paralabrax clathratus",
                                         "Paralabrax nebulifer"),
                               selected="Paralabrax clathratus"),
                   sliderInput(inputId="y0",
                               label="Select year of implementation",
                               min=1990,
                               max=2016,
                               value=2006),
                   sliderInput(inputId="y1",
                               label="Select year of implementation",
                               min=1990,
                               max=2016,
                               value=2015),
                   fileInput(inputId="indata",
                             label="Choose a file to upload",
                             accept = c(".xslx",
                                        ".csv",
                                        ".xls"))
                   ),
                 mainPanel(p("For the objectives you chose, your inputs must look like this:"),
                           img(src="Table1.png", width="600px")))
               ),
      
      #Third tab starts here
      tabPanel("Outputs",
               sidebarPanel("Difference in Difference values for each indicator",
                            tableOutput("table")),
               mainPanel(plotOutput("plot")),
               actionButton(inputId="pdf", "Downlad PDF"))
      )

# Define server logic required to draw a histogram
server = function(input, output) {
  
  # data = reactive({
  #   
  #   inFile = input$indata
  #   
  #   if (is.null(inFile)){
  #     return(NULL)
  #   }
  #   
  #   read.csv(inFile$datapath)
  # 
  #   })
  # 
  # s=reactive({richness(data, input$si)}) #Calculate species richness for Natividad
  # t=reactive({trophic(data, input$si)}) #Calculate mean trophic level for Natividad
  # D=reactive({density(data, input$si, input$sp)}) #Calculate density for P. clathratus for Natividad
  # L=reactive({fish_size(data, input$si, input$sp)}) #Calculate mean size for P. clathratus for Natividad
  # B=reactive({fish_biomass(data, input$si, input$sp)}) #Calculate biomass for P. clathratus for Natividad
  # 
  # DD_S=reactive({did(s(), input$y0, input$y1)}) # For species richness
  # DD_T=reactive({did(t(), input$y0, input$y1)}) # For mean trophic level
  # DD_D=reactive({did(D(), input$y0, input$y1)}) # For density (of P. clathratus)
  # DD_L=reactive({did(L(), input$y0, input$y1)}) # For mean size (of P. clathratus)
  # DD_B=reactive({did(B(), input$y0, input$y1)}) # For biomass (of P. clathratus)
  
  output$plot <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile = input$indata

    if (is.null(inFile)){
      return(NULL)
    }

    data=read.csv(inFile$datapath)

    s=richness(data, input$si) #Calculate species richness for Natividad
    t=trophic(data, input$si) #Calculate mean trophic level for Natividad
    D=density(data, input$si, input$sp) #Calculate density for P. clathratus for Natividad
    L=fish_size(data, input$si, input$sp) #Calculate mean size for P. clathratus for Natividad
    B=fish_biomass(data, input$si, input$sp) #Calculate biomass for P. clathratus for Natividad

    DD_S=did(s, input$y0, input$y1) # For species richness
    DD_T=did(t, input$y0, input$y1) # For mean trophic level
    DD_D=did(D, input$y0, input$y1) # For density (of P. clathratus)
    DD_L=did(L, input$y0, input$y1) # For mean size (of P. clathratus)
    DD_B=did(B, input$y0, input$y1) # For biomass (of P. clathratus)
    
    #richness plot
    plot_s=ggplot(s, aes(x=Year, y=S, color=Zone))+
      geom_point()+
      stat_summary(fun.y="mean", geom="line")+
      theme_bw()
    
    #for trophic level
    plot_t=ggplot(t, aes(x=Year, y=mean, color=Zone))+
      geom_point()+
      stat_summary(fun.y="mean", geom="line")+
      theme_bw()+
      labs(y="mean trophic level")
    
    #for density
    plot_d=ggplot(D, aes(x=Year, y=D, color=Zone))+
      geom_point()+
      stat_summary(fun.y="mean", geom="line")+
      theme_bw()+
      labs(y="D'")

    #for fish size
    plot_l=ggplot(L, aes(x=Year, y=mean, color=Zone))+
      geom_point()+
      stat_summary(fun.y="mean", geom="line")+
      theme_bw()+
      labs(y="Length (cm)")
    
    #for biomass
    
    plot_b=ggplot(B, aes(x=Year, y=`sum(W)`, color=Zone))+
      geom_point()+
      stat_summary(fun.y="mean", geom="line")+
      theme_bw()+
      labs(y="Biomass (Kg)")
    
    grid.arrange(plot_s,
                 plot_t,
                 plot_d,
                 plot_l,
                 plot_b)
  })
  
  output$table=renderTable({
    inFile = input$indata

    if (is.null(inFile)){
      return(NULL)
    }

    data=read.csv(inFile$datapath)

    s=richness(data, input$si) #Calculate species richness for Natividad
    t=trophic(data, input$si) #Calculate mean trophic level for Natividad
    D=density(data, input$si, input$sp) #Calculate density for P. clathratus for Natividad
    L=fish_size(data, input$si, input$sp) #Calculate mean size for P. clathratus for Natividad
    B=fish_biomass(data, input$si, input$sp) #Calculate biomass for P. clathratus for Natividad

    DD_S=did(s, input$y0, input$y1) # For species richness
    DD_T=did(t, input$y0, input$y1) # For mean trophic level
    DD_D=did(D, input$y0, input$y1) # For density (of P. clathratus)
    DD_L=did(L, input$y0, input$y1) # For mean size (of P. clathratus)
    DD_B=did(B, input$y0, input$y1) # For biomass (of P. clathratus)
    # 
    stdlb=data.frame(Indicator=c("Richness","Trophic Level", "Density", "Length", "Biomass"),
                     DDValue=c(coef(DD_S)[3],
                               coef(DD_T)[3],
                               coef(DD_D)[3],
                               coef(DD_L)[3],
                               coef(DD_B)[3]))
    return(stdlb)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

