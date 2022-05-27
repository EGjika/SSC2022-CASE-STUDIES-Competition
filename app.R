packages = c("shiny","DT","gplots","dygraphs","tibble","ggplot2","dplyr","Metrics","ggplot2","utf8","rpivotTable","lubridate","tidyverse")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if(!require(x,character.only = TRUE)){
      install.packages(x,dependencies = TRUE)
      library(x,character.only = TRUE) 
    }
  }
)

list_<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      "Dashboard",
      tabPanel("SSC2022-Physician ICU Performance",
               sidebarPanel(
                 fileInput("dataset","Data:")
                 ,
                 uiOutput("variable"), 	
                 uiOutput("group"),  		
                 selectInput("plot.type","Plot Type:", 
                             list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")
                 ),
                 checkboxInput("show.points", "show points", TRUE)
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Summary",
                            tags$div(verbatimTextOutput("summary"))
                            
                   ),
                   tabPanel("Visualization",
                            h3(textOutput("caption")),
                            uiOutput("plot")
                            ),
                   tabPanel("Data",
                            tags$div(h4("Table"),
                                     dataTableOutput("table"))
                   ),
                   tabPanel("About",
                            h4("This shiny app will help in understanding of the datasets used in SSC2022 competition."),
                            h4(p(span("Authors: Eralda Gjika, Armando Caci", style = "color:red"))),
                            HTML('
        <div style="clear: left;">
        <p>
        <a href="https://al.linkedin.com/in/eralda-dhamo-gjika-71879128" target="_blank"> Eralda Gjika (Dhamo)</a><br>
        Master Student, School of Mathematics and Statistics<br>
        Carleton University<br>
        More information about the competition may be found here: <br>
        <a href="https://ssc.ca/en/meetings/annual/2022-annual-meeting/case-studies-competition">SSC2022</a><br>
       More information about the Shiny App may be found here: <br>
        <a href="https://github.com/EGjika/SSC2022-CASE-STUDIES-Competition" target="_blank">GitHub</a> <br/>
        </p>
        To use the app you need to import the dataset and save it in CSV format from this source: <br>
        <a href="https://ssc.ca/en/case-study/developing-a-physician-performance-model-critical-care-assessing-quality-and-value" target="_blank">SSC2022_Data</a> <br/>
        </p>
        <br>

                                 ')
                   )
                   
                 )
               )
      ),
      
    )
  )
  ,
  server <- function(input, output,session) {
    data <- reactive({
      inFile <- input$dataset
      if (is.null(inFile)) return(NULL)
      df <- read.csv(inFile$datapath)
      df
    })
    

    output$summary<-renderPrint({
      summary(data())
    })
    
    output$table <- renderDataTable({
      datatable(data()[])
    })
    
    output$variable <- renderUI({ 
      obj<-data()[]
      var.opts<-list_(colnames(obj))
      selectInput("variable","Variable:", var.opts) 				 
    }) 
    
    output$group <- renderUI({ 
      obj<-data()[]
      
      var.opts<-list_(colnames(obj))
      selectInput("group","Groups:", var.opts)  				 
    })
    

    output$caption<-renderText({
      switch(input$plot.type,
             "boxplot" 	= 	"Boxplot",
             "histogram" =	"Histogram",
             "density" 	=	"Density plot",
             "bar" 		=	"Bar graph")
    })
    
    
    output$plot <- renderUI({
      plotOutput("p")
    })
    
    
    output$p <- renderPlot({
      
      plot.obj<-list() 
      plot.obj$data<-data()[] 
      plot.obj$variable<-with(plot.obj$data,get(input$variable)) 
      plot.obj$group<-with(plot.obj$data,get(input$group)) 
      
     
      plot.type<-switch(input$plot.type,
                        "boxplot" 	= 	geom_boxplot(),
                        "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                        "density" 	=	geom_density(alpha=.75),
                        "bar" 		=	geom_bar(position="dodge")
      )
      
      require(ggplot2)
     
      .theme<- theme(
        axis.line = element_line(colour = 'gray', size = .75), 
        panel.background = element_blank(),  
        plot.background = element_blank()
      )	 
      if(input$plot.type=="boxplot")	{		
        p<-ggplot(plot.obj$data, 
                  aes(
                    x 		= plot.obj$group, 
                    y 		= plot.obj$variable,
                    fill 	= as.factor(plot.obj$group)
                  )
        ) + plot.type
        
        if(input$show.points==TRUE)
        { 
          p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
        }
        
      } else {
        
        p<-ggplot(plot.obj$data, 
                  aes(
                    x 		= plot.obj$variable,
                    fill 	= as.factor(plot.obj$group),
                    group 	= as.factor(plot.obj$group),
                    
                  )
        ) + plot.type
      }
      
      p<-p+labs(
        fill 	= input$group,
        x 		= "",
        y 		= input$variable
      )  +
        .theme
      print(p)
    })	
    
    
    
    
  }
)



