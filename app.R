library(dplyr)
library(glmnet)
library(gtools)
library(shiny)
library(ggplot2)
library(car)
library(pairsD3)
library(GGally)
library(plotly)

# setwd("/Users/gcmac/desktop/msan/data_viz/hw3")
df <- read.csv("dataset_Facebook.csv",sep=";")
df <- na.omit(df)
df$Paid <- as.factor(ifelse(df$Paid == 1, "Paid", "Not Paid"))


ui <- fluidPage(
    titlePanel("HW3 Facebook Plots"),
    mainPanel(
    tabsetPanel("Homework 3",
        tabPanel("Bubble Plot", 
                     plotOutput("bubble", dblclick = "bub_click",
                                brush=brushOpts(id="bub_brush",resetOnNew = T)),
                     "Brush and Double Click to zoom in. Double click again to reset."),
        tabPanel("Scatterplot Matrix",pairsD3Output("scatter_mat", width="80%", height="800px")),
        tabPanel("Parallel Coordinates Plot", plotlyOutput("par_cord"),
                 sidebarPanel(
                     selectInput("type", "Select Type", choices = append(list("All"),levels(df$Type))))
                 )
        )
    )
)


server <- function(input, output){
    rs <- reactiveValues(x=c(0,850), y=c(0,5500))
    
    output$bubble <- renderPlot({
        plt <- ggplot(df, aes(x=share, y=like, size=Paid, fill=Type)) +
                 geom_point(shape=21, alpha=.75) + 
            coord_cartesian(xlim=rs$x, ylim=rs$y) + scale_size_discrete(range=c(2.5, 4.35)) + ggtitle("Popularity of Social Media Posts")
        plt <- plt + xlab("Shares") + ylab("Likes") 
        plt <- plt + theme(axis.title=element_text(size=14), plot.title=element_text(hjust=.5, size=20))
        plt
    })
    
    output$scatter_mat <- renderPairsD3({
            pairsD3(df[,c("Post.Hour","Lifetime.Post.Total.Reach",
                          "Total.Interactions","share")],group=df[,c("Type")],
                    leftmar = 50)
    })
    
    df_new <- reactive({
        if(input$type == 'All'){
            df_new <- df
        }
        else{
            df_new <- df %>% filter(Type == input$type)}
    })

    output$par_cord <- renderPlotly({
        plt <- ggparcoord(df_new(), columns = 2:5, groupColumn = "Paid",scale = 'uniminmax', mapping = c("0","1")) +
            scale_colour_manual(values = c("Not Paid" = "maroon","Paid" = "dodgerblue")) +
            theme(axis.line = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.y=element_blank(),
                  axis.text.x = element_text(colour = 'turquoise4'),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.background = element_rect(fill="white",color = "black", size = 0.5),
                  panel.grid.major = element_blank())
        ggplotly(plt, tooltip = c('colour'))
    })
    
    observeEvent(input$bub_click, {
        brush <- input$bub_brush
        if (!is.null(brush)) {
            rs$x <- c(brush$xmin, brush$xmax)
            rs$y <- c(brush$ymin, brush$ymax)
            
        } else {
            rs$x <- c(0, 850)
            rs$y <- c(0, 5500)
        }
    })
}

shinyApp(ui = ui, server = server)


