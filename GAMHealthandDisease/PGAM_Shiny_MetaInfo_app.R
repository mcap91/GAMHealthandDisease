library(shiny)
library(ggplot2)
library(colorspace)
library(Matrix)
library(plotly)

######################################################################
dn<-readRDS(file ="./dimnames.rds")
counts<-readMM(file = "./counts.txt")
dimnames(counts)<-dn
umaps<-read.table(file = "./umaps.txt")
umaps.3D<-read.table(file = "./3D_data.txt")
sampInfo<-read.table(file = "./sampInfo.txt")
sampInfo$Age<-factor(sampInfo$Age)
sampInfo$labeled.ident2<-factor(sampInfo$labeled.ident2, rev(c("Repopulating Microglia", "Homeostatic Microglia", "Sankowski.Healthy", 
                                                                 "PGAM", "Sankowski.GBM", "Tumor Core GAM", 
                                                                 "GAM - Other" , "GAM Subcluster 2", "GAM Subcluster 1" )))
umaps.3D$labeled.ident2<-factor(umaps.3D$labeled.ident2, rev(c("Repopulating Microglia", "Homeostatic Microglia", "Sankowski.Healthy", 
                                                                 "PGAM", "Sankowski.GBM", "Tumor Core GAM", 
                                                                 "GAM - Other" , "GAM Subcluster 2", "GAM Subcluster 1" )))

######################################################################

ui <- fluidPage(
 
      titlePanel("Comparison of GAM across Glioma"),

                     sidebarPanel(
                         p("11,439 microglia and myeloid cells derived from 48 samples and across 
                           two species were integrated using LIGER (Welch et. al. 2019.)"),
                         
                         br(),
                         
                         p("The cells were plotted using UMAP and the iNMF determined gene loadings. 
                           Plotted are the population annotations, and sample information. 
                           Further sample-specific information can be selected below.")),
                         
                         
                     mainPanel(
                         fluidRow(column(4,
                           selectInput(inputId = "meta",
                                      label = "Sample Info",
                                      selected = "labeled.ident2", 
                                      choices = names(sampInfo)))),
                           
                           fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                    plotOutput("DimPlot"),
                                    plotlyOutput("DimPlot3D"))),
                     
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         
                         fluidRow(column(4,
                           selectInput(inputId = "cluster",
                                        label = "Highlight by Annotation",
                                        choices = ""))),
                           plotOutput("HighPlot")
                         
                            )#main 
                        
)#page
                     
            
       server <- function(input, output, session) {
        
        
        #determine number of IDs for color palette
            n <- reactive({
               length(levels(factor(sampInfo[[input$meta]])))
                })
               
            #Plot original UMAP
            output$DimPlot <- renderPlot({
              pal <- hcl.colors(n=n(), palette = "Set2", rev = F) 
              
              data<-cbind(sampInfo, umaps)

              ggplot(data = data, aes(x=UMAP_1, y=UMAP_2, color = data[[input$meta]]), alpha=0.5)+
                geom_point() +
                scale_color_manual(values=pal)+
                ggtitle("LIGER Integration") +
                theme(legend.title = element_blank())+
                coord_fixed(ratio = 0.8)
              
            })
        
            
            #3D plot
            output$DimPlot3D <- renderPlotly({
              pal <- hcl.colors(n=n(), palette = "Set2", rev = F) 
              
              data<-cbind(sampInfo, umaps.3D)
              
              plot_ly(data = data, x=data$UMAP_1, y=data$UMAP_2, z=data$UMAP_3, 
                      type="scatter3d", color = data[[input$meta]], 
                      colors = pal, size=0.1)
              
            })
            
            
        #choices for HighPlot
        observe({
            updateSelectInput(session, "cluster",
                         choices = levels(factor(sampInfo[[input$meta]])))
        })
        
              
        #Plot cells highlight UMAP
        output$HighPlot <- renderPlot({
          
          data<-cbind(sampInfo, umaps)
          data$color <- factor(ifelse(data[input$meta]==input$cluster, "1", "2"))
          data$color[is.na(data$color)] <- "2"
          
          ggplot(data = data, aes(x=UMAP_1, y=UMAP_2, color = color), alpha=0.5)+
            geom_point(show.legend = F)+
            geom_point(data = subset(data, data[input$meta] == input$cluster), show.legend = F)+
            scale_color_manual(values=c("blue", "grey80"))+
            theme_void()+
            coord_fixed(ratio = 0.8)
        })
   
      
        
        
        
      }#server 
    
    
shinyApp(ui = ui, server = server)
    
