library(shiny)
library(tidyverse)
library(viridis)
library(cowplot)

cleaned_root <- readRDS("cleaned_root.rda")
theme_tsl <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # white background and dark border, no gridlines
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # axis options
      axis.ticks = element_line(colour = "black"),
      axis.text = element_text(size = rel(0.8), colour = "black"),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      axis.title.y = element_text(margin=margin(0,10,0,0), angle = 90),
      # facetting options
      strip.background = element_rect(fill = "grey85", colour = "grey20"),
      # match legend key to background
      legend.key = element_rect(fill = "white", colour=NA),
      
      complete = TRUE
    )
}
shinyplot <- function(data, access, exp) {
  data %>% 
    filter(accession == access,
           experiment %in% exp) %>% 
    ggplot(aes(start_x,start_y)) + 
    geom_segment(aes(xend=end_x,yend=end_y, color=hours),arrow = arrow(length = unit(0.01, "npc")),alpha=1) + 
    xlim(0,15) + 
    ggtitle(exp) + coord_fixed(ratio = 1) + 
    scale_y_reverse(limits=c(30,0)) + scale_color_gradient(low="darkred", high="gold", limits = c(0, 352)) + 
    theme_set(theme_cowplot()) + theme_tsl() + theme(legend.position = "none",
                                                     axis.title = element_blank())
  
}
  
ui <- fluidPage(
  titlePanel("GLO-Roots root system architecture"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "access1", 
                  label = "Accession A", 
                  choices = list('Adal1',	'Adal3',	'Ale-Stenar-64-24',	'App1-12',	'Ba1',	'Ba1-2',	'Ba5-1',
                                 'Bro1-6',	'Coc1-IP',	'Col-0',	'Dr10',	'Dra2-1',	'Eden2',	'Eds-1',
                                 'Fei-0',	'Fj1-2',	'Fj2-4',	'Fri2',	'Ge-0',	'Grn-12',	'Gro-3',
                                 'Had1',	'Hag2',	'Ham1',	'Hel3',	'HolA1-1',	'Hov1-10',	'Hovdala-2',
                                 'In-0',	'Kal1',	'Kas-2',	'Kia1',	'Kil-0',	'Kor3',	'Kulturen-1',
                                 'Kz-9',	'Lan1',	'Ler1',	'Liarum',	'Lis2',	'Lom1-1',	'Lu-1',
                                 'Lv-5',	'Neo-6',	'Ode2',	'Omn-5',	'Omo1-7',	'Or-1',	'Oy-0',
                                 'Pi2',	'Rak1',	'Rd17-319',	'Rev1',	'San2',	'SanMartin',	'SantaClara',
                                 'Sev',	'Spr1-2',	'Spro1',	'Sr-3',	'Ste-2',	'T1020',	'T1040',
                                 'T460',	'T690',	'T800',	'TAA04',	'TAA14',	'Tad01',	'Tal03',
                                 'TB01',	'TDr-18',	'TDr-2',	'TEDEN3',	'TGR01',	'THO-08',	'Ting-1',
                                 'TOM04',	'Tomegap2',	'Tottarp2',	'Tra01',	'Tsu-0',	'Tur4',	'Ty-0',
                                 'Ull3-4',	'UllA1',	'Var-2-6',	'Vastervik',	'Vimmerby',	'Vislv',	'Wei-0',
                                 'Yst-1',	'Zal1'), selected = 'Adal1'),
      
      selectInput(inputId = "access2", 
                  label = "Accession B", 
                  choices = list('Adal1',	'Adal3',	'Ale-Stenar-64-24',	'App1-12',	'Ba1',	'Ba1-2',	'Ba5-1',
                                 'Bro1-6',	'Coc1-IP',	'Col-0',	'Dr10',	'Dra2-1',	'Eden2',	'Eds-1',
                                 'Fei-0',	'Fj1-2',	'Fj2-4',	'Fri2',	'Ge-0',	'Grn-12',	'Gro-3',
                                 'Had1',	'Hag2',	'Ham1',	'Hel3',	'HolA1-1',	'Hov1-10',	'Hovdala-2',
                                 'In-0',	'Kal1',	'Kas-2',	'Kia1',	'Kil-0',	'Kor3',	'Kulturen-1',
                                 'Kz-9',	'Lan1',	'Ler1',	'Liarum',	'Lis2',	'Lom1-1',	'Lu-1',
                                 'Lv-5',	'Neo-6',	'Ode2',	'Omn-5',	'Omo1-7',	'Or-1',	'Oy-0',
                                 'Pi2',	'Rak1',	'Rd17-319',	'Rev1',	'San2',	'SanMartin',	'SantaClara',
                                 'Sev',	'Spr1-2',	'Spro1',	'Sr-3',	'Ste-2',	'T1020',	'T1040',
                                 'T460',	'T690',	'T800',	'TAA04',	'TAA14',	'Tad01',	'Tal03',
                                 'TB01',	'TDr-18',	'TDr-2',	'TEDEN3',	'TGR01',	'THO-08',	'Ting-1',
                                 'TOM04',	'Tomegap2',	'Tottarp2',	'Tra01',	'Tsu-0',	'Tur4',	'Ty-0',
                                 'Ull3-4',	'UllA1',	'Var-2-6',	'Vastervik',	'Vimmerby',	'Vislv',	'Wei-0',
                                 'Yst-1',	'Zal1'), selected = 'Adal1'),
      # sliderInput(inputId = "num",
      #             label = "Days after sowing (DAS)",
      #             value = 22, min = 14, max = 30),
      ),
    
    mainPanel(
      textOutput("text1"),
      plotOutput("access1"),
      textOutput("text2"),
      plotOutput("access2")
    )
  )
)
    
server <- function(input, output) {
  output$text1 <- renderText({input$access1})
  output$access1 <- renderPlot({
    
    plotall <- cleaned_root %>% 
      filter(accession == input$access1,
             experiment %in% c("GWA1", "GWA2", "GWA3",
                               "GWA4", "GWA5", "GWA6")) %>% 
      ggplot(aes(start_x,start_y)) + 
      geom_segment(aes(xend=end_x,yend=end_y, color=hours),arrow = arrow(length = unit(0.01, "npc")),alpha=1) + 
      xlim(0,15) + 
      scale_y_reverse(limits=c(30,0)) + scale_color_gradient(low="darkred", high="gold", limits = c(0, 352)) + 
      coord_fixed(ratio = 1) + 
      ggtitle("All replicates") +
      xlab("Width (cm)") + 
      ylab("Depth (cm)") + 
      theme_set(theme_cowplot()) + theme_tsl() + theme(legend.position = "bottom")
    
    plot1 <- shinyplot(cleaned_root, input$access1, "GWA1")
    plot2 <- shinyplot(cleaned_root, input$access1, "GWA2")
    plot3 <- shinyplot(cleaned_root, input$access1, "GWA3")
    plot4 <- shinyplot(cleaned_root, input$access1, "GWA4")
    plot5 <- shinyplot(cleaned_root, input$access1, "GWA5")
    plot6 <- shinyplot(cleaned_root, input$access1, "GWA6")
    
    plotrep <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2)
    plot_grid(plotall, plotrep, nrow =1, rel_widths = c(1.5, 2))
  })
  output$text2 <- renderText({input$access2})
  output$access2 <- renderPlot({
    
    plotall <- cleaned_root %>% 
      filter(accession == input$access2,
             experiment %in% c("GWA1", "GWA2", "GWA3",
                               "GWA4", "GWA5", "GWA6")) %>% 
      ggplot(aes(start_x,start_y)) + 
      geom_segment(aes(xend=end_x,yend=end_y, color=hours),arrow = arrow(length = unit(0.01, "npc")),alpha=1) + 
      xlim(0,15) + 
      scale_y_reverse(limits=c(30,0)) + scale_color_gradient(low="darkred", high="gold", limits = c(0, 352)) + 
      coord_fixed(ratio = 1) + 
      ggtitle("All replicates") +
      xlab("Width (cm)") + 
      ylab("Depth (cm)") + 
      theme_set(theme_cowplot()) + theme_tsl() + theme(legend.position = "bottom")
    
    plot1 <- shinyplot(cleaned_root, input$access2, "GWA1")
    plot2 <- shinyplot(cleaned_root, input$access2, "GWA2")
    plot3 <- shinyplot(cleaned_root, input$access2, "GWA3")
    plot4 <- shinyplot(cleaned_root, input$access2, "GWA4")
    plot5 <- shinyplot(cleaned_root, input$access2, "GWA5")
    plot6 <- shinyplot(cleaned_root, input$access2, "GWA6")
    
    plotrep <- plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2)
    plot_grid(plotall, plotrep, nrow =1, rel_widths = c(1.5, 2))
  })
}

shinyApp(ui = ui, server = server)