library(shiny)

ui <- fluidPage(
  navbarPage(
    "Palette from Pic",
    
    tabPanel(
      "Load image",
      sidebarLayout(
        sidebarPanel(
          # fileInput("pic_file", label = h3("Upload a photo")),
          numericInput(
            inputId = 'max_pic_dim',
            label = 'Rescale to maximum pic dimension:',
            value = 500
          ),
          sliderInput(
            inputId = 'crop_x',
            label = 'Crop width %',
            min = 0, max = 100, value = c(10, 90)
          ),
          sliderInput(
            inputId = 'crop_y',
            label = 'Crop height %',
            min = 0, max = 100, value = c(10, 90)
          ),
          radioButtons(
            inputId = 'pic_select',
            label   = 'Choose a picture',
            choices = c('Waffle', 'Scallop'),
            selected = character(0))
        ), ### end side panel

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("pic_dim_print"),
          plotOutput("tab1_plot")
        ) ### end main panel
      ) ### end sidebar layout
    ), ### end tab panel
    
    tabPanel(
      "Adjust image",
      sidebarLayout(
        sidebarPanel(
          'test'
        ), ### end side panel
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("tab2_plot")
        ) ### end main panel
      ) ### end sidebar layout
    ) ### end tab panel show palette
  ) ### end navbar page
)### end fluidpage


