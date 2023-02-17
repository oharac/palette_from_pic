library(shiny)

ui <- fluidPage(
  navbarPage(
    'Palette from Pic',
    
    tabPanel(
      'Load image',
      sidebarLayout(
        sidebarPanel(
          fileInput('pic_file', label = h3('Upload a photo')),
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

        # Plot the raw/rescaled pic in the main panel
        mainPanel(
          plotOutput('load_pic_plot', height = '600px'),
          textOutput('pic_dim_print')
        ) ### end main panel
      ) ### end sidebar layout
    ), ### end Load Image tab panel
    
    tabPanel(
      'Adjust image',
      sidebarLayout(
        sidebarPanel(
          checkboxInput(
            inputId = 'equalize',
            label = 'Equalize RGB histograms?',
            value = FALSE
          ),
          sliderInput(
            inputId = 'saturation',
            label = 'Adjust saturation %',
            min = -100, max = 100, value = 0
          ),
          h3('From previous tab:'),
          plotOutput('load_pic_thumb', height = '250px')
        ), ### end side panel

        # Plot the adjusted pic in the main panel
        mainPanel(
          plotOutput('adj_pic_plot', height = '600px')
        ) ### end main panel
      ) ### end sidebar layout
    ), ### end Adjust image tab panel
    
    tabPanel(
      'Generate palette',
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = 'n_cols',
            label = 'How many colors?',
            min = 2, max = 15, value = 9
          ),
          h3('From previous tab:'),
          plotOutput('adj_pic_thumb', height = '250px')
        ), ### end side panel

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput('palette_plot', height = '600px')
        ) ### end main panel
      ) ### end sidebar layout
    ), ### end generate palette tab panel
    
    tabPanel(
      'Finalize palette',
      sidebarLayout(
        sidebarPanel(
          uiOutput('pal_picker'),
          h3('From previous tab:'),
          plotOutput('palette_thumb', height = '250px')
        ), ### end side panel
        
        # Show a plot of the generated distribution
        mainPanel(
          h3('Copy this to paste your palette into R:'),
          verbatimTextOutput('palette_text'),
          plotOutput('palette_final', height = '600px')
        ) ### end main panel
      ) ### end sidebar layout
    ), ### end Select palette tab panel
    
    tabPanel(
      'extra tab',
      sidebarLayout(
        sidebarPanel(
          'Hey this is a new panel'
        ), ### end side panel
        
        # Show a plot of the generated distribution
        mainPanel(
          'A new main panel!'
        ) ### end main panel
      )
    )
  ) ### end navbar page
  
)### end fluidpage


