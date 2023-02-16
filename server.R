library(shiny)
library(imager)
# library(factoextra)
library(tidyverse)

message('reading file')
pic_default <- load.image('pics/waffle_douglas_small.jpg')
message('done!')

### helper function to saturate or desaturate as noted by slider
norm_gain <- function(x, g) {
  ### g is gain, in percent (-100 to +100)
  ### x is current value, in 0 to 1
  if(g > 0) {
    x_out <- x + (g/100 * (1 - x))
  } else {
    x_out <- x + (g/100 * x)
  }
  return(x_out)
}


server <- function(input, output, session) {
  
  ##########################
  ###    Loading pic!    ###
  ##########################
  
  pic_raw <- reactive({
    if(is.null(input$pic_file)) {
      pic_raw <- pic_default
    } else {
      message(input$pic_file)
      message(input$pic_file$datapath)
      pic_raw <- load.image(input$pic_file$datapath)
    }
    if(spectrum(pic_raw) == 4) {
      ### functions require RGB, not 4 channel (with alpha)
      message('dropping alpha!')
      pic_raw <- rm.alpha(pic_raw)
    }
    return(pic_raw)
  })
  ### resize the image based on numeric input
  pic_resize <- reactive({
    pic <- pic_raw()
    message('In reactive: pic_resize; value = ', input$max_pic_dim)

    ### resize so width is no greater than 500 pixels?
    pic_dims <- dim(pic)[1:2]
    message('Pic raw dims are ', pic_dims[1], ' x ', pic_dims[2])
    if(max(pic_dims) > input$max_pic_dim) {

      pct <- input$max_pic_dim / max(pic_dims)
      pic_dims_new <- (pic_dims * pct) %>% round()
      message('rescaling to ', round(pct * 100), '% of original dims: ', 
              pic_dims_new[1], ' x ', pic_dims_new[2])
      pic <- pic %>%
        resize(size_x = pic_dims_new[1], size_y = pic_dims_new[2])
      ### given negative values, imager::resize interprets as
      ### percent of original image;
      ### given positive, interprets as final pixel dimensions
    }
    return(pic)
  })
  
  ### helper functions to translate crop slider inputs
  x_keep <- reactive({
    x_range <- round(dim(pic_resize())[1] * input$crop_x / 100)
    return(c(xmin = x_range[1], xmax = x_range[2]))
  })
  y_keep <- reactive({
    y_range <- round(dim(pic_resize())[2] * (100 - input$crop_y) / 100) %>%
      sort()
    return(c(ymin = y_range[1], ymax = y_range[2]))
  })
  
  ##########################
  ###    Adjust pic!     ###
  ##########################
  
  ### Adjust pic: crop!  equalize?  adjust saturation?
  
  pic_adjust <- reactive({
    message('in pic_adjust reactive')
    ### Crop according to settings on tab 1
    pic_crop <- imsub(pic_resize(),
                      x %inr% x_keep(),
                      y %inr% y_keep())

    ### equalize channel histograms if checkbox checked
    if(input$equalize) {
      ### helper function to equalize one channel
      hist_eq <- function(i) as.cimg(ecdf(i)(i), dim = dim(i))

      ### Split image across colour channels
      cn <- imsplit(pic_crop, "c")  ### we now have a list of images
      cn_eq <- map_il(cn, hist_eq)  ### run hist_eq on each
      pic_crop <- imappend(cn_eq, "c")
    }

    pic_sat  <- pic_crop %>%
      RGBtoHSV() %>% imsplit('c') %>%
      modify_at(2, norm_gain, g = input$saturation) %>%
      imappend('c') %>% HSVtoRGB()

    ### return adjusted pic
    return(pic_sat)
  })
  

  
  ##########################
  ###  Generate palette  ###
  ##########################
  
  ### Convert image to dataframe of RGB
  pal_from_pic <- reactive({
    color_names <- c('r', 'g', 'b')
    
    pic_df <- pic_adjust() %>% 
      as.data.frame() %>% 
      mutate(cc = color_names[cc]) %>%
      pivot_wider(names_from = cc, values_from = value) %>%
      select(r, g, b)
    
    set.seed(42)
    colors_k <- kmeans(pic_df, 
                       centers = input$n_cols, 
                       iter.max = 20, 
                       nstart = 5)
    
    pal <- colors_k$centers %>%
      as.data.frame() %>%
      mutate(hex = rgb(r, g, b)) %>%
      pull(hex)
    
    return(pal)
    
  })
  
  ### Create a reactive checkboxGroupInput so user can select
  ### from colors
  output$pal_picker <- renderUI({
    checkboxGroupInput(
      inputId = 'pal_pick', label = 'Select colors:',
      choices = pal_from_pic(), 
      selected = pal_from_pic())
  })
  
  
  ##########################
  ###      outputs!      ###
  ##########################
  
  output$pic_dim_print <- renderPrint({
    dims_orig <- dim(pic_raw())[1:2]
    dims_new  <- dim(pic_resize())[1:2]
    x <- sprintf('Original dimensions: %s x %s; resized dimensions: %s x %s',
            dims_orig[1], dims_orig[2], dims_new[1], dims_new[2])
    cat(x)
  })
  
  ### Base image with crop lines
  output$load_pic_plot <- renderPlot({
    plot(pic_resize(), axes = FALSE)
    ### show outline of crop area
    px <- (Xc(pic_resize()) %inr% x_keep()) & (Yc(pic_resize()) %inr% y_keep())
    highlight(px)
  })
  ### and thumbnail (can't have same ID)
  output$load_pic_thumb <- renderPlot({
    plot(pic_resize(), axes = FALSE)
    ### show outline of crop area
    px <- (Xc(pic_resize()) %inr% x_keep()) & (Yc(pic_resize()) %inr% y_keep())
    highlight(px)
  })
  
  ### Cropped, saturation-adjusted image
  output$adj_pic_plot <- renderPlot({
    message('in adj_pic_plot plot renderPlot')
    plot(pic_adjust(), axes = FALSE)
  })
  ### and thumbnail
  output$adj_pic_thumb <- renderPlot({
    message('in adj_pic_plot plot renderPlot')
    plot(pic_adjust(), axes = FALSE)
  })
  
  ### Palette from image, unedited
  output$palette_plot <- renderPlot({
    scales::show_col(pal_from_pic())
  })
  ### and thumbnail
  output$palette_thumb <- renderPlot({
    scales::show_col(pal_from_pic())
  })
  
  ### Edited palette
  output$palette_final <- renderPlot({
    scales::show_col(input$pal_pick)
  })
  output$palette_text <- renderPrint({
    meat <- paste(input$pal_pick, collapse = '", "')
    sandwich <- paste0('c("', meat, '")')
    cat(sandwich)
  })
  
}
