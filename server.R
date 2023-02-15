library(shiny)
library(imager)
# library(factoextra)
library(tidyverse)

message('reading file')
pic_raw <- load.image('waffle_douglas.jpg')
message('done!')


server <- function(input, output, session) {
  
  pic_resize <- reactive({
    pic <- pic_raw
    message('In reactive: pic_raw; value = ', input$max_pic_dim)

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
    # asdf <- palmerpenguins::penguins %>%
    #   sample_n(input$max_pic_dim, replace = TRUE)
    # return(asdf)
  })
  
  pic_crop <- reactive({
    message('in pic_crop reactive')
    pic_crop <- imsub(pic_resize(),
                      x %inr% x_keep(),
                      y %inr% y_keep())
    return(pic_crop)
  })
  
  x_keep <- reactive({
    message('in x_keep reactive')
    x_range <- round(dim(pic_resize())[1] * input$crop_x / 100)
    message('new x bounds: ', x_range[1], ' x ', x_range[2])
    return(c(xmin = x_range[1],
             xmax = x_range[2]))
  })
  y_keep <- reactive({
    message('in y_keep reactive')
    y_range <- round(dim(pic_resize())[2] * (100 - input$crop_y) / 100) %>%
      sort()
    message('new y bounds: ', y_range[1], ' x ', y_range[2])
    return(c(ymin = y_range[1],
             ymax = y_range[2]))
  })
  
  output$pic_dim_print <- renderPrint({
    dims_orig <- dim(pic_raw)[1:2]
    dims_new  <- dim(pic_resize())[1:2]
    x <- sprintf('Original dimensions: %s x %s; resized dimensions: %s x %s',
            dims_orig[1], dims_orig[2], dims_new[1], dims_new[2])
    cat(x)
  })
  
  output$tab1_plot <- renderPlot({
    message('in tab1 plot renderPlot')
    px <- (Xc(pic_resize()) %inr% x_keep()) & (Yc(pic_resize()) %inr% y_keep())
    
    plot(pic_resize(), axes = FALSE)
    highlight(px)
  })
  
  output$tab2_plot <- renderPlot({
    message('in tab2 plot renderPlot')
    plot(pic_crop(), axes = FALSE)
  })
  
}
