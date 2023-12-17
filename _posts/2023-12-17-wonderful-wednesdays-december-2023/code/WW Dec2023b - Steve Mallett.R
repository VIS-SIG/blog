library(plotly)

adsl <- read_xpt("adsl.xpt")

pl_colorscale=list(c(0.0, '#19d3f3'),
                   c(0.333, '#19d3f3'),
                   c(0.333, '#e763fa'),
                   c(0.666, '#e763fa'),
                   c(0.666, '#636efa'),
                   c(1, '#636efa'))

fig <- adsl %>%
  plot_ly() 
fig <- fig %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='Age', values=~AGE),
      list(label='Height', values=~HEIGHTBL),
      list(label='Weight', values=~WEIGHTBL),
      list(label='BMI', values=~BMIBL)
    ),
    text=~TRT01P,
    marker = list(
      color = as.integer(adsl$TRT01PN),
      colorscale = pl_colorscale,
      size = 7,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  ) 

fig <- fig %>%
  layout(
    title= 'ADSL Data set',
    plot_bgcolor='rgba(240,240,240, 0.95)'
  )

fig2 <-  fig %>% style(diagonal = list(visible = F))
htmlwidgets::saveWidget(as_widget(fig2), "WW Dec2023b.html")