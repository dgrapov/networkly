#scripts for README
# setwd("assets/widgets")
#
# # @knitr plotly1
# library(plotly)
# p <- qplot(data = mtcars, x = wt, y = mpg, geom = c("point", "smooth"))
# out<-ggplotly(p)
# saveWidget(as.widget(out), file='plotly1.html')
