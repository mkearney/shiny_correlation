#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    tags$head(
        tags$style(HTML("
            @import url(https://fonts.googleapis.com/css?family=Roboto+Condensed:400,700);

            body{
            font-family: 'Roboto Condensed';
            color: #111;
            }
            #corPlot img{
            display: block;
            margin-left: auto;
            margin-right: auto;
            }
            h2{
            text-align: center;
            }
            .control-label{
            font-weight: 400;
            }

            "))
        ),
   # Application title
   titlePanel("Correlations"), br(),

   # Sidebar with a slider input for number of bins
    fluidRow(
    column(6, offset = 3,
        wellPanel(
         sliderInput("r",
                     "",
                     min = -1,
                     max = 1,
                     value = .00,
                     step = .01)
      )),
      fluidRow(
      # Show a plot of the generated distribution
        column(6, offset = 3,
            plotOutput("corPlot", height = "550px")
      )
   ), br(), br(), br(), br(),
      fluidRow(
        column(12, includeHTML("footer.html"),
          align = "center")
      ),
      br(), br())
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

   output$corPlot <- renderPlot({
       n     <- 200
       rho   <- input$r
       if (rho == 1) {
           rho <- .9999999
       } else if (rho == -1) {
           rho <- -.9999999
       }
       theta <- acos(rho)
       x     <- rnorm(n, 1, 1)
       y     <- rnorm(n, 2, 0.5)
       X     <- cbind(x, y)
       Xctr  <- scale(X, center = TRUE, scale = FALSE)
       Id    <- diag(n)
       Q     <- qr.Q(qr(Xctr[ , 1, drop = FALSE]))
       P     <- tcrossprod(Q)
       x2o   <- (Id - P) %*% Xctr[ , 2]
       Xc2   <- cbind(Xctr[ , 1], x2o)
       Y     <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))

       y <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]
       x <- scale(x)[, 1]
       x <- x / ((max(abs(x)) / 3))
       y <- scale(y)[, 1]
       y <- y / ((max(abs(y)) / 3))

       rsize <- round(cor(x,y), 2)
       cols <- colorRampPalette(c(rgb(.05, .15, .9), rgb(.9, .1, .1)))(length(seq(-1, 1, .01)) + 100)
       cols <- cols[c(1:100, 151, (length(cols) - 99):length(cols))]
       cols <- paste0(cols, "BB")
       vals <- round(seq(-1, 1, .01), 2)
       col <- cols[match(rsize, vals)]

       #if (rsize < 0) {
      #     col = "#dd3333"
      # } else if (rsize > 0) {
       #    col = "#2244dd"
       #} else if (rsize == 0) {
      #     col = "#999999"
      # }
       rsize <- gsub("0\\.", ".", sprintf("%.2f", rsize))
       rsize <- gsub("1.00", "1.0", rsize)
       ggplot(data.frame(x, y), aes(x, y)) +
         geom_vline(xintercept = 0, colour = "#00000055", size = .5) +
         geom_hline(yintercept = 0, colour = "#00000055", size = .5) +
       geom_point(fill = col, size = 4, shape = 22, colour = "#000000bb") +
         theme_minimal(base_size = 15, base_family = "Arial Narrow") +
         theme(axis.text = element_blank(),
           plot.title = element_text(face = "bold", size = rel(2), hjust = .5),
           panel.grid.major = element_line(size = rel(.08), colour = "#222222"),
           panel.grid.minor = element_line(size = rel(.04), colour = "#222222")) +
         labs(title = paste0("r = ", rsize), subtitle = "") +
         scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
         scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1))
         #par(family = "Roboto", mar = c(3, 3, 3, 3))
       #plot(x, y, bty = "n", axes = FALSE, pch = 15, cex = 1.2,
      #   main = paste("r","=",rsize), font.main = 1, col = col,
      #   xlim = c(-2.5, 2.5), ylim = c(-3.5, 3.5), cex.main = 1.6,
      #   xlab = "", ylab = "", family = "Roboto")
      # abline(h = 0, lty = 2, col = "#000000aa")
      # abline(v = 0, lty = 2, col = "#000000aa")
   })
})

# Run the application
shinyApp(ui = ui, server = server)

