library(shiny)
library(lfl)
#---------------------------------------
FEDdata <- readRDS('FEDdata.rds')

cols <- c("tone_BN","inf_exp","inf_ma1","inf_ar1","inf_arma11",
          "inf_auto","inf_naive")
FEDdata <- FEDdata[,cols]
BN_choices <- c("expert")
inf_exp_choices <- c("expert","MA(1)",
                     "AR(1)","ARMA(1,1)",
                     "AUTO.ARIMA", "NAIVE")
#------app----------------------------
ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("context_BN",
                        "Context of Tone",
                        choices = BN_choices,
                        selected = 1),

            selectInput("context_inf_exp",
                        "Context of inf. expectations",
                        choices= inf_exp_choices,
                        selected = 1),
            sliderInput("min_supp", "Minimal value of support:",
                        min = 0, max = 1, value = 0.02
            ),
            sliderInput("min_conf", "Minimal confidence:",
                        min = 0, max = 1, value = 0.5
            )

        ),

        mainPanel(
            tableOutput("out")
        )
    )
)


server <- function(input, output) {
    dejta <- reactive({
        #---------tone------
        if(input$context_BN == "expert"){
            fuzzy_BN <- lcut(
                FEDdata$tone_BN,
                hedges = "-",
                name = "tone_BN",
                context = ctx3(
                    low = - 0.4,
                    center = 0,
                    high = 0.4
                )
            )
        colnames(fuzzy_BN) <- c("negative tone","neutral tone","positive tone")
        }

        #----------inflation expectation---------



        if(input$context_inf_exp == "MA(1)"){
            fuzzy_inf_exp <- NULL
            for (i in 1:nrow(FEDdata)){
                r1 <- lcut(FEDdata$inf_exp[i],
                           hedges = "-",
                           name = "inf_exp",
                           context = ctx3(
                               low = FEDdata$inf_ma1[i] - 1,
                               center = FEDdata$inf_ma1[i],
                               high = FEDdata$inf_ma1[i] + 1
                           )
                )
                fuzzy_inf_exp <- rbind(fuzzy_inf_exp,r1)
            }

        }



        if(input$context_inf_exp == "AR(1)"){
            fuzzy_inf_exp <- NULL
            for (i in 1:nrow(FEDdata)){
                r1 <- lcut(FEDdata$inf_exp[i],
                           hedges = "-",
                           name = "inf_exp",
                           context = ctx3(
                               low = FEDdata$inf_ar1[i] - 1,
                               center = FEDdata$inf_ar1[i],
                               high = FEDdata$inf_ar1[i] + 1
                           )
                )
                fuzzy_inf_exp <- rbind(fuzzy_inf_exp,r1)
            }

        }


        if(input$context_inf_exp == "ARMA(1,1)"){
            fuzzy_inf_exp <- NULL
            for (i in 1:nrow(FEDdata)){
                r1 <- lcut(FEDdata$inf_exp[i],
                           hedges = "-",
                           name = "inf_exp",
                           context = ctx3(
                               low = FEDdata$inf_arma11[i] - 1,
                               center = FEDdata$inf_arma11[i],
                               high = FEDdata$inf_arma11[i] + 1
                           )
                )
                fuzzy_inf_exp <- rbind(fuzzy_inf_exp,r1)
            }

        }


        if(input$context_inf_exp == "AUTO.ARIMA"){
            fuzzy_inf_exp <- NULL
            for (i in 1:nrow(FEDdata)){
                r1 <- lcut(FEDdata$inf_exp[i],
                           hedges = "-",
                           name = "inf_exp",
                           context = ctx3(
                               low = FEDdata$inf_auto[i] - 1,
                               center = FEDdata$inf_auto[i],
                               high = FEDdata$inf_auto[i] + 1
                           )
                )
                fuzzy_inf_exp <- rbind(fuzzy_inf_exp,r1)
            }

        }

        if(input$context_inf_exp == "NAIVE"){
            fuzzy_inf_exp <- NULL
            for (i in 1:nrow(FEDdata)){
                r1 <- lcut(FEDdata$inf_exp[i],
                           hedges = "-",
                           name = "inf_exp",
                           context = ctx3(
                               low = FEDdata$inf_naive[i] - 1,
                               center = FEDdata$inf_naive[i],
                               high = FEDdata$inf_naive[i] + 1
                           )
                )
                fuzzy_inf_exp <- rbind(fuzzy_inf_exp,r1)
            }

        }

        if(input$context_inf_exp == "expert"){
            fuzzy_inf_exp <- lcut(
                FEDdata$inf_exp,
                name = "inf_exp",
                hedges = "-",
                context = ctx3(
                    low = 1,
                    center = 2,
                    high = 3
                )
            )

        }


        colnames(fuzzy_inf_exp) <- c("low inflation expectations", "medium inflation expectations", "high inflation expectations")
        fuzzy_data <- cbind(fuzzy_BN,fuzzy_inf_exp)
        return(fuzzy_data)

    })






    output$out <- renderTable({



        consSuffix <- paste0("inflation expectations")
        consIndices <- which(endsWith(colnames(dejta()), consSuffix))


        anteIndices <- which(!endsWith(colnames(dejta()), consSuffix))


        res <- searchrules(dejta(),
                           lhs = anteIndices,
                           rhs = consIndices,
                           n = 100,
                           minSupport = input$min_supp,
                           minConfidence = input$min_conf,
                           maxConfidence = 1,
                           maxLength = 4)

        qtab <- data.frame()
        for (i in seq_len(length(res$rules))) {
            lhs <- antecedents(res)[[i]]
            rhs <- consequents(res)[[i]]
            args <- lapply(lhs, function(l) dejta()[, l])
            x <- do.call(plukas.tnorm, args)
            y <- dejta()[, rhs]

            x <- as.vector(x)
            y <- as.vector(y)

            val <- data.frame()
            for (q in c('all', 'almost.all', 'most', 'many')) {
                if (is.null(x)) {
                    val[1, q] <- NA
                } else {
                    f <- quantifier(q)
                    val[1, q] <- f(x = lukas.residuum(x, y), w = x)
                }
            }
            qtab <- rbind(qtab, val)
        }

        qtab <- cbind(res, qtab)
        qtab <- na.omit(qtab)
    }, rownames = TRUE)


}


shinyApp(ui = ui, server = server)
