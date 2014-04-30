# interactive variance inflation factor module
library(shiny)
library(car)
library(mvtnorm)
library(effects)

shinyServer(function(input, output){
  output$plot <- renderPlot({
    r2 <- input$r2
    n <- 200
    var_error <- input$resid_var
    var_x1 <- input$var_x1
    var_x2 <- input$var_x2
    beta <- c(0, 1, 1)
    
    # users enter R^2, this backcalculates covariance
    cov_x1x2 <- sqrt(var_x1 * var_x2 * r2)
    sigma <- matrix(c(var_x1, cov_x1x2, 
                      cov_x1x2, var_x2), 
                    nrow=2)
    
    X <- array(1, dim=c(n, 3))
    X[, c(2, 3)] <- rmvnorm(n=n, sigma=sigma, method="chol")
    mu <- X %*% beta
    epsilon <- rnorm(n, 0, sqrt(var_error))
    y <- mu + epsilon
    
    X1 <- X[, 2]
    X2 <- X[, 3]
    
    model <- lm(y ~ X1 + X2)
    
    effs <- allEffects(model, 
                       xlevels=list(X1 = seq(min(X1), max(X1), 
                                             length.out=20), 
                                    X2 = seq(min(X2), max(X2), 
                                             length.out=20)
                       )
    )

    # plot results (4 panels w/ ggplot & gridExtra)
    # plot 1: effect of X1
    df1 <- with(effs$X1, 
                data.frame(x, y=fit, 
                           lower = lower, upper = upper)
    )
    
    # thanks to Ben Bolker for the next 3 lines
    # https://groups.google.com/forum/#!topic/ggplot2/4-l3dUT-h2I
    l <- list(vif = round(vif(model)[1], digits=2))
    eq <- substitute(italic(VIF) == vif, l)
    eqstr <- as.character(as.expression(eq))
    
    x1p <- ggplot(df1, aes(x=X1, y=y)) + 
      geom_line() + 
      geom_line(aes(y=lower), col="grey") + 
      geom_line(aes(y=upper), col="grey") + 
      theme_bw() + 
      geom_rug(data=data.frame(X[, 2]), sides="b") + 
      ggtitle("Effect of X1") + 
      annotate(geom="text", x=Inf, y=Inf, label=eqstr, 
               parse=TRUE, hjust = 1.5, vjust = 1.5)
    
    # plot 2: effect of X2
    df2 <- with(effs$X2, 
                data.frame(x, y=fit, 
                           lower = lower, upper = upper)
    )
    
    l2 <- list(vif = round(vif(model)[2], digits=2))
    eq2 <- substitute(italic(VIF) == vif, l2)
    eqstr2 <- as.character(as.expression(eq2))
    
    
    x2p <- ggplot(df2, aes(x=X2, y=y)) + 
      geom_line() + 
      geom_line(aes(y=lower), col="grey") + 
      geom_line(aes(y=upper), col="grey") + 
      theme_bw() + 
      geom_rug(data=data.frame(X[, 3]), sides="b") + 
      ggtitle("Estimated effect of X2")  + 
      annotate(geom="text", x=Inf, y=Inf, label=eqstr2, 
               parse=TRUE, hjust = 1.5, vjust = 1.5)
    
    # plot 3: parameter recovery
    df3 <- data.frame(truth = beta, 
                      lci = confint(model)[, 1], 
                      uci = confint(model)[, 2], 
                      est = coef(model), y=0:(length(beta) - 1))
    cip <- ggplot(df3, aes(x=truth, y=y)) + 
      geom_point(col="blue", size=5, alpha=.5) + 
      theme_bw() + 
      geom_segment(aes(x=lci, y=y, xend=uci, yend=y)) + 
      geom_point(aes(x=est, y=y), size=3, pch=1) + 
      ggtitle("Coefficient recovery") + 
      xlab("Value") + 
      ylab(expression(beta)) + 
      scale_y_continuous(breaks = c(0, 1, 2)) + 
      theme(axis.title.y = element_text(angle = 0, hjust = 0)) 
    
    # plot 4: X1 & X2 correlation plot
    xcorp <- ggplot(data.frame(X1, X2), aes(x=X1, y=X2)) + 
      geom_point(pch=1) +
      theme_bw() + 
      ggtitle("Covariate scatterplot")
    
    library(gridExtra)
    grid.arrange(x1p, x2p, 
                 cip, xcorp, 
                 nrow=2)
    
  })
})
