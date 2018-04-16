library(shiny)
source("SIM.r")

# server
shinyServer(function(input, output){
  # -------- get inputs -----------
  calc = eventReactive(input$gobutton,{
    print("button clicked")
    assign("shape1", input$shape1, envir = .GlobalEnv)
    assign("shape2", input$shape2, envir = .GlobalEnv)
    assign("scale1", input$scale1, envir = .GlobalEnv)
    assign("scale2", input$scale2, envir = .GlobalEnv)
    assign("p", input$p, envir = .GlobalEnv)
    
    G_sample = Gibbs(shape1, scale1, shape2, scale2, p)
    MH_sample = MH(shape1, scale1, shape2, scale2, p)
    return(list(G_sample = G_sample, MH_sample = MH_sample))
  })
  
  # -------- output --------------
  output$G.nconv = renderText({
    out = calc()
    return(paste("number of samples to convergence:", out$G_sample$conv.n))
  })
  output$G.plot1 = renderPlotly({
    out = calc()
    x = out$G_sample$x1[1:input$G.n]; lambda = out$G_sample$lambda1[1:input$G.n]
    xseq = seq(0,12,0.1)
    cdf = pgamma2(xseq, shape1, scale1, shape2, scale2, p)
    plot_ly(x = xseq, y = cdf, type = "scatter", mode = "line", name = "True") -> plot1
    ECDF = ecdf(lambda)
    add_lines(plot1, xseq, ECDF(xseq), line = list(color = 2), name = "Simulated") -> plot1
    return(layout(plot1, height = 300, width = 600,
                  title = "CDF of Lambda", xaxis = list(title = "Lambda"), yaxis = list(title = "Probability")))
  })
  output$G.plot2 = renderPlotly({
    out = calc()
    x = out$G_sample$x1[1:input$G.n]; lambda = out$G_sample$lambda1[1:input$G.n]
    xseq = seq(0,12,0.1)
    cdf = pnbinom2(xseq, shape1, scale1, shape2, scale2, p)
    plot_ly(x = xseq, y = cdf, type = "scatter", mode = "line", name = "True") -> plot2
    ECDF = ecdf(x)
    add_lines(plot2, xseq, ECDF(xseq), line = list(color = 2), name = "Simulated") -> plot2
    return(layout(plot2, height = 300, width = 300,
           title = "CDF of X", xaxis = list(title = "X"), yaxis = list(title = "Probability")))
  })
  output$G.plot3 = renderPlotly({
    out = calc()
    x = out$G_sample$x1[1:input$G.n]; lambda = out$G_sample$lambda1[1:input$G.n]
    pmf = dnbinom2(0:12, shape1, scale1, shape2, scale2, p); names(pmf) = 0:12
    plot_ly(x = 0:12, y = pmf, type = "bar", name = "True") -> plot3
    h = table(x)/sum(table(x))
    add_lines(plot3, 0:12, h, name = "Simulated") -> plot3
    return(layout(plot3, height = 300, width = 300,
                  title = "Histogram of X", xaxis = list(title = "X"), yaxis = list(title = "Probability")))
  })
  output$G.RandomWalk = renderPlotly({
    out = calc()
    x1 = out$G_sample$x1[1:input$G.n]; lambda1 = out$G_sample$lambda1[1:input$G.n]
    x2 = out$G_sample$x2[1:input$G.n]; lambda2 = out$G_sample$lambda2[1:input$G.n]
    x3 = out$G_sample$x3[1:input$G.n]; lambda3 = out$G_sample$lambda3[1:input$G.n]
    x4 = out$G_sample$x4[1:input$G.n]; lambda4 = out$G_sample$lambda4[1:input$G.n]
    
    plot_ly(x = 1:20) -> plot1
    add_markers(plot1, x = c(1,10,1,10), y = c(1,1,10,10), marker = list(size = 10, symbol = "square"), name = "Initial points") -> plot1
    add_trace(plot1, x = lambda1, y = x1, type = "scatter", mode = "lines",  name = "sequence 1", line = list(width = 0.5)) -> plot1
    add_trace(plot1, x = lambda2, y = x2, type = "scatter", mode = "lines",  name = "sequence 2", line = list(width = 0.5)) -> plot1
    add_trace(plot1, x = lambda3, y = x3, type = "scatter", mode = "lines",  name = "sequence 3", line = list(width = 0.5)) -> plot1
    add_trace(plot1, x = lambda4, y = x4, type = "scatter", mode = "lines",  name = "sequence 4", line = list(width = 0.5)) -> plot1
    return(layout(plot1, height = 300, width = 600,
                  title = "Random walks of 4 sampling sequences", xaxis = list(title = "Lambda"), yaxis = list(title = "X")))
  })
  
  
  output$MH.nconv = renderText({
    out = calc()
    return(paste("number of samples to convergence:", out$MH_sample$conv.n))
  })
  output$MH.plot1 = renderPlotly({
    out = calc()
    x = out$MH_sample$x1[1:input$MH.n]; lambda = out$MH_sample$lambda1[1:input$MH.n]
    xseq = seq(0,12,0.1)
    cdf = pgamma2(xseq, shape1, scale1, shape2, scale2, p)
    plot_ly(x = xseq, y = cdf, type = "scatter", mode = "line", name = "True") -> plot1
    ECDF = ecdf(lambda)
    add_lines(plot1, xseq, ECDF(xseq), line = list(color = 2), name = "Simulated") -> plot1
    return(layout(plot1, height = 300, width = 600,
                  title = "CDF of Lambda", xaxis = list(title = "Lambda"), yaxis = list(title = "Probability")))
  })
  output$MH.plot2 = renderPlotly({
    out = calc()
    x = out$G_sample$x1[1:input$MH.n]; lambda = out$G_sample$lambda1[1:input$MH.n]
    xseq = seq(0,12,0.1)
    cdf = pnbinom2(xseq, shape1, scale1, shape2, scale2, p)
    plot_ly(x = xseq, y = cdf, type = "scatter", mode = "line", name = "True") -> plot2
    ECDF = ecdf(x)
    add_lines(plot2, xseq, ECDF(xseq), line = list(color = 2), name = "Simulated") -> plot2
    return(layout(plot2, height = 300, width = 300,
                  title = "CDF of X", xaxis = list(title = "X"), yaxis = list(title = "Probability")))
  })
  output$MH.plot3 = renderPlotly({
    out = calc()
    x = out$G_sample$x1[1:input$MH.n]; lambda = out$MH_sample$lambda1[1:input$G.n]
    pmf = dnbinom2(0:12, shape1, scale1, shape2, scale2, p); names(pmf) = 0:12
    plot_ly(x = 0:12, y = pmf, type = "bar", name = "True") -> plot3
    h = table(x)/sum(table(x))
    add_lines(plot3, 0:12, h, name = "Simulated") -> plot3
    return(layout(plot3, height = 300, width = 300,
                  title = "Histogram of X", xaxis = list(title = "X"), yaxis = list(title = "Probability")))
  })
  output$MH.RandomWalk = renderPlotly({
    out = calc()
    x1 = out$MH_sample$x1[1:input$MH.n]; lambda1 = out$MH_sample$lambda1[1:input$MH.n]
    x2 = out$MH_sample$x2[1:input$MH.n]; lambda2 = out$MH_sample$lambda2[1:input$MH.n]
    x3 = out$MH_sample$x3[1:input$MH.n]; lambda3 = out$MH_sample$lambda3[1:input$MH.n]
    x4 = out$MH_sample$x4[1:input$MH.n]; lambda4 = out$MH_sample$lambda4[1:input$MH.n]
    
    plot_ly(x = 1:20) -> plot1
    add_markers(plot1, x = c(1,10,1,10), y = c(1,1,10,10), marker = list(size = 10, symbol = "square"), name = "Initial points") -> plot1
    add_trace(plot1, x = lambda1, y = x1, type = "scatter", mode = "lines",  name = "sequence 1", line = list(width = 0.5)) -> plot1
    add_trace(plot1, x = lambda2, y = x2, type = "scatter", mode = "lines",  name = "sequence 2", line = list(width = 0.5)) -> plot1
    add_trace(plot1, x = lambda3, y = x3, type = "scatter", mode = "lines",  name = "sequence 3", line = list(width = 0.5)) -> plot1
    add_trace(plot1, x = lambda4, y = x4, type = "scatter", mode = "lines",  name = "sequence 4", line = list(width = 0.5)) -> plot1
    return(layout(plot1, height = 300, width = 600,
                  title = "Random walks of 4 sampling sequences", xaxis = list(title = "Lambda"), yaxis = list(title = "X")))
  })
  
    
 })


