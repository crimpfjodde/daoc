


ui <- dashboardPage(

  dashboardHeader(title = 'DAoC'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Herald",
               tabName = 'herald',
               icon = icon("broom")),
      menuItem("Castspeed/buff",
               tabName = "castspeed",
               icon = icon("dashboard")),
      menuItem("Crafting",
               tabName = "craftprobability",
               icon = icon("bomb")),
      menuItem('Maps etc',
               tabName = 'taskmap',
               icon = icon('map-signs')),
      menuItem('About',
               tabName = 'about',
               icon = icon('question'))
    ),
    # Custom CSS to hide the default logout panel


    # The dynamically-generated user panel
    uiOutput("userpanel")
  ),

  dashboardBody(
    tags$head(tags$link(rel="shortcut icon", href="https://playphoenix.online/assets/images/favicon.png")),
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }
                              opacity:0.5 !important;'))),
    # uiChangeThemeDropdown(),
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tabItems(
      tabItem(tabName = 'herald',
              fluidRow(width = 8,
              infoBoxOutput("guild_rp"),
              infoBoxOutput("guild_48h"),
              infoBoxOutput("guild_lastweek")
              ),
              fluidRow(width = 8,

                       )
              ),
      tabItem(tabName = "craftprobability",
              # textOutput('testtext'),

              column(width = 6,
                     fluidRow(#width = 6,
                       box(width = 6, #style = "font-size: 14px;",
                           title = "Question", 'What is the probability of making atleast',
                           tags$i('x'), 'Master pieces in', tags$i('y'),  'number of tries?\n', br(),
                           tags$br(),
                           numericInput('no_mp',
                                        value = 1,
                                        step = 1,
                                        min = 0,
                                        label = "Number of MP's: "),
                           numericInput('no_tries',
                                        value = 50,
                                        step = 1,
                                        min = 1,
                                        label = 'Number of tries: ')
                       ),

                       box(width = 6,
                           title = "Answer",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           span(textOutput("probability_value"), style = "font-size:30px;font-weight: bold;font-style:oblique"),
                           textOutput('probability_text')
                       )),
                     fluidRow(
                       box(width = 12,
                           title = "View a plot",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           collapsed = TRUE,
                           plotlyOutput('distrib_plot')
                       )
                     )


              ),
              column(width = 6,

                     box(#width = 3,
                       title = 'Unknown master crafter',
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       collapsed = TRUE,
                       img(src='daoc_flex.jpg', height="100%", width="100%", align = "center"))

              )
      ),

      tabItem(tabName = 'castspeed',
              fluidRow(
                box(width = 2,
                    numericInput("delve",
                                 "Delve:",
                                 min = 1,
                                 max = 10,
                                 value = 3,
                                 step = 0.1),
                    numericInput("currentdex",
                                 "Current dex:",
                                 min = 0,
                                 max = 500,
                                 value = 300,
                                 step = 1),
                    numericInput("mota2",
                                 "MotA:",
                                 min = 0,
                                 max = 9,
                                 value = 0,
                                 step = 1),
                    numericInput('dexaug2',
                                 label = 'Aug Dex:',
                                 min = 0,
                                 max = 9,
                                 step = 1,
                                 value = 0)
                ),
                column(width = 2,
                       fluidRow(
                         box(width = 12,
                             span(textOutput("casttimenew"), style = "font-size:30px;font-weight: bold;font-style:oblique"),
                             textOutput("casttimecurrent"),
                             textOutput("diff"),
                             textOutput('pointsspent'),
                             textOutput('relativevalue'),
                             textOutput('percentDecrease'))
                       ),
                       fluidRow(
                         box( width = 12,
                              helpText('Pretty much stolen from' ,
                                       tags$b(tags$a(href = "https://docs.google.com/spreadsheets/d/1CslgNBWCDhdfEYrCxDyklMosSCPAnAacTEdpbK32esE/edit#gid=0","here"))))
                       )
                ),

                tabBox(width = 6,
                       title = "",
                       tabPanel(title = 'Time difference',
                                plotlyOutput('plotutDiff')
                       ),
                       tabPanel(title = 'Cast speed',
                                plotlyOutput('plotutSpeed')
                       ),
                       tabPanel(title = 'Percent',
                                plotlyOutput('plotutPercent'))
                )
              )#,
              # fluidRow(width = 10, box(width = 10, DT::dataTableOutput('buff_tbl')))
      ),

      tabItem(tabName = 'about',
              box('This is a hobby project of mine where i play with a raspberry Pi.',
                  tags$br(),
                  'I am not a programmer or an IT proffesional of any kind, but just a guy that likes to play around with stuff like this.',
                  tags$br(),
                  'And play DAoC ofc!',
                  tags$br(),
                  'Get in touch with ',
                  actionButton("show", "me"),
                  tags$br(),
                  tags$br(),
                  'Written in', tags$a(href = "https://shiny.rstudio.com/","Shiny"),
                  tags$br(),
                  'Plots produced with',  tags$a(href = "https://plotly.com/r/","Plotly"),
                  tags$br(),
                  'On', tags$a(href = "https://rstudio.com/products/rstudio/download-server/", "RStudio server"),
                  tags$br(),
                  'Hosted on a ',tags$a(href = "https://www.raspberrypi.org/products/raspberry-pi-4-model-b/?resellerType=home","Raspberry Pi 4"), ''

              ),

              tags$style(type = 'text/css', '#show
					{background-color: #f2f2f2;
					box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2), 0 3px 10px 0 rgba(0,0,0,0.19);
				    color: #595959;
				    padding: 1px 10px;
				    font-family: Helvetica;
				    text-align: center;
				    font-style: italic;
				    text-decoration: none;
				    display: inline-block;
				    font-size: 12px;}')),

      tabItem('taskmap',
              fluidRow(
                tabBox(width = 5,
                       title = "Task maps",
                       tabPanel(title = 'Alb',
                                img(src='task_alb.png', height="100%",  align = "center")
                       ),
                       tabPanel(title = 'Mid',
                                img(src='task_mid.png', height="100%", width="100%", align = "center")
                       ),
                       tabPanel(title = 'Hib',
                                img(src='task_hib.png', height="100%", align = "center")
                       )
                ),
                box(Title = 'Order')
              )
              # ,
              # img(src='daoc_original_map.jpg', height="100%", width = "100%", align = "center")
              # fluidRow(
              #   box(title = 'Original daoc map',
              #       solidHeader = TRUE,
              #       collapsible = TRUE,
              #       actionButton("go", "Go")
              #       ,bsModal("window", "", "go"
              #                ,uiOutput("myImage")),
              #       img(src='daoc_original_map.jpg', height="100%", width = "100%", align = "center")
              #   )
              # )
      )
    ),

    fluidRow(
      tags$head(
        # tags$style(HTML(".main-sidebar { font-size: 18px; }")),
      )
    )
  )
)

###########################################################################################################################
server <- function(input, output, session) {


  get_phoenix <- function() {
    url <- paste0('https://herald.playphoenix.online/g/Janitors')
    temp <- getURL(url)
    li <- readHTMLTable(temp, as.data.frame = TRUE)
    li
  }

  as_numeric = function() {
    x <- get_phoenix()[[7]]
    for (i in 5:9) {
      x[[i]] <- gsub(',', '', x[[i]])
      x[[i]] <- as.numeric(x[[i]])
    }
    x
  }

  higest_rp <- function() {
    as_numeric() %>% arrange(`RP 48h`)
  }
  output$guild_rp <- renderValueBox({
    li <- get_phoenix()
    infoBox(
      title = HTML(paste('Total rp<b>', br(), li[[1]][[2]][1], '</b>')),
      subtitle = HTML(paste(li[[1]][[3]][1],"server rank", br(), li[[1]][[4]][1], 'realm rank')),
      icon = icon("chart-bar"),
      color = "green"
    )
  })

  output$guild_48h <- renderValueBox({
    li <- get_phoenix()
    infoBox(
      title = HTML(paste('rp last 48h<b>', br(), li[[1]][[2]][4], '</b>')),
      subtitle = HTML(paste(li[[1]][[3]][4],"server rank", br(), li[[1]][[4]][4], 'realm rank')),
      icon = icon("chart-bar"),
      color = "green"
    )
  })

  output$guild_lastweek <- renderValueBox({
    li <- get_phoenix()
    infoBox(
      title = HTML(paste('rp last week<b>', br(), li[[1]][[2]][2], '</b>')),
      subtitle = HTML(paste(li[[1]][[3]][2],"server rank", br(), li[[1]][[4]][2], 'realm rank')),
      icon = icon("chart-bar"),
      color = "green"
    )
  })


  output$myImage <- renderUI({
    img(src='daoc_original_map.jpg', height="727px", width = "1689px", align = 'center')
  })


  # Crafting ###################################################################

  output$probability_value <- renderText({
    paste0(round(tries_ut(),0), '%')
  })

  output$probability_text <- renderText({
    paste0('The probability of making atleast ', input$no_mp, ' MP(s) in ',
           input$no_tries, ' tries is ', round(tries_ut(),3), '%')
  })

  # output$probability_out <- renderText({
  #   paste0('(', tries_ut(), '%)')
  # })

  tries_ut <- function() {
    tries = input$no_tries
    mps = input$no_mp - 1
    if(mps == -1) {
      ut = (dbinom(0, (tries), 0.02))*100
    } else {
      ut = (pbinom(mps, tries, 0.02, lower.tail = F))*100
    }
    round(ut, 4)
  }

  plot_prob <- function() {

    z = 1.96
    tries = 1:input$no_tries
    mps = input$no_mp - 1
    if(mps == -1) {
      ut = (dbinom(0, (tries), 0.02))
    } else {
      ut = (pbinom(mps, tries, 0.02, lower.tail = F))
    }
    ci_up <- ut + z*sqrt(ut*(1-ut)/tries)
    ci_up <- round(ci_up*100, 3)
    ci_up <- replace(ci_up, ci_up > 100, 100)
    ci_lo <- ut - z*sqrt(ut*(1-ut)/tries)
    ci_lo <- replace(ci_lo, ci_lo<=0, 0)
    ci_lo <- round(ci_lo*100, 3)
    ut <- round(ut*100, 3)
    df <- data.frame(tries, ut, ci_lo, ci_up)
    df
  }

  output$distrib_plot <- renderPlotly({
    plot_ly(data = plot_prob(), x = ~tries) %>%
      add_trace(y = ~ut,
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(size = 5,
                              color = 'rgb(255, 230, 230)',
                              line = list(color = 'rgb(255, 230, 230)',
                                          width = 0)),
                hoverinfo = 'text',
                text = ~paste('Tries: ', tries,
                              '\nProbability: ', round(ut,3), '%',
                              '\nCI high', ci_up, '%',
                              '\nCI low', ci_lo, '%')) %>%
      add_trace(y = ~ci_up,
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'none',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Upper') %>%
      add_trace(y = ~ci_lo,
                type = 'scatter',
                mode = 'lines',
                hoverinfo = 'none',
                fill = 'tonexty',
                fillcolor='rgba(255, 230, 230, 0.2)',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'Lower') %>%
      layout(plot_bgcolor='#343E48',
             paper_bgcolor='#343E48',
             xaxis = list(color = "white",
                          title = 'Number of tries',
                          gridcolor = toRGB("gray50"),
                          showgrid = TRUE),
             yaxis = list(color = "white",
                          gridcolor = toRGB("gray50"),
                          title = 'Probability ( 95% CI )',
                          showgrid = TRUE))
  })


  ######## Casttime #######################################################################################
  plotdf <- function(){

    cdelve <- input$delve
    cdex <- input$currentdex
    points <- c(0,1,2,4,7,10,15,20,27,34)
    plotgrid <- expand.grid(dex=c(0,4,8,12,17,22,28,34,41,48),mota=c(0,0.01,0.02,0.03,0.05,0.07,0.09,0.11,0.13,0.15))
    # dexgrid <- (cdex + plotgrid$dex)
    plotgrid$Speed <- cspeed(cdelve, (cdex + plotgrid$dex), plotgrid$mota)
    speedBase <- round(cspeed(cdelve, cdex, 0), 4)

    plotgrid <- plotgrid %>% mutate(cdelve,
                                    adex = rep(0:9, 10),
                                    amota = rep(0:9,
                                                each = 10),
                                    dpoints = rep(points, 10),
                                    mpoints = rep(points, each = 10)) %>%
      mutate(Points = dpoints + mpoints) %>%
      arrange(Points)%>%
      mutate(Diff = round((Speed - speedBase), 4),
             `Diff %` = (1 - Speed/speedBase),
             Percent = round((Speed/speedBase)*100, 4))
    plotgrid
  }

  # output$table <- DT::renderDT(plotdf())
  output$plotutDiff <- renderPlotly({
    plot_ly(data = plotdf(),
            x = ~Points) %>%
      add_trace(y = ~Diff,
                marker = list(size = 5,
                              color = 'rgb(255, 230, 230)',
                              line = list(color = 'rgb(255, 230, 230)',
                                          width = 0)),
                type = 'scatter',
                mode = 'markers',
                hoverinfo = 'text',
                text = ~paste('MotA: ', amota, '\nADex: ',
                              adex, '\nPoints:', Points, '\nSpeed: ',
                              Speed,'\nDiff: ', Diff)) %>%
      # layout(title = ~paste('All possible distributions of points\n', 'Delve:',input$delve, 'Dex:', input$currentdex)) %>%
      layout(
        # title = list(text = ~paste0(#'All possible distributions of points',
        #     'Delve:',input$delve, '  Dex:', '', input$currentdex, '\n'),
        #     x = 0.15, yref = 'Diff', y = -0.2,
        #     font=list(size=16, color = "white")
        # ),
        margin = list( pad = 5)) %>%
      layout(plot_bgcolor='#343E48') %>%
      layout(paper_bgcolor='#343E48') %>%
      layout(xaxis = list(color = "white",
                          gridcolor = toRGB("gray50"),
                          showgrid = TRUE)) %>%
      layout(yaxis = list(color = "white",
                          gridcolor = toRGB("gray50"),
                          title = 'Diff (secunds)',
                          showgrid = TRUE))
    # add_trace(y = ~diff.1, type = 'scatter', mode = 'lines+markers')
  })
  output$plotutSpeed <- renderPlotly({
    plot_ly(data = plotdf(),
            x = ~Points) %>%
      add_trace(y = ~Speed,
                marker = list(size = 5,
                              color = 'rgb(255, 230, 230)',
                              line = list(color = 'rgb(255, 230, 230)',
                                          width = 0)),
                type = 'scatter',
                mode = 'markers',
                hoverinfo = 'text',
                text = ~paste('MotA: ', amota, '\nADex: ',
                              adex, '\nPoints:', Points, '\nSpeed: ',
                              Speed)) %>%
      # layout(title = ~paste('All possible distributions of points\n', 'Delve:',input$delve, 'Dex:', input$currentdex)) %>%
      layout(
        # title = list(text = ~paste0(#'All possible distributions of points',
        #     'Delve:',input$delve, '  Dex:', '', input$currentdex, '\n'),
        #     x = 0.15,
        #     font=list(size=16, color = "white")
        # )
      ) %>%
      layout(plot_bgcolor='#343E48') %>%
      layout(paper_bgcolor='#343E48') %>%
      layout(xaxis = list(color = "white",
                          gridcolor = toRGB("gray50"),
                          showgrid = TRUE)) %>%
      layout(yaxis = list(color = "white",
                          gridcolor = toRGB("gray50"),
                          showgrid = TRUE,
                          title = 'Speed (secunds)'))
  })
  output$plotutPercent <- renderPlotly({
    plot_ly(data = plotdf(),
            x = ~Points) %>%
      add_trace(y = ~Percent,
                marker = list(size = 5,
                              color = 'rgb(255, 230, 230)',
                              line = list(color = 'rgb(255, 230, 230)',
                                          width = 0)),
                type = 'scatter',
                mode = 'markers',
                hoverinfo = 'text',
                text = ~paste('MotA: ', amota, '\nADex: ',
                              adex, '\nPoints:', Points, '\nSpeed: ',
                              Speed,'\nPercent ', Percent)) %>%
      # layout(title = ~paste('All possible distributions of points\n', 'Delve:',input$delve, 'Dex:', input$currentdex)) %>%
      layout(
        # title = list(text = ~paste0(#'All possible distributions of points',
        #     'Delve:',input$delve, '  Dex:', '', input$currentdex, '\n'),
        #     x = 0.15,
        #     font=list(size=16, color = "white")
        # )
      ) %>%
      layout(plot_bgcolor='#343E48') %>%
      layout(paper_bgcolor='#343E48') %>%
      layout(xaxis = list(color = "white",
                          gridcolor = toRGB("gray50"),
                          showgrid = TRUE)) %>%
      layout(yaxis = list(color = "white",
                          gridcolor = toRGB("gray50"),
                          showgrid = TRUE,
                          title = 'Percent of'))
  })
  dexcalc <- function(dex, augdex) {
    dex + dexut(augdex)
  }

  castspeed <- function(delve, dex, mota) {
    delve * (1 - ((dex - 60)/600)) * (1 - motaut(mota))
  }
  cspeed <- function(delve, dex, mota) {
    (delve) * (1 - ((dex - 60)/600)) * (1 - (mota))
  }
  dex1 <- function() {
    input$currentdex + dexut(input$dexaug1)
  }

  output$casttimecurrent <- renderText({
    paste("Old casttime: ",
          round(
            castspeed(
              input$delve,
              (input$currentdex),
              0), 4), " sec")
  })

  output$casttimenew <- renderText({
    paste(
      round(
        castspeed(
          input$delve,
          (input$currentdex + dexut(input$dexaug2)),
          input$mota2), 3), " sec")
  })

  output$diff <- renderText({
    paste('Diff:',
          round(
            castspeed(
              input$delve,
              (input$currentdex + dexut(input$dexaug2)),
              input$mota2) - castspeed(
                input$delve,
                (input$currentdex),
                0),4), ' sec')
  })

  output$pointsspent <- renderText({
    paste('Points spent:', costut(input$mota2) + costut(input$dexaug2))
  })

  output$relativevalue <- renderText({
    paste('Cast time decrease per point:', round(
      (castspeed(
        input$delve,
        (input$currentdex + dexut(input$dexaug2)),
        input$mota2) -
         castspeed(
           input$delve,
           (input$currentdex),
           0)) / (costut(input$mota2) + costut(input$dexaug2)),4))
  })

  output$percentDecrease <- renderText({
    paste('Cast time decrease in %:',
          round((1 -
                   castspeed(
                     input$delve,
                     (input$currentdex + dexut(input$dexaug2)),
                     input$mota2) / castspeed(
                       input$delve,
                       (input$currentdex),
                       0))*100, 4))
  })

  motaut <- function(m) {

    if (m == 0) {
      mot = 0.00
    } else if(m == 1) {
      mot = 0.01
    } else if(m == 2) {
      mot = 0.02
    } else if(m == 3) {
      mot = 0.03
    } else if(m == 4) {
      mot = 0.05
    } else if(m == 5) {
      mot = 0.07
    } else if(m == 6) {
      mot = 0.09
    } else if(m == 7) {
      mot = 0.11
    } else if(m == 8) {
      mot = 0.13
    } else {
      mot = 0.15
    }
    mot
  }

  dexut <- function(d) {

    if (d == 0) {
      mot = 0.00
    } else if(d == 1) {
      mot = 4
    } else if(d == 2) {
      mot = 8
    } else if(d == 3) {
      mot = 12
    } else if(d == 4) {
      mot = 17
    } else if(d == 5) {
      mot = 22
    } else if(d == 6) {
      mot = 28
    } else if(d == 7) {
      mot = 34
    } else if(d == 8) {
      mot = 41
    } else {
      mot = 48
    }
    mot
  }

  costut <- function(c) {

    if (c == 0) {
      mot = 0
    } else if(c == 1) {
      mot = 1
    } else if(c == 2) {
      mot = 2
    } else if(c == 3) {
      mot = 4
    } else if(c == 4) {
      mot = 7
    } else if(c == 5) {
      mot = 10
    } else if(c == 6) {
      mot = 15
    } else if(c == 7) {
      mot = 20
    } else if(c == 8) {
      mot = 27
    } else {
      mot = 34
    }
    mot
  }

  output$userpanel <- renderUI({

    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })


  # output$testtext <- renderText(names(plot_prob()))
  observeEvent(input$show, {
    showModal(modalDialog(
      title = NULL,
      "bimbo@bimbo.rocks",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui, server)