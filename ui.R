
dashboardPage(

  dashboardHeader(title = 'DAoC - Phoenix'),

  dashboardSidebar(
    # use_waiter(),
    # waiter_on_busy(),
    sidebarMenu(id = 'tabs',
                menuItem("Phoenix Herald",
                         tabName = 'herald',
                         icon = icon("broom"),
                         startExpanded = TRUE,
                         menuSubItem("Guild",
                                     tabName = "guild",
                                     icon = icon('users')),
                         conditionalPanel(
                           "input.tabs == 'guild'",
                           textInput('guild_select',
                                     'Search guild',
                                     value = 'Janitors'),
                           actionButton('guild_update', 'Search', icon = icon('users'), width = NULL)
                         ),
                         menuSubItem("Player",
                                     tabName = "player",
                                     icon = icon('user')),
                         conditionalPanel(
                           "input.tabs == 'player'",
                           textInput('player_select',
                                     'Search player',
                                     placeholder = 'Search player',
                                     value = ''),
                           actionButton('player_update', 'Search', icon = icon('user'), width = NULL)
                         )),

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


    # The dynamically-generated user panel
    uiOutput("userpanel")
  ),

  dashboardBody(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    tags$head(tags$link(rel="shortcut icon", href="https://playphoenix.online/assets/images/favicon.png")),
    tags$head(tags$style(HTML('.shiny-server-account { display: none; } opacity:0.5 !important;'))),
    tags$head(tags$style("#dri {width:200px;}")),
    tags$style(".fa-user {color:#E87722}"),
    tags$style(".fa-users {color:#00cc00}"),
    tags$style("
    .box.box-solid.box-primary{
    background:#2D3741;
    border-bottom-color:#2D3741;
    border-left-color:#2D3741;
    border-right-color:#2D3741;
    border-top-color:#2D3741;}
    .box.box-solid.box-primary.box-header{
    color:#2D3741;
    background-color:#2D3741;}"),
    # tags$style("
    # .nav-tabs-custom.tab-content{
    # background:#000;}"),


    # tags$head(tags$style(HTML('.info-box {max-width: 50px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    # uiChangeThemeDropdown(), .info-box-icon {height: 45px; line-height: 45px;}
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tabItems(

      tabItem(tabName = 'guild',
              tabPanel(title = 'Guild',

                       verbatimTextOutput('testtext'),
                       tabBox(width = 9, id = 'guild_1', selected = 'Top dogs', title = uiOutput('herald_name'),
                              # title = uiOutput('herald_name'), solidHeader = TRUE, status = "primary",
                              # fluidRow(width = 9,
                              #          box(width = 9, uiOutput('herald_name'))
                              # ),
                              tabPanel(title = 'Top dogs',
                                       fluidRow(width = 9,
                                                infoBoxOutput('guild_top_healer', width = 4),
                                                infoBoxOutput('guild_top_caster', width = 4),
                                                infoBoxOutput('guild_top_melee', width = 4)
                                                ),
                                       fluidRow(width = 9,
                                                infoBoxOutput('guild_top_stealth', widt = 4)
                                                ),
                                       fluidRow(
                                         box(width = 12,
                                             withSpinner(DT::dataTableOutput('guild_dt_all_time'), color = '#00cc00'))

                                       )
                              ),
                              tabPanel(title = 'Guild',
                                       fluidRow(width = 9,
                                                infoBoxOutput("guild_rp", width = 4),
                                                infoBoxOutput('guild__n_chars', width = 4),
                                                infoBoxOutput('rp_character_ratio', width = 4)
                                                # infoBoxOutput("player_mostrp", width = 3),
                                       ),
                                       fluidRow(width = 9,
                                                infoBoxOutput('guild_irs', width = 4),
                                                infoBoxOutput("guild_byrealm", width = 4),
                                                infoBoxOutput("rp_kills_ratio", width = 4)
                                       ),
                                       fluidRow(width = 9,
                                                infoBoxOutput('kill_death_ratio', width = 4),
                                                # infoBoxOutput('solokill_kill_ratio'),
                                                infoBoxOutput('deathblows_kill_ratio', width = 4),
                                                infoBoxOutput('deathblows_death_ratio', width = 4)
                                       ),
                                       fluidRow(width = 9
                                                # infoBoxOutput("guild_48h", width = 4),
                                                # infoBoxOutput("guild_byrealm_48h", width = 4),
                                                # infoBoxOutput('kill_death_ratio_48h', width = 4)
                                       ),
                                       # fluidRow(width = 9,
                                       #          infoBoxOutput("rp_kills_ratio_48h", width = 4),
                                       #          infoBoxOutput('deathblows_kill_ratio_48h', width = 4),
                                       #          infoBoxOutput('deathblows_death_ratio_48h', width = 4)
                                       # ),
                                       fluidRow(
                                         box(width = 12,
                                             withSpinner(DT::dataTableOutput('guild_dt_48h'), color = '#00cc00'))

                                       )
                              ),
                              tabPanel(title = 'Rooster',
                                       fluidRow(width = 9,
                                                infoBoxOutput("guild_thisweek", width = 4),
                                                infoBoxOutput("guild_byrealm_thisweek", width = 4),
                                                infoBoxOutput('kill_death_ratio_thisweek', width = 4)
                                       ),
                                       fluidRow(width = 9,
                                                infoBoxOutput("rp_kills_ratio_thisweek", width = 4),
                                                infoBoxOutput('deathblows_kill_ratio_thisweek', width = 4),
                                                infoBoxOutput('deathblows_death_ratio_thisweek', width = 4)
                                       ),
                                       fluidRow(
                                         box(width = 12,
                                                 fluidRow(width = 12, title = 'Rooster',

                                                 )
                                         )
                                       )
                              ),
                              tabPanel(title = 'Last week',
                                       fluidRow(width = 9,
                                                infoBoxOutput("guild_lastweek", width = 4),
                                                infoBoxOutput("guild_byrealm_lastweek", width = 4),
                                                infoBoxOutput('kill_death_ratio_lastweek', width = 4)
                                       ),
                                       fluidRow(width = 9,
                                                infoBoxOutput("rp_kills_ratio_lastweek", width = 4),
                                                infoBoxOutput('deathblows_kill_ratio_lastweek', width = 4),
                                                infoBoxOutput('deathblows_death_ratio_lastweek', width = 4)
                                       ),
                                       fluidRow(
                                         box(width = 12,
                                             withSpinner(DT::dataTableOutput('guild_dt_lastweek'), color = '#00cc00'))

                                       )
                              )
                       )

              )),
      tabItem(tabName = 'player',
              tabBox(id = 'player_1', width = 9, selected = 'All time', title = uiOutput('player_name'),
                     tabPanel(title = 'All time',
                              fluidRow(
                                infoBoxOutput("player_rp", width = 4),
                                infoBoxOutput('player_byrealm', width = 4),
                                infoBoxOutput("player_kill_death_ratio", width = 4)

                              ),
                              fluidRow(
                                infoBoxOutput("player_deathblows_kill_ratio", width = 4),
                                infoBoxOutput("player_deathblows_death_ratio", width = 4),
                                infoBoxOutput("player_rp_kills_ratio", width = 4)
                              ),
                              fluidRow(
                                box(width = 12,
                                    withSpinner(DT::dataTableOutput('player_dt_all_time'), color = '#E87722'))

                              )
                     ),
                     tabPanel(title = 'Last 48h',
                              fluidRow(
                                # verbatimTextOutput('testtext'),
                                infoBoxOutput("player_rp_48h", width = 4),
                                infoBoxOutput('player_byrealm_48h', width = 4),
                                infoBoxOutput('player_kill_death_ratio_48h', width = 4)

                              ),
                              fluidRow(
                                # infoBoxOutput("player_rp_48h", width = 3),
                                infoBoxOutput('player_deathblows_kill_ratio_48h', width = 4),
                                infoBoxOutput("player_deathblows_death_ratio_48h", width = 4),
                                infoBoxOutput("player_rp_kills_ratio_48h", width = 4)
                              ),
                              fluidRow(
                                box(width = 12,
                                    withSpinner(DT::dataTableOutput('player_dt_48h'), color = '#E87722'))

                              )
                     ),
                     tabPanel(title = 'This week',
                              fluidRow(
                                # verbatimTextOutput('testtext'),
                                infoBoxOutput("player_rp_thisweek", width = 4),
                                infoBoxOutput('player_byrealm_thisweek', width = 4),
                                infoBoxOutput('player_kill_death_ratio_thisweek', width = 4)

                              ),
                              fluidRow(
                                # infoBoxOutput("player_rp_48h", width = 3),
                                infoBoxOutput('player_deathblows_kill_ratio_thisweek', width = 4),
                                infoBoxOutput("player_deathblows_death_ratio_thisweek", width = 4),
                                infoBoxOutput("player_rp_kills_ratio_thisweek", width = 4)
                              ),
                              fluidRow(
                                box(width = 12,
                                    withSpinner(DT::dataTableOutput('player_dt_this_week'), color = '#E87722'))

                              )
                     ),
                     tabPanel(title = 'Last week',
                              fluidRow(
                                # verbatimTextOutput('testtext'),
                                infoBoxOutput("player_rp_lastweek", width = 4),
                                infoBoxOutput('player_byrealm_lastweek', width = 4),
                                infoBoxOutput('player_kill_death_ratio_lastweek', width = 4)

                              ),
                              fluidRow(
                                # infoBoxOutput("player_rp_48h", width = 3),
                                infoBoxOutput('player_deathblows_kill_ratio_lastweek', width = 4),
                                infoBoxOutput("player_deathblows_death_ratio_lastweek", width = 4),
                                infoBoxOutput("player_rp_kills_ratio_lastweek", width = 4)
                              ),
                              fluidRow(
                                box(width = 12,
                                    withSpinner(DT::dataTableOutput('player_dt_last_week'), color = '#E87722'))

                              )
                     )
              )
      ),
      tabItem(tabName = "craftprobability",
              column(width = 6,
                     fluidRow(#width = 6,
                       box(width = 6, #style = "font-size: 14px;",
                           title = "", 'What is the probability of making atleast',
                           tags$i('x'), 'Master pieces in', tags$i('y'),  'number of tries?\n', br(),
                           tags$br(),
                           numericInput('no_mp',
                                        value = 1,
                                        step = 1,
                                        min = 0,
                                        label = "Number of MP's: "),
                           sliderInput('no_tries',
                                       value = 50,
                                       step = 1,
                                       min = 1,
                                       max = 500,
                                       label = 'Number of tries: ')
                       ),

                       box(width = 6,
                           title = "",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           collapsed = F,
                           span(textOutput("probability_value"), style = "font-size:30px;font-weight: bold;font-style:oblique"),
                           textOutput('probability_text')
                       )),
                     fluidRow(
                       box(width = 12,
                           title = "",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           collapsed = F,
                           plotlyOutput('distrib_plot')
                       )
                     )


              ),
              column(width = 6,

                     box(#width = 3,
                       title = 'Unknown master crafter',
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       img(src='daoc_flex.jpg', height="100%", width="100%", align = "center"))

              )
      ),

      tabItem(tabName = 'castspeed',
              fluidRow(
                column(width = 2,
                       box(width = 12,
                           numericInput("delve",
                                        "Delve:",
                                        min = 1,
                                        max = 10,
                                        value = 3,
                                        step = 0.1),
                           numericInput("currentdex",
                                        "Current dex(fully buffed):",
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

                       box( width = 12,
                            helpText('Pretty much stolen from' ,
                                     tags$b(tags$a(href = "https://docs.google.com/spreadsheets/d/1CslgNBWCDhdfEYrCxDyklMosSCPAnAacTEdpbK32esE/edit#gid=0","here"))))
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
              ),
              fluidRow(width = 12,
                       box(width = 8,
                           title = 'Buff Cheat Sheet - MoA (i can not remember from who i stole it)',
                           img(src='buff_moa.jpg', height="100%", width="100%", align = "center")
                       ))
      ),

      tabItem(tabName = 'about',
              box('This is a hobby project of mine where i play with a raspberry Pi.',
                  tags$br(),
                  'I am not a programmer or an IT professional,
                  but just a guy that likes to play around with stuff like this.',
                  tags$br(),
                  'And play DAoC ofc!',
                  tags$br(),
                  'The name bimbo comes from an old climbing database and has nothing do to with real bimbos (i am blond though)',
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

              tags$style(type = 'text/css', '#show_mail
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
                box(title = 'Task order',
                    uiOutput('task_order'))
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