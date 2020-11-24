source('./R/function_daoc.R')

server <- function(input, output, session) {
#
  output$testtext <- renderText(class(guild()[[7]]))

  vals <- reactiveValues(guild = 'Janitors',
                         player = '')

  validate_guild <- function() {
    validate(need(exists_guild(vals$guild) == TRUE, 'Guild not found'))
  }

  validate_player <- function() {
    validate(need(exists_player(vals$player) == TRUE, 'Player not found'))
  }

  observeEvent(input$guild_update, {
    vals$guild <- paste(input$guild_select)
  })

  observeEvent(input$player_update, {
    vals$player <- paste(input$player_select)
  })

  guild <- reactive({
    guild <- get_guild(vals$guild)
  })

  rooster <- reactive({
    rooster <- guild()[[7]]
  })

  player <- reactive({
    player <- get_player(vals$player)
  })

  output$player_name <- renderUI({
    if(exists_player(vals$player) == F) {
      HTML(paste('<b style="color:#E87722">- ', 'Search for a player', '</b>'))
    } else {
      HTML(paste('<b style="color:#E87722">- ', vals$player, '</b>'))
    }
  })

  output$herald_name <- renderUI({
    if(exists_guild(vals$guild) == F) {
      HTML(paste('<b style="color:#00cc00">- ', 'Search for a guild', '</b>'))
    } else {
      HTML(paste('<b style="color:#00cc00">- ', vals$guild, '</b>'))
    }
  })

  # output$player_name <- renderUI({
  #   if(vals$player == '') {
  #     HTML(paste('<b style="color:#E87722">- ', 'Search for a player', '</b>'))
  #   } else {
  #     HTML(paste('<b style="color:#E87722">- ', vals$player, '</b>'))
  #   }
  # })

  output$janitor_dt <- DT::renderDataTable({
    validate_guild()
    DT::datatable(
      guild()[[7]],
      rownames = F,
      options = list(
        dom = 'tpl',
        pageLength = 10))
  })

# GUILD INFOBOX ###############################################################################


  output$guild_top_healer_lastweek <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Cleric' | Class == 'Friar') %>%
      arrange('RP last week') %>% slice(1)
    infoBox(
      title = HTML(paste('Top healer')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8], '</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })

  output$guild_top_caster_lastweek <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Cabalist' | Class == 'Necromancer' | Class == 'Sorcerer' | Class == 'Theurgist' | Class == 'Wizard') %>%
      arrange('RP last week') %>% slice(1)
    infoBox(
      title = HTML(paste('Top caster')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8],'</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })

  output$guild_top_melee_lastweek <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Armsman' | Class == 'Armswoman' | Class == 'Mercenary' | Class == 'Paladin' | Class == 'Reaver') %>%
      arrange('RP last week') %>% slice(1)
    infoBox(
      title = HTML(paste('Top melee')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8], '</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })

  output$guild_top_stealth_lastweek <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Infiltrator' | Class == 'Minstrel' | Class == 'Scout') %>%
      arrange('RP last week') %>% slice(1)
    infoBox(
      title = HTML(paste('Top sneaker')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8], '</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })
  output$guild_top_healer48 <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Cleric' | Class == 'Friar') %>%
      arrange('RP 48h') %>% slice(1)
    infoBox(
      title = HTML(paste('Top healer')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8], '</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })

  output$guild_top_caster48 <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Cabalist' | Class == 'Necromancer' | Class == 'Sorcerer' | Class == 'Theurgist' | Class == 'Wizard') %>%
      arrange('RP 48h') %>% slice(1)
    infoBox(
      title = HTML(paste('Top caster')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8],'</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })

  output$guild_top_melee48 <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Armsman' | Class == 'Armswoman' | Class == 'Mercenary' | Class == 'Paladin' | Class == 'Reaver') %>%
      arrange('RP 48h') %>% slice(1)
    infoBox(
      title = HTML(paste('Top melee')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8], '</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })

  output$guild_top_stealth48 <- renderInfoBox({
    rp48 <- rooster() %>%
      filter(Class == 'Infiltrator' | Class == 'Minstrel' | Class == 'Scout') %>%
      arrange('RP 48h') %>% slice(1)
    infoBox(
      title = HTML(paste('Top sneaker')),
      subtitle = HTML(paste(rp48[1,1], '<b>-', rp48[1,8], '</b>rps')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })
# ALL TIME ####################################################################################

  output$guild_dt_all_time <- DT::renderDataTable({
    validate_guild()
    dt_data <- time_tbl(guild(), 1)
    DT::datatable(
      dt_data,
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$guild_irs <- renderInfoBox({
    infoBox(
    title = "IRS",
    subtitle = HTML(paste('<b>', round(guild()[[1]][1,2] / guild()[[3]][1,2], 0),'</b>')),
    icon = icon("dumpster"),
    color = 'blue'
    )
  })
  output$guild_rp <- renderInfoBox({
    validate_guild()
    # guild <- guild()
    infoBox(
      title = HTML(paste('Total rp<b>', br(), guild()[[1]][[2]][1], '</b>')),
      subtitle = HTML(paste(guild()[[1]][[3]][1],"server rank", br(),
                            guild()[[1]][[4]][1], 'realm rank')),
      icon = icon("broom", lib = "font-awesome"),
      color = "olive"
    )
  })


  output$guild__n_chars <- renderInfoBox({
    validate_guild()
    n_chars <- rooster() %>% nrow()
    n_lvl50 <- rooster() %>% filter(Lvl == 50) %>% count()
    infoBox(
      title = HTML(paste('No. characters')),
      subtitle = HTML(paste('<b>', n_chars, '</b> - chars', tags$br(),
                            '<b>', n_lvl50, '</b> - lvl50')),
      icon = icon("restroom", lib = "font-awesome"),
      color = "green"
    )
  })

  output$rp_character_ratio <- renderInfoBox({
    validate_guild()
    rps <- guild()[[1]]
    n_chars <- nrow(rooster())
    n_lvl50 <- rooster() %>% filter(Lvl == 50) %>% count()
    infoBox(
      title = HTML(paste('rp / character')),
      subtitle = HTML(paste(
        '<b>', format(round((rps[1,2] / n_chars), 0), big.mark = ","), '</b>/ char', tags$br(),
        '<b>', format(round((rps[1,2] / n_lvl50), 0), big.mark = ","), '</b>/ lvl 50'
      )),
      icon = icon("bath", lib = "font-awesome"),
      color = "lime"
    )
  })

  output$guild_byrealm <- renderInfoBox({
    validate_guild()
    # guild <- guild()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', guild()[[6]][[1,4]],"</b>hib", br(),
                            '<b>', guild()[[6]][[1,3]], '</b>mid', br(),
                            '<b>', guild()[[6]][[1,2]], '</b>alb')
      ),
      icon = icon("dumpster", lib = "font-awesome"),
      color = "light-blue"
    )
  })

  output$rp_kills_ratio <- renderInfoBox({
    validate_guild()
    rp <- guild()[[1]]
    kills <- guild()[[4]]
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[1,2]/kills[1,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash"),
      color = "aqua"
    )
  })

  output$kill_death_ratio <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[1,2]/deaths[1,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("pump-soap", lib = "font-awesome"),
      color = "teal"
    )
  })

  output$solokill_kill_ratio <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    solo <- tbl_4x4_numeric(guild()[[5]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(solo[1,2]/kills[1,2], 3), '</b>')),
      title = HTML(paste('Solo kills / Kills')),
      icon = icon("toilet", lib = "font-awesome"),
      color = "maroon"
    )
  })

  output$deathblows_kill_ratio <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[1,2]/kills[1,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap", lib = "font-awesome"),
      color = "red"
    )
  })

  output$deathblows_death_ratio <- renderInfoBox({
    validate_guild()
    blows <- tbl_4x4_numeric(guild()[[2]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[1,2]/deaths[1,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Deaths')),
      icon = icon("hands-wash", lib = "font-awesome"),
      color = "orange"
    )
  })


# 48H #################################################################################

  output$guild_dt_48h <- DT::renderDataTable({
    validate_guild()
    dt_data <- time_tbl(guild(), 4)
    DT::datatable(
      dt_data,
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$guild_48h <- renderInfoBox({
    validate_guild()
    guild <- guild()
    infoBox(
      title = HTML(paste('rp last 48h<b>', br(), guild[[1]][[2]][4], '</b>')),
      subtitle = HTML(paste(guild[[1]][[3]][4],"server rank", br(),
                            guild[[1]][[4]][4], 'realm rank')),
      icon = icon("snowplow", lib = "font-awesome"),
      color = "green"
    )
  })

  output$guild_byrealm_48h <- renderInfoBox({
    validate_guild()
    guild <- guild()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', guild[[6]][[4,4]],"</b>hib", br(),
                            '<b>', guild[[6]][[4,3]], '</b>mid', br(),
                            '<b>', guild[[6]][[4,2]], '</b>alb')
      ),
      icon = icon("dumpster", lib = "font-awesome"),
      color = "light-blue"
    )
  })

  output$rp_kills_ratio_48h <- renderInfoBox({
    validate_guild()
    rp <- tbl_4x4_numeric(guild()[[1]])
    kills <- tbl_4x4_numeric(guild()[[4]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[4,2]/kills[4,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash", lib = "font-awesome"),
      color = "aqua"
    )
  })

  output$kill_death_ratio_48h <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[4,2]/deaths[4,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("pump-soap", lib = "font-awesome"),
      color = "teal"
    )
  })

  output$solokill_kill_ratio_48h <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    solo <- tbl_4x4_numeric(guild()[[5]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(solo[4,2]/kills[4,2], 3), '</b>')),
      title = HTML(paste('Solo kills / Kills')),
      icon = icon("toilet", lib = "font-awesome"),
      color = "maroon"
    )
  })

  output$deathblows_kill_ratio_48h <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[4,2]/kills[4,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap", lib = "font-awesome"),
      color = "red"
    )
  })

  output$deathblows_death_ratio_48h <- renderInfoBox({
    validate_guild()
    blows <- tbl_4x4_numeric(guild()[[2]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[4,2]/deaths[4,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Deaths')),
      icon = icon("hands-wash", lib = "font-awesome"),
      color = "orange"
    )
  })

# THIS WEEK ############################################################################

  output$guild_dt_thisweek <- DT::renderDataTable({
    validate_guild()
    player <- time_tbl(guild(), 3)
    # player <- tbl_4x4_numeric(player[[1]])
    DT::datatable(
      player,
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$guild_thisweek <- renderInfoBox({
    validate_guild()
    guild <- guild()
    infoBox(
      title = HTML(paste('rp this week<b>', br(), guild[[1]][[2]][3], '</b>')),
      subtitle = HTML(paste(guild[[1]][[3]][3],"server rank", br(),
                            guild[[1]][[4]][3], 'realm rank')),
      icon = icon("toilet-paper", lib = "font-awesome"),
      color = "lime"
    )
  })

  output$guild_byrealm_thisweek <- renderInfoBox({
    validate_guild()
    guild <- guild()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', guild[[6]][[3,4]],"</b>hib", br(),
                            '<b>', guild[[6]][[3,3]], '</b>mid', br(),
                            '<b>', guild[[6]][[3,2]], '</b>alb')
      ),
      icon = icon("dumpster", lib = "font-awesome"),
      color = "light-blue"
    )
  })

  output$rp_kills_ratio_thisweek <- renderInfoBox({
    validate_guild()
    rp <- tbl_4x4_numeric(guild()[[1]])
    kills <- tbl_4x4_numeric(guild()[[4]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[3,2]/kills[3,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash", lib = "font-awesome"),
      color = "aqua"
    )
  })

  output$kill_death_ratio_thisweek <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[3,2]/deaths[3,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("pump-soap", lib = "font-awesome"),
      color = "teal"
    )
  })

  output$solokill_kill_ratio_thisweek <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    solo <- tbl_4x4_numeric(guild()[[5]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(solo[3,2]/kills[3,2], 3), '</b>')),
      title = HTML(paste('Solo kills / Kills')),
      icon = icon("toilet", lib = "font-awesome"),
      color = "maroon"
    )
  })

  output$deathblows_kill_ratio_thisweek <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[3,2]/kills[3,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap", lib = "font-awesome"),
      color = "red"
    )
  })

  output$deathblows_death_ratio_thisweek <- renderInfoBox({
    validate_guild()
    blows <- tbl_4x4_numeric(guild()[[2]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[3,2]/deaths[3,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Deaths')),
      icon = icon("hands-wash", lib = "font-awesome"),
      color = "orange"
    )
  })

# LAST WEEK ############################################################################

  output$guild_dt_lastweek <- DT::renderDataTable({
    validate_guild()
    # guild <- time_tbl(guild(), 2)
    # player <- tbl_4x4_numeric(player[[1]])
    DT::datatable(
      time_tbl(guild(), 2),
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$guild_lastweek <- renderInfoBox({
    validate_guild()
    guild <- guild()
    infoBox(
      title = HTML(paste('rp last week<b>', br(), format(guild[[1]][[2]][2], big.mark = ","), '</b>')),
      subtitle = HTML(paste(guild[[1]][[3]][2],"server rank", br(),
                            guild[[1]][[4]][2], 'realm rank')),
      icon = icon("toilet", lib = "font-awesome"),
      color = "lime"
    )
  })

  # output$guild_irs <- renderValueBox({
  #   validate_guild()
  #   rp <- tbl_4x4_numeric(guild()[[1]][1,2])
  #   deaths <- tbl_4x4_numeric(guild()[[3]][1,2])
  #   valueBox(
  #     "IRS",
  #     rp/deaths,
  #     icon = icon("credit-card"),
  #     color = 'green'
  #   )
  # })

  output$guild_byrealm_lastweek <- renderInfoBox({
    validate_guild()
    guild <- guild()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', guild[[6]][[2,4]],"</b>hib", br(),
                            '<b>', guild[[6]][[2,3]], '</b>mid', br(),
                            '<b>', guild[[6]][[2,2]], '</b>alb')
      ),
      icon = icon("dumpster", lib = "font-awesome"),
      color = "light-blue"
    )
  })

  output$rp_kills_ratio_lastweek <- renderInfoBox({
    validate_guild()
    rp <- tbl_4x4_numeric(guild()[[1]])
    kills <- tbl_4x4_numeric(guild()[[4]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[2,2]/kills[2,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash", lib = "font-awesome"),
      color = "aqua"
    )
  })

  output$kill_death_ratio_lastweek <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[3,2]/deaths[3,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("pump-soap", lib = "font-awesome"),
      color = "teal"
    )
  })

  output$solokill_kill_ratio_lastweek <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    solo <- tbl_4x4_numeric(guild()[[5]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(solo[2,2]/kills[2,2], 3), '</b>')),
      title = HTML(paste('Solo kills / Kills')),
      icon = icon("toilet", lib = "font-awesome"),
      color = "maroon"
    )
  })

  output$deathblows_kill_ratio_lastweek <- renderInfoBox({
    validate_guild()
    kills <- tbl_4x4_numeric(guild()[[4]])
    deaths <- tbl_4x4_numeric(guild()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[2,2]/kills[2,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap", lib = "font-awesome"),
      color = "red"
    )
  })

  output$deathblows_death_ratio_lastweek <- renderInfoBox({
    validate_guild()
    blows <- tbl_4x4_numeric(guild()[[2]])
    deaths <- tbl_4x4_numeric(guild()[[3]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[2,2]/deaths[2,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Deaths')),
      icon = icon("hands-wash", lib = "font-awesome"),
      color = "orange"
    )
  })




# PLAYER INFOBOX ##############################################################################
  # LAST WEEK ########################################################

  output$player_dt_last_week <- DT::renderDataTable({
    validate_player()
    player <- time_tbl(player(), 2)
    # player <- tbl_4x4_numeric(player[[1]])
    DT::datatable(
      player,
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$player_deathblows_kill_ratio_lastweek <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[2,2]/kills[2,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap"),
      color = "red"
    )
  })

  output$player_rp_lastweek <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('rp last week<b>', br(), player[[1]][[2]][2], '</b>')),
      subtitle = HTML(paste(player[[1]][[3]][2],"server rank", br(),
                            player[[1]][[4]][2], 'realm rank')),
      icon = icon("broom"),
      color = "green"
    )
  })

  output$player_deathblows_death_ratio_lastweek <- renderInfoBox({
    validate_player()
    blows <- tbl_4x4_numeric(player()[[2]])
    deaths <- tbl_4x4_numeric(player()[[3]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[2,2]/deaths[2,2], 2), '</b>')),
      title = HTML(paste('Deathblows / deaths')),
      icon = icon("hands-wash"),
      color = "orange"
    )
  })

  output$player_kill_death_ratio_lastweek <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[2,2]/deaths[2,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("restroom"),
      color = "teal"
    )
  })

  output$player_rp_kills_ratio_lastweek <- renderInfoBox({
    validate_player()
    rp <- tbl_4x4_numeric(player()[[1]])
    kills <- tbl_4x4_numeric(player()[[4]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[2,2]/kills[2,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash"),
      color = "maroon"
    )
  })

  output$player_byrealm_lastweek <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', player[[6]][[2,4]],"</b>hib", br(),
                            '<b>', player[[6]][[2,3]], '</b>mid', br(),
                            '<b>', player[[6]][[2,2]], '</b>alb')
      ),
      icon = icon("dumpster",lib = "font-awesome"),
      color = "lime"
    )
  })

  # THIS WEEK ########################################################

  output$player_dt_this_week <- DT::renderDataTable({
    validate_player()
    player <- time_tbl(player(), 3)
    # player <- tbl_4x4_numeric(player[[1]])
    DT::datatable(
      player,
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$player_deathblows_kill_ratio_thisweek <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[3,2]/kills[3,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap"),
      color = "red"
    )
  })

  output$player_rp_thisweek <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('rp this week<b>', br(), player[[1]][[2]][3], '</b>')),
      subtitle = HTML(paste(player[[1]][[3]][3],"server rank", br(),
                            player[[1]][[4]][3], 'realm rank')),
      icon = icon("broom"),
      color = "green"
    )
  })

  output$player_deathblows_death_ratio_thisweek <- renderInfoBox({
    validate_player()
    blows <- tbl_4x4_numeric(player()[[2]])
    deaths <- tbl_4x4_numeric(player()[[3]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[3,2]/deaths[3,2], 2), '</b>')),
      title = HTML(paste('Deathblows / deaths')),
      icon = icon("hands-wash"),
      color = "orange"
    )
  })

  output$player_kill_death_ratio_thisweek <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[3,2]/deaths[3,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("restroom"),
      color = "teal"
    )
  })

  output$player_rp_kills_ratio_thisweek <- renderInfoBox({
    validate_player()
    rp <- tbl_4x4_numeric(player()[[1]])
    kills <- tbl_4x4_numeric(player()[[4]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[3,2]/kills[3,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash"),
      color = "maroon"
    )
  })

  output$player_byrealm_thisweek <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', player[[6]][[3,4]],"</b>hib", br(),
                            '<b>', player[[6]][[3,3]], '</b>mid', br(),
                            '<b>', player[[6]][[3,2]], '</b>alb')
      ),
      icon = icon("dumpster"),
      color = "lime"
    )
  })

  # LAST 48 H ########################################################

  output$player_dt_48h <- DT::renderDataTable({
    validate_player()
    player <- time_tbl(player(), 4)
    # player <- tbl_4x4_numeric(player[[1]])
    DT::datatable(
      player,
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$player_deathblows_kill_ratio_48h <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[4,2]/kills[4,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap"),
      color = "red"
    )
  })

  output$player_rp_48h <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('rp last 48h<b>', br(), player[[1]][[2]][4], '</b>')),
      subtitle = HTML(paste(player[[1]][[3]][4],"server rank", br(),
                            player[[1]][[4]][4], 'realm rank')),
      icon = icon("broom"),
      color = "green"
    )
  })

  output$player_deathblows_death_ratio_48h <- renderInfoBox({
    validate_player()
    blows <- tbl_4x4_numeric(player()[[2]])
    deaths <- tbl_4x4_numeric(player()[[3]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[4,2]/deaths[4,2], 2), '</b>')),
      title = HTML(paste('Deathblows / deaths')),
      icon = icon("hands-wash"),
      color = "orange"
    )
  })

  output$player_kill_death_ratio_48h <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[4,2]/deaths[4,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("restroom"),
      color = "teal"
    )
  })

  output$player_rp_kills_ratio_48h <- renderInfoBox({
    validate_player()
    rp <- tbl_4x4_numeric(player()[[1]])
    kills <- tbl_4x4_numeric(player()[[4]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[4,2]/kills[4,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash"),
      color = "maroon"
    )
  })

  output$player_byrealm_48h <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', player[[6]][[4,4]],"</b>hib", br(),
                            '<b>', player[[6]][[4,3]], '</b>mid', br(),
                            '<b>', player[[6]][[4,2]], '</b>alb')
      ),
      icon = icon("dumpster"),
      color = "lime"
    )
  })

  # ALL TIME ###############################################################################

  output$player_dt_all_time <- DT::renderDataTable({
    validate_player()
    player <- time_tbl(player(), 1)
    # player <- tbl_4x4_numeric(player[[1]])
    DT::datatable(
      player,
      rownames = T,
      options = list(
        dom = '',
        pageLength = 10))
  })

  output$player_rp <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('Total rp<b>', br(), player[[1]][[2]][1], '</b>')),
      subtitle = HTML(paste(player[[1]][[3]][1],"server rank", br(),
                            player[[1]][[4]][1], 'realm rank')),
      icon = icon("broom"),
      color = "olive"
    )
  })

  output$player_kill_death_ratio <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[3]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(kills[1,2]/deaths[1,2], 2), '</b>')),
      title = HTML(paste('Kills / Death')),
      icon = icon("restroom", lib = "font-awesome"),
      color = "teal"
    )
  })

  output$player_deathblows_kill_ratio <- renderInfoBox({
    validate_player()
    kills <- tbl_4x4_numeric(player()[[4]])
    deaths <- tbl_4x4_numeric(player()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(deaths[1,2]/kills[1,2], 2), '</b>')),
      title = HTML(paste('Deathblows / Kills')),
      icon = icon("soap", lib = "font-awesome"),
      color = "red"
    )
  })

  output$player_deathblows_death_ratio <- renderInfoBox({
    validate_player()
    blows <- tbl_4x4_numeric(player()[[3]])
    deaths <- tbl_4x4_numeric(player()[[2]])
    infoBox(
      subtitle  = HTML(paste('<b>', round(blows[1,2]/deaths[1,2], 2), '</b>')),
      title = HTML(paste('Deathblows / deaths')),
      icon = icon("hands-wash", lib = "font-awesome"),
      color = "orange"
    )
  })

  output$player_rp_kills_ratio <- renderInfoBox({
    validate_player()
    rp <- tbl_4x4_numeric(player()[[1]])
    kills <- tbl_4x4_numeric(player()[[4]])
    infoBox(
      subtitle =  HTML(paste('<b>', round(rp[1,2]/kills[1,2], 2), '</b>')),
      title = HTML(paste('RP / Kill')),
      icon = icon("trash", lib = "font-awesome"),
      color = "maroon"
    )
  })

  output$player_byrealm <- renderInfoBox({
    validate_player()
    player <- player()
    infoBox(
      title = HTML(paste('Kills by realm')),
      subtitle = HTML(paste('<b>', player[[6]][[1,4]],"</b>hib", br(),
                            '<b>', player[[6]][[1,3]], '</b>mid', br(),
                            '<b>', player[[6]][[1,2]], '</b>alb')
      ),
      icon = icon("dumpster", lib = "font-awesome"),
      color = "lime"
    )
  })

#
  #
  #   output$player_mostrp <- renderInfoBox({
  #     validate_guild()
  #     feh <- rooster() %>% arrange(desc(`Realm Points`))
  #     infoBox(
  #       title = HTML(paste('', feh[1,1], '-',feh[1,4], '','')),
  #       subtitle = HTML(paste('<b>', format(feh[1,5], big.mark=","),'</b>', tags$br(),
  #                             'Most rp ever')),
  #       icon = icon("trophy"),
  #       color = "aqua"
  #     )
  #   })
  #
  #   output$guild_player_48h <- renderInfoBox({
  #     validate_guild()
  #     feh <- rooster() %>% arrange(desc(`RP 48h`))
  #     infoBox(
  #       title = HTML(paste('', feh[1,1],'-',feh[1,4], '', '')),
  #       subtitle = HTML(paste('<b>', format(feh[1,8], big.mark=","), '</b>', tags$br(),
  #                             'Most rp last 48h')),
  #       icon = icon("hands-wash"),
  #       color = "light-blue"
  #     )
  #   })
  #
  #   output$guild_player_lastweek <- renderInfoBox({
  #     validate_guild()
  #     feh <- rooster() %>% arrange(desc(`RP last week`))
  #     infoBox(
  #       title = HTML(paste('', feh[1,1], '-', feh[1,4], '', '')),
  #       subtitle = HTML(paste('<b>',format(feh[1,6], big.mark=","), '</b>', tags$br(),
  #                             'Most rp last week')),
  #       icon = icon("soap"),
  #       color = "blue"
  #     )
  #   })
  #
  #   output$guild_player_thisweek <- renderInfoBox({
  #     validate_guild()
  #     feh <- rooster() %>% arrange(desc(`RP this week`))
  #     infoBox(
  #       title = HTML(paste('', feh[1,1], '-', feh[1,4], '', '')),
  #       subtitle = HTML(paste('<b>',format(feh[1,7], big.mark=","), '</b>', tags$br(),
  #                             'Most rp this week')),
  #       icon = icon("hand-sparkles"),
  #       color = "blue"
  #     )
  #   })

  #
  #     output$player_deathblow <- renderInfoBox({
  #       validate_guild()
  #       feh <- get_deathblow(vals$guild)
  #       if ( nrow(feh) < 1) {
  #         subtitle = paste('No character in realm top 250')
  #       } else {
  #         subtitle = HTML(paste('<b>',feh[1,2],'</b>', tags$br(),
  #                               feh[1,7], 'dbs', tags$br(),
  #                               feh[1,1], 'realm rank'))
  #       }
  #       infoBox(
  #         title = HTML(paste('Most deathblows')),
  #         subtitle = subtitle,
  #         icon = icon("hand-rock", lib = "font-awesome"),
  #         color = "yellow"
  #       )
  #     })
  #
  #     output$player_solokills <- renderInfoBox({
  #       validate_guild()
  #       feh <- get_solokills(vals$guild)
  #
  #       if ( nrow(feh) < 1) {
  #         subtitle = paste('No character in realm top 250')
  #       } else {
  #         subtitle = HTML(paste('<b>',feh[1,2],'</b>', tags$br(),
  #                               feh[1,7], 'kills', tags$br(),
  #                               feh[1,1], 'realm rank'))
  #       }
  #       infoBox(
  #         title = HTML(paste('Most solokills')),
  #         subtitle = subtitle,
  #         icon = icon("hand-rock", lib = "font-awesome"),
  #         color = "orange"
  #       )
  #     })
  #
  # output$player_deaths <- renderInfoBox({
  #   validate_guild()
  #   feh <- get_deaths(vals$guild)
  #   if ( nrow(feh) < 1) {
  #     subtitle = paste('No character in realm top 250')
  #   } else {
  #     subtitle = HTML(paste('<b>',feh[1,2],'</b>', tags$br(),
  #                           feh[1,7], 'deaths',  tags$br(),
  #                           feh[1,1], 'realm rank'))
  #   }
#
#     infoBox(
#       title = HTML(paste('Deadiest')),
#       subtitle = subtitle,
#       icon = icon("hand-sparkles", lib = "font-awesome"),
#       color = "yellow"
#     )
#   })



  output$task_order <- renderUI(
    HTML(paste(
      '<b> Ellan Vannin -></b>', tags$br(),
      '<b style="color:red">Hadrians Wall -></b>',
      '<b style="color:powderblue">Odins Gate -></b>',
      '<b style="color:green">Emain Macha -></b>', tags$br(),
      '<b> Ellan Vannin -></b>', tags$br(),
      '<b style="color:green">Breifine -></b>',
      '<b style="color:powderblue">Jamtland Mountains -></b>',
      '<b style="color:red">Pennine Mountains -></b>')
    )
  )
  # Crafting ###################################################################



  output$myImage <- renderUI({
    img(src='daoc_original_map.jpg', height="727px", width = "1689px", align = 'center')
  })


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


  output$userpanel <- renderUI({

    # session$user is non-NULL only in authenticated sessions
    if (!is.null(session$user)) {
      sidebarUserPanel(
        span("Logged in as ", session$user),
        subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
    }
  })

  observeEvent(input$show_mail, {
    showModal(modalDialog(
      title = NULL,
      "bimbo@bimbo.rocks",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}
