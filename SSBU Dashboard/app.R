library(tidyverse)
library(dplyr)
library(ggimage)
library(ggrepel)
library(shiny)
library(ggplot2)
library(DT)

smash_data <- read_csv("https://raw.githubusercontent.com/sbudataanlyst/sbudata/main/github_data.csv")


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(style="display:inline-block",selectInput("stage", "Stage:",
                                               c("Battlefield"="Battlefield",
                                                 "Castle Siege"="Castle Siege",
                                                 "Final Destination"="Final Destination",
                                                 "Fountain of Dreams"="Fountain of Dreams",
                                                 "Frigate Orpheon"="Frigate Orpheon",
                                                 "Hollow Bastion"="Hollow Bastion",
                                                 "Kalos Pokemon League"="Kalos Pokemon League",
                                                 "Lylat Cruise"="Lylat Cruise",
                                                 "Magicant"="Magicant",
                                                 "Mario Circuit"="Mario Circuit",
                                                 "Mementos"="Mementos",
                                                 "Northern Cave"="Northern Cave",
                                                 "Pokemon Stadium"="Pokemon Stadium",
                                                 "Pokemon Stadium 2"="Pokemon Stadium 2",
                                                 "Skyloft"="Skyloft",
                                                 "Small Battlefield"="Small Battlefield",
                                                 "Smashville"="Smashville",
                                                 "Town & City"="Town & City",
                                                 "Unova Pokemon League"="Unova Pokemon League",
                                                 "WarioWare"="WarioWare",
                                                 "Yggdrasil's Alter"="Yggdrasil's Alter",
                                                 "Yoshi's Island"="Yoshi's Island",
                                                 "Yoshi's Story"="Yoshi's Story",
                                                 "Overall" = "All"))),
  div(style="display:inline-block",selectInput("player1", "Player1:",
                                               c("Banjo-Kazooie"="Banjo-Kazooie",
                                                 "Bayonetta"="Bayonetta",
                                                 "Bowser Jr."="Bowser Jr.",
                                                 "Bowser"="Bowser",
                                                 "Byleth"="Byleth",
                                                 "Captain Falcon"="Captain Falcon",
                                                 "Chrom"="Chrom",
                                                 "Cloud"="Cloud",
                                                 "Corrin"="Corrin",
                                                 "Daisy"="Daisy",
                                                 "Dark Pit"="Dark Pit",
                                                 "Dark Samus"="Dark Samus",
                                                 "Diddy Kong"="Diddy Kong",
                                                 "Donkey Kong"="Donkey Kong",
                                                 "Dr. Mario"="Dr. Mario",
                                                 "Duck Hunt"="Duck Hunt",
                                                 "Falco"="Falco",
                                                 "Fox"="Fox",
                                                 "Ganondorf"="Ganondorf",
                                                 "Greninja"="Greninja",
                                                 "Hero"="Hero",
                                                 "Ice Climbers"="Ice Climbers",
                                                 "Ike"="Ike",
                                                 "Incineroar"="Incineroar",
                                                 "Inkling"="Inkling",
                                                 "Isabelle"="Isabelle",
                                                 "Jigglypuff"="Jigglypuff",
                                                 "Joker"="Joker",
                                                 "Kazuya"="Kazuya",
                                                 "Ken"="Ken",
                                                 "King Dedede"="King Dedede",
                                                 "King K. Rool"="King K. Rool",
                                                 "Kirby"="Kirby",
                                                 "Link"="Link",
                                                 "Little Mac"="Little Mac",
                                                 "Lucario"="Lucario",
                                                 "Lucas"="Lucas",
                                                 "Lucina"="Lucina",
                                                 "Luigi"="Luigi",
                                                 "Mario"="Mario",
                                                 "Marth"="Marth",
                                                 "Megaman"="Megaman",
                                                 "Meta Knight"="Meta Knight",
                                                 "Mewtwo"="Mewtwo",
                                              #   "Mii Brawler"="Mii Brawler",
                                              #   "Mii Gunner"="Mii Gunner",
                                              #   "Mii Sword Fighter"="Mii Sword Fighter",
                                                 "Min Min"="Min Min",
                                                 "Mr. Game and Watch"="Mr. Game and Watch",
                                                 "Ness"="Ness",
                                                 "Olimar"="Olimar",
                                                 "Pacman"="Pacman",
                                                 "Palutena"="Palutena",
                                                 "Peach"="Peach",
                                                 "Pichu"="Pichu",
                                                 "Pikachu"="Pikachu",
                                                 "Piranha Plant"="Piranha Plant",
                                                 "Pit"="Pit",
                                                 "Pokemon Trainer"="Pokemon Trainer",
                                                 "Pyra"="Pyra",
                                               #  "Random"="Random",
                                                 "Richter"="Richter",
                                                 "Ridley"="Ridley",
                                                 "Rob"="Rob",
                                                 "Robin"="Robin",
                                                 "Rosalina"="Rosalina",
                                                 "Roy"="Roy",
                                                 "Ryu"="Ryu",
                                                 "Samus"="Samus",
                                                 "Sephiroth"="Sephiroth",
                                                 "Sheik"="Sheik",
                                                 "Shulk"="Shulk",
                                                 "Simon"="Simon",
                                                 "Snake"="Snake",
                                                 "Sonic"="Sonic",
                                                 "Sora"="Sora",
                                                 "Steve"="Steve",
                                                 "Terry"="Terry",
                                                 "Toon Link"="Toon Link",
                                                 "Villager"="Villager",
                                                 "Wario"="Wario",
                                                 "Wii Fit Trainer"="Wii Fit Trainer",
                                                 "Wolf"="Wolf",
                                                 "Yoshi"="Yoshi",
                                                 "Young Link"="Young Link",
                                                 "Zelda"="Zelda",
                                                 "Zero Suit Samus"="Zero Suit Samus"))),
  
  div(style="display:inline-block",selectInput("player2", "Player2:",
                                               c("Banjo-Kazooie"="Banjo-Kazooie",
                                                 "Bayonetta"="Bayonetta",
                                                 "Bowser Jr."="Bowser Jr.",
                                                 "Bowser"="Bowser",
                                                 "Byleth"="Byleth",
                                                 "Captain Falcon"="Captain Falcon",
                                                 "Chrom"="Chrom",
                                                 "Cloud"="Cloud",
                                                 "Corrin"="Corrin",
                                                 "Daisy"="Daisy",
                                                 "Dark Pit"="Dark Pit",
                                                 "Dark Samus"="Dark Samus",
                                                 "Diddy Kong"="Diddy Kong",
                                                 "Donkey Kong"="Donkey Kong",
                                                 "Dr. Mario"="Dr. Mario",
                                                 "Duck Hunt"="Duck Hunt",
                                                 "Falco"="Falco",
                                                 "Fox"="Fox",
                                                 "Ganondorf"="Ganondorf",
                                                 "Greninja"="Greninja",
                                                 "Hero"="Hero",
                                                 "Ice Climbers"="Ice Climbers",
                                                 "Ike"="Ike",
                                                 "Incineroar"="Incineroar",
                                                 "Inkling"="Inkling",
                                                 "Isabelle"="Isabelle",
                                                 "Jigglypuff"="Jigglypuff",
                                                 "Joker"="Joker",
                                                 "Kazuya"="Kazuya",
                                                 "Ken"="Ken",
                                                 "King Dedede"="King Dedede",
                                                 "King K. Rool"="King K. Rool",
                                                 "Kirby"="Kirby",
                                                 "Link"="Link",
                                                 "Little Mac"="Little Mac",
                                                 "Lucario"="Lucario",
                                                 "Lucas"="Lucas",
                                                 "Lucina"="Lucina",
                                                 "Luigi"="Luigi",
                                                 "Mario"="Mario",
                                                 "Marth"="Marth",
                                                 "Megaman"="Megaman",
                                                 "Meta Knight"="Meta Knight",
                                                 "Mewtwo"="Mewtwo",
                                               #  "Mii Brawler"="Mii Brawler",
                                              #   "Mii Gunner"="Mii Gunner",
                                              #   "Mii Sword Fighter"="Mii Sword Fighter",
                                                 "Min Min"="Min Min",
                                                 "Mr. Game and Watch"="Mr. Game and Watch",
                                                 "Ness"="Ness",
                                                 "Olimar"="Olimar",
                                                 "Pacman"="Pacman",
                                                 "Palutena"="Palutena",
                                                 "Peach"="Peach",
                                                 "Pichu"="Pichu",
                                                 "Pikachu"="Pikachu",
                                                 "Piranha Plant"="Piranha Plant",
                                                 "Pit"="Pit",
                                                 "Pokemon Trainer"="Pokemon Trainer",
                                                 "Pyra"="Pyra",
                                              #   "Random"="Random",
                                                 "Richter"="Richter",
                                                 "Ridley"="Ridley",
                                                 "Rob"="Rob",
                                                 "Robin"="Robin",
                                                 "Rosalina"="Rosalina",
                                                 "Roy"="Roy",
                                                 "Ryu"="Ryu",
                                                 "Samus"="Samus",
                                                 "Sephiroth"="Sephiroth",
                                                 "Sheik"="Sheik",
                                                 "Shulk"="Shulk",
                                                 "Simon"="Simon",
                                                 "Snake"="Snake",
                                                 "Sonic"="Sonic",
                                                 "Sora"="Sora",
                                                 "Steve"="Steve",
                                                 "Terry"="Terry",
                                                 "Toon Link"="Toon Link",
                                                 "Villager"="Villager",
                                                 "Wario"="Wario",
                                                 "Wii Fit Trainer"="Wii Fit Trainer",
                                                 "Wolf"="Wolf",
                                                 "Yoshi"="Yoshi",
                                                 "Young Link"="Young Link",
                                                 "Zelda"="Zelda",
                                                 "Zero Suit Samus"="Zero Suit Samus"))),
  #change this to Overall later
  DT::dataTableOutput("data"),
  plotOutput("plot") #,
 # print("Data is sourced from smash.gg's player database. Character icons from Tv_Toshi's Reddit post. This dashboard is not intended to be used as a definitive predictive probability of match ups. Many factors including player ranking and play styles were not accounted for.")
)

server <- function(input, output) {
  data_filter <- reactive({
    smash_data %>% filter(
      (Player1_categorical == input$player1 & Player2_categorical == input$player2) |
        (Player1_categorical == input$player2 & Player2_categorical == input$player1))
  })
  
  output$data <- 
    DT::renderDataTable(filter ='none',{
      datatable(
        if (!(input$player1 %in% data_filter()$Player1_categorical ))  {
          data_filter() %>%
            dplyr::mutate(Player1_new = Player2_categorical, 
                          Player2_new = Player1_categorical) %>%
            dplyr::mutate(P1_total_wins_new = total_games - P1_total_wins,
                          P1_total_loss_new = total_games - P1_total_loss) %>%
            dplyr::mutate(pcnt_win_new = paste(round(100 - 100*pcnt_win),"%"),
                          pcnt_loss_new = paste(round(100 - 100*pcnt_loss),"%")) %>%
            dplyr::select(stage_categorical,Player1_new, Player2_new, P1_total_wins_new, P1_total_loss_new, 
                          total_games,pcnt_loss_new,pcnt_win_new,
                          Player1_image,Player2_image) %>%
            dplyr::select(stage_categorical,
                          Player1_categorical = Player1_new,
                          Player2_categorical = Player2_new,
                          P1_total_wins = P1_total_wins_new,
                          P1_total_loss = P1_total_loss_new,
                          total_games,
                          pcnt_loss = pcnt_loss_new,
                          pcnt_win = pcnt_win_new
            ) %>%
            dplyr::filter(Player1_categorical == input$player1 
                          & Player2_categorical == input$player2) %>%
            #& stage_categorical == input$stage ) %>%
            dplyr::select("Stage" = stage_categorical, 
                          "Player 1" = Player1_categorical,
                          "Player 2" = Player2_categorical, 
                          "Player 1 Wins" = P1_total_wins, 
                          "Player 2 Losses" = P1_total_loss,
                          "Total Games Played" = total_games, 
                          "% Wins" = pcnt_win,
                          "% Loss" = pcnt_loss)
        } else {
          data_filter() %>% 
            dplyr::filter(Player1_categorical == input$player1 
                          & Player2_categorical == input$player2 ) %>%
            #& stage_categorical == input$stage ) %>%
            dplyr::mutate(pcnt_loss = paste(round(100*pcnt_loss,0),"%"),
                          pcnt_win = paste(round(100* pcnt_win,0),"%")) %>%
            dplyr::select("Stage" = stage_categorical, 
                          "Player 1" = Player1_categorical,
                          "Player 2" = Player2_categorical, 
                          "Player 1 Wins" = P1_total_wins, 
                          "Player 2 Losses" = P1_total_loss,
                          "Total Games Played" = total_games, 
                          "% Wins" = pcnt_win,
                          "% Loss" = pcnt_loss) 
          
        }, 
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          pageLength=25
        ),
        rownames = FALSE
      )
    })
  
  
  
  
  
  
  
  #### feels redundant could just create 1 reactive table that can be used for both but whatever 
  pie_filter <- reactive({    
    if (!(input$player1 %in% data_filter()$Player1_categorical ))  {
      data_filter() %>%
        dplyr::mutate(Player1_new = Player2_categorical, 
                      Player2_new = Player1_categorical) %>%
        dplyr::mutate(P1_total_wins_new = total_games - P1_total_wins,
                      P1_total_loss_new = total_games - P1_total_loss) %>%
        dplyr::mutate(pcnt_win_new = 1 - pcnt_win,
                      pcnt_loss_new = 1- pcnt_loss) %>%
        dplyr::select(stage_categorical,
                      Player1_categorical = Player1_new,
                      Player2_categorical = Player2_new,
                      P1_total_wins = P1_total_wins_new,
                      P1_total_loss = P1_total_loss_new,
                      total_games,
                      pcnt_loss = pcnt_loss_new,
                      pcnt_win = pcnt_win_new,
                      Player1_image,Player2_image) %>%
        #arrange(desc(Player1_categorical)) %>% #
        dplyr::filter(Player1_categorical == input$player1 
                      & Player2_categorical == input$player2  
                      & stage_categorical == input$stage)
    } else {
      data_filter() %>% 
        #arrange(desc(Player1_categorical)) %>% #
        dplyr::filter(Player1_categorical == input$player1
                      & Player2_categorical == input$player2
                      & stage_categorical == input$stage ) 
    }
  })
  
  
  character <- reactive({ 
    c(pie_filter()$Player1_categorical, 
      pie_filter()$Player2_categorical)
  })
  
  pie_win_pcnt <- reactive({
    c(as.double(pie_filter()$pcnt_win),
      as.double(pie_filter()$pcnt_loss))
  })
  
  #image <- c("chara_0_link_00.png", "chara_0_mario_00.png") #  
  
  image <- reactive({
    c(pie_filter()$Player1_image,
      pie_filter()$Player2_image)
  })
  
  pie_final <- reactive({
    data.frame(character = character(), pie_win_pcnt = pie_win_pcnt(), image = image())
  })
  
  
  
  
  output$plot <- renderPlot({
    pie_final() %>%
      arrange(desc(character)) %>%
      mutate(
        csum = 100 * cumsum(pie_win_pcnt),
        pos = .5 * (csum + lag(csum, default = 0))
      ) %>%
      ggplot(aes(x = "", y = 100*pie_win_pcnt, fill = character)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_col(width = 1, color = 1) + 
      theme_void() +
      geom_label_repel(
        aes(y = pos, label = paste0(round(100*pie_win_pcnt,0), "%")),
        size = 6, nudge_x = 1, show.legend = FALSE) +
      
      theme(legend.position = "none") +
      geom_image(aes(image=image, group = character),
                 #nudge_x = 1,
                 position = position_stack(.45), 
                 size = .15) +
      scale_fill_brewer(palette="Set1") 
  }, bg = "transparent")
  
}


shinyApp(ui,server)
