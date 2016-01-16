---
title: "NBA Stat in a Shiny App"
author: "Jun"
date: "January 13, 2016"
output: html_document
---

## Introduction  
[Shiny app](http://shiny.rstudio.com/) is a new way to present data interactively. Unlike methods like D3, shiny performs complex calculation in real time. As a result, shiny app is more powerful and versatile. However, this also means one cannot simply embed an interactive shiny app in an html document. It needs to be hosted on a shiny server. I rented one on [digital ocean](https://m.do.co/c/e695f62404f4), and here is my [shiny app](http://159.203.208.211/shiny/NBAshooting/) for NBA stat in [previous post](http://junma5.weebly.com/data-blog/the-value-of-a-player-added-to-team-offence-nba-shot-log-analysis). You may open that article and play with the app while reading this one. Note the code in this markdown file does not evaluate.

## app.R  
A shiny app has two main component: UI for app layout and server for computation and output. These two are linked by a shiny app object app.R shown below as an example. Libraries, data and functions are loaded first (make sure data is loaded globally). You can also load data locally in server.R. 

In the example below, I used navbar page layout with name 'NBA'. It has one tab 'Shooting Statistics'. It is also easy to included multiple tabs by adding `tabPanel()` inside `navbarPage`.

```{r, eval=FALSE}
library(shiny)
library(tools)
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)

rm(list = ls())
source('shotAnalysis.R')
source('loadData.R')
title_with_subtitle = function(title, subtitle = "") {
  ggtitle(bquote(atop(.(title), atop(.(subtitle)))))
}

# Define the UI
ui <- navbarPage('NBA', 
                 tabPanel('Shooting Statistics', source(file.path("UI", 'ui.R'), local = TRUE)$value)
                 # multiple tabs
                 # tabPanel('Panel Name', UI file)
  )
  
# Define the server code
server <- function(input, output, session) {
  source(file.path("Server", 'server.R'), local = TRUE)
  }

# Return a Shiny app object
shinyApp(ui = ui, server = server)
```

## Define UI  
I choose a sidebar layout comprised of a sidebar and a main panel. Also bear in mind that the layout may be different depending on the size of your browser. The side bar is mainly for user input. For example, a user can choose different NBA seasons and different players. Because player list is different for each season, the app will decide which list to display based on which season the user has chosen. To realize ths function, I use `uiOutput()` in which the selection panel is defined in server.R. In main panel, there are 3 tabs: 'FG%', 'Shot Selection' and 'Value'. Each has one `plotOutput()` except the second one has two. We can also specify the width and height for the plot. The default value is auto, which means it will resize according to app window.

```{r, eval=FALSE}
sidebarLayout( 
  # Define the sidebar with inputs
  sidebarPanel(
    selectInput("season", "Season:", 
                choices=c( '2015-2016', '2014-2015')),
    
    uiOutput("playerSelect"),
    
    downloadButton('downloadData', 'Download csv')
  ),
  # main panel for displaying results
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("FG%", 
                           fluidRow(
                             column(12, plotOutput("FGpt",width = 640, height = 480))
                           )),
                
                tabPanel("Shot Selection", 
                         fluidRow(
                             column(12, plotOutput("shotSel",width = 450, height = 400))
                           ),
                           fluidRow( 
                             column(12, plotOutput("shotSelDef",width = 450, height = 400))
                           )),
                tabPanel("Value", 
                         fluidRow(
                             column(12, plotOutput("value", width = 640, height = 480))
                           ))
    )
  )
)
```

Notice the first argument of each `plotOutput()` is the object we need to construct in server.R.

## Computation in server.R  
So now comes to the main contributor behind the scene, the server file. Basically, what needs to be updated is all in this file. Like I mentioned before, it is also responsible for one of the UI components `uiOutput()`, depending on the season selected, it will provide the corresponding player list for user to select from. The `reactive()` function will update every time a user performs a new action. In this case, it will update the shooting statistics data frame according to user selection.

```{r, eval=FALSE}
output$playerSelect <- renderUI({
  if(is.null(input$season)) {return(NULL) }
  else if (input$season == '2014-2015'){  
    player.info <- read.csv('data/2014-2015player.csv', header = T, stringsAsFactors = F)
  } else if (input$season == '2015-2016'){  
  player.info <- read.csv('data/2015-2016player.csv', header = T, stringsAsFactors = F)
  }
  
  name <- player.info %>%
    select(PLAYER_NAME) %>%
    arrange(PLAYER_NAME)
  
  selectInput("player", "Player", choices=c('League Average', name))
})

shot.pt <- reactive({
  if (input$player == 'League Average' & input$season == '2014-2015') {shot.pt <- league.1415}
  else if (input$player == 'League Average' & input$season == '2015-2016') {shot.pt <- league.1516}
  else if (input$player != 'League Average' & input$season == '2014-2015'){
    shot.pt <- shotAnalysis(filter(shot.1415, playerName == input$player))}
  else if (input$player != 'League Average' & input$season == '2015-2016'){
    shot.pt <- shotAnalysis(filter(shot.1516, playerName == input$player))
    }
})
```

The real output is rendered every time there is an update from user. `output$'name'` is the object being rendered here and displayed in UI. Note the class of the object shoule be consistant in UI and server. For example, we need to use `renderPlot()` in server and `plotOutput()` in UI for the same object.

The first plot shows the FG percentage at different location with different defender distance. The semi-transparent bars are for league average and the numbers at the top of the bar is "FG made/FG Attempt". The update action in this plot happens in `shot.pt <- shot.pt()`, where `shot.pt()` returns the `reactive()` output defined earlier.

```{r, eval=FALSE}
output$FGpt <- renderPlot({
  if(is.null(input$player)) {return(NULL) }
  
  shot.pt <- shot.pt()
  
  shot.plot <- ggplot(shot.pt, aes(x = ShotDist, y = `FG%`, fill = factor(DefDist))) + 
    geom_bar(stat = "identity",  position = position_dodge(width = .9), width = 0.6) + 
    ylab('FG%') + xlab('Shot Distance (ft)') + ylim(0, 1) +
    geom_text(aes(label = paste(totalFGM, '/', totalFGA, sep = '')), 
              position = position_dodge(width = .9), vjust = -0.5, size =3) +
    scale_x_discrete(limits=unique(shot.pt()$ShotDist))+
    theme_bw(base_size = 15) + scale_fill_discrete(name="Defender\nDistance (ft)")+
    theme(legend.position = c(0.65, 0.85), legend.background = element_blank(), legend.key = element_blank())
  
  if ( input$season == '2014-2015') {shot.plot <- shot.plot + 
    geom_bar(aes(x = league.1415$ShotDist, y = league.1415$`FG%`, fill = factor(DefDist)), 
             stat = "identity", position = position_dodge(width = .9), width = 0.9, alpha = 0.4, show.legend = FALSE) + 
    title_with_subtitle(paste(input$player, ' FG% '), paste('Season', input$season))}
  else if ( input$season == '2015-2016') {shot.plot <- shot.plot + 
    geom_bar(aes(x = league.1516$ShotDist, y = league.1516$`FG%`, fill = factor(DefDist)), 
             stat = "identity", position = position_dodge(width = .9), width = 0.9, alpha = 0.4, show.legend = FALSE) + 
    title_with_subtitle(paste(input$player, ' FG% '), paste('Season', input$season, ', as of 01/03/2016')) }
 
  shot.plot
})
```

In the second tab, we return two pie charts. `shotSel.dist` and `shotSel.def` are two dataframes consist of FGA stats. It is useful to see the percentage of different shot selections of a player and compare it with league average.

```{r, eval=FALSE}
output$shotSel <- renderPlot({
  if(is.null(input$player) )  {return(NULL) } else {}
  
  shot.pt <- shot.pt()
  shotSel.dist <<- shot.pt %>%
    group_by(ShotDist) %>%
    summarise(totalFGA = sum(totalFGA)) %>%
    slice(c(1, 8, 2:7)) %>%
    mutate(perc = totalFGA/sum(totalFGA), y.breaks = cumsum(perc) - perc/2)
    
  
  if ( input$season == '2014-2015') {sel <- ggplot() + 
    geom_bar(aes(x = factor(1), y = shotSel.dist.1415$perc, fill = shotSel.dist.1415$ShotDist ), width = 1.2, stat="identity", alpha = 0.6) + 
    scale_y_continuous(breaks = shotSel.dist.1415$y.breaks[-8], labels=percent(shotSel.dist.1415$perc)[-8] )
  }
  else if ( input$season == '2015-2016') {sel <- ggplot() + 
    geom_bar(aes(x = factor(1), y = shotSel.dist.1516$perc, fill = shotSel.dist.1516$ShotDist ), width = 1.2, stat="identity", alpha = 0.6) + 
    scale_y_continuous(breaks = shotSel.dist.1516$y.breaks[-8], labels=percent(shotSel.dist.1516$perc)[-8] )}
  
  
  sel <- sel  + geom_bar(aes(x = factor(1), y = shotSel.dist$perc, fill = shotSel.dist$ShotDist ), width = 1, stat="identity") +
    scale_fill_discrete(breaks=shotSel.dist$ShotDist, name="Shot \nDistance (ft)") +
    coord_polar(theta="y") + theme_bw(base_size = 15) + 
    geom_text(aes(x = factor(1), y=shotSel.dist[shotSel.dist$perc > 0.02,]$y.breaks, 
                  label=percent(shotSel.dist[shotSel.dist$perc > 0.02,]$perc)), size = 4)+
    title_with_subtitle(paste(input$player, 'Shot Selection '), paste('- Distance to Basket, Season', input$season)) +
    theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank(),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), legend.key = element_blank())
  
  sel
})

output$shotSelDef <- renderPlot({
  if(is.null(input$player) )  {return(NULL) } else {}
  
  shot.pt <- shot.pt()
  shotSel.def <<- shot.pt %>%
    group_by(DefDist) %>%
    summarise(totalFGA = sum(totalFGA)) %>%
    mutate(perc = totalFGA/sum(totalFGA), y.breaks = cumsum(perc) - perc/2)

  
  if ( input$season == '2014-2015') {def <- ggplot() + 
    geom_bar(aes(x = factor(1), y = shotSel.def.1415$perc, fill = shotSel.def.1415$DefDist ), width = 1.2, stat="identity", alpha = 0.6) + 
    scale_y_continuous(breaks = shotSel.def.1415$y.breaks, labels=percent(shotSel.def.1415$perc))
  }
  else if ( input$season == '2015-2016') {def <- ggplot() + 
    geom_bar(aes(x = factor(1), y = shotSel.def.1516$perc, fill = shotSel.def.1516$DefDist ), width = 1.2, stat="identity", alpha = 0.6) + 
    scale_y_continuous(breaks = shotSel.def.1516$y.breaks, labels=percent(shotSel.def.1516$perc))}
  
  def <- def + 
    scale_fill_discrete(breaks=shotSel.def$DefDist, name="Defender\nDistance (ft)") +
    coord_polar(theta="y") + theme_bw(base_size = 15) +
    geom_bar(aes(x = factor(1), y = shotSel.def$perc, fill = shotSel.def$DefDist ), width = 1, stat="identity") +
    geom_text(aes(x = factor(1), y=shotSel.def[shotSel.def$perc > 0.02,]$y.breaks, label=percent(shotSel.def[shotSel.def$perc > 0.02,]$perc)), size = 4)+
    title_with_subtitle(paste(input$player, 'Shot Selection '), paste('- Distance to Defender, Season', input$season)) +
    theme(axis.ticks=element_blank(), axis.title=element_blank(), 
          axis.text.y=element_blank(), panel.border = element_blank(),
          panel.grid = element_blank(), legend.key = element_blank())
  def
  
})
```

Finally, we can evaluate offensive performance of a player in the following plot. The analysis is in my [previous post](http://junma5.weebly.com/data-blog/the-value-of-a-player-added-to-team-offence-nba-shot-log-analysis). The table is pre-constructed and loaded in the app. You can select a play to see his performance highlited in red with player's name shown on the plot.

```{r, eval=FALSE}
output$value <- renderPlot({
  if(is.null(input$player) | input$player == 'League Average')  {return(value.plot.1516) } else {show.name <<- input$player}

  if ( input$player != 'League Average' & input$season == '2014-2015') {
    value.plot <- value.plot.1415 +
      geom_text(aes(value.1415[value.1415$name == show.name,]$FGA, value.1415[value.1415$name == show.name,]$PtsDiff), 
                label = value.1415[value.1415$name == show.name,]$name, size = 5,hjust=0.5, vjust=-0.9) + 
      geom_point(aes(value.1415[value.1415$name == show.name,]$FGA, value.1415[value.1415$name == show.name,]$PtsDiff), size = 4, color = 'red')
    }
  
  else if ( input$player != 'League Average' & input$season == '2015-2016') {
    value.plot <- value.plot.1516 +
      geom_text(aes(value.1516[value.1516$name == show.name,]$FGA, value.1516[value.1516$name == show.name,]$PtsDiff), 
                label = value.1516[value.1516$name == show.name,]$name, size = 5,hjust=0.5, vjust=-0.9) +
      geom_point(aes(value.1516[value.1516$name == show.name,]$FGA, value.1516[value.1516$name == show.name,]$PtsDiff), size = 4, color = 'red')
  }
  
  value.plot
})
```

## Conclusion
As you can see, it is quite easy to construct a shny app once you have the data. It is especially efficient if you need to show lots of similar data inside a large data set. In this scenario, shiny lets you construct one plot and you can simply change the input to ask shiny to update the plot. IMO, this is the biggest advantage of shiny to other interative visualization methods.
