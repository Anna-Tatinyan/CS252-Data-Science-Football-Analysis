library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(robustbase)
library(stringr)


football <- read.csv("football.csv", check.names = F)
football_df <- football %>%
    mutate(TeamLabel = 0) %>%
    select("League", "GroundPs", "Poss", "Clearance", "Offsides", "Sh", "Gls", "GA", "Season", 
           "Saves", "KPsThird", "Squad", "TeamLabel")


n <- 1

helper <- function(data, season, name) {
    for(i in 1:nrow(data)) {
        if(data[i,1] == name && data[i,9] == season) {
            data$TeamLabel[i] = n
            n = n+1
        }
    }
    n <- 1
    return(data)
}


football_df <- helper(football_df,"2017 - 2018", "Spain") %>%
    helper("2017 - 2018", "England") %>%
    helper("2017 - 2018", "Germany") %>%
    helper("2017 - 2018", "Italy") %>%
    helper("2018 - 2019", "Spain") %>%
    helper("2018 - 2019", "England") %>%
    helper("2018 - 2019", "Germany") %>%
    helper("2018 - 2019", "Italy") %>%
    helper("2019 - 2020", "Spain") %>%
    helper("2019 - 2020", "England") %>%
    helper("2019 - 2020", "Germany") %>%
    helper("2019 - 2020", "Italy")
    
football_df <- football_df[order(football_df$TeamLabel),]


ui <- dashboardPage( skin = "yellow",
    dashboardHeader(title ="Football analysis"),
    dashboardSidebar(
        sidebarMenu(color ="green",
            id = "sidebarid",
            menuItem("Introduction", tabName = "Introduction", icon = icon("Introduction")),
            conditionalPanel(
                'input.sidebarid == "dashboard"'
            ),
            menuItem("ScatterPlots", icon = icon("th"), tabName = "ScatterPlots", badgeColor = "green"),
            conditionalPanel(
                'input.sidebarid == "ScatterPlots"',
                selectInput(inputId = "leag1", label="Input for League name for Correlation between Ball Possesion and Goals Scored",
                            choices = football_df$League,
                            selected = "-"),
                selectInput(inputId = "leag2", label="Input for League name for Correlation between the number of Shots and Goals Scored",
                            choices = football_df$League,
                            selected = "-"),
                selectInput(inputId = "leag3", label="Input for League name for Correlation between the number of Clearances and Goals Allowed of the last three seasons",
                            choices = football_df$League,
                            selected = "-"),
                selectInput(inputId = "leag4", label="Input for League name for Correlation between the number of Saves and Goals Allowed of the last three seasons",
                            choices = football_df$League,
                            selected = "-")
            ),
            menuItem("Visual Plots", icon = icon("th"), tabName = "Visual_Plots", badgeColor = "green"),
            conditionalPanel(
                'input.sidebarid == "Visual_Plots"',
                selectInput(inputId = "season1", label="Input for Season for Offside Percent per League pie chart",
                            choices = football_df$Season,
                            selected = "-"),
                selectInput(inputId = "season2", label="Input for League name Distribution of Key Passes in last 30 minutes by Leagues",
                            choices = football_df$Season,
                            selected = "-")
            ),
            menuItem("Team Style Prediction", icon = icon("th"), tabName = "Team_Style_Prediction", badgeColor = "green"),
            conditionalPanel(
                'input.sidebarid == "Team_Style_Prediction"',
                selectInput(inputId = "season3", label="Input for Season for Team Style Prediction",
                            choices = football_df$Season,
                            selected = "-"),
                selectInput(inputId = "squad", label="Input for Team name for Team Style Prediction",
                            choices = football$Squad,
                            selected = "-")
            )
        )
    ),
               
    dashboardBody(
        tags$head(tags$style(HTML('
        /* tabBox background */
        .content-wrapper {
         background-image: url("https://i.pinimg.com/originals/4a/1f/d0/4a1fd04bcd5797a3c02e18a86d1b4b01.jpg?fbclid=IwAR3COdh2e_DW2uFkfLknuzHluVMRiSOIHasYOYC8Dl2Wuj1NcL5a8Iyn4pI");
         background-repeat: no-repeat;
         background-size: 100% 100%;
         height: auto;
        }
        .textbox {
            background-color: rgba(250, 250, 250, .7) ;
            padding: 20px;
            border-radius: 15px;
            font-size: 22px;
            margin : 15px;
        }
        div.texts{
        white-space: pre-wrap;
         text-align:center;
         margin:auto;
         padding-right: 15px;
         color: white;
        }    
                
        '
        ),)),
        tabItems(
            tabItem(tabName = "Introduction",
                    HTML(" <div class = 'textbox'>"),
                    p("Introduction",style='color:darkslategray;font-size:30px;'),
                    p("Our project is about the top four football league styles and their characteristics. The dataset that is used covers the last three seasons of games of La Liga(Spain), Premier League(England),  Serie A(Italy), and Bundesliga(Germany). The dataset contains 234 observation of 178 variables. By analyzing the data we created a program that determines the style of the particular club in a particular season based on the statistical calculations of the last three seasons. The program takes as an input one of the teams that played during 19/20, 18/19, and 17/18 seasons and determines the style of each of the teams, moreover below one can find additional information on styles mentioned and key style differences in each league.  In order to make things more evident, we created several graphs and statistical visualizations to show correlations between some of the variables (also to point out some significant differences in each league).
                    The project is done by students of the American University of Armenia Hovhannes Torosan, Rozi Tunyan, Anna Tatinyan, Hovsep Avagyan, and Knarik Manukyan."),
                    tags$a(href="https://fbref.com/en/", "The Data was taken from here!"),
                    HTML("</div>")
            ),
            tabItem(tabName = "ScatterPlots",
                    fluidRow(
                        splitLayout(cellArgs = list(style = "padding: 25px"),
                                    plotOutput("scatterplot1",  width = "95%"), 
                                    plotOutput("scatterplot2",  width = "95%"))
                    ),
                    br(),
                    fluidRow(
                        splitLayout(cellArgs = list(style = "padding: 25px"),
                            tags$div(class="texts", checked=NA,
                                     textOutput("scatterplot1t"),
                            ),
                            tags$div(class="texts", checked=NA,
                                     textOutput("scatterplot2t")  
                            )
                        )
                    ),
                    fluidRow(
                        splitLayout(cellArgs = list(style = "padding: 25px"),
                                    plotOutput("scatterplot3",  width = "95%",),
                                    plotOutput("scatterplot4",  width = "95%"))
                    ),
                    br(),
                    fluidRow(
                        splitLayout(cellArgs = list(style = "padding: 25px"),
                                tags$div(class="texts", checked=NA,
                                         textOutput("scatterplot3t"),
                                ),
                                tags$div(class="texts", checked=NA,
                                         textOutput("scatterplot4t")  
                                )
                        )
                    ),
            ),
            tabItem(tabName = "Visual_Plots",
                    fluidRow(
                        splitLayout(cellArgs = list(style = "padding: 25px"),
                                    plotOutput("piechart",  width = "95%"),
                                    plotOutput("dodge",  width = "95%"))
                    ),
                    br(),
                    fluidRow(
                        splitLayout(cellArgs = list(style = "padding: 25px"),
                                tags$div(class="texts", checked=NA,
                                         textOutput("piechartt"),
                                ),
                                tags$div(class="texts", checked=NA,
                                         textOutput("dodget")  
                                )
                        )
                    ),
                    fluidRow(
                        splitLayout(
                            cellArgs = list(style = "padding: 25px"),
                            plotOutput("stackplot",  width = "85%"), align = "center")
                    ),
                    fluidRow(
                        splitLayout(cellArgs = list(style = "padding: 25px"),
                                tags$div(class="texts", checked=NA,
                                         textOutput("stackplott")  
                                ))
                    )
            ),
            tabItem(tabName = "Team_Style_Prediction",
                    HTML(" <div class = 'textbox'>"),
                    h2("Team Style Prediction"),
                    br(),
                    p("In European football, it is accepted to distinguish the top 5 leagues, however in that list, The Ligue 1 of France is in the fifth position, so the top 4 of Europe are La Liga(Spain), Premier League(England), Bundesliga(Germany) and A Seria(Italy). From the aspect of tactics, styles, common strategies, these 4 leagues have some similarities; however, the differences overtake similarities and make each league somehow unique. The key differences of style, tactics, and common strategies of each league are listed below."),
                    HTML(" </div>"),
                    splitLayout(plotOutput("prediction",  width = "60%"),  align = "center"),
                    br(),
                    HTML(" <div class = 'textbox'>"),
                    h3("Premier League"),
                    br(),
                    p("In English Premier League, the coaches expect each of their players to be extremely fit and healthy. One of the reasons is that some of the teams in PL play more games than teams of other leagues on average. For Instance, in England, there are 3 domestic tournaments compared to 2 in the other 3 leagues. Those 3 are Premier League, FA Cup, and Carabao Cup. Rotation is another identifier of this league, as the starting XI is changing quite frequently. Being tactical in a PL match is difficult as the game is much faster and pressure is greater than in the other leagues. Physical abilities are seriously taken into consideration in this league. From the viewpoint of tactics, most of the mid- and low-table teams, that have tall players, are keen to create dangerous moments from set-pieces, e.g. crosses, corners, free kicks, etc.. "),
                    h3("La Liga"),
                    br(),
                    p("In the Spanish La Liga, coaches expect their players to be more talented and be more technical with the ball. The physical abilities of a player are not the most important factor here. One of the common tactics in La Liga is called Tiki Taka. While playing by this strategy, a team usually has a high possession percentage, a lot of ground passes, distance covered with no ball, etc. High crosses are not common for the majority of the teams as the strikers are mainly technical rather than physical. Based on statistics, La Liga has the highest portion of the time, when the ball is in the central part of the pitch(2/3 of the pitch from both sides)."),
                    h3("A Seria"),
                    br(),
                    p("Italian football has always differentiated by its defensive style and  Seria A is not an exception. This league has the lowest average goals scored per game ratio compared to the other 3 leagues. Coaches in A Seria pay very special attention to their team's defense. In the last decade, Seria A has the most number of clean sheets compared to the other 3 leagues. This means that keeping a clean sheet is a higher priority here than in other leagues. Moreover, Some coaches do not play with classic 4-3-3 or 4-2-3-1, they prefer 3-4-3 or tactics with 5 defenders which makes it very difficult for the opponents to score a goal."),
                    h3("Bundesliga"),
                    br(),
                    p("In contrast to the other three leagues, where there are 20 clubs in a league, Bundesliga has only 18 teams, which makes their calendar a little easier. Based on statistics, Bundesliga had the highest number of average goals scored per game for the last decade. Clean sheets are not very common here, as the teams mostly play an open game, which in result brings goals. Except for having open games, German Bundesliga does not have some unique identifiers, hence it is a mix of several styles. In contrast to PL and Seria A, crosses are not very common here, moreover, average dribbles per game are the second-highest compared to the three other leagues, after La Liga."),
                    HTML(" </div>"),
                    
            )
        )
    )
)

server <- function(input, output){
    output$scatterplot1 <- renderPlot({
        football_df[football_df$League == input$leag1,] %>%
            ggplot(aes(x=Poss, y=Gls)) + geom_point() + 
            geom_smooth(method = "lm", se=F, color = "red3",size=1.5) + 
            labs(title = paste("Correlation between Ball Possesion and Goals Scored"),
                 x = "Ball Possesion", y = "Goals Scored") +
            theme(panel.background = element_rect(fill = alpha("Yellow1",0.1))) 
    })

    output$scatterplot1t <- renderText({ 
        "In the graph above one can see the correlation of ball possession and goals scored. By choosing the league one can observe different positive correlations by looking at the trend line."
    })
    output$scatterplot2 <- renderPlot({
        football_df[football_df$League == input$leag2,] %>%
            ggplot(aes(x=Sh, y=Gls)) + geom_point() + 
            geom_smooth(method = "lm", se=F, color = "red3",size=1.5) +
            labs(title = paste("Correlation between the number of Shots and Goals Scored"),
                 x = "Number of Shots", y = "Goals Scored") +
            theme(panel.background = element_rect(fill = alpha("Yellow1",0.1)))
    })
    output$scatterplot2t <- renderText({ 
        "In the graph above one can see the correlation of the number of shots and goals scored. As in the previous plot, here also we observe positive strong correlation which, in fact, is quite intuitive."
            })
    output$scatterplot3 <- renderPlot({
        football_df[football_df$League == input$leag3,] %>%
            ggplot(aes(x=Clearance, y=GA)) + geom_point() + 
            geom_smooth(method = "lm", se=F, color = "red3",size=1.5) +
            labs(title = paste("Correlation between the number of Clearances and Goals Allowed"),
                 x = "Number of Clearances", y = "Goals Allowed") +
            theme(panel.background = element_rect(fill = alpha("Yellow1",0.1)))
    })
    output$scatterplot3t <- renderText({ 
        "In the graph above one can see the correlation of clearances and goals allowed. Clearance shows the number of times the ball was cleared from dangerous areas.  We see a positive correlation, which is not intuitive but can be explained as follows; The more one gets attacked the more there are chances to get a goal."
    })
    output$scatterplot4 <- renderPlot({
        football_df[football_df$League == input$leag4,] %>%
            ggplot(aes(x=Saves, y=GA)) + geom_point() + 
            geom_smooth(method = "lm", se=F, color = "red3",size=1.5) +
            labs(title = paste("Correlation between the number of Saves and Goals Allowed"),
                 x = "Number of Saves", y = "Goals Allowed")+
            theme(panel.background = element_rect(fill = alpha("Yellow1",0.1)))
    })
    output$scatterplot4t <- renderText({ 
        "In the graph above one can see the correlation of the number of saves and goals allowed. This plot is also not intuitive but the explanation lies as follows; a big number of saves means more shots on target and we know that shots on target and goals, intuitively must have a positive correlation."
    })
    output$dodge <- renderPlot({
        ggplot(data = football_df, aes(fill=League, x=Season, y = GroundPs)) +
            geom_bar(position="dodge", stat = "identity") + 
            scale_fill_brewer(palette = "Paired") +
            labs(title = paste("Number of Ground Passes in each League"),
                 x = "Season", y = "Ground Passes") +
            theme(panel.background = element_blank())
    })
    output$dodget <- renderText({ 
        "The following bar plot shows us the total number of ground passes in each league during the last three seasons. Obviously, for each season we have the same top 4 leagues."
    })
    output$piechart <- renderPlot({
        tempDf <- football_df[football_df$Season == input$season1,]
        tempDf %>%
            group_by(League) %>%
            summarise(perc = sum(Offsides)) %>%
            mutate(label = round(perc/sum(perc)*100)) %>%
            ggplot(aes(x="", y=perc, fill = League)) + geom_bar(stat = "identity") + 
            coord_polar("y") +
            theme_void() +
            geom_text(aes(x=1, y = cumsum(perc) - perc/4, label=paste0(label,"%")))+
            labs(title = "Offside Percent per League") +
            scale_fill_brewer(palette = "Paired") +
            theme(plot.title = element_text(hjust = 0.5),
                  panel.background = element_blank())
    })
    output$piechartt <- renderText({ 
        "In the piechart above one can observe the percentages of offsides of each of the leagues. By changing the season we can observe different values for our top 4 leagues."
    })
    output$stackplot <- renderPlot({
        level = c("Germany", "England", "Italy", "Spain")
        football_df[football_df$Season == input$season2,] %>%
            ggplot(aes(y=KPsThird, x = TeamLabel, fill = factor(League, level = level))) + 
            geom_area() +
            labs(title = paste("Distribution of Key Passes in the last 30 minutes by Leagues"),
                 x = "Teams", y = "Key Passes") +
            labs(fill = "Legend") +
            scale_y_continuous(breaks = seq(0,6000,200)) +
            scale_x_continuous(breaks = seq(0,20,1)) +
            scale_fill_brewer(palette = "Paired") +
            theme(panel.background = element_blank())
        
    })
    output$stackplott <- renderText({ 
        "The stack plot shows us the number of key passes made during the last thirty minutes of the match for each team in each league. As the number of teams is not important in this case we labeled each team from each league from 1 to 20 and only German Bundesliga has 18 teams that is why the stack plot shows us only the first 18 teams instead of 20.  In order to understand how much ground passes did the first team in each league make, it is enough to subtract from the value in the highest point of the league the value in the lowest point. So for example,  the first team in Spain made 800 - 0 = 800 ground passes, the first team in Italy made 2200-800 = 1400 ground passes, and so on."
    })
    output$prediction <- renderPlot({
        football <- football[,!grepl("90",names(football))]
        
        team_to_detect <- football %>%
            filter(Squad == input$squad & Season ==  input$season3) %>%
            mutate(CS = ifelse(is.na(CS), 0, CS)) %>%
            mutate(`CS%` = ifelse(is.na(`CS%`), 0, `CS%`))
        
        ttd <- as.numeric(team_to_detect)
        names(ttd) <- names(team_to_detect)
        ttd <- ttd[5:length(ttd)]
        ttd <- na.omit(ttd)
        
        spain <- football[football$League == 'Spain',]
        england <- football[football$League == 'England',]
        italy <- football[football$League == 'Italy',]
        germany <- football[football$League == 'Germany',]
        
        makeTableNumeric <- function(table) {
            for (i in 1:nrow(table)) {
                for (j in 1:ncol(table)) {
                    table[i,j] <- str_replace(table[i,j], "^\\d+[^0-9]+", "") 
                }
            }
            return(table)
        }
        
        getStyleQuartiles <- function(style) {
            style <- summary(style)
            style <- style[-c(1,3,4,6),-c(1:4)]
            style <- makeTableNumeric(style)
            style <- na.omit(style)
            return(style)
        }
        
        spain_quartiles <- getStyleQuartiles(spain)
        england_quartiles <- getStyleQuartiles(england)
        italy_quartiles <- getStyleQuartiles(italy)
        germany_quartiles <- getStyleQuartiles(germany)
        
        isTeamInStyleRange <- function(vect, table) {
            result <- c()
            for (j in 1:ncol(table)) {
                temp <- TRUE
                if (vect[j] < as.numeric(table[1,j])) {
                    temp <- FALSE
                }
                if (vect[j] > as.numeric(table[2,j])) {
                    temp <- FALSE
                }
                result <- c(result, temp)
            }
            return(result)
        }
        
        setStyleLabels <- function(styles) {
            result <- c()
            for (j in 1:ncol(styles)) {
                isInHomeLeague <- FALSE
                tempVec <- c()
                tempVal <- ""
                for (i in 1:nrow(styles)) {
                    if(styles[i,j] == TRUE) {
                        tempVec <- c(tempVec, rownames(styles)[i])
                        if (str_sub(rownames(styles)[i], 1,3) == str_sub(team_to_detect$League, 1,3)) {
                            isInHomeLeague <- TRUE
                            tempVal <- rownames(styles)[i]
                        }
                    }
                }
                if(isInHomeLeague) {
                    result <- c(result, tempVal)
                }
                else {
                    result <- c(result, tempVec)
                }
            }
            return(result)
        }
        
        determineTeamStyle <- function(team) {
            Spanish <- isTeamInStyleRange(team, spain_quartiles)
            English <- isTeamInStyleRange(team, england_quartiles)
            Italian <- isTeamInStyleRange(team, italy_quartiles)
            German <- isTeamInStyleRange(team, germany_quartiles)
            
            stylesMatrix <- rbind(Spanish, English, Italian, German)
            
            result <- setStyleLabels(stylesMatrix)
            
            return(result)
        }
        
        ttd_labels <- determineTeamStyle(ttd)
        ttd_labels_df <- as.data.frame(prop.table(table(ttd_labels)))
        
        ggplot(ttd_labels_df, aes(x = 2, y = Freq, fill = ttd_labels)) +
            geom_bar(stat = "identity", color = "white") +
            coord_polar(theta = "y", start = 0) +
            geom_text(aes(label = paste0(format(round(Freq * 100, 2), nsmall = 2), "%")),
                      position = position_stack(vjust = 0.5),
                      color = "white",
                      size = 5) +
            scale_fill_manual(values=c("red2", "green4", "orangered1", "gold1")) +
            theme_void() +
            xlim(0.5, 2.5) +
            labs(fill = "Style:",
                 x = NULL,
                 y = NULL,
                 title = paste0("Football styles detected for ",
                                team_to_detect$Squad,
                                " in season ",
                                team_to_detect$Season))
        
        
    })
    
}

shinyApp(ui, server)
