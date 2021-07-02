#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# See: https://rstudio.github.io/thematic/
thematic_shiny(font = 'auto')

# Define UI
ui <- fluidPage(
    
    theme = bslib::bs_theme(
        bg = 'cornsilk',
        fg = '#573320',
        primary = '#CC5500',
        base_font = font_google("Josefin Sans"),
        # heading_font = font_google("Josefin Sans"),
        font_scale = 1.2
    ),
    
    # Spinners
    use_waiter(),
    
    # CSS Styles
    includeCSS("styles.css"),

    fluidRow(
        column(
            7,
            # Application title
            h2("Kaggle Machine Learning and Data Science Survey (2020) Analysis"),
            tags$p(
                "This is a companion Shiny app to this ", 
                tags$a(href = "https://rnpython.netlify.app/post/2021-01-18-r-prevalence-in-india/",
                       'blog post',target="_blank"),
                "which analyzes the prevalence of R in India and compares it to Python.
        This Shiny app reproduces all the plots in that blog post to enable a similar analysis for the Top 10 countries."
            )
        ),
        column(
            4,
            offset = 1,
            fluidRow(class = "country-sel",
                selectInput(
                    "country_sel_dd",
                    shiny::HTML("<p><b><span style='color: #CC5500'>Select Country to Analyze</span></b></p>"),
                    choices = c(Choose='',top_10_countries)
                )
            ),
            # pickerInput(
            #     "country_sel_dd",
            #     "Select Country to Analyze:  ",
            #     choices = c(Choose='',top_10_countries),
            #     inline = T,
            #     width = 'fit'
            # )
        )
    ),
    tabsetPanel(
        tabPanel(
            "Part I: Global Context",
            fluidRow(class = 'content-margin',
                column(
                    10, 
                    offset = 1,
                    plotOutput("top_10_countries", height = "300px"),
                    plotOutput("top_10_python_n_r")
                )
            )
        ),
        tabPanel(
            "Part II: R and Python",
            uiOutput("panel2_country_sel_msg_ui"),
            fluidRow(class = 'content-margin',
                column(
                    10,
                    offset = 1,
                    plotOutput("global_comparison_pl"),
                    plotOutput("global_comparison_r_python"),
                    plotOutput("r_and_python"),
                    plotOutput("r_ba_da_activity_2018",height = 600),
                    plotOutput("r_ba_da_activity_2020", height = 700),
                    # plotOutput("trends_edu_background"),
                    # plotOutput("trends_edu_profile"),
                    plotOutput("trends_titles", height = 600),
                    # plotOutput("trends_titles_edu_background",height = 600),
                    plotOutput("comp_levels_python_r", height = 800),
                    plotOutput("comp_levels_ba_da_python_r", height = 800)
                )
            )
        ),
        tabPanel(
            "Part III: R Prevalence",
            fluidRow(class = 'content-margin',
                column(
                    10,
                    offset = 1,
                    uiOutput("panel3_country_sel_msg_ui"),
                    plotOutput("viz_lib",height = 500),
                    plotOutput("viz_lib_titles"),
                    plotOutput("ml_lib"),
                    plotOutput("ml_lib_titles"),
                    plotOutput("ml_algo", height = 500)
                    # plotOutput("company_n_team_size", height = 500),
                    # plotOutput("r_prog_activity", height = 500),
                    # plotOutput("r_num_activity", height = 500)
                )
            )
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    # Panel 1 Spinners
    w1<-Waiter$new(id = c("top_10_countries",
                          "top_10_python_n_r"),
                   color = 'orange',
                   html = spin_wave())
    
    # Panel 2 Spinners
    w2<- Waiter$new(id = c("global_comparison_pl",
                           "global_comparison_r_python",
                           "r_and_python",
                           "r_ba_da_activity_2018",
                           "r_ba_da_activity_2020",
                           # "trends_edu_background",
                           # "trends_edu_profile",
                           "trends_titles",
                           # "trends_titles_edu_background",
                           "comp_levels_python_r",
                           "comp_levels_ba_da_python_r"
    ),
                    color = 'orange',
                    html = spin_wave()
    )
    
    # Panel 3 Spinners
    w3<- Waiter$new(id = c("r_prog_activity",
                           "r_num_activity",
                           "company_n_team_size",
                           "viz_lib",
                           "viz_lib_titles",
                           "ml_lib",
                           "ml_algo"),
                    color = 'orange',
                    html = spin_wave()
    )

    # Message to prompt Country selection
    output$panel2_country_sel_msg_ui<-renderUI({
        if(input$country_sel_dd == "") {
            h4("Please select country to analyse")
        }        
    })
    
    # Message to prompt Country selection
    output$panel3_country_sel_msg_ui<-renderUI({
        if(input$country_sel_dd == "") {
            h4("Please select country to analyse")
        }        
    })
    
    # Country Data
    country_data<-reactive({
        req(input$country_sel_dd)
        
        data_2020_df<-mldss_df %>% filter(Q3 == input$country_sel_dd)
        data_2019_df<-mldss_2019_df %>% filter(Q3 == input$country_sel_dd)
        data_2018_df<-mldss_2018_df %>% filter(Q3 == input$country_sel_dd)
        
        return(list(
            data_2020 = data_2020_df,
            data_2019 = data_2019_df,
            data_2018 = data_2018_df
        ))
    })
    
    # Panel 1 : Plot 1
    top_10_countries_plot<-reactive({
        
        w1$show()
        
        top_10_df %T>%
            { t10_total_pct <<- sum(.['pct_resp']) } %>%
            mutate(Q3 = case_when(
                Q3 == "United Kingdom of Great Britain and Northern Ireland"~"UK",
                Q3 == "United States of America"~"USA",
                TRUE~Q3
            )) %>%
            ggplot(aes(x=reorder(Q3,-pct_resp),y=pct_resp)) +
            geom_col(fill="#F79862") +
            ylim(c(0,max(top_10_df$pct_resp) + 10)) +
            labs(title = "Top 10 Countries in terms of Percentage of Respondents",
                 subtitle = paste("They represent",
                                  paste0(t10_total_pct,"%"),
                                  "of respondents", sep = " "),
                 # tag = "Figure 1",
                 caption = "UK = United Kingdom of Great Britain and Northern Ireland") +
            theme(
                plot.margin=unit(c(1.5,0.5,1.0,0.5),"cm"),
                title = element_text(size = 20),
                axis.title.x=element_blank(),
                axis.text.x=element_text(angle=0,size=14),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                plot.caption = element_text(size = 14),
                plot.tag = element_text(size = 14),
                legend.position = "none",
                plot.subtitle=element_text(size=16, face="italic", color="dodgerblue")) +
            geom_text(aes(label=paste0(pct_resp,"%")), size = 6, vjust=-0.5)
    })
    
    output$top_10_countries<-renderPlot({
        req(top_10_countries_plot())
        
        top_10_countries_plot()
    })
    
    # Global Context Panel ####
    # Panel 1 : Plot 2
    top_10_python_n_r_plot<-reactive({
        
        w1$show()
        t10_country_levels<-top_10_countries %>%
            str_replace("United States of America","USA") %>%
            str_replace("United Kingdom of Great Britain and Northern Ireland","UK")
        
        mldss_df %>%
            filter(Q3 %in% top_10_countries) %>%
            select("Q3","Q7_Part_1","Q7_Part_2") %>%
            mutate(Q7_Part_1 = ifelse(Q7_Part_1 == "", "PythonNotSpecified", Q7_Part_1),
                   Q7_Part_2 = ifelse(Q7_Part_2 == "", "RNotSpecified", Q7_Part_2)) %>%
            pivot_longer(cols = c("Q7_Part_1","Q7_Part_2"),names_to = c("pl_question"),
                         values_to = c("PL")) %>%
            group_by(Q3,PL) %>%
            summarize(Count = n()) %>%
            pivot_wider(names_from = PL, values_from = Count) %>%
            rowwise() %>%
            mutate(Total = sum(c(PythonNotSpecified,Python)),
                   python_pct= round(100*Python/Total),
                   r_pct = round(100*R/Total),
                   Q3 = case_when(
                       Q3 == "United Kingdom of Great Britain and Northern Ireland"~"UK",
                       Q3 == "United States of America"~"USA",
                       TRUE~Q3
                   ),
                   Q3 = factor(Q3, levels = t10_country_levels)
            ) %>%
            select(Q3,python_pct,r_pct) %>%
            rename(Python = python_pct, R = r_pct) %>%
            pivot_longer(cols = c('Python','R'), names_to = "Language", values_to = "Percentage") %T>% {
                max_y<<-max(.["Percentage"])
            } %>% 
            ggplot(aes(x=Q3, y = Percentage,fill = Language)) + geom_col(position = 'dodge') +
            labs(title = "Percentage of Respondents programming in R and Python in Top 10 Countries",
                 # tag = "Figure 2",
                 caption = paste(
                     "UK = United Kingdom of Great Britain and Northern Ireland",
                     "",
                     "NOTE: Percentages for a country may add upto more than 100%,
                     since respondents program in multiple languages",
                     sep = '\n')
            ) +
            ylim(c(0,100)) +
            theme(
                plot.margin=unit(c(1.5,0.5,1.0,0.5),"cm"),
                title = element_text(size = 20),
                axis.title.x = element_blank(),
                axis.text.x=element_text(angle=0,size=14),
                axis.title.y = element_blank(),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                plot.tag = element_text(size = 14),
                plot.caption = element_text(size = 12),
                legend.position = 'bottom',
                legend.title = element_blank(),
                legend.text = element_text(size = 14)) +
            geom_text(aes(label=paste0(Percentage,"%")), 
                      position=position_dodge(width=0.9),
                      size = 5, vjust=-0.5) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC"))
    })
    
    output$top_10_python_n_r<-renderPlot({
        req(top_10_python_n_r_plot())
        
        top_10_python_n_r_plot()
        
    })
    
    ## Python and R Panel ####
    # Panel 2 : PLot 1
    global_comparison_pl_plot<-reactive({
        
        req(country_data())
        
        w2$show()
        
        pl_world<-mldss_df %>%
            select(contains("Q7_")) %>%
            pivot_longer(cols = names(.), names_to = "pl_question", values_to = "PL") %>%
            select(PL) %>%
            filter(!PL %in% c("","None")) %>%
            count(PL,name = "num_resp", sort = T) %>%
            mutate(Rank = seq(1,nrow(.)),
                   Percentage = round(100*num_resp/num_worldwide_respondents),
                   PL = as.factor(PL)
            ) %T>%
            { max_ww_pct<<- max(.["Percentage"]) } %>%
            ggplot(aes(x=reorder(PL,-Percentage),y=Percentage,
                       fill = ifelse(PL %in% c('R','Python'), 'ds_language','no_ds_language'))) +
            geom_col() +
            labs(
                # tag = "Figure 3a",
                title = "Global Programming Language Preferences",
                y = "Percentage of Respondents"
            ) +
            ylim(c(0,max_ww_pct + 3)) +
            theme(title = element_text(size = 16),
                  axis.title.x=element_blank(),
                  axis.title.y=element_text(size = 14),
                  axis.text.x=element_text(angle=45, hjust=1,size=12),
                  axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.tag = element_text(size = 12),
                  legend.position = "none") +
            geom_text(aes(label=paste0(Percentage,"%")), vjust=-0.5) +
            scale_fill_manual(values = c("#F79862","#E97451"))
        # scale_fill_manual(values = c("#8DA0CB","#B3B3B3"))
        
        sel_country<-ifelse(input$country_sel_dd == "United States of America","USA",
                             ifelse(input$country_sel_dd == "United Kingdom of Great Britain and Northern Ireland","UK & NI",
                                    input$country_sel_dd)
        )
        
        pl_country<-country_data()$data_2020 %T>%
            { 
                num_country_respondents <<- nrow(.)
            } %>%
            select(contains("Q7_")) %>%
            pivot_longer(cols = names(.), names_to = "pl_question", values_to = "PL") %>%
            select(PL) %>%
            filter(!PL %in% c("","None")) %>%
            count(PL,name = "num_resp", sort = T) %>%
            mutate(Rank = seq(1,nrow(.)),
                   Percentage = round(100*num_resp/num_country_respondents),
                   PL = as.factor(PL)
            ) %T>%
            { max_in_pct<<- max(.["Percentage"]) } %>%
            ggplot(aes(x=reorder(PL,-Percentage),y=Percentage,
                       fill = ifelse(PL %in% c('R','Python'), 'ds_language','no_ds_language'))) +
            geom_col() +
            labs(
                # tag = "Figure 3b",
                y = "Percentage of Respondents",
                title = paste(sel_country,"Programming Language Preferences")) +
            ylim(c(0,max_in_pct + 3)) +
            theme(title = element_text(size = 16),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1,size=12),
                  axis.title.y=element_text(size = 14),
                  axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.tag = element_text(size = 12),
                  legend.position = "none") +
            geom_text(aes(label=paste0(Percentage,"%")), vjust=-0.5) +
            scale_fill_manual(values = c("#F79862","#E97451"))
            # scale_fill_manual(values = c("#8DA0CB","#B3B3B3"))
        
        pl_world  + pl_country
    })
 
    output$global_comparison_pl<-renderPlot({
        req(global_comparison_pl_plot())
        
        global_comparison_pl_plot()
    })
    
    # Panel 2: Plot 2
    global_comparison_r_python_plot<-reactive({
        req(country_data())
        
        w2$show()
        
        sel_country<-ifelse(input$country_sel_dd == "United States of America","USA",
                             ifelse(input$country_sel_dd == "United Kingdom of Great Britain and Northern Ireland","UK & NI",
                                    input$country_sel_dd)
        )
        
        p1<-mldss_df %>%
            select(contains("Q7_")) %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            count(python_r_choice, name = "resp_counts") %>%
            filter(python_r_choice != 'No') %>%
            mutate(resp_pct = 100*round(resp_counts/nrow(mldss_df),2),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R"))) %T>%
            { max_global_y <<- max(.["resp_pct"])} %>%
            ggplot(aes(x=python_r_choice,y=resp_pct,
                       fill = python_r_choice)) +
            geom_col() +
            ylim(c(0,max_global_y + 10)) +
            labs(
                # tag = "Figure 4a",
                y="Percentage of Respondents", x = "Language",
                 title = "Global Preferences : R and Python"
            ) +
            theme(title = element_text(size = 16),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.tag = element_text(size = 12),
                  legend.title = element_blank(),
                  legend.position = 'bottom',
                  legend.text = element_text(size = 12)) +
            geom_text(aes(label=paste(resp_pct,"%",sep="")), vjust = -0.5, size = 6) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC","#966FD6"))  # Pastel Red, Blue and Purple
        
        p2<-country_data()$data_2020 %>%
            select(contains("Q7_")) %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            count(python_r_choice, name = "resp_counts") %>%
            filter(python_r_choice != 'No') %>%
            mutate(resp_pct = 100*round(resp_counts/nrow(country_data()$data_2020),2),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R"))) %T>%
            { max_country_y <<- max(.["resp_pct"])} %>%
            ggplot(aes(x=python_r_choice,y=resp_pct,
                       fill = python_r_choice)) +
            geom_col() +
            ylim(c(0,max_country_y + 10)) +
            labs(
                # tag = "Figure 4b",
                y="Percentage of Respondents", x = "Language",
                title = paste(sel_country, " Preferences : R and Python", sep = " ")
            ) +
            theme(title = element_text(size = 16),
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(size = 14),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.tag = element_text(size = 12),
                  legend.title = element_blank(),
                  legend.position = 'bottom',
                  legend.text = element_text(size = 12)) +
            geom_text(aes(label=paste(resp_pct,"%",sep="")), vjust = -0.5, size = 6) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC","#966FD6"))  # Pastel Red, Blue and Purple
        
        p1 + p2
    })
    
    
    output$global_comparison_r_python<-renderPlot({
        req(global_comparison_r_python_plot())
        
        global_comparison_r_python_plot()
    })

    # Panel 2: Plot 3
    r_and_python_plot<-reactive({
        req(country_data())
        
        w2$show()
        
        p2020<-country_data()$data_2020 %>%
            select(contains("Q7_")) %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            count(python_r_choice, name = "resp_counts") %>%
            filter(python_r_choice != 'No') %>%
            mutate(resp_pct = 100*round(resp_counts/nrow(country_data()$data_2020),3),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R"))) %>%
            mutate(Year = "2020")
        
        p2019<-country_data()$data_2019 %>%
            select(contains("Q18_")) %>%
            mutate(python_r_choice =
                       ifelse(Q18_Part_1 == "Python" & Q18_Part_2 == "R", "Python + R",
                              ifelse(Q18_Part_1 == "Python" & Q18_Part_2 == "", "Python",
                                     ifelse(Q18_Part_1 == "" & Q18_Part_2 == "R", "R","No")))) %>%
            count(python_r_choice, name = "resp_counts") %>%
            filter(python_r_choice != 'No') %>%
            mutate(resp_pct = 100*round(resp_counts/nrow(country_data()$data_2019),3),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R"))) %>%
            mutate(Year = "2019")
        
        p2018<-country_data()$data_2018 %>%
            select(contains("Q16_")) %>%
            mutate(python_r_choice =
                       ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "R", "Python + R",
                              ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "", "Python",
                                     ifelse(Q16_Part_1 == "" & Q16_Part_2 == "R", "R","No")))) %>%
            count(python_r_choice, name = "resp_counts") %>%
            filter(python_r_choice != 'No') %>%
            mutate(resp_pct = 100*round(resp_counts/nrow(country_data()$data_2018),3),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R"))) %>%
            mutate(Year = "2018")
        
        bind_rows(p2020,p2019,p2018) %T>% 
            { max_pct_y <<- max(.["resp_pct"])} %>%
            filter(python_r_choice != 'No') %>%
            ggplot(aes(x=python_r_choice,y=resp_pct, fill = python_r_choice)) +
            geom_col() +
            ylim(c(0,max_pct_y + 5)) +
            labs(
                # tag = "Figure 5",
                y="Number of Respondents",
                title = "R and Python over the years",
                caption = paste("Number of Respondents",
                                 paste0("Year 2020: ",nrow(country_data()$data_2020)),
                                 paste0("Year 2019: ",nrow(country_data()$data_2019)),
                                 paste0("Year 2018: ",nrow(country_data()$data_2018)),
                                 sep = "\n")) +
            facet_wrap(~Year) +
            theme(title = element_text(size = 16),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(size = 12),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.tag = element_text(size = 12),
                  strip.text = element_text(face="bold", size=16,
                                            margin = margin(10,0,10,0)),
                  strip.background = element_rect(fill="bisque"),
                  legend.position = "bottom",
                  legend.text = element_text(size = 12),
                  legend.title = element_blank()) +
            geom_text(aes(label=paste0(resp_counts," (",resp_pct,"%)")), size = 5, vjust=-1) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC","#966FD6"))  # Pastel Red, Blue and Purple
        
    })
    
    output$r_and_python<-renderPlot({
        req(r_and_python_plot())
        
        r_and_python_plot()
    })
    
    
    # Panel 2: Plot 4
    r_ba_da_activity_2018_plot<-reactive({
        req(country_data())
        
        w2$show()
        
        country_data()$data_2018 %>% 
            select(Q6,contains("Q11_"),"Q16_Part_1","Q16_Part_2") %>%
            select(-Q11_OTHER_TEXT) %>%
            mutate(python_r_choice =
                       ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "R", "Python + R",
                              ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "", "Python",
                                     ifelse(Q16_Part_1 == "" & Q16_Part_2 == "R", "R","No")))) %>%
            select(-contains("Q16_")) %>%
            pivot_longer(cols = -c('Q6','python_r_choice'), names_to = 'activity_ques', values_to = "activity") %>%
            filter(activity != "", Q6 %in% c("Business Analyst","Data Analyst", "Data Scientist"),
                   python_r_choice != "No") %>%
            count(Q6,python_r_choice,activity, name = "resp_count") %T>%
            {max_y <<- max(.["resp_count"])} %>%
            mutate(Q6 = factor(Q6, levels = c("Business Analyst","Data Analyst", "Data Scientist")),
                   python_r_choice = factor(python_r_choice, levels = c("Python","R","Python + R"))) %>%
            ggplot(aes(x=Q6,y=resp_count,fill = activity)) +
            geom_col(position='dodge') +
            ylim(c(0,max_y + 10)) +
            theme(
                # plot.margin=unit(c(1.0,1.0,1.0,1.0),"cm"),
                plot.background = element_rect(fill = "cornsilk"),
                panel.background = element_rect(fill = 'cornsilk'),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                title = element_text(size = 16),
                axis.title.x=element_blank(),
                axis.text.x=element_text(size = 12,vjust = -2),
                axis.title.y=element_text(size = 14),
                axis.text.y=element_blank(),
                axis.ticks = element_blank(),
                legend.position = "bottom",
                legend.background = element_rect(fill = 'cornsilk'),
                legend.title = element_blank(),
                strip.text = element_text(size=16,
                                          face="bold",
                                          angle = 270,
                                          margin = margin(0,5,0,5)
                                          ),
                strip.background = element_rect(fill="bisque")) +
            facet_wrap( ~ python_r_choice, ncol = 1, scales = "free_y",strip.position = 'right') +
            guides(fill = guide_legend(ncol = 1,
                                       label.theme = element_text(size = 12))) +
            labs(y = "Number of Respondents", title = "Activities performed by respondents using R in 2018",
                 subtitle = "Titles represented: Business Analyst, Data Analyst","Data Scientist") +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9), size = 4, vjust=-0.5) +
            scale_fill_brewer(palette = 'Set2')
    })
    
    output$r_ba_da_activity_2018 <-renderPlot({
        req(r_ba_da_activity_2018_plot())

        r_ba_da_activity_2018_plot()
    })
    
    # Panel 2: Plot 5
    r_ba_da_activity_2020_plot<-reactive({
        req(country_data())
        
        w2$show()
        
        country_data()$data_2020 %>% 
            select(Q5,contains("Q23_"),"Q7_Part_1","Q7_Part_2") %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(-contains("Q7_")) %>%
            pivot_longer(cols = -c('Q5','python_r_choice'), names_to = 'activity_ques', values_to = "activity") %>%
            filter(!activity %in% c("","Experimentation and iteration to improve existing ML models"),
                   Q5 %in% c("Business Analyst","Data Analyst", "Data Scientist"),
                   python_r_choice != "No") %>%
            count(Q5,python_r_choice,activity, name = "resp_count") %T>%
            {max_y <<- max(.["resp_count"])} %>%
            mutate(Q5 = factor(Q5, levels = c("Business Analyst","Data Analyst", "Data Scientist")),
                   python_r_choice = factor(python_r_choice, levels = c("Python","R","Python + R"))) %>%
            ggplot(aes(x=Q5,y=resp_count,fill = activity)) +
            geom_col(position='dodge') +
            ylim(c(0,max_y + 10)) +
            theme(
                # plot.margin=unit(c(1.0,1.0,1.0,1.0),"cm"),
                plot.background = element_rect(fill = "cornsilk"),
                panel.background = element_rect(fill = 'cornsilk'),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                title = element_text(size = 16),
                axis.title.x=element_blank(),
                axis.text.x=element_text(size = 12,vjust = -2),
                axis.title.y=element_text(size = 14),
                axis.text.y=element_blank(),
                axis.ticks = element_blank(),
                legend.position = "bottom",
                legend.background = element_rect(fill = 'cornsilk'),
                legend.title = element_blank(),
                strip.text = element_text(size=16,
                                          face="bold",
                                          angle = 270,
                                          margin = margin(0,5,0,5)
                ),
                strip.background = element_rect(fill="bisque")) +
            facet_wrap( ~ python_r_choice, ncol = 1, scales = "free_y",strip.position = 'right') +
            guides(fill = guide_legend(ncol = 1,
                                       label.theme = element_text(size = 12))) +
            labs(y = "Number of Respondents", title = "Activities performed by respondents using R in 2020",
                 subtitle = "Titles represented: Business Analyst, Data Analyst","Data Scientist") +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9), size = 4, vjust=-0.5) +
            scale_fill_brewer(palette = 'Set2')
    })
    
    output$r_ba_da_activity_2020 <-renderPlot({
        req(r_ba_da_activity_2020_plot())
        
        r_ba_da_activity_2020_plot()
    })
    
    # Not used currently
    trends_edu_background_plot<-reactive({
        req(country_data())
        
        w2$show()
        
        python_r_edu_summary<-country_data()$data_2020 %>%
            select("Q4","Q7_Part_1","Q7_Part_2") %>%
            # Replace the 'odd' apostrophe with the regular one
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            count(Q4,python_r_choice,name = "resp_count") %>%
            mutate(Year = "2020")
        
        python_r_edu_2019_summary<-country_data()$data_2019 %>%
            select("Q4","Q18_Part_1","Q18_Part_2") %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            mutate(python_r_choice =
                       ifelse(Q18_Part_1 == "Python" & Q18_Part_2 == "R", "Python + R",
                              ifelse(Q18_Part_1 == "Python" & Q18_Part_2 == "", "Python",
                                     ifelse(Q18_Part_1 == "" & Q18_Part_2 == "R", "R","No")))) %>%
            count(Q4,python_r_choice, name = "resp_count") %>%
            mutate(Year = "2019")
        
        python_r_edu_2018_summary<-country_data()$data_2018 %>%
            select("Q4","Q16_Part_1","Q16_Part_2") %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            mutate(python_r_choice =
                       ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "R", "Python + R",
                              ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "", "Python",
                                     ifelse(Q16_Part_1 == "" & Q16_Part_2 == "R", "R","No")))) %>%
            count(Q4,python_r_choice, name = "resp_count") %>%
            mutate(Year = "2018")
        
        ignored_responses<-c("I prefer not to answer",
                             "No formal education past high school",
                             "Some college/university study without earning a bachelor's degree")
        
        combined_edu_df<-bind_rows(
            python_r_edu_summary,
            python_r_edu_2019_summary,
            python_r_edu_2018_summary
        ) %>%
            filter(!Q4 %in% ignored_responses,
                   python_r_choice != 'No') %>%
            mutate(
                Q4 = factor(Q4,levels = c("Bachelor's degree","Master's degree",
                                          "Doctoral degree","Professional degree"),
                            ),
                python_r_choice = factor(python_r_choice,
                                         levels = c("Python","R","Python + R"))
            )
        
        # max_y <- max(combined_edu_df$Python)
        
        p<-combined_edu_df %T>%
            { max_y <<- max(.$resp_count) } %>%
            ggplot(aes(x=Q4,y=resp_count,fill = python_r_choice)) +
            geom_col(position='dodge') +
            facet_wrap(~Year,nrow = 3, strip.position = "right") +
            ylim(c(0,max_y + 2)) +
            theme(title = element_text(size=16),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(size = 14),
                  axis.title.y=element_text(size = 14),
                  axis.text.y=element_blank(),
                  plot.tag = element_text(size = 14),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  strip.text = element_text(face="bold", angle = 270, size=18,
                                            margin = margin(0,10,0,10)),
                  strip.background = element_rect(fill="bisque")) +
            labs(
                # tag = "Figure 6",
                y = "Number of Respondents", 
                title = "Trends: Distribution of Educational Background of Respondents from 2018-2020"
            ) +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9), size = 5, vjust=-0.5) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC","#966FD6"))  # Pastel Red, Blue and Purple
        
        p
    })
    
    output$trends_edu_background<-renderPlot({
        req(trends_edu_background_plot())
        
        trends_edu_background_plot()
    })
    
    # Not used currently
    trends_edu_profile_plot<-reactive({
        req(country_data())
        
        w2$show()
        edu_summary<-country_data()$data_2020 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            count(Q4, name = "resp_count") %>%
            filter(Q4 != "") %>%
            mutate(Year = "2020")
        
        edu_2019_summary<-country_data()$data_2019 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            count(Q4, name = "resp_count") %>%
            filter(Q4 != "") %>%
            mutate(Year = "2019")
        
        edu_2018_summary<-country_data()$data_2018 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            count(Q4, name = "resp_count") %>%
            filter(Q4 != "") %>%
            mutate(Year = "2018")
        
        ignored_responses<-c("I prefer not to answer",
                             "No formal education past high school",
                             "Some college/university study without earning a bachelor's degree")
        
        combined_edu_summary_df<-bind_rows(
            edu_summary,
            edu_2019_summary,
            edu_2018_summary
        ) %>%
            filter(!Q4 %in% ignored_responses) %>%
            mutate(Q4 = factor(Q4,levels = c("Bachelor's degree","Master's degree",
                                             "Doctoral degree","Professional degree")))
        
        max_y <- max(combined_edu_summary_df$resp_count)
        
        combined_edu_summary_df %>%
            filter(!Q4 %in% ignored_responses) %>% 
            ggplot(aes(x=Q4,y=resp_count,fill = Q4)) +
            geom_col(position='dodge') +
            facet_wrap(~Year,nrow = 3, strip.position = "right") + 
            ylim(c(0,max_y + 1000)) +
            theme(title = element_text(size=16),
                  axis.title.x=element_blank(),
                  axis.title.y=element_text(size = 12),
                  axis.text.x=element_text(size = 12),
                  axis.text.y=element_blank(),
                  plot.tag = element_text(size = 14),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14),
                  strip.text = element_text(face="bold", angle = 270, size=18,
                                            margin = margin(0,10,0,10)),
                  strip.background = element_rect(fill="bisque")) +
            labs(
                # tag = "Figure 7",
                y = "Number of Respondents",
                title = "Trends: Educational Background of all Respondents"
            ) +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9), size = 6, vjust=-0.5) +
            scale_fill_manual(values = c("#66C2A5","#FC8D62","#A6D854","#B3B3B3"))
    })
    
    output$trends_edu_profile<-renderPlot({
        req(trends_edu_profile_plot())
        trends_edu_profile_plot()
    })
    
    # Panel 2: Plot 6
    trends_titles_plot<-reactive({
        req(country_data())
        
        w2$show()
        
        python_r_title_summary<-country_data()$data_2020 %>%
            select("Q5","Q7_Part_1","Q7_Part_2") %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            count(Q5,python_r_choice, name = "resp_count") %>%
            mutate(Year = "2020",
                   Q5 = replace(Q5, Q5 == "Machine Learning Engineer", "ML Engineer"),
                   Q5 = replace(Q5, Q5 == "Currently not employed", "Not employed"))
        
        python_r_title_2019_summary<-country_data()$data_2019 %>%
            select("Q5","Q18_Part_1","Q18_Part_2") %>%
            mutate(python_r_choice =
                       ifelse(Q18_Part_1 == "Python" & Q18_Part_2 == "R", "Python + R",
                              ifelse(Q18_Part_1 == "Python" & Q18_Part_2 == "", "Python",
                                     ifelse(Q18_Part_1 == "" & Q18_Part_2 == "R", "R","No")))) %>%
            count(Q5,python_r_choice, name = "resp_count") %>%
            mutate(Year = "2019")
        
        python_r_title_2018_summary<-country_data()$data_2018 %>%
            select("Q6","Q16_Part_1","Q16_Part_2") %>%
            rename(Q5 = Q6) %>%
            mutate(python_r_choice =
                       ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "R", "Python + R",
                              ifelse(Q16_Part_1 == "Python" & Q16_Part_2 == "", "Python",
                                     ifelse(Q16_Part_1 == "" & Q16_Part_2 == "R", "R","No")))) %>%
            count(Q5,python_r_choice, name = "resp_count") %>%
            mutate(Year = "2018")
        
        title_levels_combined<-c("Business Analyst","Data Analyst", "Statistician","Research Scientist",
                                 "Data Scientist","DBA/Database Engineer","Data Engineer",
                                 "Product/Project Manager","Software Engineer","Student",
                                 "ML Engineer")
        
        combined_title_df<-bind_rows(
            python_r_title_summary,
            python_r_title_2019_summary,
            python_r_title_2018_summary
        ) %>% 
            filter(Q5 %in% title_levels_combined,
                   python_r_choice != 'No') %>%
            mutate(Q5 = factor(Q5, levels = title_levels_combined),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R")))
        
        combined_title_df %>%
            ggplot(aes(x=Q5,y=resp_count,fill = python_r_choice)) + geom_col(position='dodge') +
            facet_wrap(~Year,nrow = 3, strip.position = "right") +
            ylim(c(0,max_y + 50)) +
            theme(title = element_text(size = 16),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1,size = 12),
                  axis.title.y=element_text(size = 14),
                  axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.tag = element_text(size = 14),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  strip.text = element_text(face="bold", angle = 270, size=16,
                                            margin = margin(0,5,0,5)),
                  strip.background = element_rect(fill="bisque")) +
            labs(
                # tag = "Figure 8",
                y = "Number of Respondents",
                title = "Trends: Distribution of Respondents' Job Titles from 2018-2020",
            ) +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9), size = 3, vjust=-0.5) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC","#966FD6"))  # Pastel Red, Blue and Purple
        
    })
    
    
    output$trends_titles<-renderPlot({
        req(trends_titles_plot())
        
        trends_titles_plot()
    })
    
    trends_titles_edu_background_plot<-reactive({
        req(country_data())
        
        w1$show()
        
        r_edu_summary<-country_data()$data_2020 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            filter(Q7_Part_1 == "",Q7_Part_2 == "R") %>%
            filter(Q4 %in% c("Bachelor's degree","Master's degree","Doctoral degree","Professional degree")) %>%
            filter(Q5 %in% c("Business Analyst","Data Analyst")) %>%
            count(Q4,Q5, name = "resp_count") %>%
            mutate(Year = "2020")
        
        r_edu_2019_summary<-country_data()$data_2019 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            filter(Q18_Part_1 == "",Q18_Part_2 == "R") %>%
            filter(Q4 %in% c("Bachelor's degree","Master's degree","Doctoral degree","Professional degree")) %>%
            filter(Q5 %in% c("Business Analyst","Data Analyst")) %>%
            count(Q4,Q5, name = "resp_count") %>%
            mutate(Year = "2019")
        
        r_edu_2018_summary<-country_data()$data_2018 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            filter(Q16_Part_1 == "",Q16_Part_2 == "R") %>%
            filter(Q4 %in% c("Bachelor's degree","Master's degree","Doctoral degree","Professional degree")) %>%
            filter(Q6 %in% c("Business Analyst","Data Analyst")) %>%
            count(Q4,Q6, name = "resp_count") %>%
            rename(Q5 = Q6) %>% 
            mutate(Year = "2018")
        
        combined_edu_df<-bind_rows(
            r_edu_summary,
            r_edu_2019_summary,
            r_edu_2018_summary
        ) %>% 
            data.frame() %>% 
            complete(expand(.,Q4,Q5,Year), fill = list(resp_count = 0))

        
        max_r_count<-max(combined_edu_df$resp_count)
        
        r_plot<-combined_edu_df %>%
            select(Q4,Q5,Year,resp_count) %>%
            ggplot(aes(x=Q4,y=resp_count,fill=Q5)) +
            geom_col(position = "dodge") +
            ylim(c(0,max_r_count + 5)) +
            facet_wrap(~Year,nrow = 3, strip.position = "left") +
            theme(title = element_text(size = 14),
                  axis.text.x=element_text(angle=45, hjust=1, size = 14),
                  axis.title.x=element_blank(),
                  # axis.title.y=element_text(size = 14),
                  axis.text.y=element_blank(),
                  plot.tag = element_text(size = 12),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  strip.text = element_text(face="bold", size=16, 
                                            angle = 90,
                                            margin = margin(0,5,0,5)),
                  strip.background = element_rect(fill="bisque")) +
            labs(
                # tag = "Figure 9a",
                x = "Educational Background",y = "Number of Respondents",
                title = "R Business & Data Analyst Respondents",
                subtitle = "Distribution of Respondents by Educational Background",
                fill = "Respondent Title",
            ) +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9), size = 4, vjust=-0.5) +
            scale_fill_manual(values = c("#FFD92F","#E5C494"))
        
        python_edu_summary<-country_data()$data_2020 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            filter(Q7_Part_1 == "Python",Q7_Part_2 == "") %>%
            filter(Q4 %in% c("Bachelor's degree","Master's degree","Doctoral degree","Professional degree")) %>%
            filter(Q5 %in% c("Business Analyst","Data Analyst")) %>%
            count(Q4,Q5, name = "resp_count") %>%
            mutate(Year = "2020")
        
        python_edu_2019_summary<-country_data()$data_2019 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            filter(Q18_Part_1 == "Python",Q18_Part_2 == "") %>%
            filter(Q4 %in% c("Bachelor's degree","Master's degree","Doctoral degree","Professional degree")) %>%
            filter(Q5 %in% c("Business Analyst","Data Analyst")) %>%
            count(Q4,Q5, name = "resp_count") %>%
            mutate(Year = "2019")
        
        python_edu_2018_summary<-country_data()$data_2018 %>%
            mutate(Q4 = str_replace(Q4,"’","'")) %>% 
            filter(Q16_Part_1 == "Python",Q16_Part_2 == "") %>%
            filter(Q4 %in% c("Bachelor's degree","Master's degree","Doctoral degree","Professional degree")) %>%
            filter(Q6 %in% c("Business Analyst","Data Analyst")) %>%
            count(Q4,Q6, name = "resp_count") %>%
            rename(Q5 = Q6) %>% 
            mutate(Year = "2018")
        
        combined_python_edu_df<-bind_rows(
            python_edu_summary,
            python_edu_2019_summary,
            python_edu_2018_summary
        ) #%>% 
            #data.frame() %>% 
            #complete(expand(.,Q4,Q5,Year), fill = list(resp_count = 0))
        
        max_python_count<-max(combined_python_edu_df$resp_count)
        
        python_plot<-combined_python_edu_df %>%
            select(Q4,Q5,Year,resp_count) %>%
            ggplot(aes(x=Q4,y=resp_count,fill=Q5)) +
            geom_col(position = "dodge") +
            ylim(c(0,max_python_count + 15)) +
            facet_wrap(~Year,nrow = 3, strip.position = "right") +
            theme(title = element_text(size = 14),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1, size = 14),
                  axis.title.y=element_text(size = 14),
                  axis.text.y=element_blank(),
                  plot.tag = element_text(size = 12),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  strip.text = element_text(face="bold", size=16, 
                                            angle = 270,
                                            margin = margin(0,5,0,5)),
                  strip.background = element_rect(fill="bisque"),
                  panel.spacing.x=unit(2,"lines")
            ) +
            labs(
                # tag = "Figure 9b",
                x = "Educational Background",y = "Number of Respondents",
                title = "Python Business & Data Analyst Respondents",
                subtitle = "Distribution of Respondents by Educational Background",
                fill = "Respondent Title",
            ) +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9), size = 4, vjust=-0.5) +
            scale_fill_manual(values = c("#FFD92F","#E5C494"))
        
        r_plot + python_plot
        
    })
    
    output$trends_titles_edu_background<-renderPlot({
        req(trends_titles_edu_background_plot())
        
        trends_titles_edu_background_plot()
    })
    
    # Panel 2: Plot 7
    comp_levels_python_r_plot<-reactive({
        req(country_data())
        
        w1$show()
        comp_levels<-c("$0-999", "1,000-1,999", "2,000-2,999", "3,000-3,999", "4,000-4,999",
                       "5,000-7,499", "7,500-9,999", "10,000-14,999", "15,000-19,999",
                       "20,000-24,999", "25,000-29,999", "30,000-39,999", "40,000-49,999",
                       "50,000-59,999", "60,000-69,999", "70,000-79,999", "80,000-89,999",
                       "90,000-99,999", "100,000-124,999","125,000-149,999" ,"150,000-199,999",
                       "200,000-249,999", "250,000-299,999", "300,000-500,000","> $500,000")
        
        country_data()$data_2020 %>%
            select("Q24","Q7_Part_1","Q7_Part_2") %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            count(Q24,python_r_choice, name = "resp_count") %>%
            mutate(Q24 = case_when(
                Q24 == "" ~"NS",
                TRUE ~ Q24
            )) %>%
            pivot_wider(names_from = Q24, values_from = resp_count, values_fill = 0,names_repair = 'minimal') %>%
            rowwise(python_r_choice) %>%
            mutate(total_resp = sum(c_across(where(is.numeric)))) %>%
            ungroup() %>%
            mutate(across(2:length(.), ~ . / total_resp)) %>%
            select(-total_resp,-NS) %>%
            pivot_longer(-python_r_choice, names_to = 'Comp', values_to = "resp_pct") %>%
            filter(python_r_choice != 'No') %>%
            mutate(Comp = factor(Comp, levels = comp_levels),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R"))) %>%
            ggplot(aes(x=Comp,y=resp_pct,fill = python_r_choice)) +
            geom_col(position = "dodge") +
            labs(
                # tag = "Figure 10",
                y = "Percentage of Respondents",
                title = "Distribution of Respondents across Compensation Levels",
                subtitle = "All Titles"
            ) +
            theme(
                  title = element_text(size = 16),
                  plot.tag = element_text(size = 14),
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y=element_text(size = 12),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  strip.text = element_text(face="bold", size=14,
                                            margin = margin(5,0,5,0)),
                  strip.background = element_rect(fill="bisque"),
                  panel.spacing.y=unit(1,"lines")
            ) +
            scale_y_continuous(labels=percent) +   # scales package
            facet_wrap ( ~Comp, scales = 'free', ncol = 5) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC","#966FD6"))  # Pastel Red, Blue and Purple
    })
    
    output$comp_levels_python_r<-renderPlot({
        req(comp_levels_python_r_plot())
        
        comp_levels_python_r_plot()
    })
    
    # Panel 2: Plot 8
    comp_levels_ba_da_python_r_plot<-reactive({
        req(country_data())
        
        w1$show()
        comp_levels<-c("$0-999", "1,000-1,999", "2,000-2,999", "3,000-3,999", "4,000-4,999",
                       "5,000-7,499", "7,500-9,999", "10,000-14,999", "15,000-19,999",
                       "20,000-24,999", "25,000-29,999", "30,000-39,999", "40,000-49,999",
                       "50,000-59,999", "60,000-69,999", "70,000-79,999", "80,000-89,999",
                       "90,000-99,999", "100,000-124,999","125,000-149,999" ,"150,000-199,999",
                       "200,000-249,999", "250,000-299,999", "300,000-500,000","> $500,000")
        
        country_data()$data_2020 %>%
            select("Q5","Q24","Q7_Part_1","Q7_Part_2") %>%
            filter(Q5 %in% c("Business Analyst","Data Analyst")) %>%
            select(-Q5) %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            count(Q24,python_r_choice, name = "resp_count") %>%
            mutate(Q24 = case_when(
                Q24 == "" ~"NS",
                TRUE ~ Q24
            )) %>%
            pivot_wider(names_from = Q24, values_from = resp_count, values_fill = 0,names_repair = 'minimal') %>%
            rowwise(python_r_choice) %>%
            mutate(total_resp = sum(c_across(where(is.numeric)))) %>%
            ungroup() %>%
            mutate(across(2:length(.), ~ . / total_resp)) %>%
            select(-total_resp,-NS) %>%
            pivot_longer(-python_r_choice, names_to = 'Comp', values_to = "resp_pct") %>%
            filter(python_r_choice != 'No') %>%
            mutate(Comp = factor(Comp, levels = comp_levels),
                   python_r_choice = factor(python_r_choice,
                                            levels = c("Python","R","Python + R"))) %>%
            ggplot(aes(x=Comp,y=resp_pct,fill = python_r_choice)) +
            geom_col(position = "dodge") +
            labs(
                # tag = "Figure 10",
                y = "Percentage of Respondents",
                title = "Distribution of Respondents across Compensation Levels",
                subtitle = "Business Analysts and Data Analysts"
            ) +
            theme(
                  title = element_text(size = 16),
                  plot.tag = element_text(size = 14),
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y=element_text(size = 12),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  strip.text = element_text(face="bold", size=14,
                                            margin = margin(5,0,5,0)),
                  strip.background = element_rect(fill="bisque"),
                  panel.spacing.y=unit(1,"lines")
            ) +
            scale_y_continuous(labels=percent) +   # scales package
            facet_wrap ( ~Comp, scales = 'free', ncol = 5) +
            scale_fill_manual(values = c("#FF6961","#5C8FEC","#966FD6"))  # Pastel Red, Blue and Purple
    })
    
    output$comp_levels_ba_da_python_r<-renderPlot({
        req(comp_levels_ba_da_python_r_plot())
        
        comp_levels_ba_da_python_r_plot()
    })
    
    
    
    # Panel 3: Plot Not used currently
    r_prog_activity_plot<-reactive({
        req(country_data())
        
        w3$show()
        
        title_levels_activity<-c("Business Analyst","Data Analyst", "Statistician","Research Scientist",
                                 "Data Scientist","DBA/Database Engineer","Data Engineer",
                                 "Product/Project Manager","Software Engineer")
        
        country_data()$data_2020 %>%
            filter(Q7_Part_1 == "",Q7_Part_2 == 'R') %>%
            select(Q5,contains("Q23_")) %>%
            pivot_longer(cols = names(.)[-1], names_to = 'ActivityQuestion', values_to = "Activity") %>%
            filter(Activity != "") %>%
            filter(Q5 %in% title_levels_activity) %>%
            group_by(Q5,Activity) %>%
            summarize(Count = n()) %T>%
            { max_y<<-max(.["Count"]) } %>% 
            mutate(Q5 = factor(Q5, levels = title_levels_activity)) %>%
            ggplot(aes(x=Q5,y=Count,fill = Activity)) +
            geom_col(position='dodge') +
            labs(
                # tag = "Figure 11",
                y = "Number of Respondents",
                title = "Activities performed by R Programmers",
            ) + 
            ylim(c(0,max_y+5)) +
            theme(title = element_text(size = 16),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1, size = 12),
                  axis.title.y=element_text(size = 14),
                  axis.text.y=element_text(size = 12),
                  plot.tag = element_text(size = 14),
                  legend.position = "bottom",
                  legend.title = element_blank()) +
            guides(fill = guide_legend(ncol = 2,
                                       label.theme = element_text(size = 12))) +
            geom_text(aes(label=Count), position=position_dodge(width=0.9), size = 5, vjust=-0.5) +
            scale_fill_brewer(palette = 'Set1')
        
    })
    
    output$r_prog_activity<-renderPlot({
        req(r_prog_activity_plot())
        
        r_prog_activity_plot()
        
    })
    
    # Panel 3: Plot not used currently
    r_num_activity_plot<-reactive({
        req(country_data())
        
        w3$show()
        
        my_colors <- c(
            RColorBrewer::brewer.pal(n = 8, name = "Set1"),
            #### Out of first 8, select the last four
            RColorBrewer::brewer.pal(n = 8, name = "Set3")[5:8]
        )
        
        country_data()$data_2020 %>%
            filter(Q7_Part_1 == "",Q7_Part_2 == 'R') %>%
            select(Q5,Q21,contains("Q23_")) %>% 
            rowwise() %>% 
            mutate(across(starts_with('Q23_'), ~ ifelse(. == "",0,1))) %>% 
            # rowwise() %>% 
            mutate(
                num_activities = sum(c_across(Q23_Part_1:Q23_OTHER))
            ) %>% 
            group_by(Q5,Q21) %>% 
            filter(!Q21 %in% c("","0")) %>% 
            summarize("avg_count" = round(mean(num_activities))) %T>% {
                max_y<<- max(.["avg_count"])
            } %>% 
            mutate(Q21 = factor(Q21, levels = c('1-2','3-4','5-9','10-14','15-19','20+'))) %>%
            ggplot(aes(x=Q21, y=avg_count, fill = Q5)) +
            geom_col(position='dodge') +
            ylim(c(0,max_y + 2)) +
            labs(title = "Number of different activities performed by respondents using R",
                 subtitle = "By Team Size",
                 # x = "Team Size",
                 y = "Activity Count", x = "Size of Team") +
            geom_text(aes(label=avg_count),
                      position=position_dodge(width=0.9), size = 4, vjust=-0.5) +
            theme(title = element_text(size = 16),
                  axis.text.x=element_blank(),
                  axis.title.y=element_text(size = 14),
                  axis.text.y=element_blank(),
                  plot.tag = element_text(size = 14),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  strip.text = element_text(size = 14,
                                            face = "bold",
                                            margin = margin(5,0,5,0)),
                  strip.background = element_rect(fill = 'bisque')
            ) +
            facet_wrap(~ Q21,
                       # strip.position = 'bottom',
                       scales = 'free_x') +
            # scale_fill_manual(values = my_colors)
            scale_fill_brewer(palette='Set1')
        
    })
    
    output$r_num_activity<-renderPlot({
        req(r_num_activity_plot())
        
        r_num_activity_plot()
    })
    
    # Panel 3: Plot 1
    viz_lib_plot<-reactive({
        req(country_data())
        
        w3$show()
        
        lang_resp_count<-country_data()$data_2020 %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(python_r_choice) %>%
            count(python_r_choice,name = "resp_count")
        
        country_data()$data_2020 %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(python_r_choice,contains("Q14_")) %>%
            pivot_longer(cols = names(.)[-1], names_to = "viz_ques", values_to = "viz_lib") %>%
            select(python_r_choice,viz_lib) %>%
            filter(!viz_lib %in% c("","None","Other")) %>%
            count(python_r_choice,viz_lib, name = "num_resp") %>%
            pivot_wider(names_from = viz_lib, values_from = num_resp, values_fill = 0) %>%
            inner_join(lang_resp_count) %>% 
            mutate(across(2:length(.), ~ . / resp_count)) %>%
            select(-resp_count) %>%
            pivot_longer(-python_r_choice, names_to = 'viz_lib', values_to = "resp_pct") %>%
            filter(python_r_choice %in% c('R','Python + R')) %>%
            mutate(
                resp_pct = round(100*resp_pct),
                python_r_choice = factor(python_r_choice,
                                         levels = c("R","Python + R"))) %T>%
            {
                max_y<<-max(.["resp_pct"])
            } %>%
            ggplot(aes(x=fct_reorder2(viz_lib,viz_lib,resp_pct),y=resp_pct,
                       fill = python_r_choice)) +
            geom_col(position = "dodge") +
            ylim(c(0,max_y + 10)) +
            theme(title = element_text(size = 16),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1,size = 12),
                  axis.title.y = element_text(size = 14),
                  axis.text.y=element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.tag = element_text(size = 14),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12)) +
            geom_text(aes(label=paste0(round(resp_pct),"%")), 
                      position=position_dodge(width=0.9), size = 5, vjust=-0.5) +
            labs(
                # tag = "Figure 13",
                y = "Percentage of Respondents",
                title = "Visualization tools utilized by respondents using R / Python + R") +
            scale_fill_manual(values = c("#5C8FEC","#966FD6"))  # Pastel Blue and Purple
    })
    
    output$viz_lib<-renderPlot({
        req(viz_lib_plot())
        
        viz_lib_plot()
    })
    
    # Panel 3: Plot 2
    viz_lib_titles_plot<-reactive({
        req(country_data())
        
        w3$show()
        
        title_levels<-c("Business\nAnalyst","Data\nAnalyst", "Data\nScientist",
                        "Research\nScientist","Statistician","Student")
        
        country_data()$data_2020 %>%
            mutate(python_r_choice = 
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            filter(python_r_choice %in% c("R","Python + R"),
                   Q5 %in% c("Business Analyst","Data Analyst", "Data Scientist",
                             "Research Scientist","Statistician","Student")) %>% 
            select(Q5,python_r_choice,contains("Q14_")) %>% 
            pivot_longer(cols = names(.)[-1:-2], names_to = "viz_ques", values_to = "viz_lib") %>%
            select(Q5,python_r_choice,viz_lib) %>%
            mutate(viz_lib = str_trim(viz_lib)) %>% 
            filter(viz_lib %in% c("Ggplot / ggplot2","Shiny")) %>% 
            count(Q5,python_r_choice,viz_lib,name = 'resp_count') %T>%
            {
                max_y<<-max(.["resp_count"])
            } %>% 
            mutate(Q5 = factor(
                case_when(
                    Q5 == "Business Analyst" ~ "Business\nAnalyst",
                    Q5 == "Data Analyst" ~ "Data\nAnalyst",
                    Q5 == "Data Scientist" ~ "Data\nScientist",
                    Q5 == "Research Scientist" ~ "Research\nScientist",
                    TRUE ~ Q5
                ),
                levels = title_levels
            ),
            python_r_choice = factor(python_r_choice, levels = c("R","Python + R")),
            viz_lib = factor(viz_lib)) %>% 
            ggplot(aes(x=viz_lib,y=resp_count,fill = viz_lib)) +
            geom_col(position = "dodge") +
            ylim(c(0,max_y + 10)) +
            facet_grid(python_r_choice ~ Q5, switch = 'both') +
            theme(
                title = element_text(size = 16),
                axis.title.x=element_blank(),
                axis.title.y=element_text(size = 14),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "bottom",
                legend.background = element_rect(fill = 'cornsilk'),
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                strip.text.x = element_text(size=14,
                                            margin = margin(5,0,5,0)),
                strip.text.y = element_text(face="bold", size=16,angle = 90,
                                            margin = margin(0,5,0,5)),
                strip.background = element_rect(fill="bisque"),
                panel.spacing.y=unit(1,"lines")
            ) +
            labs(y = "Number of Respondents", title = "Shiny and ggplot usage across job titles") +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9),
                      size = 5, vjust=-0.5) +
            scale_fill_manual(values = c("#FB8072","#8DD3C7")) +
            NULL
        
    })
    
    output$viz_lib_titles<-renderPlot({
        req(viz_lib_titles_plot())
        
        viz_lib_titles_plot()
    })
    
    # Panel 3: Plot 3
    ml_lib_plot<-reactive({
        req(country_data())
        
        w3$show()
        
        lang_resp_count<-country_data()$data_2020 %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(python_r_choice) %>%
            count(python_r_choice,name = "resp_count")
        
        country_data()$data_2020 %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(python_r_choice,contains("Q16_")) %>%
            pivot_longer(cols = names(.)[-1], names_to = "ml_ques", values_to = "ml_lib") %>%
            select(python_r_choice,ml_lib) %>%
            filter(!ml_lib %in% c("","None","Other")) %>%
            count(python_r_choice,ml_lib,name = "num_resp") %>%
            pivot_wider(names_from = ml_lib, values_from = num_resp, values_fill = 0) %>%
            inner_join(lang_resp_count) %>% 
            mutate(across(2:length(.), ~ . / resp_count)) %>%
            select(-resp_count) %>%
            pivot_longer(-python_r_choice, names_to = 'ml_lib', values_to = "resp_pct") %>%
            filter(python_r_choice %in% c('R','Python + R')) %>%
            mutate(
                resp_pct = round(100*resp_pct),
                python_r_choice = factor(python_r_choice,
                                         levels = c("R","Python + R"))) %T>%
            {
                max_y<<-max(.["resp_pct"])
            } %>%
            ggplot(aes(x=fct_reorder2(ml_lib,ml_lib,resp_pct),y=resp_pct,fill = python_r_choice)) +
            geom_col(position = "dodge") +
            ylim(c(0,max_y + 2)) +
            theme(
                title = element_text(size = 16),
                axis.title.x=element_blank(),
                axis.text.x=element_text(size=12),
                axis.title.y=element_text(size = 14),
                axis.text.y=element_blank(),
                axis.ticks.y = element_blank(),
                plot.tag = element_text(size = 14),
                legend.position = "bottom",
                legend.title = element_blank(),
                legend.text = element_text(size = 12)) +
            geom_text(aes(label=paste0(round(resp_pct),"%")),
                      position=position_dodge(width=0.9), size = 4,
                      vjust=-0.5) +
            labs(
                # tag = "Figure 15",
                y = "Percentage of Respondents",
                title = "ML Libraries employed by respondents using R / Python + R") +
            scale_fill_manual(values = c("#5C8FEC","#966FD6"))  # Pastel Blue and Purple
            
    })
    
    output$ml_lib<-renderPlot({
        req(ml_lib_plot())
        
        ml_lib_plot()
    })
    
    # Panel 3: Plot 4
    ml_lib_titles_plot<-reactive({
        req(country_data())
        
        w3$show()
        title_levels<-c("Business\nAnalyst","Data\nAnalyst", "Data\nScientist",
                        "Research\nScientist","Statistician","Student")
        
        country_data()$data_2020 %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(Q5,python_r_choice,contains("Q16_")) %>%
            filter(python_r_choice %in% c("R","Python + R"),
                   Q5 %in% c("Business Analyst","Data Analyst", "Data Scientist",
                             "Research Scientist","Statistician","Student")) %>%
            pivot_longer(cols = names(.)[-1:-2], names_to = "ml_ques", values_to = "ml_lib") %>%
            select(Q5,python_r_choice,ml_lib) %>%
            mutate(ml_lib = str_trim(ml_lib)) %>%
            filter(ml_lib %in% c("Caret","Tidymodels")) %>%
            count(Q5,python_r_choice,ml_lib,name = 'resp_count') %T>%
            {
                max_y<<-max(.["resp_count"])
            } %>% 
            mutate(Q5 = factor(
                case_when(
                    Q5 == "Business Analyst" ~ "Business\nAnalyst",
                    Q5 == "Data Analyst" ~ "Data\nAnalyst",
                    Q5 == "Data Scientist" ~ "Data\nScientist",
                    Q5 == "Research Scientist" ~ "Research\nScientist",
                    TRUE ~ Q5
                ),
                levels = title_levels
            ),
            python_r_choice = factor(python_r_choice, levels = c("R","Python + R")),
            viz_lib = factor(ml_lib)) %>% 
            ggplot(aes(x=ml_lib,y=resp_count,fill = ml_lib)) +
            geom_col(position = "dodge") +
            ylim(c(0,max_y + 10)) +
            facet_grid(python_r_choice ~ Q5, switch = 'both') +
            theme(
                title = element_text(size = 16),
                axis.title.x=element_blank(),
                axis.title.y=element_text(size = 14),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks = element_blank(),
                legend.position = "bottom",
                legend.background = element_rect(fill = 'cornsilk'),
                legend.title = element_blank(),
                legend.text = element_text(size = 12),
                strip.text.x = element_text(size=14,
                                            margin = margin(5,0,5,0)),
                strip.text.y = element_text(face="bold", size=16,angle = 90,
                                            margin = margin(0,5,0,5)),
                strip.background = element_rect(fill="bisque"),
                panel.spacing.y=unit(1,"lines")
            ) +
            labs(y = "Number of Respondents", title = "Caret and Tidymodels usage across job titles") +
            geom_text(aes(label=resp_count), position=position_dodge(width=0.9),
                      size = 5, vjust=-0.5) +
            scale_fill_manual(values = c("#FB8072","#8DD3C7")) +
            NULL
        
    })
    
    output$ml_lib_titles<-renderPlot({
        req(ml_lib_titles_plot())
        
        ml_lib_titles_plot()
    })
    
    ml_algo_plot<-reactive({
        req(country_data())
        
        w3$show()
        
        lang_resp_count<-country_data()$data_2020 %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(python_r_choice) %>%
            count(python_r_choice,name = "resp_count")
        
        country_data()$data_2020 %>%
            mutate(python_r_choice =
                       ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "R", "Python + R",
                              ifelse(Q7_Part_1 == "Python" & Q7_Part_2 == "", "Python",
                                     ifelse(Q7_Part_1 == "" & Q7_Part_2 == "R", "R","No")))) %>%
            select(python_r_choice,contains("Q17_")) %>%
            pivot_longer(cols = names(.)[-1], names_to = "ml_ques", values_to = "ml_algo") %>%
            select(python_r_choice,ml_algo) %>%
            filter(!ml_algo %in% c("","None","Other")) %>%
            count(python_r_choice,ml_algo,name = "num_resp") %>%
            pivot_wider(names_from = ml_algo, values_from = num_resp, values_fill = 0) %>%
            inner_join(lang_resp_count) %>% 
            mutate(across(2:length(.), ~ . / resp_count)) %>%
            select(-resp_count) %>%
            pivot_longer(-python_r_choice, names_to = 'ml_algo', values_to = "resp_pct") %>%
            filter(python_r_choice %in% c('R','Python + R')) %>%
            mutate(
                resp_pct = round(100*resp_pct),
                python_r_choice = factor(python_r_choice,
                                         levels = c("R","Python + R"))) %T>%
            {
                max_x<<-max(.["resp_pct"])
            } %>%
            ggplot(aes(x=reorder(ml_algo,resp_pct),y=resp_pct,fill = python_r_choice)) +
            geom_col(position = "dodge") +
            ylim(c(0,max_x + 5)) +
            theme(title = element_text(size = 16),
                  axis.text.x=element_blank(),
                  axis.text.y=element_text(size=14),
                  axis.title.y=element_blank(),
                  plot.tag = element_text(size = 14),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12)) +
            geom_text(aes(label=paste0(round(resp_pct),"%")), position=position_dodge(width=0.9), size = 5, hjust=-0.25) +
            labs(y = "Percentage of Respondents",
                 title = "ML Algorithms employed by respondents using R / Python + R") +
            coord_flip() +
            scale_fill_manual(values = c("#5C8FEC","#966FD6"))  # Pastel Blue and Purple
    })
    
    output$ml_algo<-renderPlot({
        req(ml_algo_plot())
        
        ml_algo_plot()
    })
    
    company_n_team_size_plot<-reactive({
        req(country_data())
        
        w2$show()
        
        company_size_plot<-country_data()$data_2020 %>%
            filter(Q7_Part_1 == "",Q7_Part_2 == 'R') %>%
            select(Q20) %>%
            mutate(Q20 = replace(Q20, Q20 == "", "Not Specified")) %>%
            group_by(Q20) %>%
            summarize(Count = n()) %T>% {
                max_y<<-max(.["Count"])
            } %>% 
            mutate(Q20 = factor(Q20,
                                levels = c('Not Specified',
                                           '0-49 employees',
                                           '50-249 employees',
                                           '250-999 employees',
                                           '1000-9,999 employees',
                                           '10,000 or more employees'))) %>%
            ggplot(aes(x=Q20, y = Count)) +
            geom_col(fill = "#FC8D62") +
            ylim(c(0,max_y + 5)) +
            labs(
                # tag = "Figure 17a",
                title = "Typical Company sizes",
                y  = "Number of Respondents",
            ) +
            theme(title = element_text(size = 16),
                  axis.title.x = element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1, size = 12),
                  plot.tag = element_text(size = 12)) +
            scale_fill_brewer(palette = 'Set1') +
            geom_text(aes(label=Count), position=position_dodge(width=0.9), size = 5, vjust=-0.5)
            
        
        team_size_plot<-country_data()$data_2020 %>%
            filter(Q7_Part_1 == "",Q7_Part_2 == 'R') %>%
            select(Q21) %>% 
            mutate(Q21 = replace(Q21, Q21 == "", "Not Specified")) %>%
            group_by(Q21) %>%
            summarize(Count = n()) %T>% {
                max_y<<-max(.["Count"])
            } %>% 
            mutate(Q21 = factor(Q21, levels = c('Not Specified',
                                                '0','1-2','3-4','5-9','10-14','15-19','20+'))) %>%
            ggplot(aes(x=Q21, y = Count)) +
            geom_col(fill = "#FC8D62") +
            ylim(c(0,max_y + 5)) +
            labs(
                # tag = "Figure 17b",
                title = "Typical Team sizes",
                y  = "Number of Respondents",
            ) +
            theme(title = element_text(size = 16),
                  axis.title.x = element_blank(),
                  axis.text.x=element_text(angle=45, hjust=1, size = 12),
                  plot.tag = element_text(size = 12)) +
            scale_fill_brewer(palette = 'Set1') +
            geom_text(aes(label=Count), position=position_dodge(width=0.9), size = 5, vjust=-0.5)
        
        company_size_plot + team_size_plot
        
    })
    
    output$company_n_team_size<-renderPlot({
        req(company_n_team_size_plot())

        company_n_team_size_plot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
