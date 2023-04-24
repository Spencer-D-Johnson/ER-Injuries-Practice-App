# Copy of Hadley Wickham's ER data app, with a few minor changes
# Spencer Johnson
# 4/23/23

#############################

# load packages
library(shiny)
library(vroom)
library(tidyverse)

# Comment below was before Andy's announcement
# The URL for the download code provided in the book no longer works. I found the files at "https://github.com/hadley/mastering-shiny/blob/main/neiss/", but using the download code with this URL gave me files that vroom wouldn't read properly (not actually the raw data or something?). There are comments about this issue on the "issues" tab for mastering-shiny on GitHub, but the alternate URLs they provided didn't work either. I ended up manually downloading the injuries file from Hadley's page and then using download.file() to get the text files for the other two.
injuries <- vroom("injuries.tsv.gz")
products <- vroom("products.tsv")
population <- vroom("population.tsv")

# pull out product numeric codes
prod_codes <- setNames(products$prod_code, products$title)

# for a given variable, get weighted totals for 5 most frequent entries and then a lump sum for the rest
count_top <- function(df, var, n = 5) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

# My addition: set ggplot theme to bw
theme_set(theme_bw())

##############################

ui <- fluidPage(
    # First row: select product of injury and y-axis preference for plot
    fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%")),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
    ),
    # Second row: generate 3 tables showing most common diagnosis, body part, and location of injury with product
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    # 3rd row: make plot of frequency/rate by age and sex
    fluidRow(
        column(12, plotOutput("age_sex"))
    ),
    # 4th row: spit out random description of injury for selected product
    fluidRow(
        column(2, actionButton("story", "Tell me a story")),
        column(10, textOutput("narrative"))
    )
)

######################################

server <- function(input, output, session) {
    # Filter full injury table to selected product (changed reactive name from "selected")
    selected_product <- reactive(injuries %>% filter(prod_code == input$code))
    
    # Create the 3 tables in row 2, make each take up full 1/3 of page
    output$diag <- renderTable(count_top(selected_product(), diag), width = "100%")
    output$body_part <- renderTable(count_top(selected_product(), body_part), width = "100%")
    output$location <- renderTable(count_top(selected_product(), location), width = "100%")
    
    # get count and rate by age and sex for plot (changed reactive name from "summary")
    age_sex_summary <- reactive({
        selected_product() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    
    # Make plot (dependent on user input for y-axis)
    output$age_sex <- renderPlot({
        if (input$y == "count") {
            age_sex_summary() %>%
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
                labs(y = "Estimated number of injuries")
        } else {
            age_sex_summary() %>%
                ggplot(aes(age, rate, colour = sex)) +
                geom_line(na.rm = TRUE) +
                labs(y = "Injuries per 10,000 people")
        }
    }, res = 96)
    
    # get random narrative if button is pressed
    narrative_sample <- eventReactive(
        input$story,  # I can't figure out why the original code used list(). This seems to work too.
        #list(input$story, selected_product()),
        selected_product() %>% pull(narrative) %>% sample(1)
    )
    output$narrative <- renderText(narrative_sample())
}

#################################

shinyApp(ui, server)
