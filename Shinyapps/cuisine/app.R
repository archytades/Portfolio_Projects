#app to explore ingredients unique to world cuisines
library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(d3wordcloud)
library(DT)

recipes<-readRDS("recipes.rds") %>% rename(ingredient=ingredients)
cuisine_words<-unnest(recipes, ingredient) %>% select(-id) %>% 
  count(cuisine, ingredient, sort=TRUE)
total_words<-cuisine_words %>% 
  group_by(cuisine) %>% 
  summarise(total=sum(n))
cuisine_words<-left_join(cuisine_words, total_words)
recipes_enriched<-cuisine_words %>% 
  bind_tf_idf(ingredient, cuisine, n) 

ui <- fluidPage(
  titlePanel('Explore Cuisines'),
  sidebarLayout(
    sidebarPanel(
      selectInput('cuisine', 'Select Cuisine', unique(recipes$cuisine)),
      sliderInput('nb_ingredients', 'Select No. of Ingredients', 5, 100, 20),
    ),
    mainPanel(
      tabsetPanel(
        # CODE BELOW: Add `d3wordcloudOutput` named `wc_ingredients` in a `tabPanel`
        tabPanel('Word Cloud', d3wordcloud::d3wordcloudOutput('wc_ingredients')),
        tabPanel('Plot', plotly::plotlyOutput('plot_top_ingredients')),
        tabPanel('Table', DT::DTOutput('dt_top_ingredients'))
      )
    )
  )
)

server <- function(input, output, session){
  # CODE BELOW: Render an interactive wordcloud of top ingredients and 
  # the number of recipes they get used in, using `d3wordcloud::renderD3wordcloud`,
  # and assign it to an output named `wc_ingredients`.
  output$wc_ingredients<-d3wordcloud::renderD3wordcloud({
    empty()
    d<-rval_top_ingredients()
    d3wordcloud(d$ingredient, d$tf_idf, tooltip=T)
  })
  
  
  
  rval_top_ingredients <- reactive({
    recipes_enriched %>% 
      filter(cuisine == input$cuisine) %>% 
      arrange(desc(tf_idf)) %>% 
      head(input$nb_ingredients) %>% 
      mutate(ingredient = forcats::fct_reorder(ingredient, tf_idf))
  })
  
  output$plot_top_ingredients <- plotly::renderPlotly({
    rval_top_ingredients() %>%
      ggplot(aes(x = ingredient, y = tf_idf)) +
      geom_col() +
      coord_flip()
  })
  
  output$dt_top_ingredients <- DT::renderDT({
    recipes %>% 
      filter(cuisine == input$cuisine) %>% 
      count(ingredient, name = 'nb_recipes') %>% 
      arrange(desc(nb_recipes)) %>% 
      head(input$nb_ingredients)
  })
}

shinyApp(ui = ui, server= server)