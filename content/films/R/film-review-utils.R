chart_scores = function(tom, hannah) {
  
  Tom = lapply(1:floor(tom), function(x) {
    tibble(Name = 1, Rating = x)
  }) %>% bind_rows() %>% mutate(Max = max(Rating))
  
  Hannah = lapply(1:floor(hannah), function(x) {
    tibble(Name = 2, Rating = x)
  }) %>% bind_rows() %>% mutate(Max = max(Rating))
  
  df = Tom %>% union(Hannah)
  
  
  hchart(df,
         "scatter", 
         hcaes(x = Rating, y = Name), 
         color = "#F7C919",
         name = "Overall Rating"
  ) %>%
  hc_yAxis(
    min = 1, max = 2, lineWidth = 0,
    categories = c("","Tom", "Hannah"),
    breaks = list(from = 1, to = 2, breakSize = 1),
    title = list(text = "")
  ) %>% 
  #hc_chart(height = 250) %>%
  hc_xAxis(
    min = 0, max = 10, tickWidth = 0,
    lineWidth = 0.5, 
    lineColor = "#d3d3d3",
    gridLineColor = "#d3d3d3",
    labels = list(),
    title = list(text = "")
  ) %>%
  hc_plotOptions(
    scatter = list(
      marker = list(
        symbol = "circle",
        lineWidth = 0,
        radius = 8
      )
    )
  ) %>%
  hc_tooltip(
    shape = "square",
    formatter = JS("function(){return('<b> Overall Rating: </b>' + this.point.Max + '/10')}")
  ) %>%
  hc_size(height = "150px")
  
}

chart_radar = function(movie, acting = 5, dialogue = 5, story = 5, entertainment = 5, cinematography = 5) {
  
  labs <- c("Cinematography", "Acting", "Dialogue", "Story", "Entertainment")
  
  scores <- list(
    movie = c(cinematography, acting, dialogue, story, entertainment)
  )
  
  chartJSRadar(scores = scores,
               labs = labs,
               maxScale = 10,
               showLegend = FALSE,
               polyAlpha = 0.1,
               labelSize=10, 
               height = "300px",
               width = "300px",
               scaleStepWidth = 5,
               colMatrix = matrix(c(247,201,25))
  )
  
}

compile_review = function(yaml_body, edition) {
  
  #film_title = yaml_chunk$title
  #yaml_body = yaml_chunk %>% pluck(film_title)
  edition = stringr::str_pad(edition, 2, pad = "0")

  # Pick of Week Text
  pow_text = ifelse(
    yaml_body$`pick-of-the-week`,
    '<span class="label upper outline error"> PICK OF THE WEEK </span>',
    ''
  )

  div(class = "film-review",
    
    div(class = "film-title",
      
      h1(yaml_body$title)

    ),

    div(class="film-review-left",
           
      # Image
      tags$img(src = glue("/img/films/{edition}/{yaml_body$image}")),

      # Tags
      HTML(
        glue(
        '<div class = "film-tags"> 
        <span class="label upper outline warning"> {toupper(yaml_body$genre)} </span> {pow_text}
        </div>
        '
        )
      ),

      # Reviewer
      HTML(glue("<b> {yaml_body$reviewer} : </b>")),
      
      # Review Text
      markdown::markdownToHTML(
        text = yaml_body$`review-text`, 
        fragment.only = TRUE
      ) %>% HTML()

    ),
    
    div(class="film-review-right",
      
      div(class = "review-component",
        HTML(glue("<b> Similar to: </b> {yaml_body$`similar-film`}"))
      ),
  
      # Rating Overall
      div(class="ratings review-component",
        chart_scores(tom = yaml_body$ratings$overall$tom, hannah = yaml_body$ratings$overall$hannah)
      ),
      
      # Rating Radar
      div(class = "review-component",
        chart_radar(
          film_title, 
          acting = yaml_body$ratings$components$acting, 
          dialogue = yaml_body$ratings$components$dialogue, 
          story = yaml_body$ratings$components$story, 
          entertainment = yaml_body$ratings$components$entertainment, 
          cinematography = yaml_body$ratings$components$cinematography
        )
      )
      
    )
    
  )
  
}


compile_ratings = function(yaml_chunk) {
  
  if (is_empty(yaml_chunk)) return(reactable(tibble()))
  
  ratings = 
    yaml_chunk %>%
    enframe() %>%
    select(-name) %>%
    #unnest_longer(value, indices_to = "film") %>%
    unnest_wider(value) %>%
    select(title, year, genre, ratings) %>%
    unnest_wider(ratings) %>%
    unnest_wider(overall) %>%
    unnest_wider(components) %>%
    rename(film = title) %>%
    select(film, year, genre, tom, hannah, everything())
  
  rating_stars <- function(rating, max_rating = 10) {
    star_icon <- function(empty = FALSE) {
      tagAppendAttributes(shiny::icon("star"),
                          style =  paste("color:", if (empty) "#edf0f2" else "#F7C919"),
                          "aria-hidden" = "true"
      )
    }
    rounded_rating <- floor(rating + 0.5)  # always round up
    stars <- lapply(seq_len(max_rating), function(i) {
      if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
    })
    label <- sprintf("%s out of %s", rating, max_rating)
    div(title = label, "aria-label" = label, role = "img", stars)
  }
  
  ratings %>%
    select(film, year, genre, tom, hannah) %>%
    reactable(
      compact = TRUE,
      class = "other-films",
      pagination = FALSE,
      defaultColDef = colDef(
        headerClass = "other-films-header", 
        align = "right",
        footerStyle = list(fontWeight = "bold")
      ),
      details = 
        function(index) {
          film <- ratings[index, ] %>% select(acting, dialogue, cinematography, entertainment, story)
          htmltools::div(
            style = "padding: 16px", 
            reactable(film, outlined = TRUE,
            defaultColDef = colDef(header = str_to_upper)
          )
          )
        }
      ,
      
      columns = list(
        film = colDef(name =  "Film"),
        year = colDef(name =  "Year"),
        genre = colDef(
          name =  "Genre",
          cell = function(value) {
            htmltools::span(class = "label upper outline warning", value)
          }  
        ),
        tom = colDef(name =  "Tom", cell=function(value) rating_stars(value)),
        hannah = colDef(name =  "Hannah",  cell=function(value) rating_stars(value))
      )
    )

}

rating_stars <- function(rating, max_rating = 10) {
  star_icon <- function(empty = FALSE) {
    tagAppendAttributes(shiny::icon("star"),
                        style =  paste("color:", if (empty) "#edf0f2" else "#F7C919"),
                        "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })
  label <- sprintf("%s out of %s", rating, max_rating)
  div(title = label, "aria-label" = label, role = "img", stars)
}
