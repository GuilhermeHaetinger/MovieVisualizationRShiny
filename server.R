library(shiny)
library(shinythemes) # Temas para a interface do shiny
library(RCurl)       # Abstração de comandos 'curl' (HTTP requests)
library(dplyr)
library(jsonlite)    # Leitura de JSONs
library(glue)        # Operações em strings parametrizadas (como uma 'F-string')
library(ggplot2)
library(purrr)       # Contém a função 'map', muito útil para aplicar funções em listas
library(plotly)
library(data.table)
library(GGally)
library(network)

APIKey <- '10987c9503d01085cdfa137320d65474'
num_of_pages <- 10

fetch_person <- function(person_id) {
  url  <- 'https://api.themoviedb.org/3/person/'
  args <- glue('{person_id}?api_key={APIKey}')
  request  <- glue(url, args)

  # Mandar request
  result <- tryCatch ({
    httpGET(request)
    }, error = function(e) {
      return('{}')
    })

  result
}

fetch_cast <- function(movie_id, size=10) {
  url  <- 'https://api.themoviedb.org/3/movie/'
  args <- glue('{movie_id}/credits?api_key={APIKey}')
  request  <- glue(url, args)

  # Mandar request
  result <- httpGET(request) %>%
    # 'Parse' o resultado e coletar o item 'cast', que contém os atores
    fromJSON() %>% .$cast %>%
    # Transformar em Data Frame
    as.data.frame()

  result <- head(result, size)

  actor_jsonls <- paste(c(lapply(result$id, fetch_person)), collapse=',')
  actor_jsonls <- paste(c('[', actor_jsonls, ']'), collapse='')
  result <- actor_jsonls %>% fromJSON() %>% as.data.frame()

  result
}

fetch_dir <- function(movie_id, size=5) {
  url  <- 'https://api.themoviedb.org/3/movie/'
  args <- glue('{movie_id}/credits?api_key={APIKey}')
  request  <- glue(url, args)

  # Mandar request
  result <- httpGET(request) %>%
    # 'Parse' o resultado e coletar o item 'cast', que contém os atores
    fromJSON() %>% .$crew %>%
    # Transformar em Data Frame
    as.data.frame()

  direct <- result[result$job == 'Director', ] %>% head(5)

  dir_jsonls <- paste(lapply(direct$id, fetch_person), collapse=',')
  dir_jsonls <- paste(c('[', dir_jsonls, ']'), collapse='')
  result <- dir_jsonls %>% fromJSON() %>% as.data.frame()

  result
}

fetch_movies <- function(num_pages, section) {
  # Parar quando estivermos buscando pela página 0 (inexistente)
  if (num_pages == 0) {
    return(data.frame())
  }

  url  <- 'https://api.themoviedb.org/3/movie/'
  args <- glue("{section}?api_key={APIKey}&language=en-US&page={num_pages}")
  request  <- glue(url, args)

  # Mandar request
  result <- httpGET(request) %>%
    # 'Parse' o resultado e coletar o item 'result', que contém as informações
    fromJSON() %>% .$results %>%
    # Transformar em Data Frame
    as.data.frame()

  # Criar coluna 'cast' e populá-la com os atores
  result$cast <- result$id %>%
    lapply(fetch_cast)

  # Criar coluna 'dir' e populá-la com os diretores
  result$dir <- result$id %>%
    lapply(fetch_dir)

  # Concatenar as 'rows' da página anterior
  rbind(fetch_movies(num_pages-1, section), result)
}

if (!file.exists("mov_pop.RDS") || !file.exists("mov_top.RDS")){
  mov_pop <- fetch_movies(num_of_pages, 'popular')
  mov_top <- fetch_movies(num_of_pages, 'top_rated')
  saveRDS(mov_pop, file="mov_pop.RDS")
  saveRDS(mov_top, file="mov_top.RDS")
} else {
  mov_pop <- readRDS("mov_pop.RDS")
  mov_top <- readRDS("mov_top.RDS")
}
names(mov_pop)

setup_node_types <- function(mov, cast, dir) {
  mov <- mov[!duplicated(mov), ]
  cast <- cast[!duplicated(cast$name), ]
  mov_type <- rep(F, nrow(mov))
  cast_type <- rep(T, nrow(cast))
  dir_type <- rep(T, nrow(dir))
  node_df <- data.frame(name=c(mov$title, cast$name, dir$name),
                        type=c(mov_type, cast_type, dir_type))
  node_df
}

setup_node_color_by_genre <- function(mov, cast, dir) {
  mov <- mov[!duplicated(mov), ]
  cast <- cast[!duplicated(cast$name), ]
  dir <- dir[!duplicated(dir$name), ]
  cast_type <- rep(0, nrow(cast))
  dir_type <- rep(-1, nrow(dir))
  mov_type <- apply(mov, 1, function(x) unlist(x$genre_ids)[1])
  node_df <- data.frame(name=c(mov$title, cast$name, dir$name),
                        genre=as.character(c(mov_type, cast_type, dir_type)))
  node_df
}

setup_relation <- function(mov, group, row_num=1, num_head=5, movie_to_rel = T) {

  if (row_num > nrow(mov)) {
    return(data.frame())
  }

  mov_row <- mov[row_num,]
  mov_name <- mov_row$title
  links <- data.frame()
  if (num_head != 0) {
    links <- as.data.frame(mov_row[[group]]) %>% head(num_head)
  } else {
    links <- as.data.frame(mov_row[[group]])
  }

  if (nrow(links) == 0) {
    return(setup_relation(mov, group, row_num=row_num+1, movie_to_rel = movie_to_rel))
  }

  repeated_movie_ls <- vector("list", length=nrow(links))
  repeated_movie_ls[1:nrow(links)] = mov_name
  repeated_movie_ls <- unlist(repeated_movie_ls)

  relation_df <- data.frame()
  if (movie_to_rel) {
    relation_df <- data.frame(from=repeated_movie_ls, to=links$name)
  } else {
    relation_df <- data.frame(from=links$name, to=repeated_movie_ls)
  }

  relation_df <- rbind(setup_relation(mov, group, row_num=row_num+1, movie_to_rel = movie_to_rel),
                       relation_df)
  relation_df
}

get_standalone_group <- function(mov_df, group, row_num=1, num_head=5) {
  if (row_num > nrow(mov_df)) {
    return(data.frame())
  }

  mov_row = mov_df[row_num,]
  grp = as.data.frame(mov_row[[group]]) %>% head(num_head)

  next_df = get_standalone_group(mov_df, group, row_num=row_num+1)

  grp <- rbind(next_df, grp)

  # Probably inneficient to do this for every row -> unecessary, also
  as.data.frame(grp[!duplicated(grp), ])
}

setup_sorted_graph_links <- function(linked_df, node_df) {
  node_df$rowid <- as.numeric(row.names(node_df))
  linked_df <- linked_df[!duplicated(linked_df), ]
  node_df <- node_df[!duplicated(node_df), ]
  linked_df$source <- mapply(function(x) node_df[node_df$name == x, ]$rowid-1, linked_df$from)
  linked_df$target <- mapply(function(x) node_df[node_df$name == x, ]$rowid-1, linked_df$to)
  linked_df
}

get_cast_occurances_df <- function(person, rel) {
  nrow(rel[rel$to == person, ]) + nrow(rel[rel$from == person, ])
}

setup_final_connections <- function(prim_df, sec_df, nodes) {
  sec_df <- sec_df[sec_df$to %in% prim_df$from, ]
  sec_df <- sec_df[!(sec_df$from %in% prim_df$to), ]
  rbind(prim_df, sec_df)
}

setup_sankey <- function(prim_df, sec_df, nodes, power=3, threshold=0) {
  prim_df$value <- apply(prim_df, 1, function(x) (get_cast_occurances_df(x['to'], prim_df) ** power))
  sec_df$value <- apply(sec_df, 1, function(x) (get_cast_occurances_df(x['from'], sec_df) ** power))
  prim_df <- prim_df[prim_df$value ** (1/power) > threshold, ]
  nodes <- nodes[(nodes$name %in% prim_df$from | nodes$name %in% prim_df$to |
                  nodes$name %in% sec_df$from), ]
  nodes <- nodes[!duplicated(nodes$name), ]
  nodes <- data.frame(name=nodes$name, genre=nodes$genre)
  links <- setup_final_connections(prim_df, sec_df, nodes)
  nodes <- nodes[(nodes$name %in% links$from | nodes$name %in% links$to), ]
  nodes <- data.frame(name=nodes$name, genre=nodes$genre)
  links <- setup_sorted_graph_links(links, nodes)
  list(links, nodes)
}

get_pic <- function(cast, dir, movs, nodes, point) {
  if (point >= nrow(nodes)) return(NULL)
  element <- nodes[point+1, ]
  url <- "https://image.tmdb.org/t/p/w300_and_h450_bestv2/"
  db <- NULL
  if (element$genre == 0) {
    db <- cast[!duplicated(cast$name), ]
    item = "name"
    image_item = "profile_path"
  } else if(element$genre == -1) {
    db <- dir[!duplicated(dir$name), ]
    item = "name"
    image_item = "profile_path"
  } else {
    db <- movs[!duplicated(movs$title), ]
    item = "title"
    image_item = "poster_path"
  }
  profile_path <- db[db[item] == element$name, ][[image_item]]
  glue(url, profile_path)
}

get_cast_str <- function(actor) {
  glue(
    glue("<h2>{toString(actor$name)}</h2><br/>"),
    glue("Birthday: {toString(actor$birthday)}<br/>"),
    glue("Popularity: {toString(actor$popularity)}<br/><br/>"),
    glue('<p style="font-size: 8pt">{actor$biography}')
  )
}

get_dir_str <- function(direct) {
  glue(
    glue("<h2>{toString(direct$name)}</h2><br/>"),
    glue("Birthday: {toString(direct$birthday)}<br/>"),
    glue("Popularity: {toString(direct$popularity)}<br/><br/>"),
    glue('<p style="font-size: 8pt">{direct$biography}')
  )
}

get_mov_str <- function(mov) {
  glue(
    glue("<h2>{toString(mov$title)}</h2><br/>"),
    glue("Release Date: {toString(mov$release_date)}<br/>"),
    glue("Popularity: {toString(mov$popularity)}<br/><br/>"),
    glue('<p style="font-size: 8pt">{mov$overview}')
  )
}

get_info_str <- function(cast, dir, movs, nodes, point) {
  if (point >= nrow(nodes)) return(NULL)
  element <- nodes[point+1, ]
  url <- "https://image.tmdb.org/t/p/w300_and_h450_bestv2/"
  db <- NULL
  if (element$genre == 0) {
    db <- cast[cast$name == element$name, ]
    db <- db[!duplicated(db$name), ]
    return(get_cast_str(db))
  } else if(element$genre == -1) {
    db <- dir[dir$name == element$name, ]
    db <- db[!duplicated(db$name), ]
    return(get_dir_str(db))
  } else {
    db <- movs[movs$title == element$name, ]
    db <- db[!duplicated(db$title), ]
    return(get_mov_str(db))
  }
}

set_cast_pop_color <- function(nodes, cast, dir, cast_limit=30, dir_limit=10) {
  nodes$colors <- unlist(apply(nodes, 1, function(x) {
    if (x["genre"] == 0) {
      pop = cast[cast$name == x["name"], ]
      pop = pop[!duplicated(pop$name), ]
      pop = pop$popularity
      return(rgb(min(1.0, pop/cast_limit), 0, 0))
    } else if (x["genre"] == -1) {
      pop = dir[dir$name == x["name"], ]
      pop = pop[!duplicated(pop$name), ]
      pop = pop$popularity
      return(rgb(0, min(1.0, pop/dir_limit), 0))
    }
    return(x["colors"])
  }))
  nodes
}

library(igraph)
net_vis <- function(links, nodes) {
  G <- graph_from_data_frame(links, vertices=nodes)
  deg <- degree(G)
  V(G)$color <- factor(V(G)$genre)
  g <- ggnet2(G, size = deg**3, label=TRUE, mode="fruchtermanreingold",
              color="color", palette="Paired", legend.position="none") +
    geom_text(aes(label=label), check_overlap=TRUE, size=deg/2, color="white", alpha=0.6) +
    guides(color = FALSE, size = FALSE) +
    theme(
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    )
  ggplotly(g, source="main", width=1000, height=800)
}

sankey_vis <- function(links, nodes, cast, dir) {
  na_list <- rep(NA, nrow(nodes))
  na_df <- data.frame(from=na_list, to=na_list, source=na_list, target=na_list, value=na_list)
  links <- rbind(na_df, links)
  color_df <- data.frame(genre=double(), color=character())
  nodes$colors <-unlist(lapply(as.numeric(nodes$genre), function(x) {
    if (x %in% color_df$genre) return(color_df[color_df$genre==x,]["color"])
    color_df <<- rbind(color_df, data.frame(genre=x, color=rgb(runif(1)*255, runif(1)*255, ((1+runif(1))/2)*255, maxColorValue=255)))
    return(color_df[color_df$genre==x,]["color"])
  }
  ))
  nodes <- set_cast_pop_color(nodes, cast, dir)
  plot_ly(
    type="sankey",
    orientation="h",
    source="main",
    node = list(
      label=nodes$name,
      color=nodes$colors,
      pad = 0,
      thickness = 50,
      line = list(
        color = "black",
        width = 0
      )
    ),
    link = list(
      source = links$source,
      target = links$target,
      value = links$value,
      color = 'rgba(255,255,127,0.1)'
    ),

    width=1000,
    height=800
    ) %>% layout(
            autosize=T,
            plot_bgcolor = "rgba(0, 0, 0, 0)",
            paper_bgcolor = "rgba(0, 0, 0, 0)",
            font = list(
              size = 10,
              color = "rgba(1, 1, 1, 1)"
            ),
            xaxis = list(showgrid = F, zeroline = F),
            yaxis = list(showgrid = F, zeroline = F)
          )
}

df_ls <- list("Popular Movies" = mov_pop,
              "Top Rated Movies" = mov_top)

pop_cast <- get_standalone_group(mov_pop, "cast")
pop_dir <- get_standalone_group(mov_pop, "dir")
top_cast <- get_standalone_group(mov_top, "cast")
top_dir <- get_standalone_group(mov_top, "dir")

cast_ls <- list("Popular Movies" = pop_cast,
              "Top Rated Movies" = top_cast)
dir_ls <- list("Popular Movies" = pop_dir,
              "Top Rated Movies" = top_dir)

buf_frame <- NULL


server <- function(input, output) {

  nodes <- NULL

  output$plot <- renderPlotly({

    nodes <<- setup_node_color_by_genre(df_ls[[input$df]], cast_ls[[input$df]], dir_ls[[input$df]])
    act_rel <- setup_relation(df_ls[[input$df]], "cast")
    dir_rel <- setup_relation(df_ls[[input$df]], "dir", movie_to_rel=F, num_head = 0)
    act_links <- setup_sorted_graph_links(act_rel, nodes)
    dir_links <- setup_sorted_graph_links(dir_rel, nodes)
    ans <- setup_sankey(act_links, dir_links, nodes, threshold=input$threshold)

    links <- ans[[1]]
    nodes <<- ans[[2]]
    if (input$sankey) {
      plot <- sankey_vis(links, nodes, cast_ls[[input$df]], dir_ls[[input$df]])
    } else {
      plot <- net_vis(links, nodes)
    }
  })
  output$frame <- renderUI({
    eventdat <- event_data('plotly_hover', source="main") # get event data from source main
    if(is.null(eventdat) == T) return(buf_frame)        # If NULL dont do anything
    point <- as.numeric(eventdat[['pointNumber']]) # Index of the data point being charted
    url <- get_pic(cast_ls[[input$df]], dir_ls[[input$df]], df_ls[[input$df]], nodes, point)
    if(is.null(url)) return(buf_frame)
    buf_frame <<- tags$image(src=url, width=300, height=450)
    if (input$sankey) {
      output$description <- renderText(get_info_str(cast_ls[[input$df]], dir_ls[[input$df]], df_ls[[input$df]], nodes, point))
      return(buf_frame)
    } else {
      output$description <- NULL
      buf_frame <<- NULL
      return(NULL)
    }
  })
}
