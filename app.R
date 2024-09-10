library(shiny)
library(bslib)
library(glue)
library(httr)
library(jsonlite)
library(digest)
library(memoise)
library(typedjs)

OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
GITHUB_API_KEY <- Sys.getenv("GITHUB_API_KEY")

# Memoized API functions
fetch_github_profile <- memoise(function(username) {
  url <- paste0("https://api.github.com/users/", username)
  
  headers <- add_headers(
    "Accept" = "application/json",
    "User-Agent" = "github-roast.pages.dev"
  )
  
  if (GITHUB_API_KEY != "") {
    headers <- c(headers, add_headers("Authorization" = paste("token", GITHUB_API_KEY)))
  }
  
  response <- GET(url, headers)
  if (response$status_code == 200) {
    return(content(response, as = "parsed"))
  } else {
    stop("Failed to fetch GitHub profile.")
  }
})

fetch_github_repos <- memoise(function(username) {
  url <- paste0("https://api.github.com/users/", username, "/repos?sort=updated&per_page=10")
  
  headers <- add_headers(
    "Accept" = "application/json",
    "User-Agent" = "github-roast.pages.dev"
  )
  
  response <- GET(url, headers)
  if (response$status_code == 200) {
    return(content(response, as = "parsed"))
  } else {
    stop("Failed to fetch GitHub repositories.")
  }
})

fetch_readme <- memoise(function(username) {
  readme_urls <- c(
    paste0("https://raw.githubusercontent.com/", username, "/", username, "/main/README.md"),
    paste0("https://raw.githubusercontent.com/", username, "/", username, "/master/README.md")
  )
  
  for (url in readme_urls) {
    response <- GET(url)
    if (response$status_code == 200) {
      return(content(response, as = "text"))
    }
  }
  
  return("")
})

# OpenAI API function
generate_roast <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o-mini",
      temperature = 1,
      messages = list(
        list(role = "system", content = ""),
        list(role = "user", content = prompt)
      )
    )
  )
  
  content <- content(response, as = "parsed")
  return(content$choices[[1]]$message$content)
}

# UI
ui <- page(
  lang = "en",
  includeCSS("www/style.css"),
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/gsap/3.9.1/gsap.min.js"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
  ),
  
  fixedPanel(
    bottom = 10, left = 10,
    HTML('<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="noamane" data-color="#FFDD00" data-emoji=""  data-font="Cookie" data-text="Buy me a coffee" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script>')
  ),
  
  fluidRow(
    column(6,
           div(style = "padding: 100px;",
               card(
                 card_header(
                   card_title(
                     fluidRow(
                       tags$div(class = "flex-grow-1 mr-2",
                                tags$div(class = "input-group",
                                         tags$div(class = "input-group-prepend",
                                                  tags$span(class = "input-group-text btn-light", style = "border-radius: 0.25rem 0 0 0.25rem;", "https://github.com/")
                                         ),
                                         tags$input(type = "text", class = "form-control", id = "username", placeholder = "hadley")
                                )
                       )
                     ),
                     br(),
                     fluidRow(
                       column(6, actionButton(inputId = "roast", label = "🔥 Roast", width = "100%", class = "btn btn-light")),
                       column(6, actionButton(inputId = "compliment", label = "❤️ Compliment", width = "100%", class = "btn btn-light"))
                     )
                   )
                 ),
                 card_body(
                   fluidRow(
                     column(12, uiOutput("response"))
                   )
                 )
               )
           )
    ),
    column(6, uiOutput("cardolo"))
  )
)

# Server
server <- function(input, output, session) {
  github_data <- reactiveVal(NULL)
  
  # Function to fetch GitHub data
  fetch_github_data <- function(username) {
    tryCatch({
      profile <- fetch_github_profile(username)
      repos <- fetch_github_repos(username)
      readme <- fetch_readme(username)
      
      list(
        profile = profile,
        repos = repos,
        readme = readme
      )
    }, error = function(e) {
      showNotification(paste("Error fetching data:", e$message), type = "error")
      NULL
    })
  }
  
  # Generate GitHub roast or compliment
  generate_github_roast <- function(username, roast = TRUE) {
    data <- github_data()
    
    profile_data <- list(
      name = data$profile$name,
      bio = data$profile$bio,
      company = data$profile$company,
      location = data$profile$location,
      followers = data$profile$followers,
      following = data$profile$following,
      public_repos = data$profile$public_repos,
      profile_readme = data$readme,
      last_10_repositories = lapply(data$repos, function(repo) {
        list(
          name = repo$name,
          description = repo$description,
          language = repo$language,
          stargazers_count = repo$stargazers_count,
          open_issues_count = repo$open_issues_count,
          license = repo$license,
          fork = repo$fork
        )
      })
    )
    
    if (roast) {
      prompt <- paste("Give a short and harsh roast with some emojis for the following GitHub profile:", username,
                      ". Here are the details:", toJSON(profile_data))
    } else {
      prompt <- paste("Give a short thoughtful, motivating, and positive compliment with some emojis for the following GitHub profile:", username,
                      ". Highlight their strengths and achievements. Here are the details:", toJSON(profile_data))
    }
    
    generate_roast(prompt)
  }
  
  # Function to handle button clicks
  handle_button_click <- function(roast = TRUE) {
    req(input$username)
    username <- input$username
    
    # Fetch GitHub data
    data <- fetch_github_data(username)
    if (is.null(data)) return()
    
    github_data(data)
    
    # Generate and display roast/compliment
    result <- generate_github_roast(username, roast)
    output$response <- renderUI({
      typedjs::typed(result)
    })
    
    # Update GitHub card
    output$cardolo <- renderUI({
      req(github_data())
      data <- github_data()
      
      div(
        id = "app",
        tags$section(
          class = "ticket",
          tags$header(
            class = "front",
            tags$div(class = "holo"),
            tags$img(class = "logo", src = "https://www.svgrepo.com/show/353783/github-octocat.svg"),
            tags$aside(class = "divider")
          ),
          tags$section(
            class = "back",
            tags$div(class = "holo"),
            tags$div(
              class = "data",
              tags$a(href = 'https://github.com/anuraghazra/github-readme-stats',
                     tags$img(width = "100%", align = "center", src = glue("https://github-readme-stats.vercel.app/api?username={username}&show_icons=true&theme=transparent"))
              ),
              br(),
              tags$a(href = 'https://github.com/anuraghazra/github-readme-stats',
                     tags$img(width = "100%", align = "center", src = glue("https://github-readme-stats.vercel.app/api/top-langs/?username={username}&layout=compact&theme=transparent"))
              )
            ),
            tags$aside(
              class = "divider",
              tags$div(
                tags$img(id = "avatar", src = data$profile$avatar_url, alt = ""),
                tags$h3(data$profile$name)
              )
            )
          )
        ),
        includeScript("www/cust.js")
      )
    })
  }
  
  # Roast button
  observeEvent(input$roast, {
    handle_button_click(roast = TRUE)
  })
  
  # Compliment button
  observeEvent(input$compliment, {
    handle_button_click(roast = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)