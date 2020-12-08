source('functions/MovieRecommByGenre.R')

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# reading the movies data

MoviesDataURL = paste0("data/","movies.dat")
RatingsDataURL = paste0("data/","ratings.dat")
UserDataURL = paste0("data/","users.dat")

movies = readLines(MoviesDataURL)
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
movies$seqno = 1:nrow(movies)

# reading the ratings data
ratings = read.csv(RatingsDataURL, sep = ':', 
                   colClasses = c('integer', 'NULL'), header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# Splitting the genres into different Movie genre columns
movie_genre_df = as.data.frame(movies$Genres,stringsAsFactors = FALSE)
movie_genre_df = as.data.frame(tstrsplit(movie_genre_df[,1],"[|]",type.convert = TRUE))
colnames(movie_genre_df) = c("Genre1","Genre2","Genre3","Genre4","Genre5","Genre6")
movie_genre_df[,"MovieID"] = movies$MovieID

shinyServer(function(input, output, session) {
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbutton is clicked
  df1 <- eventReactive(input$btn1,{    
    withBusyIndicatorServer("btn1", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      genre_movie_ids = get_movie_genre_recomm2(input$genrelist,movie_genre_df,ratings)
      
      movieidlist = c()
      movietitlelist = c()
      
      for (i in 1:length(genre_movie_ids)) {
        movieid = genre_movie_ids[i]
        
        movieidseqno = movies[movies$MovieID==movieid,5]
        movietitle = movies[movies$MovieID==movieid,2]
        
        movieidlist= c(movieidlist,movieidseqno)
        movietitlelist = c(movietitlelist,movietitle)
      }
      
      #M1 <- paste(movieidlist, collapse = "#")
      #M2 <- paste(movietitlelist, collapse = "#")
      
      #output$verb1 <- renderText({ M1 })
      #output$verb2 <- renderText({ M2 })
      
      recom_results1 <- data.table(Rank = 1:10, 
                                   MovieID = movieidlist, 
                                   Title = movietitlelist)
      
    }
    )
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results1 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_results1 <- df1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_results1$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_results1$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  # Calculate recommendations when the sbutton is clicked
  df <- eventReactive(input$btn2,{    
      withBusyIndicatorServer("btn2", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      user_results = (1:10)/10
      user_predicted_ids = 1:10
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    })
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function