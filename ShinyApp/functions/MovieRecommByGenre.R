library(data.table)
library(dplyr)

get_movie_genre_recomm1 <- function(genre_input,movie_genre,ratings) {
  
  if(genre_input == "Children"){
    userSelectedGenre = "Children's"
  }
  else{
    userSelectedGenre = genre_input  
  }
  
  #write.csv(movie_genre,"movie_genre.csv")
  
  dat.query <- filter(movie_genre, Genre1==userSelectedGenre|Genre2==userSelectedGenre|Genre3==userSelectedGenre|Genre4==userSelectedGenre|Genre5==userSelectedGenre|Genre6==userSelectedGenre)
  MoviesByGenre <- ratings %>% filter(MovieID %in% dat.query$MovieID)
  
  totalmovies = nrow(dat.query)
  totalreviews = nrow(MoviesByGenre)
  
  genre_matrix = matrix(0,totalmovies,11)
  
  colnames(genre_matrix) = c("MovieID","totalratingbygenre","totalratingbyMovie","5star","4star","3star","2star","1star","AvgStarRating","TotalWeight","CalcRank")
  
  genre_matrix[,1] = as.integer(dat.query[,"MovieID"])
  genre_matrix[,2] = as.integer(totalreviews)
  
  for (i in 1:nrow(genre_matrix)){
    tmp = MoviesByGenre %>% filter(MovieID %in% genre_matrix[i,1])
    genre_matrix[i,3] = as.integer(nrow(tmp))
    star.5 = tmp %>% filter(Rating %in% 5)
    star.4 = tmp %>% filter(Rating %in% 4)
    star.3 = tmp %>% filter(Rating %in% 3)
    star.2 = tmp %>% filter(Rating %in% 2)
    star.1 = tmp %>% filter(Rating %in% 1)
    genre_matrix[i,4] = as.integer(nrow(star.5))
    genre_matrix[i,5] = as.integer(nrow(star.4))
    genre_matrix[i,6] = as.integer(nrow(star.3))
    genre_matrix[i,7] = as.integer(nrow(star.2))
    genre_matrix[i,8] = as.integer(nrow(star.1))
    genre_matrix[i,9] = ( (genre_matrix[i,4] * 5) + (genre_matrix[i,5]*4) + (genre_matrix[i,6]*3) + (genre_matrix[i,7]*2) + (genre_matrix[i,8]*1))/genre_matrix[i,3]
    genre_matrix[i,10] = genre_matrix[i,3]/genre_matrix[i,2]
    genre_matrix[i,11] = genre_matrix[i,9] * genre_matrix[i,10]
    
  } 
  
  result = genre_matrix[order(genre_matrix[,11],decreasing = TRUE),]
  
  pred_movie_id = c(result[1:10,1])
  
  return(pred_movie_id)
  
}


get_movie_genre_recomm2 <- function(genre_input,movie_genre,ratings) {
  
  if(genre_input == "Children"){
    userSelectedGenre = "Children's"
  }
  else{
    userSelectedGenre = genre_input  
  }
  
  dat.query <- filter(movie_genre, Genre1==userSelectedGenre|Genre2==userSelectedGenre|Genre3==userSelectedGenre|Genre4==userSelectedGenre|Genre5==userSelectedGenre|Genre6==userSelectedGenre)
  MoviesByGenre <- ratings %>% filter(MovieID %in% dat.query$MovieID)
  
  totalmovies = nrow(dat.query)
  totalreviews = nrow(MoviesByGenre)
  
  # Calculate minimum number of ratings in the filtered genre
  genre_matrix = matrix(0,totalmovies,6)
  m = quantile(MoviesByGenre$Rating,0.90)
  
  colnames(genre_matrix) = c("MovieID","v","m","R","C","Score")
  genre_matrix[,1] = as.integer(dat.query[,"MovieID"])
  
  for (i in 1:nrow(genre_matrix)){
    tmp = MoviesByGenre %>% filter(MovieID %in% genre_matrix[i,1])
    n = as.integer(nrow(tmp))
    star.5 = tmp %>% filter(Rating %in% 5)
    star.4 = tmp %>% filter(Rating %in% 4)
    star.3 = tmp %>% filter(Rating %in% 3)
    star.2 = tmp %>% filter(Rating %in% 2)
    star.1 = tmp %>% filter(Rating %in% 1)
    # total number of ratings for this movie
    genre_matrix[i,"v"] = n
    # minimum number of ratings needed 
    genre_matrix[i,"m"] = m
    #average rating for the movie
    genre_matrix[i,"R"] = ((nrow(star.5) * 5) + (nrow(star.4) * 4) + (nrow(star.3) * 3) + (nrow(star.2) * 2) + (nrow(star.1) * 1))/n
  }
  
  #mean of average rating for this genre
  genre_matrix[,"C"] = mean(genre_matrix[,"R"])
  
  for (i in 1:nrow(genre_matrix)){
    genre_matrix[i,"Score"] = ((genre_matrix[i,"R"] * genre_matrix[i,"v"]) + (genre_matrix[i,"C"] * genre_matrix[i,"m"]))/(genre_matrix[i,"v"] + genre_matrix[i,"m"]) 
  }  
  
  result = genre_matrix[order(genre_matrix[,"Score"],decreasing = TRUE),]
  
  pred_movie_id = c(result[1:10,1])
  
  return(pred_movie_id)
  
}