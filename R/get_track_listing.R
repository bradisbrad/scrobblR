get_track_listing <- function(album, artist, api_key){

  ## Root path
  rpath <- "http://ws.audioscrobbler.com/2.0/?method=album.getinfo"

  ## Parameters
  params <- list()

  ## Required
  params[['api_key']] <- sprintf('&api_key=%s', api_key)
  params[['artist']] <- sprintf('&artist=%s', artist)
  params[['album']] <- sprintf('&album=%s', album)
  params[['json']] <- '&format=json'


  param_string <- paste0(unlist(params, use.names = F), collapse = '')

  call <- paste0(rpath, param_string, collapse = '')
  res <- jsonlite::fromJSON(call)

  res_tbl <- tibble::tibble(
    track_no = res$album$tracks$track$`@attr`$rank,
    track = res$album$tracks$track$name,
    duration_seconds = as.numeric(res$album$tracks$track$duration),
    duration_mins = paste0(duration_seconds %/% 60, ':', str_pad(duration_seconds %% 60, 2, side = 'left', pad = '0')),
    artist = res$album$artist
  )

  res_tbl %>%
    purrr::map2(artist, track, get_track_info, api_key = api_key)
}
