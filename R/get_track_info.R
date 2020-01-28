get_track_info <- function(track, artist, api_key){

  ## Root path
  rpath <- "http://ws.audioscrobbler.com/2.0/?method=track.getinfo"

  ## Parameters
  params <- list()

  ## Required
  params[['api_key']] <- sprintf('&api_key=%s', api_key)
  params[['artist']] <- sprintf('&artist=%s', artist)
  params[['track']] <- sprintf('&track=%s', track)
  params[['json']] <- '&format=json'


  param_string <- paste0(unlist(params, use.names = F), collapse = '')

  call <- paste0(rpath, param_string, collapse = '')
  res <- jsonlite::fromJSON(call)

  res_tbl <- tibble::tibble(
    track_no = as.numeric(res$track$album$`@attr`$position),
    track = res$track$name,
    duration_seconds = as.numeric(res$track$duration)/1000,
    duration_mins = paste0(duration_seconds %/% 60, ':', str_pad(duration_seconds %% 60, 2, side = 'left', pad = '0')),
    listeners = as.numeric(res$track$listeners),
    playcount = as.numeric(res$track$playcount),
    avg_plays = playcount / listeners,
    artist = res$track$artist$name
  )

  res_tbl
}
