#' Analyze the theme of lyrics
#'
#' Perform sentiment analysis using the Bing lexicon from the tidytext package
#'
#' @param lyrics_df A dataframe containing song lyrics, including columns named 'song' and 'lyrics'
#' @param song_title The title of the song to analyze
#'
#' @return A data frame with sentiment score, and number of positive and negative words
#' @import dplyr
#' @import tidytext
#' @export
#'
#' @examples
#' get_sentiment(downward_lyrics, "Line")


get_sentiment <- function(lyrics_df, song_title) {
  requireNamespace("dplyr")
  requireNamespace("tidytext")

  song_lyrics <- lyrics_df %>%
    dplyr::filter(song == song_title)

  if (nrow(song_lyrics) == 0) {
    stop("Song not found.")
  }

  sentiment_data <- song_lyrics %>%
    tidytext::unnest_tokens(word, lyrics) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::count(sentiment)

  sentiment_summary <- sentiment_data %>%
    tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    dplyr::mutate(sentiment_score = positive - negative)

  return(sentiment_summary)
}
