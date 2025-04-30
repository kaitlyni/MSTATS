#' Create a word cloud of the most frequently used words in an artist's songs
#'
#' Generates a wordcloud by removing stopwords, tokenizing lyrics, and visualizes most frequently used words
#'
#' @param lyrics_df A dataframe containing song lyrics, including columns named 'song' and 'lyrics'
#' @param artist_name The artist that is going to be analyzed
#'
#' @return A word cloud plot of the most frequently used words in the artist's songs
#' @import dplyr
#' @import tidytext
#' @import wordcloud2
#' @export
#'
#' @examples
#' word_cloud(downward_lyrics, "Downward")


word_cloud <- function(lyrics_df, artist_name) {
  requireNamespace("dplyr")
  requireNamespace("tidytext")
  requireNamespace("wordcloud2")

  artist_lyrics <- lyrics_df %>%
    dplyr::filter(artist == artist_name)

  if (nrow(artist_lyrics) == 0) {
    stop("Artist not found.")
  }

  word_frequencies <- artist_lyrics %>%
    tidytext::unnest_tokens(word, lyrics) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word") %>%
    dplyr::count(word, sort = TRUE)

  wordcloud2::wordcloud2(word_frequencies)
}
