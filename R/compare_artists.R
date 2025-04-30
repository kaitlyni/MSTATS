#' Compare Sentiment Analysis between Two Artists
#'
#' Compares lyrical sentiment analysis betweem two artists and visualizes sentiment score and word frequency distrubution.
#'
#' @param artist_lyrics_1 A dataframe containing song lyrics, including columns named 'song' and 'lyrics'
#' @param artist_lyrics_2 A dataframe containing song lyrics, including columns named 'song' and 'lyrics'
#' @param artist_name_1 Name of first artist
#' @param artist_name_2 Name of second artist
#'
#' @return A List Containing:
#' \item{sentiment_comparison_plot}{Plot comparing overall sentiment score of both artists}
#' \item{sentiment_summary}{summary of sentiment plot}
#' \item{word_frequency_comparison_plot}{Plot comparing word frequency distribution between both artists}
#' @import dplyr
#' @import tidytext
#' @import ggplot2
#' @import wordcloud2
#' @import scales
#' @import textdata
#' @export
#'
#' @examples
#' compare_artists(downward_lyrics, ethelcain_lyrics, "Downward", "Ethel Cain")


compare_artists <- function(artist_lyrics_1, artist_lyrics_2, artist_name_1, artist_name_2) {
  requireNamespace("dplyr")
  requireNamespace("tidytext")
  requireNamespace("ggplot2")
  requireNamespace("wordcloud2")
  requireNamespace("scales")
  requireNamespace("textdata")


  sentiment_1 <- artist_lyrics_1 %>%
    tidytext::unnest_tokens(word, lyrics) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::count(sentiment) %>%
    tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    dplyr::mutate(sentiment = positive - negative, artist = artist_name_1)

  sentiment_2 <- artist_lyrics_2 %>%
    tidytext::unnest_tokens(word, lyrics) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::count(sentiment) %>%
    tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    dplyr::mutate(sentiment = positive - negative, artist = artist_name_2)

  sentiment_data <- dplyr::bind_rows(sentiment_1, sentiment_2)

  sentiment_comparison_plot <- ggplot2::ggplot(sentiment_data, ggplot2::aes(x = artist, y = sentiment, fill = artist)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Comparison of Sentiment Scores Between Artists", y = "Sentiment Score")

  sentiment_summary <- sentiment_data %>%
    dplyr::group_by(artist) %>%
    dplyr::summarize(
      mean_sentiment = mean(sentiment),
      median_sentiment = median(sentiment),
      sd_sentiment = sd(sentiment)
    )

  word_freq_1 <- artist_lyrics_1 %>%
    tidytext::unnest_tokens(word, lyrics) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word") %>%
    dplyr::count(word, sort = TRUE)

  word_freq_2 <- artist_lyrics_2 %>%
    tidytext::unnest_tokens(word, lyrics) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word") %>%
    dplyr::count(word, sort = TRUE)

top_words_1 <- word_freq_1 %>%
  dplyr::top_n(10, n) %>%
  dplyr::mutate(artist = artist_name_1)

top_words_2 <- word_freq_2 %>%
  dplyr::top_n(10, n) %>%
  dplyr::mutate(artist = artist_name_2)

word_freq_data <- dplyr::bind_rows(top_words_1, top_words_2)

  word_frequency_comparison_plot <- ggplot2::ggplot(word_freq_data, ggplot2::aes(x = word, y = n, fill = artist)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Comparison of Word Frequency Between Artists", x = "Word", y = "Frequency") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  list(
    sentiment_comparison_plot = sentiment_comparison_plot,
    sentiment_summary = sentiment_summary,
    word_frequency_comparison_plot = word_frequency_comparison_plot
  )
}
