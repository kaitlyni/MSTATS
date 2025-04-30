#' Compare Sentiment Between Two Songs
#'
#' Compares sentiment scores and word contribution between two songs
#'
#' @param lyrics_df A dataframe containing song lyrics, including columns named 'song' and 'lyrics'
#' @param song_title_1 Name of first song
#' @param song_title_2 Name of second song
#'
#' @return A List Containing:
#' \item{sentiment_comparison_plot}{barplot comparing overall sentiment of two songs}
#' \item{word_contribution_plot}{plot showing top contributing sentiment words per song}
#' @import dplyr
#' @import ggplot2
#' @import tidytext
#' @export
#'
#' @examples
#' compare_songs(ethelcain_lyrics, "American Teenager", "Strangers")


compare_songs <- function(lyrics_df, song_title_1, song_title_2) {
  requireNamespace("dplyr")
  requireNamespace("tidytext")
  requireNamespace("ggplot2")

  selected_lyrics <- lyrics_df %>%
    dplyr::filter(song %in% c(song_title_1, song_title_2))

  tokenized <- selected_lyrics %>%
    tidytext::unnest_tokens(word, lyrics) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::group_by(song, word, sentiment) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(
      score = ifelse(sentiment == "positive", 1, -1),
      contribution = count * score
    )

  sentiment_scores <- tokenized %>%
    dplyr::group_by(song) %>%
    dplyr::summarize(sentiment_score = sum(contribution))

  sentiment_comparison_plot <- ggplot2::ggplot(sentiment_scores, ggplot2::aes(x = song, y = sentiment_score, fill = song)) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "Overall Sentiment Score by Song", x = "Song", y = "Sentiment Score")

  word_contribution_plot <- tokenized %>%
    dplyr::group_by(song) %>%
    dplyr::top_n(10, wt = abs(contribution)) %>%
    ggplot2::ggplot(ggplot2::aes(x = tidytext::reorder_within(word, contribution, song), y = contribution, fill = sentiment)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~song, scales = "free_y") +
    tidytext::scale_x_reordered() +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Top Sentiment Contributing Words per Song", x = "Word", y = "Contribution")

  return(list(
    sentiment_comparison_plot = sentiment_comparison_plot,
    word_contribution_plot = word_contribution_plot
  ))
}
