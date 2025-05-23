% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_songs.R
\name{compare_songs}
\alias{compare_songs}
\title{Compare Sentiment Between Two Songs}
\usage{
compare_songs(lyrics_df, song_title_1, song_title_2)
}
\arguments{
\item{lyrics_df}{A dataframe containing song lyrics, including columns named 'song' and 'lyrics'}

\item{song_title_1}{Name of first song}

\item{song_title_2}{Name of second song}
}
\value{
A List Containing:
\item{sentiment_comparison_plot}{barplot comparing overall sentiment of two songs}
\item{word_contribution_plot}{plot showing top contributing sentiment words per song}
}
\description{
Compares sentiment scores and word contribution between two songs
}
\examples{
compare_songs(ethelcain_lyrics, "American Teenager", "Strangers")
}
