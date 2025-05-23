% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_artists.R
\name{compare_artists}
\alias{compare_artists}
\title{Compare Sentiment Analysis between Two Artists}
\usage{
compare_artists(artist_lyrics_1, artist_lyrics_2, artist_name_1, artist_name_2)
}
\arguments{
\item{artist_lyrics_1}{A dataframe containing song lyrics, including columns named 'song' and 'lyrics'}

\item{artist_lyrics_2}{A dataframe containing song lyrics, including columns named 'song' and 'lyrics'}

\item{artist_name_1}{Name of first artist}

\item{artist_name_2}{Name of second artist}
}
\value{
A List Containing:
\item{sentiment_comparison_plot}{Plot comparing overall sentiment score of both artists}
\item{sentiment_summary}{summary of sentiment plot}
\item{word_frequency_comparison_plot}{Plot comparing word frequency distribution between both artists}
}
\description{
Compares lyrical sentiment analysis betweem two artists and visualizes sentiment score and word frequency distrubution.
}
\examples{
compare_artists(downward_lyrics, ethelcain_lyrics, "Downward", "Ethel Cain")
}
