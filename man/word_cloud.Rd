% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/word_cloud.R
\name{word_cloud}
\alias{word_cloud}
\title{Create a word cloud of the most frequently used words in an artist's songs}
\usage{
word_cloud(lyrics_df, artist_name)
}
\arguments{
\item{lyrics_df}{A dataframe containing song lyrics, including columns named 'song' and 'lyrics'}

\item{artist_name}{The artist that is going to be analyzed}
}
\value{
A word cloud plot of the most frequently used words in the artist's songs
}
\description{
Generates a wordcloud by removing stopwords, tokenizing lyrics, and visualizes most frequently used words
}
\examples{
word_cloud(downward_lyrics, "Downward")
}
