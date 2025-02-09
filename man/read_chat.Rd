\name{read_chat}
\alias{read_chat}
\title{Read and Parse a WhatsApp Chat Export File}
\description{
  Parses a WhatsApp chat text file and returns a structured dataframe with message details.
}
\usage{
  read_chat(file_path, exclude = NULL)
}
\arguments{
  \item{file_path}{Character string specifying the path to the chat text file.}
  \item{exclude}{Optional character vector specifying message types to exclude. Possible values: \code{"settings"}, \code{"media"}, \code{"deleted"}, \code{"text"}. Default is \code{NULL} (no exclusions).}
}
\details{
  This function reads a WhatsApp chat export file, identifies message timestamps, and extracts the sender, content, and message type. If a line does not start with a timestamp, it is treated as a continuation of the previous message.

  Messages are classified into the following types:
  \itemize{
    \item \code{"settings"} - System messages without a sender.
    \item \code{"media"} - Messages containing \code{"<Media omitted>"}.
    \item \code{"deleted"} - Messages containing \code{"This message was deleted"}.
    \item \code{"text"} - Regular chat messages with a sender.
  }

  The function returns a dataframe where \code{sender} and \code{type} columns are stored as factors.
}
\value{
  A dataframe with four columns:
  \tabular{ll}{
    \strong{Column} \tab \strong{Description} \cr
    \code{time} \tab Timestamp of the message. \cr
    \code{sender} \tab Name of the sender (NA for system messages). \cr
    \code{content} \tab Message content. \cr
    \code{type} \tab Message type (\code{"settings"}, \code{"media"}, \code{"deleted"}, or \code{"text"}). \cr
  }

  If \code{exclude} is specified, messages of those types are removed from the output.
}
\examples{
# Read full chat without exclusions
df <- read_chat("example.txt")

# Exclude system messages
df <- read_chat("example.txt", exclude = "settings")

# Exclude media and deleted messages
df <- read_chat("example.txt", exclude = c("media", "deleted"))
}

