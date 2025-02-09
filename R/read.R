read_chat <- function(file_path, exclude = NULL) {
  lines <- readLines(file_path, warn = FALSE)
  timestamp_pattern <- "^\\d{2}/\\d{2}/\\d{4},"

  # Pre-allocate list with max possible messages
  result <- vector("list", length(lines))
  msg_index <- 0

  current_time <- ""
  current_sender <- NA
  current_content <- character()
  current_type <- ""

  for (line in lines) {
    if (grepl(timestamp_pattern, line)) {
      # Store previous message
      if (current_time != "") {
        msg_index <- msg_index + 1
        result[[msg_index]] <- list(
          time = current_time,
          sender = current_sender,
          content = paste(current_content, collapse = "\n"),
          type = current_type
        )
      }

      # Extract time and content
      parts <- strsplit(line, " - ", fixed = TRUE)[[1]]
      current_time <- parts[1]
      message_content <- parts[2]

      # Extract sender and message text
      msg_parts <- strsplit(message_content, ": ", fixed = TRUE)[[1]]
      if (length(msg_parts) > 1) {
        current_sender <- msg_parts[1]
        current_content <- msg_parts[2]
        current_type <- ifelse(current_content == "<Media omitted>", "media",
                               ifelse(current_content == "This message was deleted", "deleted", "text"))
      } else {
        current_sender <- NA
        current_content <- message_content
        current_type <- "settings"
      }

      # Store content as vector
      current_content <- c(current_content)
    } else {
      # Append continuation lines efficiently
      current_content <- c(current_content, line)
    }
  }

  # Store last message
  if (current_time != "") {
    msg_index <- msg_index + 1
    result[[msg_index]] <- list(
      time = current_time,
      sender = current_sender,
      content = paste(current_content, collapse = "\n"),
      type = current_type
    )
  }

  # Trim list to actual size
  result <- result[1:msg_index]

  # Convert to dataframe properly
  df <- as.data.frame(do.call(rbind, result), stringsAsFactors = FALSE)
  df <- as.data.frame(lapply(df, unlist))

  # Convert columns to proper types
  df$sender <- factor(df$sender)
  df$type <- factor(df$type, levels = c("text", "media", "deleted", "settings"))

  # Apply exclusion filter if needed
  if (!is.null(exclude)) {
    df <- df[!(df$type %in% exclude), ]
    row.names(df) <- NULL  # Reset row indices
  }

  return(df)
}

# Example usage:
# df <- read_chat("chat.txt")
# print(df)
