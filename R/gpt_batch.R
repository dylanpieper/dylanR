#' GPT Batch
#'
#' Using OpenAI's GPT models, generate a completion in a new data frame column called \code{gpt_output}.
#' Completions are batched for error handling and saving your progress,
#' For API etiquette, there is a 2-5s delay between each completion and 30-60s delay between each batch.
#'
#' @param df A data frame (to preserve all columns).
#' @param input An input vector from \code{df} (to generate the completion).
#' @param prompt A system prompt for the GPT model.
#' @param batch_size The number of rows to process in each batch. Default is 10.
#' @param retries The maximum number of retries in case of errors. Default is 1.
#' @param model A GPT model. Default is "gpt-3.5-turbo-1106".
#' @param temperature A temperature for the GPT model. Default is 0.1.
#' @return Writes the GPT completion to \code{gpt_output.RDS}. Writes the progress to \code{last_completed_batch.RDS}.
#' @export
#' @examples
#' library(dylanR)
#'
#' Sys.setenv(OPENAI_API_KEY = "...")
#'
#' objects <- dplyr::tibble(user = c("chair", "table", "book", "pen", "lamp", "phone", "keyboard", "door", "window", "cup"))
#'
#' gpt_batch(
#'   objects,
#'   objects$user,
#'   "write a one sentence description"
#' )
#'
#' objects_described <- readRDS("gpt_output.Rds")
gpt_batch <- function(df, input, prompt, batch_size = 10, retries = 1,
                       model = "gpt-3.5-turbo-1106",
                       temperature = .1) {
  progress <- load_saved_progress()

  if (!is.null(progress)) {
    output <- progress$output
    last_completed_batch <- progress$last_completed_batch
  } else {
    output <- NULL
    last_completed_batch <- 0
  }

  vector_name <- sub(".+\\$", "", deparse(substitute(input)))
  num_batches <- ceiling(nrow(df) / batch_size)

  for (batch_num in (last_completed_batch+1):num_batches) {
    cat("Starting batch ", batch_num, "\n")  ## Changed to batch_num instead of batch_num + 1
    start_row <- (batch_num - 1) * batch_size + 1
    end_row <- min(((batch_num) * batch_size), nrow(df))  ## Changed to (batch_num) instead of (batch_num + 1)

    retry_flag <- TRUE
    counter <- 1
    while (retry_flag && (counter <= retries)) {
      tryCatch(
        {
          output_batch <- batch_mutate(df[start_row:end_row, ],
                                       df_col = input[start_row:end_row],
                                       system_prompt = prompt,
                                       batch_size = batch_size,
                                       model = model,
                                       temperature = temperature
          )
          temp_output <- if (exists("output")) rbind(output, output_batch) else output_batch
          output <- temp_output[!duplicated(temp_output[[vector_name]]), ]

          save_progress(output, batch_num)
          cat("Completed batch", batch_num, "with", nrow(output), "total rows processed", "\n\n") # Changed to batch_num instead of batch_num + 1
          retry_flag <- FALSE
        },
        error = function(e) {
          if (counter >= retries) {
            stop("Maximum retries limit reached")
          } else {
            print(paste(
              "Error occurred, trying again. Data processed up to row", end_row, ": ",
              conditionMessage(e), ". Attempt: ", counter, "."
            ))
            counter <- counter + 1
            Sys.sleep(runif(1, min = 2, max = 5))
          }
        }
      )
    }
    if (nrow(output) == nrow(df)) {
      cat("All rows processed\n")
      break
    }
  }
}

#' Mutate Row
#'
#' This function takes in a data frame \code{df}, a column from the data frame \code{df_col},
#' a system prompt \code{system_prompt}, a GPT model \code{model}, and a temperature \code{temperature}.
#' It uses the GPT model to generate a completion based on the system prompt and the content input.
#' The completion is assigned to the \code{gpt_output} column of the data frame.
#'
#' @param df A data frame.
#' @param df_col A column from the data frame.
#' @param system_prompt A system prompt for the GPT model.
#' @param model A GPT model. Default is "gpt-3.5-turbo".
#' @param temperature A temperature for the GPT model. Default is 0.1.
#' @return A data frame with the mutated row.
mutate_row <- function(df, df_col, system_prompt, model, temperature) {
  content_input <- as.character(df_col)

  if (length(content_input) > 0) {
    completion <- create_chat_completion(
      model = model,
      temperature = temperature,
      messages = list(
        list(
          "role" = "system",
          "content" = system_prompt
        ),
        list(
          "role" = "user",
          "content" = content_input
        )
      )
    )

    tryCatch(
      {
        content_output <- completion$choices$message.content

        if (!is.null(content_output)) {
          df$gpt_output <- rep(content_output, length.out = 1)
        } else {
          message("Error: content_output returned NULL")
          df$gpt_output <- NA
        }
      },
      error = function(e) {
        message("Error occurred: ", conditionMessage(e))
        df$gpt_output <- NA
      }
    )
  } else {
    df$gpt_output <- NA
  }

  return(df)
}

#' Save Progress
#'
#' This function takes in an output and a last completed batch,
#' and saves them as RDS files.
#'
#' @param output The output to be saved.
#' @param last_completed_batch The last completed batch to be saved.
save_progress <- function(output, last_completed_batch) {
  saveRDS(output, file = "gpt_output.RDS")
  saveRDS(last_completed_batch, file = "last_completed_batch.RDS")
}

#' Load Saved Progress
#'
#' This function loads the previously saved output and last completed batch
#' from RDS files.
#'
#' @return A list containing the loaded output and last completed batch,
#' or NULL if no existing saved progress is found.
load_saved_progress <- function() {
  if (file.exists("gpt_output.RDS") && file.exists("last_completed_batch.RDS")) {
    return(list(
      output = readRDS(file = "gpt_output.RDS"),
      last_completed_batch = readRDS(file = "last_completed_batch.RDS")
    ))
  } else {
    warning("No existing saved progress found")
    return(NULL)
  }
}

#' Batch Mutate
#'
#' This function applies the \code{mutate_row} function in batches to a data frame.
#' It processes a specified number of rows at a time and saves the progress.
#' It returns the final output data frame after all the rows have been processed.
#'
#' @param df A data frame.
#' @param df_col A column from the data frame.
#' @param system_prompt A system prompt for the GPT model.
#' @param batch_size The number of rows to process in each batch. Default is 100.
#' @param model A GPT model. Default is "gpt-3.5-turbo".
#' @param temperature A temperature for the GPT model. Default is 0.1.
#' @return The final output data frame after processing all the rows.
batch_mutate <- function(df, df_col, system_prompt, batch_size, model, temperature) {
  out <- vector("list", 0)
  rows_processed <- 0
  total_rows <- nrow(df)

  while (rows_processed < total_rows) {
    start_row <- rows_processed + 1
    end_row <- min(rows_processed + batch_size, total_rows)
    batch_out <- vector("list", end_row - start_row + 1)

    for (i in start_row:end_row) {
      batch_out[[i - start_row + 1]] <- mutate_row(df[i, ], df_col[i], system_prompt,
        model = model,
        temperature = temperature
      )
      cat("Processed row", i, "\n")
      Sys.sleep(runif(1, min = 0.1, max = 0.5))
    }

    rows_processed <- end_row
    out <- c(out, batch_out)
  }

  Sys.sleep(runif(1, min = 30, max = 60))
  out <- do.call(rbind, out)
  return(out)
}
