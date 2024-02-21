# gpt_batch()

Use OpenAI's GPT model to conduct natural language tasks using rows from a column as the input. The **batching** feature is particularly useful for completing tasks on large datasets with little to no supervision.

![](data/dylanR.png)

```{r}
library(dylanR) # devtools::install_github("dylanpieper/dylanR")

Sys.setenv(OPENAI_API_KEY = "...")

objects <- dplyr::tibble(user = c("screw the world", "love the world", "the world is round"))

gpt_batch(
  df = objects,
  input = objects$user,
  prompt = "classify the sentiment using one word: 'positive', 'negative', or 'neutral'.",
  batch_size = 3, # batching adds a delay and saves your progress at each iteration
  temperature = 0.8
)

objects_described <- readRDS("gpt_output.RDS")

print(objects_described)

# user               gpt_output
# <chr>              <chr>
# screw the world    negative
# love the world     positive
# the world is round neutral
```
