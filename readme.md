# gpt_batch()

![](data/dylanR.png)

```{r}
library(dylanR) # devtools::install_github("dylanpieper/dylanR")

Sys.setenv(OPENAI_API_KEY = "...")

objects <- dplyr::tibble(user = c("screw the world", "love the world", "the world is round"))

gpt_batch(
  objects,
  objects$user,
  batch_size = 3,
  temperature = 0.8,
  "classify the sentiment using one word: 'positive', 'negative', or 'neutral'."
)

objects_described <- readRDS("gpt_output.RDS")

print(objects_described)

# user               gpt_output
# <chr>              <chr>
# screw the world    negative
# love the world     positive
# the world is round neutral
```
