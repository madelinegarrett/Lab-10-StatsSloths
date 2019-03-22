# Lab-10-StatsSloths
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(tidyverse)
```

```{r}
answers <- read_csv("Answers_trunc.csv")
questions <- read_csv("Questions_trunc.csv")
questions <- setnames(questions, old=c("Id"), new=c("ParentId"))
merged <- answers %>%
  left_join(questions, by = "ParentId") %>%
  mutate(total_time = CreationDate.x - CreationDate.y) 

merged 

ggplot(data = (merged)) + 
  geom_jitter(mapping = aes(x = total_time, y = Score.x))

```
```{r}
answers <- read_csv("Answers_trunc.csv") %>%
  separate(CreationDate, into = c("Date", "Time"), sep = " ", convert = TRUE)%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)%>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":", convert=TRUE)
questions <- read.csv("Questions_trunc.csv") %>%
  separate(CreationDate, into = c("Date", "Time"), sep = "T", convert = TRUE)%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)%>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":", convert=TRUE)%>%
  separate(Second, into = c("Second", "Z"), sep = "Z", convert=TRUE)
questions$Z <- NULL
```
