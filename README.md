### Team Section 
Madeline Garrett, Zandy Boone, Kevin Luth and Katie Stewart

# Analyzing Timeliness and Score

Our first task was to analyze the relationship between the timeliness of an answer and its score. It is though that the first few answers to a question tend to get higher scores than later responses

```{r}
answers <- read_csv("Answers_trunc.csv")
questions <- read_csv("Questions_trunc.csv")

merged <- answers %>%
  left_join(questions, c('ParentId' = 'Id')) %>%
  mutate(total_time = CreationDate.x - CreationDate.y) %>%
  filter(total_time>0)

ggplot(data = (merged)) + 
  geom_jitter(mapping = aes(x = total_time, y = Score.x))
```
Conclusion: We found that despite our thought that the longer the time the lower the score it seems that there is no strong coorelation between either. While there is a spike at the beginning the entire graph does not have a strong enough relationship in order for us to conclude that there is a trend between the first few answers to a question tend to getting higher scores than later responses.


### Overall Features That Affect The Data
* Madeline found that questions that have Wh question words in them  tend to get slightly higher scores than those that don't
* Kevin found that for  both questions and answers, if a link providing further explanation of the topic is included, the ratio of high scores to low scores increases. This means that there is a higher chance of a question or answer with a link to earn a higher score.
* Katie found that answers with fewer characters tend to have higher scores while questions with moderate length (around 50 characters) have higher scores as well.
* Zandy found

#### Madeline's Section

Feature Question: Do questions that start with WH words (Who, What, Where, Why, How, Which, When) have a tendancy to have lower scores? 
Why: I find that I like questiions that are direct and to the point and questions that start with WH words tend to be that way. 

```{r}
questions1
Whdata <- filter(merged, str_detect(merged$Title, "Wh"))  %>%
  group_by(Score.x) 

ggplot(data = Whdata) + 
  geom_boxplot(mapping = aes(x = "", y = Score.x), color = 'lightblue3')+
  coord_cartesian(ylim = c(0, 50))+
  ggtitle("WH Questions in the Data Set")+
  ylab("Score")

ggplot(data = merged) + 
  geom_boxplot(mapping = aes(x = "", y = Score.x), color = 'lavenderblush3') +
  coord_cartesian(ylim = c(0, 50))+
  ggtitle("All Questions in the Data Set")+
  ylab("Score")
    
```
Conclusions: From the two boxplots that I made I can come  to the conclusion that there is slightly higher score for questions that start with Wh words which are (Who, What, Where, Why, How, Which, When). This is probably because these questions get right to the point and don't flip around so people rate them higher. 


#### Kevin's Section: 
* Feature Question: Does adding a link that describes what is being asked or answered affect score?
* Why: Whenever I am looking for answers online, the first answer I see does not always make sense to me so I find it helpful when another explanation or link is provided. This makes me want to know if this is true for all people.

```{r}
questions <- (read_csv("Questions_trunc.csv")) %>%
  select(-7)
answers <- (read_csv("Answers_trunc.csv")) %>%
  select(-7)
a <- questions %>%
  summarise(quantile(Score, 0.90))
#a = 41
b <- answers %>%
  summarise(quantile(Score, 0.90))
#b = 19

q_ind <- questions %>%
  mutate(link = str_detect(Body, "http:")) %>%
  mutate(s = case_when((Score>=41)~"high", ((Score<41)~"low")))
ggplot(data = q_ind) +
  geom_bar(mapping = aes(s, fill = s)) +
  facet_wrap(~ link) +
  labs(title = "Effect of Link on Score for Questions", x = "Score", y = "Count")

a_ind <- answers %>%
  mutate(link = str_detect(Body, "http:")) %>%
  mutate(s = case_when((Score>=19)~"high", ((Score<19)~"low")))
ggplot(data = a_ind) +
  geom_bar(mapping = aes(s, fill = s)) +
  facet_wrap(~ link) +
  labs(title = "Effect of Link on Score for Answers", x = "Score", y = "Count")
```

* Findings: For both questions and answers, if a link providing further explanation of the topic is included, the ratio of high scores to low scores increases. This means that there is a higher chance of a question or answer with a link to earn a higher score.
* Explanation: This finding can be explained by the fact that including a link provides the reader with a second interpretation of what is being described. This means it is more likely the reader will understand what is being said, thus leading to them giving an upvote.

### Katie's Section
* Feature Question: Does the length of a question or answer result in higher scores?
* Why: When using reddit I feel like I typically see more upvotes on longer questions and responses than on shorter questions and responses.
```{r}
answers <- read_csv("Answers_trunc.csv") %>%
  separate(CreationDate, into = c("Date", "Time"), sep = " ", convert = TRUE)%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)%>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":", convert=TRUE)%>%
  mutate(str_count(answers$Body))
questions <- read.csv("Questions_trunc.csv") %>%
  separate(CreationDate, into = c("Date", "Time"), sep = "T", convert = TRUE)%>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-", convert = TRUE)%>%
  separate(Time, into = c("Hour", "Minute", "Second"), sep = ":", convert=TRUE)%>%
  separate(Second, into = c("Second", "Z"), sep = "Z", convert=TRUE)%>%
  mutate(str_count(questions$Title))
questions$Z <- NULL
```
```{r}
ggplot(data = answers)+
  geom_bar(mapping = aes(x = str_count(answers$Body), fill = Score))+
  labs(title = "Answer Length vs Score", x ="Answer Length", y = "Score")
```
```{r}
ggplot(data = questions) +
  geom_bar(mapping = aes(x = str_count(questions$Title), fill = Score))+
  labs(title= "Question Length vs Score", x = "Question Length", y = "Score")
```
Conclusion: I found that shorter answers have higher scores than answers that are extremely long. For questions, lengths around 30-60 characters in length have the highest score and the score decreases steadly once around 75 characters are hit.


### Zandy's Section
* Feature Question: Does the time of year affect the score of a question or answer such as does a question asked or a question answered differe in score based on the summer vs Fall or Fall vs Spring.
* Why: I am interested if upvotes on a question or answer go up during the time of year when there is school or in the summer months and if upvotes go up in say a fall vs. spring semester

## Team Report:
* I, Kevin Luth, looked into whether the inclusion of a link to a source further explaining the topic of discussion leads to higher scores. I first found the score representing the 90th percentile in both the questions and answers dataset using the summarise and quantile function and then mutated a column indicating if the score was high or low based on the 90th percentile number. Next I used str_detect in  both the questions and answers to find any posts that include "http:" which signifies a link. I plotted my results using a bar graph that was color coded to show if the post received a high or low score. I also used a facet_wrap by whether or not the post had a link.

* I, Madeline Garrett, helped to create the team graph by left joiining the data sets. I then created my own individual plot by filtering for questions that start with WH and then making two box plots that show the relationship and mean of those two data sets. I came to the conclustion that WH questions get slightly higher scores than non WH questions. 

* I, Katie Stewart, separated the time of the post into year, month, day, hour, minute and second columns. I also, added a line to each dataset contatining the length of the body for the answers dataset and the length of the title in the question dataset. I then plotted both of my findings using geom bar and added titles as well as changing the x and y axis names.
