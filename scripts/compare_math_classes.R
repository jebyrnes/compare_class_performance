#' ------------------------------------------------------------------------
#' Let's look at performance by bio majors in math classes
#' ------------------------------------------------------------------------

# some libraries
library(readxl)
library(ggplot2)
library(dplyr)

theme_set(theme_bw(base_size = 14))

# math class performance data
grades <- read_excel("./data/tmpBioStudentsMathBiology.xlsx")


# let's filter to the calc or stats classes our majors are likely to take at a low level
math_classes <- grades |>
  filter(CLASS_TITLE %in% c("Calculus I", "Survey Of Calculus", "Introductory Statistics")) |>
  mutate(CLASS_TITLE = factor(CLASS_TITLE, levels = c("Calculus I", "Survey Of Calculus", "Introductory Statistics"))) |>
  filter(!(grade_cd %in% c("P", "NA"))) #P and NA are not useful

# a pretty plot of grade distributions
ggplot(math_classes,
       mapping = aes(x = CLASS_TITLE, fill = grade_cd)) +
  geom_bar(position = "fill") + 
  scale_fill_viridis_d()+
  labs(x = "", y = "") +
  scale_y_continuous(labels = paste0(seq(0,100, length.out = 5), "%"))

# Now let's summarize to look at percent at the high and bottom end
math_classes_summary <- math_classes|>
  group_by(CLASS_TITLE) |>
  mutate(total = n()) |>
  group_by(CLASS_TITLE, grade_cd) |>
  summarize(percent = n()/total[1]*100) |>
  ungroup()


ggplot(math_classes_summary |> filter(grade_cd %in% c("A",  "A-", "B+")),
       mapping = aes(x = CLASS_TITLE, y = percent)) +
  geom_col() + 
  facet_wrap(~grade_cd, scale = "free_y")+
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


ggplot(math_classes_summary |> filter(grade_cd %in% c("F",  "W")),
       mapping = aes(x = CLASS_TITLE, y = percent)) +
  geom_col() + 
  facet_wrap(~grade_cd, scale = "free_y") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


math_classes_demo <- math_classes|>
  group_by(CLASS_TITLE, grade_cd) |>
  mutate(total = n()) |>
  group_by(CLASS_TITLE, gender, race_federal, grade_cd) |>
  summarize(percent = n()/total[1]*100) |>
  ungroup() |>
  filter(gender != "U")


ggplot(math_classes_demo,# |> filter(grade_cd %in% c("F")),
       mapping = aes(x = grade_cd, y = percent, fill = gender)) +
  geom_col(position = "stack") + 
  facet_grid(vars(race_federal), vars(CLASS_TITLE), scale = "free_y") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = -90, hjust = 0))
