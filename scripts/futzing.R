#' ------------------------------------------------------------------------
#' Let's look at performance by bio majors in bio VERSYS math classes
#' ------------------------------------------------------------------------

# some libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

theme_set(theme_bw(base_size = 14))

#### Load and prep grades ####
grades <- read_excel("./data/tmpBioStsMathBioCrses_combined.xlsx") |>
  #for ease of filtering and use on course number
  mutate(CATALOG_NBR = as.numeric(CATALOG_NBR)) |> 
  filter(!is.na(CATALOG_NBR)) |>

  #not useful grades
  filter(!(grade_cd %in% c("P", "-", "AUD", "NA"))) |> 

  #make grades factors for plotting
  mutate(grade_cd = factor(grade_cd, levels = rev(sort(unique(grade_cd))))) |>
  
  # ethnicities with too few to be useful
  filter(!(race_federal %in% c("0MULTI", "5HAWPAC", "6INDALK")))

##
# let's filter to the calc, stats and bio 100s and 290 (which uses math)
classes <- grades |>
  filter(CLASS_TITLE %in% c("Calc I for Life & Environ Sci", "Calculus I", 
                            "Survey Of Calculus", "Introductory Statistics",
                            "General Biology I", "General Biology II",
                            "Population Biology"
                            ))


# Let's look at relationship between grades in Calc I and general bio
classes |>
  filter(CLASS_TITLE %in% c("Calculus I", 
                            "General Biology I"
  )) |>
  group_by(EMPLID) |>
  mutate(has_both = length(unique(CLASS_TITLE)) == 2) |>
  ungroup() |>
  filter(has_both) |>
  group_by(EMPLID, CLASS_TITLE) |> 
  slice(n()) |> #last time they took it
  group_by(EMPLID, CLASS_TITLE) |> 
  select(EMPLID, gender, race_federal, residency_tuition, college_short, 
         dept_short, CLASS_TITLE, grade_cd) |>
  pivot_wider(names_from = "CLASS_TITLE", values_from = "grade_cd") |>
  group_by(`Calculus I`, `General Biology I`) |>
  summarize(count = n(),
            id = paste(`Calculus I`, `General Biology I`)[1]) |>
  ungroup() |>
  pivot_longer(cols = c(`Calculus I`, `General Biology I`),
               names_to = "CLASS_TITLE", values_to = "grade_cd") |>
  filter(grade_cd != "NA") |>
  ggplot(aes(x = CLASS_TITLE, y = grade_cd, group = id)) +
  geom_line(aes(linewidth = count, alpha = count)) +
  scale_linewidth_continuous(range = c(0.5,3))


####
library(ggalluvial)

classes_wide <- classes |>
  filter(CLASS_TITLE %in% c("Calculus I", 
                            "General Biology I"
  )) |>
  group_by(EMPLID) |>
  mutate(has_both = length(unique(CLASS_TITLE)) == 2) |>
  ungroup() |>
  filter(has_both) |>
  group_by(EMPLID, CLASS_TITLE) |> 
  slice(n()) |> #last time they took it
  group_by(EMPLID, CLASS_TITLE) |> 
  select(EMPLID, gender, race_federal, residency_tuition, college_short, 
         dept_short, CLASS_TITLE, grade_cd) |>
  pivot_wider(names_from = "CLASS_TITLE", values_from = "grade_cd") |>
  group_by(`Calculus I`, `General Biology I`) |>
  summarize(count = n(),
            id = paste(`Calculus I`, `General Biology I`)[1]) 


ggplot(classes_wide, 
       aes(x = `Calculus I`, y = `General Biology I`, size = count)) +
  geom_point()

ggplot(classes_wide, 
    aes(y = count, axis1 = `General Biology I`, axis2 = `Calculus I`)) +
  geom_alluvium( aes(fill = count), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("bio", "calc"), expand = c(.05, .05))
#  scale_x_discrete(limits = c("Bio", "Calc"), expand = c(.05, .05)) +
# scale_fill_brewer(type = "div", palette = "Set1") 
  
####
classes |>
  filter(CLASS_TITLE %in% c("Calculus I", 
                            "General Biology I"
  )) |>
  mutate(CLASS_TITLE = factor(CLASS_TITLE, levels = rev(unique(CLASS_TITLE)))) |>
  group_by(EMPLID) |>
  mutate(has_both = length(unique(CLASS_TITLE)) == 2) |>
  ungroup() |>
  filter(has_both) |>
  group_by(EMPLID, CLASS_TITLE) |> 
  slice(n()) |> #last time they took it
  ungroup() |>
  mutate(performance = factor(grade_cd),
         grade_cd = forcats::fct_collapse(performance,
                                             strong = c("A", "A-", "B+"),
                                             sat = c("B", "B-", "C+"),
                                             weak = c("C", "C-", "D+"),
                                             poor = c("D", "D-"),
                                             f_w = c("F", "INC", "W"))) |>
  group_by(EMPLID, CLASS_TITLE) |> 
  select(EMPLID, gender, race_federal, residency_tuition, college_short, 
         dept_short, CLASS_TITLE, grade_cd) |>
  filter(race_federal != "0MULTI") |>
  #filter((grade_cd == "A" & CLASS_TITLE == "General Biology I") | CLASS_TITLE != "General Biology I") |>
  #filter((grade_cd == "A" & CLASS_TITLE == "General Biology I") | CLASS_TITLE != "General Biology I") |>
  ggplot(aes(x = CLASS_TITLE, stratum = grade_cd, alluvium = EMPLID,
            label = grade_cd)) +
  geom_flow( aes.flow = "forward",
            color = "darkgrey", alpha = 0.4, mapping = aes(fill = gender)) +
  geom_stratum() +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)),
             size = 4) +
  facet_wrap(vars(race_federal), scale = "free_y")
  


# Let's follow students.......maybe?

trajectory <- grades |>
  mutate(course_level = floor(as.numeric(CATALOG_NBR)/100)*100) |>
  filter(!is.na(course_level))|>
  group_by(EMPLID, gender, race_federal, dept_short, course_level) |>
  tally() |>
  filter(course_level < 400)


ggplot(trajectory |> filter(dept_short=="BIOL"),
       aes(x = course_level, y = n,
           color = gender)) +
  geom_line() +
  facet_wrap(vars(race_federal), scale = "free_y") +
  scale_x_continuous()


ggplot(grades |> filter(as.numeric(CATALOG_NBR) < 400), 
       aes(x = as.numeric(CATALOG_NBR), y = grade_cd, group = EMPLID)) +
  geom_line(alpha = 0.05) +
  facet_wrap(vars(race_federal)) 
  


trajectory<- grades |>
  mutate(course_level = floor(as.numeric(CATALOG_NBR)/100)*100) |>
  filter(!is.na(course_level))|>
  group_by(EMPLID, gender, race_federal, SBJCT_CD, course_level) |>
  summarize(present = 1) 
  
trajectory_summary <- trajectory |>
  group_by(gender, race_federal, SBJCT_CD, course_level) |>
  summarize(taken = sum(present)) 


ggplot(trajectory_summary |> filter(SBJCT_CD=="BIOL")|> filter(course_level <400),
       aes(x = course_level, y = taken,
           color = gender)) +
  geom_line() +
  facet_wrap(vars(race_federal), scale = "free_y") +
  scale_x_continuous(breaks = c(100, 200, 300)) +
  labs(subtitle = "bio")

ggplot(trajectory_summary |> filter(SBJCT_CD=="MATH")|> filter(course_level <400),
       aes(x = course_level, y = taken,
           color = gender)) +
  geom_line() +
  facet_wrap(vars(race_federal), scale = "free_y") +
  scale_x_continuous() +
  labs(subtitle = "math")


ggplot(trajectory |> filter(SBJCT_CD=="BIOL")|> filter(course_level <400),
       aes(x = course_level, y = present,
           color = gender)) +
  geom_line() +
  facet_wrap(vars(race_federal), scale = "free_y") +
  scale_x_continuous(breaks = c(100, 200, 300)) +
  labs(subtitle = "bio")

  