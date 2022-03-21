##### Load Packages #####
pacman::p_load(officer, lubridate, stringi, jpeg, timeperiodsR, 
               tidyverse, data.table, magrittr, 
               ggh4x, grid, directlabels, flextable, here, scales, gridExtra, 
               RecordLinkage)

##### Load and Preprocess Data #####
source(paste0("C:/Users/", Sys.getenv("username"), 
              "/Department of Veterans Affairs/Operations Triage Group (OTG) - Analytics - Documents/Publication/preProcess.R"))

s0 = s_all %>% 
  mutate_at(vars(opened_at, promoted_on), 
            list(~lubridate::as_datetime(as.character(.), format = "%Y-%m-%d %H:%M:%S", tz="EST"))) %>%
  mutate(s_datetime = ifelse(is.na(promoted_on), 
                             opened_at %>% as.character() %>% as_datetime(), promoted_on) %>% as_datetime(tz="EST"))
         # sys_created_by = grepl('integration', sys_created_by, ignore.case = TRUE) & 
         #   (grepl('soi|appdynamics|splunk|dynatrace|liveaction|solarwinds', sys_created_by, ignore.case = TRUE)))

vasi = fread(paste0(dataPath, 'VASI/VASystemsInventory_02242022.csv'), 
             stringsAsFactors = FALSE, strip.white=TRUE, skip=1)

colnames(vasi) = make_nice_cols(colnames(vasi))

p_all = fread(here("data/problem.csv"))
colnames(p_all) = make_nice_cols(colnames(p_all))

p_d = p_all %>% 
  mutate(caused_by_change = ifelse(u_problem_caused_by_change == "TRUE", 1, 0), 
         request_number = grepl("CHG", u_change_request_numbernumber) %>% as.numeric, 
         planned = case_when(request_number == 1 ~ 'Planned',
                             caused_by_change == 1 & request_number == 0 ~ 'Unplanned',
                             TRUE ~ 'Not Caused by Change')) %>% 
  group_by(parentnumber) %>%
  summarize(nprobs = n_distinct(number), 
            ncaused_by = sum(caused_by_change), 
            nchanges = sum(request_number), 
            planned = unique(planned))

sy = fread(paste0(dataPath, 'OTG SP Data Extracts/Systems and Product Lines.csv'), 
           stringsAsFactors = FALSE, strip.white=TRUE)

colnames(sy) = make_nice_cols(colnames(sy))

d = d_all %>% 
  mutate(nChar = nchar(as.character(Incident_Ticket))) %>% 
  rowwise() %>% 
  mutate(number = ifelse(nChar == 10, paste0("INC0", str_split(as.character(Incident_Ticket), "INC")[[1]][2]),
                         as.character(Incident_Ticket))) %>% 
  ungroup() %>% 
  # inner_join(s0 %>% select(number, s_datetime, problem_idu_problem_caused_by_change, sys_created_by),
  inner_join(s0 %>% select(number, incident_state, s_datetime, problem_idu_problem_caused_by_change, automation),
             by = 'number') %>% 
  mutate(s_date = lubridate::as_date(s_datetime), 
         month_n = substr(s_date, 1, 7) %>% factor %>% as.numeric(), 
         month = lubridate::month(s_date), 
         month_name = month.abb[month],
         year = lubridate::year(s_date), 
         yearMonth = paste0(year, '-', ifelse(nchar(month)==2, month, paste0('0',month))),
         month_name_year = paste0(month_name, "-", year)) %>% 
  arrange(s_date) %>% 
  filter(!(Incident_Ticket %in% omitOutliersTicket)) %>% 
  left_join(sy %>% select(System_Acronym, VASI_Id, AMOITAM_Portfolio),
            by = c('Primary_Culpable_System' = "System_Acronym")) %>%
  left_join(vasi %>% mutate_at(vars(VASI_Id), as.character), by = 'VASI_Id') %>%
  left_join(p_d, by = c('number' = 'parentnumber')) %>%
  mutate(Modified_System_Acronym = 
           ifelse(AMOITAM_Portfolio=='Unknown (Not in VASI)', 'Unknown (Not in VASI)', 
                  as.character(System_Acronym)), 
         planned = ifelse(is.na(planned), "Not Caused by Change", planned))

refDate = str_split(getwd(), "/")[[1]] %>% last() %>% as_date()  ## This MUST be a Friday
curWeekSun = refDate - days(12) # Sunday of week before
curWeekSat = refDate - days(6) # Saturday of week before

d %<>% filter(s_date <= curWeekSat)

d$month_name_year = factor(d$month_name_year, levels = unique(d$month_name_year))

yearMonth_levels = unique(c(d$yearMonth %>% unique, substr(startOfCurrentMonth, 1, 7)))
d$yearMonth = factor(d$yearMonth, levels = yearMonth_levels)
d$Incident_Priority = factor(d$Incident_Priority, levels = c("1 - Critical", "2 - High"))
d$Incident_Pillar = factor(d$Incident_Pillar, levels = c("Benefits", "Corporate", "Enterprise Services", "Health"))
d$Incident_State = factor(d$Incident_State, levels = c("Canceled", "In Progress", "Resolved"))

weekTable = data.frame(date = seq.Date("2019-07-14" %>% as_date(),
  # d %>% pull(s_date) %>% first(), 
                                       curWeekSat, by = 'day')) %>% 
  mutate(weekday = lubridate::wday(date, label = TRUE)) %>% 
  arrange(desc(date))

sunTable = weekTable %>% filter(weekday == "Sun") %>%  
  mutate(x = 1, weekNum = cumsum(x) - 1, 
         weekName = paste0(date, " to ", date + lubridate::days(6)))
  
weekTable %<>% left_join(sunTable %>% select(-x), by = c("date", "weekday")) %>% 
  arrange(desc(date)) %>% 
  fill(weekNum, weekName, .direction = "up")

d %<>% left_join(weekTable %>% select(date, weekNum, weekName), by = c('s_date' = 'date')) %>% 
  mutate(Primary_Culpable_System = as.character(Primary_Culpable_System),
         Primary_SystemApplication_Affected = as.character(Primary_SystemApplication_Affected),
         Primary_Culpable_System = ifelse(Primary_Culpable_System == "Telecommunications Services", "Telecom Services", Primary_Culpable_System),
         Primary_SystemApplication_Affected = ifelse(Primary_SystemApplication_Affected == "Telecommunications Services", "Telecom Services", Primary_SystemApplication_Affected),
         curWeek = ifelse(weekNum == 0, 1, 0))

d %<>% 
  mutate(Primary_Culpable_System = ifelse(Primary_Culpable_System == "Other" | 
                                            Primary_Culpable_System == "Unknown", 
                                          "Under Review", Primary_Culpable_System),
         incident_state = as.character(incident_state), 
         state = case_when(incident_state == "Resolved" | incident_state == "Closed" ~ "Resolved", 
                           incident_state == "On Hold" ~ "In Progress",
                           TRUE ~ incident_state)) 

d_bkup <- d

d %>% filter(curWeek == 1) %>% 
  select(number, s_datetime, ttr, state, 
         Primary_Culpable_System, planned, 
         Incident_Short_Description) %>% 
  arrange(s_datetime) %>% 
  mutate(proposed_on = as.character(s_datetime)) %>% 
  select(number, proposed_on, everything(), -s_datetime) %>% 
  fwrite(here("data/current_week_data.csv"))



# new_culps = fread(here("data/updated_culps.csv")) %>% 
#   rename(p1 = Primary_Culpable_System)
# 
# d %<>% left_join(new_culps, by = 'number') %>% 
#   rowwise() %>% 
#   mutate(Primary_Culpable_System = ifelse(number %in% new_culps$number, p1, Primary_Culpable_System)) %>% 
#   ungroup()
# d %>% filter(number %in% new_culps$number) %>% select(number, Primary_Culpable_System, p1)

##### Read in Template Powerpoint #####
# my_pres <- read_pptx(here("slides/blank_title.pptx"))

##### Powerpoint variables and External Images #####

# variable that uses date from the date of the local folder
cioBriefDueDate = here() %>% str_split("/") %>% 
  pluck(1) %>% last()

titleFile = here("titlePage.jpg")
footerFile = here("background.jpg")

# box height, box width, x position, y position
title_L = c(.94, 8.45, 3.56, 2.11)  
title_L2 = c(.29, 4.36, 3.56, 4.99) 

titlePositions = c(.85, 11.5, .3, .13)
subTitlePositions = c(.55, 4.52, 8.51, .4)

slide1_a = c(1.8, 6, .3, .98)
slide1_b = c(2.48, 6.3, 6.73, 1)
slide1_c = c(3.91, 6.17, .25, 3.23)
slide1_d = c(3.38, 6.59, 6.57, 3.75)

slide2_a = c(3.35, 7.94, 0, 3.75)
legend_a = c(.64, 1.4, 5.87, 4.48)
slide2_b = c(5.77, 4.5, 7.94, .98)
slide2_text = c(1.72, 7.89, .2, 1.05)

briefDate_Positions = c(3.51, 4.52)

slideNum_Positions = c(10.4, 7.2)

currentWeek_text = d %>% filter(weekNum == 0) %>% 
  pull(weekName) %>% unique()

##### Text for Slides #####
day1 = paste0(lubridate::month(curWeekSun, abbr = TRUE, label = TRUE), " ",
              lubridate::day(curWeekSun)) %>% toupper()
day2 = paste0(lubridate::month(curWeekSat, abbr = TRUE, label = TRUE), " ",
              lubridate::day(curWeekSat), " ", lubridate::year(curWeekSat)) %>% 
  toupper()

title_title = paste0("WEEKLY INCIDENT REVIEW\nW/O ", 
                     day1, " - ", day2) %>% 
  ftext(prop = fp_text(font.size = 30, font.family = "Calibri", 
                       bold = TRUE, color = '#175594')) %>% 
  fpar()

title_subtitle = cioBriefDueDate %>% as_date() %>% 
  format("%B %d, %Y") %>% 
  ftext(prop = fp_text(font.size = 16, font.family = "Calibri", 
                       color = '#175594')) %>% 
  fpar()

w1 = paste0(lubridate::wday(curWeekSun, abbr = FALSE, label = TRUE), ", ",
             lubridate::month(curWeekSun, abbr = TRUE, label = TRUE), " ",
             lubridate::day(curWeekSun), " ",
             lubridate::year(curWeekSun))
w11 = paste0(lubridate::month(curWeekSun, abbr = TRUE, label = TRUE), " ",
            lubridate::day(curWeekSun))
w2 = paste0(lubridate::wday(curWeekSat, abbr = FALSE, label = TRUE), ", ",
            lubridate::month(curWeekSat, abbr = TRUE, label = TRUE), " ",
            lubridate::day(curWeekSat), " ",
            lubridate::year(curWeekSat))
w22 = paste0(lubridate::month(curWeekSat, abbr = TRUE, label = TRUE), " ",
             lubridate::day(curWeekSat), " ",
             lubridate::year(curWeekSat))

week_subTitle = paste0(w1, " - ", w2) %>% 
  ftext(prop = fp_text(font.size = 14, font.family = "Arial", 
                       bold = TRUE)) %>% 
  fpar()

##### Saving SNOW and SP cleaned files in folder #####
fwrite(s, paste0("raw_snow_data_", cioBriefDueDate, ".csv"))
fwrite(d, paste0("raw_sp_data_", cioBriefDueDate, ".csv"))


##### Powerpoint Functions #####
getSlideText <- function(slideNum, bold = FALSE){
  paste0("Slide ", slideNum) %>% 
    ftext(prop = fp_text(color = "white", font.size = 14, bold = bold)) %>% 
    fpar()
}

getText <- function(textVal, fSize = 24, tAlign = "left", 
                    col = "black", bold = FALSE, family = "Arial"){
  textVal %>% 
    ftext(prop = fp_text(font.size = fSize, color = col, bold = bold, font.family = family)) %>% 
    fpar(fp_p = fp_par(text.align = tAlign))
}


##### Top Left Table #####

memBen_n = d %>%
  filter(weekNum %in% 0:12 & Incident_State == "Resolved") %>% 
  count(Product_Line) %>% filter(grepl("memorial", Product_Line, ignore.case = TRUE)) %>% 
  nrow()
if(memBen_n == 0){
  mem_d = data.frame(Incident_Pillar = "- Memorial and Services",
                     crit_cur = 0, 
                     high_cur = 0, 
                     crit_12 = 0,
                     high_12 = 0)
} else{
  mem_d = data.frame(Incident_Pillar = "- Memorial and Services",
                     crit_cur = "x", 
                     high_cur = "x", 
                     crit_12 = "x",
                     high_12 = "x")
}

slide_topLeft_table = d %>%
  filter(curWeek == 1 & state == "Resolved") %>% #Filter for current month only
  group_by(Incident_Priority, Incident_Pillar, .drop = FALSE) %>%
  summarise(N=n(), .groups = 'drop') %>%
  spread(key = Incident_Priority, value = N) %>%
  merge(d %>%
          filter(weekNum %in% 1:12 & state == "Resolved") %>% #Excludes the past week
          group_by(Incident_Priority, Incident_Pillar, .drop = FALSE) %>%
          summarise(N=n(), .groups = 'drop') %>%
          spread(key = Incident_Priority, value = N), by = "Incident_Pillar")

colnames(mem_d) = colnames(slide_topLeft_table)

slide_topLeft_table2 = slide_topLeft_table %>% 
  bind_rows(mem_d) %>% 
  slice(1, 5, 2:4)

slide_topLeft_finished = slide_topLeft_table2 %>% 
  bind_rows(slide_topLeft_table %>%
              summarise(across(where(is.numeric), sum))) %>%
  mutate(Incident_Pillar = fct_expand(c("Benefits", "- Memorial and Services", "Corporate", 
                                        "Enterprise Services", "Health", "Grand Totals"))) %>%
  set_colnames(c('Business Line','1-Critical ',"2-High ","1-Critical","2-High")) %>%
  flextable() %>% 
  add_header_row(values = c("", paste0(w11, ' to ', w22), "Previous 12 Weeks"),
                 colwidths = c(1, 2, 2)) %>% 
  bg(i = 1:5, j = c(2,4), bg = '#ed7d31') %>% 
  bg(i = 1:5, j = c(3,5), bg = '#F9C642') %>% 
  bg(i = 1, j = 2:5, part = "header", bg = "#4773AA") %>% 
  bg(i = 2, part = "header", bg = "#4773AA") %>% 
  # bg(i = 5, part = "body", bg = "#d9d9d9") %>% 
  color(i = 1:2, part = "header", color = "white") %>% 
  bold(i = 1:2, part = "header") %>% 
  bold(i = 6, part = "body") %>% 
  italic(i = 2, part = "body") %>% 
  align(i = 1:2, part = "header", align = "center") %>% 
  # align(i = 1:5, j = 2:5, part = "body", align = "right") %>% 
  align(i = 1:6, j = 2:5, part = "body", align = "center") %>% 
  align(j = 1, part = "body", align = "left") %>% 
  border_remove() %>%
  width(j = 1, width = 2) %>%
  width(j = 2:5, width = 1) %>% 
  fontsize(i = 2, part = "body", size = 10) %>% 
  flextable::border(i = 2, j = 2:5, part = "header", border.top = fp_border(color = "white")) %>% 
  flextable::border(i = 1, j = 2:5, part = "header", border.bottom = fp_border(color = "white")) %>% 
  flextable::border(i = 1, part = "body", border.top = fp_border(color = "black")) %>% 
  flextable::border(i = 5, part = "body", border.bottom = fp_border(color = "black")) %>% 
  flextable::border(j = 3, part = "all", border.right = fp_border(color = "black"))

# check if there's memorial product line in anything to be reported
d %>%
  filter(weekNum %in% 0:12 & Incident_State == "Resolved") %>% 
  count(Product_Line) %>% filter(grepl("memorial", Product_Line, ignore.case = TRUE))

##### Top Right Table #####

slide_topRight_table = d %>% filter(curWeek == 1) %>% 
  select(s_date, state, ttr) %>% 
  right_join(weekTable %>% filter(weekNum == 0) %>% select(date, weekday), by = c('s_date' = 'date')) %>% 
  group_by(weekday, s_date) %>% 
  summarize(Canceled = sum(state == "Canceled"),
            `In Progress` = sum(state == "In Progress"),
            Resolved = sum(state == "Resolved"),
            `Total Time to Repair (TTR)` = sum(ttr, na.rm = TRUE) %>% round(1), 
            .groups = 'drop') %>% 
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% 
  rename(Weekday = weekday, Date = s_date)

slide_topRight_table %<>% 
  mutate(Date = as.character(Date)) %>% 
  bind_rows(data.frame(Weekday = "Grand Totals", Date = "") %>% 
              cbind(slide_topRight_table %>% summarise(across(where(is.numeric), sum)))) %>% 
  rename(`CPI/HPI Start Date` = Date)

slide_topRight_finished = slide_topRight_table %>% 
  flextable() %>% 
  # bg(i = 8, bg = "#d9d9d9") %>%
  bold(i = 8, part = "body") %>% 
  align(j = 3:5, part = "all", align = "center") %>% 
  # width(j = 1, width = 1.3) %>%
  width(j = 1:5, width = 1) %>%
  width(j = 6, width = 1.3)


##### Bottom Left graph #####

weeklyPlot_d = d %>% filter(weekNum <= 51 & state == "Resolved") %>% 
  arrange(weekNum) %>% 
  group_by(weekNum, curWeek) %>% 
  summarize(mttr = mean(ttr, na.rm = TRUE), 
            min = min(ttr, na.rm = TRUE), 
            max = max(ttr, na.rm = TRUE), 
            sundayDate = str_split(weekName, " ")[[1]][1],
            .groups = 'drop') %>%
  mutate(colortext = ifelse(weekNum %% 2 == 0, "black", "white"),
         sizetext = ifelse(weekNum %% 2 == 0, 8, 1)) %>% 
  arrange(desc(weekNum)) %>% 
  mutate(sundayDate = factor(sundayDate))
  

weeklyPlot = weeklyPlot_d %>% 
  # mutate(sundayDate2 = ifelse(weekNum %% 4 == 0, as.character(sundayDate), "") %>% factor()) %>% 
  ggplot(aes(sundayDate, mttr)) + 
  geom_pointrange(aes(ymin = min, ymax = max, color = as.character(curWeek))) + 
  theme_minimal() + 
  # scale_x_date(breaks = date_breaks("1 month")) + 
  # scale_x_continuous(breaks = seq(4, 52, by = 4), minor_breaks = NULL, limits = c(0, 52)) + 
  scale_y_continuous(breaks = seq(0, 1200, by = 300), minor_breaks = seq(150, 1200, by = 150)) + 
  scale_color_manual("", values = c("1" = 'red', "0" = "#757373")) + 
  ylab("TTR (Hours)") + 
  xlab("\n Week (Date on Sunday)") + 
  ggtitle("Mean Time to Repair (TTR), Max TTR, and Min TTR by Week") + 
  theme(legend.position = 'none', 
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, 
                                   size = weeklyPlot_d$sizetext,
                                   colour = weeklyPlot_d$colortext))
  
pillarCounts = d %>% 
  filter(date >= "2019-09-01") %>% 
  count(weekNum, weekName, Incident_Pillar) %>% rowwise() %>% 
  mutate(sunday = str_split(weekName, " ")[[1]][1]) %>% ungroup() %>% 
  filter(!is.na(sunday)) %>% 
  select(-weekName) %>% 
  pivot_wider(id_cols = c(weekNum, sunday), names_from = Incident_Pillar, values_from = n) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% 
  pivot_longer(cols = -c(weekNum, sunday)) 

pillarColors = ColorVA %>% 
  filter(Description %in% c("gray-light", "gold", "green-darker", "OTG - Blue")) %>% 
  slice(c(2,1,3,4)) %>% 
  pull(Color)

names(pillarColors) = c("Health", "Enterprise Services", "Corporate", "Benefits")

pillarPlot = pillarCounts %>% 
  ggplot(aes(sunday %>% as_date(), value)) + 
  geom_col(aes(fill = name)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b - %Y") + 
  scale_y_continuous(breaks = seq(0, 26, by = 2)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.position = 'top') + 
  ylab("Count") + 
  xlab("Date of Week Start (Sunday)") + 
  scale_fill_manual("Business Line", values = pillarColors)

planned_d = d %>% filter(weekNum %in% 0:12) %>% 
  mutate(group = ifelse(weekNum == 0, paste0(w11, ' to ', w22), "Previous 12 Weeks")) %>% 
  group_by(group) %>% 
  count(planned) %>% 
  mutate(fraction = n / sum(n), 
         ymax = cumsum(fraction), 
         ymin = lag(ymax, default = 0), 
         labelPosition = (ymax + ymin) / 2, 
         label = paste0(planned, "\n Count: ", n)) 

plannedPlot1 = ggplot(planned_d %>% filter(group != "Previous 12 Weeks"), 
                      aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=planned)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  ggtitle(paste0(w11, ' to ', w22)) + 
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, size = 10))  

plannedPlot2 = ggplot(planned_d %>% filter(group == "Previous 12 Weeks"), 
       aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=planned)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  ggtitle("Previous 12 Weeks") + 
  theme(legend.position = "none", 
        plot.title=element_text(hjust=0.5, size = 10)) 

arrangeGrob(plannedPlot1, plannedPlot2, ncol = 2, 
                          top = textGrob("Major Incidents with Planned Changes, Unplanned Changes,\nand Not Caused by Change",
                                         gp=gpar(fontsize=12,font="Ariel"))) %>% 
  ggsave(file = here("data/plannedPlot.png"), height = slide1_c[1], width = slide1_c[2])

planned_d = d %>% filter(s_date > '2021-03-01') %>% 
  mutate(sundayDate = map_chr(weekName, ~str_split(., " ")[[1]][1])) %>% 
  count(planned, weekNum, sundayDate) %>% 
  arrange(desc(weekNum)) %>% 
  mutate(sundayDate = factor(sundayDate))

plannedPlot = ggplot(planned_d, aes(sundayDate, n, fill = planned)) + 
  geom_col() + 
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, 25, by = 5), minor_breaks = NULL) + 
  scale_fill_manual("", values = c("Not Caused by Change" = 'dark grey', "Planned" = "orange", 
                                    "Unplanned" = "red")) + 
  ylab("Count\n") + 
  xlab("\n Week (Date on Sunday)") + 
  ggtitle("Weekly Counts of Major Incidents by Change Status R12 Months") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, 
                                   size = weeklyPlot_d$sizetext,
                                   colour = weeklyPlot_d$colortext))

planned_percents = d %>% filter(s_date > '2021-03-01') %>% 
  count(planned) %>% mutate(p = prop.table(n)*100)

##### Bottom Right graph #####

top5_culp = d %>% filter(weekNum <= 26 & state == "Resolved" & 
                           Primary_Culpable_System != "Under Review") %>% 
  count(Primary_Culpable_System) %>% 
  arrange(desc(n)) %>% head(5)

top5_bar = d %>% filter(curWeek == 1 & state == "Resolved") %>% 
  count(Primary_Culpable_System) %>% 
  rename(n_curWeek = n) %>% 
  right_join(top5_culp, by = 'Primary_Culpable_System') %>% 
  replace_na(list(n_curWeek = 0)) %>% 
  arrange(desc(n)) %>% 
  mutate(Primary_Culpable_System = factor(Primary_Culpable_System, levels = rev(Primary_Culpable_System %>% as.character())))
  
top5_bar_finished = top5_bar %>% pivot_longer(c(n_curWeek, n)) %>% 
  mutate(name = ifelse(name == "n", "Previous Data", "Current Week")) %>% 
  ggplot(aes(value, Primary_Culpable_System)) + 
  geom_col(aes(fill = name)) + 
  geom_text(data = top5_bar %>% filter(n_curWeek != 0), aes(n + n_curWeek + 5, Primary_Culpable_System, label = n_curWeek), col = 'red') + 
  geom_text(data = top5_bar, aes(n - 5, Primary_Culpable_System, label = n), col = 'white') + 
  ylab("") + 
  xlab("Number of Incidents") + 
  ggtitle("Top 5 Culpable Systems for Past 6 months") + 
  theme_minimal() + 
  scale_fill_manual("Data Grouping", values = c("Current Week" = 'red', 'Previous Data' = "#757373"))

avg_telecom = d %>% filter(weekNum <= 26 & curWeek != 1 & state == "Resolved" & 
                             Primary_Culpable_System == "Telecom Services") %>% 
  count(weekNum) %>% summarize(mean = mean(n))
avg_network = d %>% filter(weekNum <= 26 & curWeek != 1 & state == "Resolved" & 
                             Primary_Culpable_System == "Network Services") %>% 
  count(weekNum) %>% summarize(mean = mean(n))

##### Last time broken #####

count_culps = d %>% filter(curWeek == 1 & state == "Resolved") %>% 
  group_by(Primary_Culpable_System) %>% 
  summarize(n = n(), 
            tttr = sum(ttr, na.rm = TRUE) %>% round(1))

culp_systems = count_culps %>% 
  pull(Primary_Culpable_System) %>% as.character()

culpTable_0 = d %>% filter(curWeek == 0) %>% 
  filter(Primary_Culpable_System %in% culp_systems) %>% 
  group_by(Primary_Culpable_System) %>% 
  slice(n()) %>% select(s_date, Primary_Culpable_System, curWeek) %>% 
  ungroup() %>% 
  bind_rows(d %>% filter(curWeek == 1) %>% 
              group_by(Primary_Culpable_System, curWeek) %>% 
              slice(n()) %>% 
              ungroup() %>% 
              select(s_date, Primary_Culpable_System, curWeek)) %>%
  mutate(s_date = as.character(s_date)) %>% 
  pivot_wider(names_from = curWeek, values_from = s_date) %>% 
  mutate(`0` = ifelse(Primary_Culpable_System == "Under Review", NA, `0`),
         days_since = as_date(`1`) - as_date(`0`)) %>% 
  inner_join(count_culps, by = 'Primary_Culpable_System') %>% 
  arrange(days_since) %>% 
  select(Primary_Culpable_System, n, tttr, `0`, `1`, days_since) %>% 
  rename(`Culpable System` = Primary_Culpable_System, 
         `Total TTR (Hours)` = tttr,
         Count = n,
         `Most Recent Date Prior to Current Week` = `0`,
         `Earliest Date for Current Week` = `1`,
         `Days Since Last Major Incident` = days_since)

if("EHRM" %in% culpTable_0$`Culpable System`){
  ehrm_culp = d %>% filter(curWeek == 0) %>% 
    filter(grepl("cerner|ehrm", Primary_Culpable_System, ignore.case = TRUE)) %>% 
    group_by(Primary_Culpable_System) %>% 
    slice(n()) %>% select(s_date, Primary_Culpable_System, curWeek) %>% 
    ungroup() %>% 
    arrange(s_date) %>% 
    slice(n())
  
  culpTable_0 %<>% 
    mutate(`Most Recent Date Prior to Current Week` = ifelse(`Culpable System` == "EHRM", 
                                                             ehrm_culp$s_date %>% as.character(),
                                                             `Most Recent Date Prior to Current Week`),
           `Days Since Last Major Incident` = as_date(`Earliest Date for Current Week`) - as_date(`Most Recent Date Prior to Current Week`)) %>% 
    arrange(`Days Since Last Major Incident`)
}

if("Under Review" %in% culpTable_0$`Culpable System`){
  culpTable_1 = culpTable_0 %>% 
    filter(`Culpable System` != "Under Review")
  
  culpTable_0 = culpTable_1 %>% 
    bind_rows(culpTable_0 %>% filter(`Culpable System` == "Under Review"))
}

culpTable = culpTable_0 %>% 
  flextable() %>% 
  width(j = 1, width = .95) %>%
  width(j = 2, width = .55) %>%
  width(j = 3, width = .7) %>%
  width(j = 4:6, width = 1)

# d %>% filter(Primary_Culpable_System == "DMDC DS Logon") %>%
#   arrange(date) %>% select(Primary_Culpable_System, Incident_Ticket, date, Incident_Category)


##### Last time system affected #####

count_affs = d %>% filter(curWeek == 1 & Incident_State == "Resolved") %>% 
  group_by(Primary_SystemApplication_Affected) %>% 
  summarize(n = n(), 
            tttr = sum(ttr, na.rm = TRUE) %>% round(1))

aff_systems = count_affs %>% 
  pull(Primary_SystemApplication_Affected) %>% as.character()

affTable = d %>% filter(curWeek == 0) %>% 
  filter(Primary_SystemApplication_Affected %in% aff_systems) %>% 
  group_by(Primary_SystemApplication_Affected) %>% 
  slice(n()) %>% select(s_date, Primary_SystemApplication_Affected, curWeek) %>% 
  ungroup() %>% 
  bind_rows(d %>% filter(curWeek == 1) %>% 
              group_by(Primary_SystemApplication_Affected, curWeek) %>% 
              slice(n()) %>% 
              ungroup() %>% 
              select(s_date, Primary_SystemApplication_Affected, curWeek)) %>%
  mutate(s_date = as.character(s_date)) %>% 
  pivot_wider(names_from = curWeek, values_from = s_date) %>% 
  mutate(days_since = as_date(`1`) - as_date(`0`)) %>% 
  inner_join(count_affs, by = 'Primary_SystemApplication_Affected') %>% 
  arrange(days_since) %>% 
  select(Primary_SystemApplication_Affected, n, tttr, `0`, `1`, days_since) %>% 
  rename(`Affected System` = Primary_SystemApplication_Affected, 
         `Total TTR (Hours)` = tttr,
         Count = n,
         `Most Recent Date Prior to Current Week` = `0`,
         `Earliest Date for Current Week` = `1`,
         `Days Since Last Issue` = days_since) %>% 
  flextable() %>% 
  width(j = 1, width = .95) %>%
  width(j = 2, width = .55) %>%
  width(j = 3, width = .7) %>%
  width(j = 4:6, width = 1)

##### Comparing similar text #####

texts = d %>% filter(curWeek == 1 & Incident_State == "Resolved" &
               # (grepl('Telecom', Primary_Culpable_System) |
               grepl('Database', Primary_Culpable_System)) %>%
  select(Incident_Ticket, Primary_Culpable_System, Incident_Category, Incident_Short_Description) %>%
  pull(Incident_Short_Description) %>% as.character()

dAfter = d %>% filter(curWeek == 0) %>% mutate(Incident_Short_Description = as.character(Incident_Short_Description))
adist1 = levenshteinSim(texts[2], dAfter %>% pull(Incident_Short_Description))
dAfter %>% slice(which.max(adist1)) %>% select(Incident_Ticket, date, Incident_Short_Description)

dates1 = d %>% filter(curWeek == 0 & grepl("JLV", Incident_Short_Description) & 
               grepl("Nationwide", Incident_Short_Description) & 
               grepl("unavailable", Incident_Short_Description, ignore.case = T)) %>% 
  select(number, s_date, Incident_Short_Description) %>% pull(s_date)

(dates1[2:length(dates1)] - lag(dates1)[2:length(dates1)]) %>% mean()

dates1 = d %>% filter(curWeek == 0 & Primary_Culpable_System == "VMware vCenter Server") %>% 
  pull(s_date)

##### Distributions of Weekly Counts and Total TTR #####

d_dist = d %>% 
  # filter(state == "Resolved") %>% 
  filter(!is.na(ttr)) %>% 
  group_by(weekNum) %>% 
  summarize(n = n(), 
            total_ttr = sum(ttr, na.rm = TRUE))

dd_counts = density(d_dist$n)
maxVal_counts = dd_counts$y[dd_counts$x > mean(d_dist$n) - 1 & dd_counts$x < mean(d_dist$n) + 1] %>% median()
weekVal_counts = d_dist %>% filter(weekNum == 0) %>% pull(n)
xVal_counts = dd_counts$y[dd_counts$x > weekVal_counts - 1 & dd_counts$x < weekVal_counts + 1] %>% median()

dplot1 = d_dist %>% 
  ggplot(aes(n)) + 
  geom_density() + 
  geom_segment(aes(x = mean(n), xend = mean(n), y = 0, yend = maxVal_counts), linetype = 'dashed') + 
  geom_segment(aes(x = weekVal_counts, xend = weekVal_counts, y = 0, yend = xVal_counts), col = 'red', linetype = 'dashed') + 
  xlab("\nWeekly Count") + 
  scale_y_continuous(breaks = seq(0, 25, by = 5), minor_breaks = NULL) + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(size = 10),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank())

d_dist1 = d_dist %>%
  count(n)
maxVal_counts = d_dist1 %>% arrange(desc(nn)) %>% slice(1) %>% pull(nn)
weekVal_counts = d_dist %>% filter(weekNum == 0) %>% pull(n)
xVal_counts = dd_counts$y[dd_counts$x > weekVal_counts - 1 & dd_counts$x < weekVal_counts + 1] %>% median()
max1Count = d_dist1 %>% arrange(desc(nn)) %>% slice(1) %>% pull(nn)

dplot1 = d_dist1 %>%
  ggplot(aes(n, nn)) +
  geom_col(color = 'black', fill = 'white') +
  geom_vline(data = d_dist, aes(xintercept = mean(n)), linetype = 'dashed') +
  geom_vline(aes(xintercept = weekVal_counts), linetype = 'dashed', col = 'red') +
  # geom_segment(aes(x = mean(n), xend = mean(n), y = 0, yend = maxVal_counts), linetype = 'dashed') +
  # geom_segment(aes(x = weekVal_counts, xend = weekVal_counts, y = 0, yend = xVal_counts), col = 'red', linetype = 'dashed') +
  xlab("\nWeekly Count") +
  ylab("Number of Occurrences") +
  scale_y_continuous(breaks = seq(0, max1Count, by = 2), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(1, 25, by = 1), minor_breaks = NULL) +
  theme_minimal() +
  theme(
      axis.text.x = element_text(size = 6),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank())


dd_ttr = density(d_dist$total_ttr)
maxVal_ttr = dd_ttr$y[dd_ttr$x > mean(d_dist$total_ttr) - 10 & dd_ttr$x < mean(d_dist$total_ttr) + 10] %>% median()

weekVals = d_dist %>% filter(weekNum %in% 0:12) %>% pull(total_ttr)
xVals = map_dbl(weekVals, function(x) dd_ttr$y[dd_ttr$x > x - 2 & dd_ttr$x < x + 2] %>% median())
prev5 = data.frame(x = weekVals[2:length(weekVals)], y = xVals[2:length(weekVals)])

breakLines = d_dist %>% pull(total_ttr) %>% 
  quantile(probs = seq(.1, .9, by = .1), na.rm = TRUE)

breakLinesy = map_dbl(breakLines, function(x){
  dd_ttr$y[dd_ttr$x > x - 2 & dd_ttr$x < x + 2] %>% median()
})

dplot2 = d_dist %>% 
  ggplot(aes(total_ttr)) + 
  geom_density() + 
  scale_x_continuous(breaks = seq(0, 1250, by = 250), minor_breaks = NULL) +
  # scale_x_continuous(breaks = breakLines, minor_breaks = NULL) + 
  geom_segment(aes(x = mean(total_ttr), xend = mean(total_ttr), y = 0, yend = maxVal_ttr), linetype = 'dashed') + 
  geom_segment(data = prev5, aes(x = x, xend = x, y = 0, yend = y), col = "grey", linetype = 'dashed') + 
  geom_segment(aes(x = 950, xend = 1050, y = breakLinesy[6] + .0005, yend = breakLinesy[6] + .0005), linetype = 'dashed') + 
  geom_segment(aes(x = 950, xend = 1050, y = breakLinesy[6] + .00025, yend = breakLinesy[6] + .00025), col = 'red', linetype = 'dashed') + 
  geom_segment(aes(x = 950, xend = 1050, y = breakLinesy[6], yend = breakLinesy[6]), col = "grey", linetype = 'dashed') +
  geom_segment(aes(x = weekVals[1], xend = weekVals[1], y = 0, yend = xVals[1]), col = 'red', linetype = 'dashed') + 
  geom_text(aes(x = 1185, y = breakLinesy[6] + .00051, label = "Average Value"), linetype = 'dashed', size = 2.2) + 
  geom_text(aes(x = 1183, y = breakLinesy[6] + .00026, label = "Current Week"), col = 'red', linetype = 'dashed', size = 2.2) + 
  geom_text(aes(x = 1175, y = breakLinesy[6] + .00001, label = "Past Weeks"), col = "grey", linetype = 'dashed', size = 2.2) +
  geom_text(data = data.frame(x = breakLines %>% as.vector(), y = breakLinesy, text = names(breakLines)), 
            aes(x = x, y = y, label = text), size = 2) + 
  xlab("\nTotal Weekly TTR (Hours)") + 
  theme_minimal() + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank())

arrangeGrob(dplot1, dplot2, ncol = 2, 
             top = textGrob("Distributional Comparison: Weekly Counts and Total Weekly TTR for All Resolved Major Incidents ",
                            gp=gpar(fontsize=12,font="Ariel"))) %>% 
  ggsave(file = here("data/doubleDens.png"), height = slide2_a[1], width = slide2_a[2])



##### Problem managment #####

# d_probs = fread(here("data/problems_caused_by.csv"))
# 
# d_probs %>% select(number, u_problem_caused_by_change, parent.number, parent.opened_at, parent.priority) %>% 
#   right_join(d %>% filter(weekNum == 1) %>% select(Incident_Ticket, date), 
#                        by = c("parent.number" = "Incident_Ticket"))

N_caused_by = d %>% 
  filter(curWeek == 1 & problem_idu_problem_caused_by_change) %>% nrow()



##### Automated tracking/monitoring #####

# d_track = fread(here("data/incident_automation.csv"))

N_monitor_alerts = d %>% filter(curWeek == 1 & automation) %>% nrow()

b1 = paste0(N_caused_by, " Incidents had problem records caused by change and there were ",
            N_monitor_alerts, " incidents alerted by monitoring. All culpable systems are monitored")
s2_b1 = getText(b1, fSize = 16, family = "Calibri")
s2_b2 = getText("Bullet 2", fSize = 16, family = "Calibri")
s2_b3 = getText("Bullet 3", fSize = 16, family = "Calibri")

bl <- block_list(s2_b1, s2_b2, s2_b3)


##### Create and Save Slides #####

my_pres <- read_pptx(here("slides/blank_title.pptx"))

s_s <- slide_size(my_pres)
s_w <- s_s$width # width of slides
s_h <- s_s$height # height of slides

#### Title Slide #####
my_pres %<>%
  ph_with(value = title_title,
          location = ph_location(height = title_L[1], width = title_L[2],
                                 left = title_L[3], top = title_L[4])) %>% 
  ph_with(value = title_subtitle,
          location = ph_location(height = title_L2[1], width = title_L2[2],
                                 left = title_L2[3], top = title_L2[4]))

# Adding Slide 1
my_pres %<>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value = external_img(footerFile, width = s_w, height = s_h), 
          location = ph_location(left = 0, top = 0), use_loc_size = FALSE) %>%
  ph_with(value = getText("Major Incidents", fSize = 36), 
          location = ph_location(height = titlePositions[1], width = titlePositions[2],
                                 left = titlePositions[3], top = titlePositions[4])) %>% 
  ph_with(value = week_subTitle, 
          location = ph_location(height = subTitlePositions[1], width = subTitlePositions[2],
                                 left = subTitlePositions[3], top = subTitlePositions[4])) %>% 
  ph_with(value = slide_topLeft_finished, 
          location = ph_location(height = slide1_a[1], width = slide1_a[2],
                                 left = slide1_a[3], top = slide1_a[4])) %>% 
  ph_with(value = slide_topRight_finished, 
          location = ph_location(height = slide1_b[1], width = slide1_b[2],
                                 left = slide1_b[3], top = slide1_b[4])) %>% 
  ph_with(value = pillarPlot,
          location = ph_location(height = slide1_c[1], width = slide1_c[2],
                                 left = slide1_c[3], top = slide1_c[4])) %>%
  # ph_with(value = external_img(here("data/plannedPlot.png")), 
  #         location = ph_location(height = slide1_c[1], width = slide1_c[2],
  #                                left = slide1_c[3], top = slide1_c[4])) %>% 
  ph_with(value = top5_bar_finished, 
          location = ph_location(height = slide1_d[1], width = slide1_d[2],
                                 left = slide1_d[3], top = slide1_d[4])) %>% 
  ph_with(value = getSlideText(1),
          location = ph_location(left = slideNum_Positions[1], top = slideNum_Positions[2],
                                 width = 1, height = .3))
  
  
# Adding Slide 2
my_pres %<>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(value = external_img(footerFile, width = s_w, height = s_h), 
          location = ph_location(left = 0, top = 0), use_loc_size = FALSE) %>%
  ph_with(value = getText("Major Incidents: Conclusions", fSize = 36), 
          location = ph_location(height = titlePositions[1], width = titlePositions[2],
                                 left = titlePositions[3], top = titlePositions[4])) %>% 
  ph_with(value = week_subTitle, 
          location = ph_location(height = subTitlePositions[1], width = subTitlePositions[2],
                                 left = subTitlePositions[3], top = subTitlePositions[4])) %>% 
  ph_with(value = external_img(here("data/doubleDens.png")), 
          location = ph_location(height = slide2_a[1], width = slide2_a[2],
                                 left = slide2_a[3], top = slide2_a[4])) %>% 
  # ph_with(value = external_img(here("../markdown_files/density_legend.png")), 
  #         location = ph_location(height = legend_a[1], width = legend_a[2],
  #                                left = legend_a[3], top = legend_a[4])) %>% 
  ph_with(value = culpTable, 
          location = ph_location(left = slide2_b[3], top = slide2_b[4])) %>% 
  # ph_with(value = bl, 
  #         level_list = seq_along(bl),
  #         location = ph_location(height = slide2_text[1], width = slide2_text[2],
  #                                 left = slide2_text[3], top = slide2_text[4])) %>% 
  ph_with(value = getSlideText(2),
          location = ph_location(left = slideNum_Positions[1], top = slideNum_Positions[2],
                                 width = 1, height = .3))


print(my_pres, target = here("slides/CIO_Brief.pptx")) %>% 
  invisible()

