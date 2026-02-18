# Main index table
index_2024 <- data_us_2024 %>% 
  select(
    wt,
    RID,
    
    # EVs
    ev_ne_national,
    ev_adoption,
    ev_consider, 
    ev_ee, 
    ev_pnb, 
    ev_intention, 
    ev_personal.benefit, 
    ev_self.efficacy, 
    ev_difficulty, 
    ev_outcome.efficacy,
    ev_policy.support,
    ev_policy.ee,
    ev_policy.info,
    ev_policy.contact,
    
    # Install solar panels
    solar_adoption,
    solar_ne_national,
    solar_pnb,
    solar_ee,
    solar_consider, 
    solar_ee, 
    solar_pnb, 
    solar_intention, 
    solar_personal.benefit, 
    solar_self.efficacy, 
    solar_difficulty, 
    solar_outcome.efficacy,
    solar_policy.support,
    solar_policy.ee,
    solar_policy.info,
    solar_policy.contact,
    
    # Community solar
    community.solar_adoption,
    community.solar_consider, 
    community.solar_ee_national, 
    community.solar_pnb, 
    community.solar_intention, 
    community.solar_ne_national, 
    community.solar_personal.benefit, 
    community.solar_self.efficacy, 
    community.solar_difficulty, 
    community.solar_outcome.efficacy,
    community.solar_policy.support,
    community.solar_policy.ee,
    community.solar_policy.info,
    community.solar_policy.contact,
    
    # Offsets
    offset_adoption,
    offset_consider,
    offset_ee_national,
    offset_ne_national,
    offset_intention,
    offset_pnb, 
    offset_intention, 
    offset_personal.benefit, 
    offset_self.efficacy, 
    offset_difficulty, 
    offset_outcome.efficacy,
    offset_policy.support,
    offset_policy.ee,
    offset_policy.info,
    offset_policy.contact,
    
    # Heat pump AC
    hp_ne_national,
    hp_adoption,
    hp_consider, 
    hp_ee_national, 
    hp_pnb, 
    hp_intention, 
    hp_personal.benefit, 
    hp_self.efficacy, 
    hp_difficulty, 
    hp_outcome.efficacy,
    hp_policy.support,
    hp_policy.ee,
    hp_policy.info,
    hp_policy.contact,
    
    # Beef
    beef_adoption, 
    beef_consider, 
    beef_difficulty, 
    beef_ee_national, 
    beef_pnb, 
    beef_ne_national, 
    beef_intention,
    #beef_last_three_meals,
    beef_personal.benefit, 
    beef_self.efficacy, 
    beef_difficulty, 
    beef_outcome.efficacy,
    beef_policy.support,
    beef_policy.ee,
    beef_policy.info,
    beef_policy.contact,
    
    # Food waste
    fw_adoption,
    fw_consider,
    fw_pnb,
    fw_ee_national,
    fw_ne_national,
    fw_intention,
    fw_personal.benefit,
    fw_self.efficacy,
    fw_difficulty,
    fw_outcome.efficacy,
    fw_policy.support,
    fw_policy.ee,
    fw_policy.info,
    fw_policy.contact, 
    
    age_group, sex, ETHNICITY, hispanic, political_viewpoint_wvs, political_party, income, gender, enviro_donation,
    
    worry)

# Clean behavioral variables
index_2024 %<>% mutate(ev_adoption = case_when(ev_adoption == 0 ~ 0,
                                               ev_adoption == 1 ~ 1,
                                               ev_adoption == 2 ~ 1,
                                               ev_adoption == 5 ~ 1,
                                               ev_adoption == 10 ~ 1,
                                               TRUE ~ 0),
                       
                       ev_policy.support = case_when(
                         ev_policy.support %in% c(3, 4) ~ 1,
                         ev_policy.support %in% c(1, 2) ~ 0,
                       ),
                       solar_policy.support = case_when(
                         solar_policy.support %in% c(3, 4) ~ 1,
                         solar_policy.support %in% c(1, 2) ~ 0,
                       ),
                       community.solar_policy.support = case_when(
                         community.solar_policy.support %in% c(3, 4) ~ 1,
                         community.solar_policy.support %in% c(1, 2) ~ 0,
                       ),
                       offset_policy.support = case_when(
                         offset_policy.support %in% c(3, 4) ~ 1,
                         offset_policy.support %in% c(1, 2) ~ 0,
                       ),
                       hp_policy.support = case_when(
                         hp_policy.support %in% c(3, 4) ~ 1,
                         hp_policy.support %in% c(1, 2) ~ 0,
                       ),
                       beef_policy.support = case_when(
                         beef_policy.support %in% c(3, 4) ~ 1,
                         beef_policy.support %in% c(1, 2) ~ 0,
                       ),
                       fw_policy.support = case_when(
                         fw_policy.support %in% c(3, 4) ~ 1,
                         fw_policy.support %in% c(1, 2) ~ 0,
                       ))

index_2024 <- index_2024 %>%
  select(RID, wt,  ev_ne_national:worry)

survey_2024 <- index_2024


write.csv(survey_2024, "survey_for_gpt.csv")

index_2024 <- mutate_all(index_2024, function(x) as.numeric(as.character(x)))

index_2024 <- index_2024 %>%
  pivot_longer(
    cols = contains(c("intention",  "ee", "pnb", "ne", "adoption", "consider", "personal.benefit", "self.efficacy", "outcome.efficacy", "difficulty", 
                      "policy.support", "policy.ee", "policy.info", "policy.contact")),
    names_to = "indicator",
    values_to = "value",
    values_drop_na = TRUE)

index_2024 <- index_2024 %>%
  mutate(behavior = indicator)

fin_table <- index_2024 %>% 
  mutate(behavior = sapply(strsplit(index_2024$indicator, split = '_', fixed = TRUE),function(x) (x[1])))

fin_table <- fin_table %>% 
  mutate(ind = sapply(strsplit(fin_table$indicator, split = '_', fixed = TRUE),function(x) (x[2])))

fin_table %<>% dplyr::select(-RID, -indicator)

t1 <- group_by(fin_table, behavior, ind)

t1 <- summarise(t1, w.mean = weighted.mean(value, wt, na.rm = TRUE))

t1 %<>% 
  group_by(behavior) %>% 
  pivot_wider(names_from = ind, values_from = w.mean)

t1 <- t1 %>% 
  mutate(adoption = adoption * 100,
         consider = consider * 100,
         ee = ee*10,
         ne = ne*10,
         pnb = pnb*100,
         personal.benefit = (personal.benefit - 1)/4*100,
         outcome.efficacy = (outcome.efficacy - 1)/4*100,
         self.efficacy = (self.efficacy - 1)/4*100,
         policy.contact = policy.contact * 100,
         policy.info = policy.info * 100,
         policy.support = policy.support * 100,
         policy.ee = policy.ee*10)

t1 <- t1 %>% 
  rename('Behavior' = behavior,
         'Consideration' = consider,
         'Adoption' = adoption,
         'Intention' = intention,
         'Belief that others are engaging in the behavior' = ee,
         'Belief that others should engage in the behavior' = pnb,
         'Belief that others think people should engage in the behavior' = ne,
         'Self-efficacy' = self.efficacy,
         'Outcome efficacy' = outcome.efficacy,
         'Perceived personal benefit' = personal.benefit,
         'Perceived ease of adoption' = difficulty,
         'Policy support' = policy.support,
         'Perceived policy support' = policy.ee,
         'Openness to learn about the policy' = policy.info,
         'Openness to contact a representative' = policy.contact)

# Rename rows
t1$Behavior[t1$Behavior == "ev"] <- "Drive an EV"
t1$Behavior[t1$Behavior == "offset"] <- "Buy carbon offsets"
t1$Behavior[t1$Behavior == "solar"] <- "Install solar panels"
t1$Behavior[t1$Behavior == "beef"] <- "Eat less beef"
t1$Behavior[t1$Behavior == "community.solar"] <- "Sign up for community solar"
t1$Behavior[t1$Behavior == "fw"] <- "Reduce food waste"
t1$Behavior[t1$Behavior == "hp"] <- "Install a heat pump"

# Re-order variables
t1 <- t1 %>% select(Behavior, Consideration, Adoption, Intention, 
                `Belief that others are engaging in the behavior`, 
                `Belief that others should engage in the behavior`, 
                `Belief that others think people should engage in the behavior`, 
                `Self-efficacy`, `Outcome efficacy`,
                `Perceived personal benefit`, `Perceived ease of adoption`,
                `Policy support`, `Perceived policy support`, `Openness to learn about the policy`, `Openness to contact a representative`)

# Pretty table
core_t = reactable(t1,
                      style = list(fontSize = "12px"),
                      defaultSortOrder = 'asc',
                      defaultSorted = 'Behavior',
                      showSortIcon = TRUE,
                      # ALL one page option (no scrolling or page swapping)
                      pagination = TRUE,
                      # compact for an overall smaller table width wise
                      compact = TRUE,
                      borderless = FALSE,
                      striped = TRUE,
                      highlight = TRUE,  
                      # fullWidth - either fit to width or not
                      fullWidth = TRUE,
                      # apply defaults
                      # 100 px and align to center of column
                      defaultColDef = colDef(
                        minWidth = 150,
                        align = "center"),
                      # Add theme for the top border
                      theme = reactableTheme(
                        headerStyle = list(
                          "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                          "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),
                          borderColor = "#555"
                        )
                      ),
                      columns = list(
                        
                        Behavior = colDef(
                          align = "left",
                          maxWidth = 500,
                          sticky = "left",
                          # Add a left border style to visually distinguish the sticky column
                          style = list(borderLeft = "1px solid #eee"),
                          headerStyle = list(borderLeft = "1px solid #eee")),
                        
                        Adoption = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$Adoption)) / (max(t1$Adoption) - min(t1$Adoption))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Reported adoption",
                          format = colFormat(digits = 2),
                          maxWidth = 65),
                        
                        Intention = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$Intention)) / (max(t1$Intention) - min(t1$Intention))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Average intention",
                          format = colFormat(digits = 2),
                          maxWidth = 65),
                        
                        `Belief that others are engaging in the behavior` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Belief that others are engaging in the behavior`)) / (max(t1$`Belief that others are engaging in the behavior`) - min(t1$`Belief that others are engaging in the behavior`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Others have adopted",
                          maxWidth = 100,
                          format = colFormat(digits = 2)),
                        
                        `Belief that others should engage in the behavior` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Belief that others should engage in the behavior`)) / (max(t1$`Belief that others should engage in the behavior`) - min(t1$`Belief that others should engage in the behavior`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Others should adopt",
                          maxWidth = 100,
                          format = colFormat(digits = 2)),
                        
                        `Belief that others think people should engage in the behavior` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Belief that others think people should engage in the behavior`)) / (max(t1$`Belief that others think people should engage in the behavior`) - min(t1$`Belief that others think people should engage in the behavior`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Others think people should adopt",
                          format = colFormat(digits = 2),
                          maxWidth = 150),
                        
                        
                        `Self-efficacy` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Self-efficacy`)) / (max(t1$`Self-efficacy`) - min(t1$`Self-efficacy`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          format = colFormat(digits = 2),
                          maxWidth = 90),
                        
                        `Consideration` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Consideration`)) / (max(t1$`Consideration`) - min(t1$`Consideration`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          format = colFormat(digits = 2),
                          maxWidth = 95),
                        
                        `Perceived personal benefit` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Perceived personal benefit`)) / (max(t1$`Perceived personal benefit`) - min(t1$`Perceived personal benefit`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Personal benefit",
                          format = colFormat(digits = 2),
                          maxWidth = 120),
                        
                        `Outcome efficacy` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Outcome efficacy`)) / (max(t1$`Outcome efficacy`) - min(t1$`Outcome efficacy`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          format = colFormat(digits = 2),
                          maxWidth = 120),
                        
                        `Perceived ease of adoption` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Perceived ease of adoption`)) / (max(t1$`Perceived ease of adoption`) - min(t1$`Perceived ease of adoption`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Reported difficulty",
                          format = colFormat(digits = 2),
                          maxWidth = 120),
                        
                        `Policy support` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Policy support`)) / (max(t1$`Policy support`) - min(t1$`Policy support`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          format = colFormat(digits = 2),
                          maxWidth = 120),
                        
                        `Perceived policy support` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Perceived policy support`)) / (max(t1$`Perceived policy support`) - min(t1$`Perceived policy support`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          format = colFormat(digits = 2),
                          maxWidth = 120),
                        
                        `Openness to learn about the policy` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Openness to learn about the policy`)) / (max(t1$`Openness to learn about the policy`) - min(t1$`Openness to learn about the policy`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Learn about the policy",
                          format = colFormat(digits = 2),
                          maxWidth = 120),
                        
                        `Openness to contact a representative` = colDef(
                          style = function(value) {
                            normalized <- (value - min(t1$`Openness to contact a representative`)) / (max(t1$`Openness to contact a representative`) - min(t1$`Openness to contact a representative`))
                            color <- good_color(normalized)
                            list(background = color)
                          },
                          name = "Contact a representative",
                          format = colFormat(digits = 2),
                          maxWidth = 120)), 
                      
                      columnGroups = list(
                        colGroup(name = "Beliefs that...", columns = c("Belief that others are engaging in the behavior", 
                                                                       "Belief that others should engage in the behavior", 
                                                                       "Belief that others think people should engage in the behavior")),
                        colGroup(name = "Openness to...", columns = c("Openness to learn about the policy",
                                                                      "Openness to contact a representative"))))

core_t
