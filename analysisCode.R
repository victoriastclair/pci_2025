#############################################################################
######### Analysis code for St Clair et al. under review ####################
#############################################################################

## Packages ----- 
require('Hmisc') # for chi square tests
require('cowplot') # for plots
require('dplyr') # for data 
require('ggplot2') # for plotting
require('ppcor') # for partial correlations

## Data ----
tg_shifts <- data.frame(read.csv('your/path/here')) # Load TJ shifts data
cja <- data.frame(read.csv('your/path/here')) # Load cja with referent data

## Demo variables ----
demographics <- tibble(
  totalPCI = n_distinct(tg_shifts$id),
  mean_age_months = mean(tg_shifts$age, na.rm = TRUE) / 30.44,
  sd_age_months = sd(tg_shifts$age, na.rm = TRUE) / 30.44,
  min_age_months = min(tg_shifts$age, na.rm = TRUE) / 30.44,
  max_age_months = max(tg_shifts$age, na.rm = TRUE) / 30.44,
  gender_counts = paste(names(table(tg_shifts$gender)), table(tg_shifts$gender), collapse = ", "),
  median_SES = median(tg_shifts$SES, na.rm = TRUE),
  median_mat_ed = median(tg_shifts$mat_ed, na.rm = TRUE)
)

unique(tg_shifts$ethnicity)

tg_shifts_b <- filter(lang_group == "bilingual")
mean(tg_shifts_b$english_exposure)
mean(tg_shifts_b$second_lang_exposure)

## Descriptive stats ----
vars <- c(
  "overall_cja", "mutual_gaze", 
  "child_referential", "parent_referential", 
  "shared_object_gaze",
  "num_parent_words", "num_unique_parent_words",
  "num_referential_labels", "num_unique_referential_labels",
  "parent_tg_shifting", "child_tg_shifting",
  "cja_with_referent"
)

summary_stats <- cja %>%
  dplyr::select(any_of(unlist(map(vars, ~ grep(.x, names(fin), value = TRUE, fixed = TRUE))))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value, na.rm = TRUE),
    Min  = min(Value, na.rm = TRUE),
    Max  = max(Value, na.rm = TRUE),
    .groups = "drop"
  )

## Hyp 1: Triadic gaze shifting ----
### Covariate checks ----
cor.test(tg_shifts$parent_tg_shifting, tg_shifts$child_tg_shifting) #ns (p = .791)

cor.test(tg_shifts$parent_tg_shifting, tg_shifts$age) #ns (p = .516)
cor.test(tg_shifts$child_tg_shifting, tg_shifts$age) #ns (p = .280)

chiSquare(parent_tg_shifting ~ household_income, data = tg_shifts) #ns (p = .568)
chiSquare(child_tg_shifting ~ household_income, data = tg_shifts) #ns (p = .628)

chiSquare(parent_tg_shifting ~ maternal_ed, data = tg_shifts) #ns (p = .501)
chiSquare(child_tg_shifting ~ maternal_ed, data = tg_shifts) #ns (p = .203)

t.test(parent_tg_shifting ~ lang_group, data = tg_shifts) #ns (p = .107)
t.test(child_tg_shifting ~ lang_group, data = tg_shifts) #ns (p = .332)

### Modelling ----
#### Receptive & Expressive MSEL @ 15-18mo ----
m0 <- lm(rec_1518mo ~ child_tg_shifting, data = tg_shifts); summary(m0) #ns (p = .721)
m0 <- lm(prod_1518mo ~ child_tg_shifting, data = tg_shifts); summary(m0) #ns (p = .707)

m0 <- lm(rec_1518mo ~ parent_tg_shifting, data = tg_shifts); summary(m0) #sig (p = .002)
confint(m0)

m0 <- lm(prod_1518mo ~ parent_tg_shifting, data = tg_shifts); summary(m0) #sig (p = .019)
confint(m0)

#### Partial correlations @ 15-18mo ----
# Definitions: PR_SO = Parent referential to shared object &
# SO_PR = Shared object to parent referential

pcor.test(tg_shifts$rec_1518mo, tg_shifts$PR_SO, tg_shifts$SO_PR) #ns (p = .357)
pcor.test(tg_shifts$rec_1518mo, tg_shifts$SO_PR, tg_shifts$PR_SO) #ns (p = .953)

pcor.test(tg_shifts$prod_1518mo, tg_shifts$PR_SO, tg_shifts$SO_PR) #ns (p = .610)
pcor.test(tg_shifts$prod_1518mo, tg_shifts$SO_PR, tg_shifts$PR_SO) #ns (p = .840)

#### Parent-Report Receptive & Expressive Vocabulary @ 24mo ----
t.test(rec_24mo ~ lang_group, data = tg_shifts) #sig (p = .017)
t.test(prod_24mo ~ lang_group, data = tg_shifts) #sig (p = .052)

m0 <- lm(rec_24mo ~ lang_group + rec_1518mo + child_tg_shifting, data = tg_shifts); summary(m0) #ns switch (p = .513)
m0 <- lm(prod_24mo ~ lang_group + prod_1518mo + child_tg_shifting, data = tg_shifts); summary(m0) #ns switch (p = .271)
m0 <- lm(prod_24mo ~ lang_group + prod_1518mo + parent_tg_shifting, data = tg_shifts); summary(m0) #ns switch (p = .111)

m0 <- lm(rec_24mo ~ lang_group + rec_1518mo + parent_tg_shifting, data = tg_shifts); summary(m0) #sig group (p = .023); sig rec (p = .003); sig switch (p = .017)
confint(m0)

## Hyp 2: Referential labelling during CJA ----
cja_m <- cja %>% filter(group == 'M')

### Covariate checks ----
cor.test(cja_m$cja_with_referent, cja_m$child_tg_shifting) #ns (p = .126)
cor.test(cja_m$cja_with_referent, cja_m$parent_tg_shifting) #ns (p = .676)

chiSquare(cja_with_referent ~ household_income, data = cja_m) #ns (p = .400)
chiSquare(cja_with_referent ~ maternal_ed, data = cja_m) #ns (p = .410)

### Modelling ----
m0 <- lm(prod_1518mo ~ cja_with_referent, data = cja_m); summary(m0) #ns (p = .773)
m0 <- lm(rec_1518mo ~ cja_with_referent, data = cja_m); summary(m0) #sig (p = .005)
confint(m0)

m0 <- lm(rec_1518mo ~ cja_with_referent + duration_cja + rpm_cja + total_eps_cja, 
         data = cja_m); summary(m0) #sig cja_with_referent (p = .016)
confint(m0)

m0 <- lm(rec_1518mo ~ cja_with_referent + num_parent_words + num_unique_parent_words + 
           num_referential_labels + num_unique_referential_labels, 
         data = cja_m); summary(m0) #sig cja_with_referent (p = .003) & sig num_referential_labels (p = .011)
confint(m0)

m0 <- lm(rec_1518mo ~ cja_with_referent + num_parent_words + num_unique_parent_words + 
           num_referential_labels + num_unique_referential_labels, 
         data = cja_m); summary(m0) #sig cja_with_referent (p = .003) & sig num_referential_labels (p = .011)
confint(m0)

#cr_with_referent = child referential gaze with referent; pr_with_referent = parent referential gaze with referent
#mg_with_referent = mutual gaze with referent; so_with_referent = shared object gaze with referent
m0 <- lm(rec_1518mo ~ `mg_with_referent`, data = fin_sp); summary(m0) #ns (p = .272)
m0 <- lm(rec_1518mo ~ `cr_with_referent`, data = fin_sp); summary(m0) #ns (p = .060)

m0 <- lm(rec_1518mo ~ `pr_with_referent`, data = fin_sp); summary(m0) #sig (p = .010)
confint(m0)
m0 <- lm(rec_1518mo ~ `so_with_referent`, data = fin_sp); summary(m0) #sig (p = .010)
confint(m0)


# Plots ----

shift_plot_rl <- tg_shifts %>% 
  ggplot(aes(x = parent_tg_shifting, y = rec_1518mo)) +
  geom_point() +
  ylim(20,80) + 
  labs(y = "Receptive Language at 15-18mo", x = "Parent Triadic Gaze Shifting at 15-18mo") +
  theme(
    axis.title.x = element_text(family = 'Times New Roman', size = 14, margin = margin(t = 10)),
    axis.text.x = element_text(family = 'Times New Roman', size = 14),
    axis.text.y = element_text(family = 'Times New Roman', size = 14),
    axis.title.y = element_text(family = 'Times New Roman', size = 14, margin = margin(r = 10)),
    plot.title = element_text(family = 'Times New Roman', size = 14, hjust = 0)
  ) +
  geom_smooth(method = 'lm', se = TRUE, size = 0.5); shift_plot_rl

shift_plot_el <- tg_shifts %>% 
  ggplot(aes(x = parent_tg_shifting, y = prod_1518mo)) +
  geom_point() +
  ylim(20,80) + 
  labs(y = "Expressive Language at 15-18mo", x = "Parent Triadic Gaze Shifting at 15-18mo") +
  theme(
    axis.title.x = element_text(family = 'Times New Roman', size = 14, margin = margin(t = 10)),
    axis.text.x = element_text(family = 'Times New Roman', size = 14),
    axis.text.y = element_text(family = 'Times New Roman', size = 14),
    axis.title.y = element_text(family = 'Times New Roman', size = 14, margin = margin(r = 10)),
    plot.title = element_text(family = 'Times New Roman', size = 14, hjust = 0)
  ) +
  geom_smooth(method = 'lm', se = TRUE, size = 0.5); shift_plot_el

shift_rec24_plot <- tg_shifts %>% 
  ggplot(aes(x = parent_tg_shifting, y = rec_24mo)) +
  geom_point() +
  labs(y = "Receptive Vocabulary at 24mo", x = "Parent Triadic Gaze Shifting at 15-18mo") +
  theme(
    axis.title.x = element_text(family = 'Times New Roman', size = 14, margin = margin(t = 10)),
    axis.text.x = element_text(family = 'Times New Roman', size = 14),
    axis.text.y = element_text(family = 'Times New Roman', size = 14),
    axis.title.y = element_text(family = 'Times New Roman', size = 14, margin = margin(r = 10)),
    plot.title = element_text(family = 'Times New Roman', size = 14, hjust = 0)
  ) +
  geom_smooth(method = 'lm', se = TRUE, size = 0.5); shift_rec24_plot

cja_ref_plot <- cja_m %>% 
  ggplot(aes(x = (cja_with_referent * 100), y = rec_1518mo)) +
  geom_point() +
  ylim(20,80) + 
  labs(y = "Receptive Language at 15-18mo", x = "Referential Labelling during Coordinated Attention at 15-18mo") +
  theme(
    axis.title.x = element_text(family = 'Times New Roman', size = 14, margin = margin(t = 10)),
    axis.text.x = element_text(family = 'Times New Roman', size = 14),
    axis.text.y = element_text(family = 'Times New Roman', size = 14),
    axis.title.y = element_text(family = 'Times New Roman', size = 14, margin = margin(r = 10)),
    plot.title = element_text(family = 'Times New Roman', size = 14, hjust = 0)
  ) +
  geom_smooth(method = 'lm', se = TRUE, size = 0.5); cja_ref_plot

final_plot <- plot_grid(shift_plot_rl, shift_plot_el, shift_rec24_plot, cja_ref_plot, ncol = 2, labels = c("A", "B", "C", "D"),
                        label_size = 14,rel_heights = c(1, 1)); final_plot

library(showtext)
showtext_auto()
ggsave("/your/save/path/here", final_plot, width = 12, height = 12)

