library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


contexts <- read.csv("/Users/lauralondono/Desktop/Data Care Robots/Documents Analysis Jasp/jasp_contexts.csv", stringsAsFactors = FALSE)

# Identify participant ID column (CASE/case)
id_col <- intersect(c("CASE", "case"), names(contexts))
if (length(id_col) == 0) stop("Could not find CASE/case column in contexts.")
id_col <- id_col[[1]]

# language column (language/Language), if present
lang_col <- intersect(c("language", "Language"), names(contexts))
lang_col <- if (length(lang_col) == 1) lang_col[[1]] else NULL
if (is.null(lang_col)) stop("Could not find language/Language column.")

dir.create("results", showWarnings = FALSE)

# DEFINE use_cols (multiple-selection columns) 
use_cols <- use_cols[str_detect(use_cols, "_use_[1-5]$")]

if (length(use_cols) == 0) {
  stop("No columns matched '_use_[1-5]$'. Check variable naming.")
}

#Recode 1/2: 1 not selected, 2 selected

contexts_bin <- contexts %>%
  mutate(across(all_of(use_cols), ~ case_when(
    .x == 2 ~ 1L,   # SELECTED
    .x == 1 ~ 0L,   # NOT selected
    TRUE ~ NA_integer_
  )))

cat("\nSanity check pooled (should be 0/1/NA):\n")
print(table(unlist(contexts_bin[use_cols]), useNA = "ifany"))

# Build long dataset (participant × robot × context)

contexts_long <- contexts_bin %>%
  pivot_longer(
    cols = all_of(use_cols),
    names_to = "variable",
    values_to = "selected"
  ) %>%
  mutate(
    participant_id = .data[[id_col]],
    robot = sub("_use_.*", "", variable),
    context_id = sub(".*_use_", "", variable),
    robot_label = recode(robot,
                         ds  = "Delivering supplies",
                         bed = "Helping into bed",
                         mvs = "Monitoring vital signs",
                         am  = "Assisting with mobility",
                         .default = robot
    ),
    context_label = recode(context_id,
                           "1" = "General hospitals",
                           "2" = "Pediatric hospitals",
                           "3" = "Geriatric hospitals",
                           "4" = "Psychiatric hospitals",
                           "5" = "Trauma & orthopedic",
                           .default = context_id
    )
  ) %>%
  filter(selected %in% c(0, 1)) %>%
  mutate(
    participant_id = as.factor(participant_id),
    robot_label = as.factor(robot_label),
    context_label = as.factor(context_label)
  ) %>%
  droplevels()

cat("\nSanity check: prop selected by robot x context:\n")
print(
  contexts_long %>%
    group_by(robot_label, context_label) %>%
    summarise(prop = mean(selected), .groups = "drop")
)
# Descriptive plot

desc_table <- contexts_long %>%
  group_by(robot_label, context_label) %>%
  summarise(
    n = n(),
    prop_selected = mean(selected),
    percent_selected = 100 * mean(selected),
    .groups = "drop"
  )

write.csv(desc_table, "results/rq2_desc_prop_selected_robot_by_context.csv", row.names = FALSE)

p_obs <- ggplot(desc_table, aes(x = context_label, y = prop_selected, fill = robot_label)) +
  geom_col(position = "dodge") +
  labs(
    x = "Medical context",
    y = "Observed proportion selected",
    fill = "Robot type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_obs)

ggsave("results/rq2_observed_proportions_robot_by_context.png",
       p_obs, width = 10, height = 5, dpi = 300)

#Cochran´s Q per robot

wide_for_q <- contexts_long %>%
  select(participant_id, robot_label, context_label, selected) %>%
  pivot_wider(names_from = context_label, values_from = selected)

ctx_cols <- setdiff(names(wide_for_q), c("participant_id", "robot_label"))
if (length(ctx_cols) < 3) stop("Not enough context columns after pivot_wider(). ")

q_results <- wide_for_q %>%
  group_by(robot_label) %>%
  group_modify(~{
    df <- .x %>% select(all_of(ctx_cols))
    df_cc <- df[complete.cases(df), , drop = FALSE]
    
    k <- ncol(df_cc)
    cm <- colMeans(df_cc)
    rm <- rowMeans(df_cc)
    
    denom <- (k * sum(rm) - (sum(cm))^2)
    
    if (abs(denom) < 1e-12) {
      Q <- 0
      p_val <- 1
    } else {
      Q <- (k - 1) * (k * sum(rm^2) - sum(cm^2)) / denom
      p_val <- pchisq(Q, df = k - 1, lower.tail = FALSE)
    }
    
    tibble(
      n_participants = nrow(df_cc),
      statistic = Q,
      df = k - 1,
      p_value = p_val
    )
  }) %>%
  ungroup()

print(q_results)

write.csv(
  q_results,
  "results/rq2_cochransQ_by_robot.csv",
  row.names = FALSE
)

# Language distribution

print(table(contexts[[lang_col]]))

# Mixed model with language
# Cochran's Q manual 
cochran_q_manual <- function(df_cc) {
  k <- ncol(df_cc)
  cm <- colMeans(df_cc)
  rm <- rowMeans(df_cc)
  
  denom <- (k * sum(rm) - (sum(cm))^2)
  
  if (k < 3 || nrow(df_cc) == 0) {
    return(list(Q = NA_real_, df = k - 1, p = NA_real_, n = nrow(df_cc)))
  }
# if there is no variation between columns, Q=0, p=1
  if (sd(cm) < 1e-12 || abs(denom) < 1e-12) {
    return(list(Q = 0, df = k - 1, p = 1, n = nrow(df_cc)))
  }
  
  Q <- (k - 1) * (k * sum(rm^2) - sum(cm^2)) / denom
  p <- pchisq(Q, df = k - 1, lower.tail = FALSE)
  
  list(Q = Q, df = k - 1, p = p, n = nrow(df_cc))
}

# # Wide by participant, robot, language, with columns 1..5  
wide_for_q_lang <- contexts_long %>%
  select(participant_id, language, robot_label, context_label, selected) %>%
  pivot_wider(names_from = context_label, values_from = selected)

ctx_cols <- setdiff(names(wide_for_q_lang), c("participant_id", "language", "robot_label"))
if (length(ctx_cols) < 3) stop("No hay suficientes contextos (columnas) para Cochran's Q.")

# Cochran's Q robot x language
q_results_lang <- wide_for_q_lang %>%
  group_by(language, robot_label) %>%
  group_modify(~{
    df <- .x %>% select(all_of(ctx_cols))
    df_cc <- df[complete.cases(df), , drop = FALSE]
    
    out <- cochran_q_manual(df_cc)
    tibble(
      n_participants = out$n,
      statistic = out$Q,
      df = out$df,
      p_value = out$p
    )
  }) %>%
  ungroup() %>%
  arrange(language, robot_label)

print(q_results_lang)

write.csv(q_results_lang, "results/rq2_cochransQ_by_robot_by_language.csv", row.names = FALSE)

# Plot by language

desc_lang <- contexts_long %>%
  group_by(language, robot_label, context_label) %>%
  summarise(
    prop_selected = mean(selected),
    .groups = "drop"
  )

p_lang <- ggplot(
  desc_lang,
  aes(x = context_label,
      y = prop_selected,
      fill = robot_label)
) +
  geom_col(position = "dodge") +
  facet_wrap(~ language) +   # <-- sepate by language
  labs(
    x = "Medical context",
    y = "Observed proportion selected",
    fill = "Robot type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

print(p_lang)

ggsave(
  "/Users/lauralondono/Desktop/Data Care Robots/results/rq2_observed_proportions_by_language.png",
  p_lang,
  width = 11,
  height = 5,
  dpi = 300
)

p_lang_lines <- ggplot(
  desc_lang,
  aes(x = context_label,
      y = prop_selected,
      color = robot_label,
      group = robot_label)
) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~ language) +
  labs(
    x = "Medical context",
    y = "Probability of selection",
    color = "Robot type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_lang_lines)
 
getwd()
    