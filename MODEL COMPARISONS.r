
# AU Basketball — Model Comparison


library(tidyverse)
library(rpart)        # single decision tree
library(rpart.plot)   # tree visualisation
library(ranger)       # fast random forest
library(pROC)         # AUC / ROC curves
library(caret)        # confusion matrix


game     <- read.csv("~/Downloads/Game_Cleaned (1).csv",     stringsAsFactors = FALSE)
practice <- read.csv("~/Downloads/Practice_Cleaned (1).csv", stringsAsFactors = FALSE)


prep_shots <- function(df) {
  df %>%
    mutate(
      made      = as.integer(MadeShot == "TRUE" | MadeShot == TRUE),
      shot_zone = factor(ShotZone,
                         levels = c("In the Paint", "Mid Range 2's",
                                    "Corner 3's", "Above Break 3's")),
      contest   = factor(Shot.Contest,
                         levels = c("Wide Open", "Lightly Contested",
                                    "Highly Contested", "Blocked")),
      shot_type = factor(Shot.Type),
      shot_clock = case_when(
        Shot.Clock == "5-0"   ~ 1,
        Shot.Clock == "10-6"  ~ 2,
        Shot.Clock == "21-11" ~ 3,
        Shot.Clock == "22+"   ~ 4,
        TRUE ~ NA_real_
      ),
      shot_value = as.numeric(ShotValue)
    ) %>%
    filter(!is.na(made), !is.na(shot_clock), !is.na(shot_value))
}

train <- prep_shots(practice)   # practice  → train
test  <- prep_shots(game)       # game      → test


PREDS <- c("shot_zone", "shot_clock", "contest", "shot_type")


# MODEL 1 — Logistic Regression

cat("\n── Logistic Regression ─────────────────────────────────────────────────\n")

glm_fit <- glm(
  made ~ shot_zone + shot_clock + contest + shot_type,
  data   = train,
  family = binomial(link = "logit")
)

summary(glm_fit)

test$p_glm <- predict(glm_fit, newdata = test, type = "response")


# MODEL 2 — Single Decision Tree (rpart)

cat("\n── Decision Tree (rpart) ───────────────────────────────────────────────\n")

tree_fit <- rpart(
  made ~ shot_zone + shot_clock + contest + shot_type,
  data    = train,
  method  = "class",
  control = rpart.control(
    minsplit = 20,   
    cp       = 0.005 
  )
)


printcp(tree_fit)


best_cp   <- tree_fit$cptable[which.min(tree_fit$cptable[, "xerror"]), "CP"]
tree_pruned <- prune(tree_fit, cp = best_cp)

cat("\nPruned tree:\n")
print(tree_pruned)


par(mar = c(1, 1, 2, 1))
rpart.plot(
  tree_pruned,
  type    = 4,      # all labels at nodes
  extra   = 104,    # show probability + % obs at leaves
  fallen.leaves = TRUE,
  main    = "Pruned Decision Tree — Shot Make Probability"
)


test$p_tree <- predict(tree_pruned, newdata = test, type = "prob")[, "1"]


# MODEL 3 — Random Forest (ranger)

cat("\n── Random Forest (ranger) ──────────────────────────────────────────────\n")

# ranger needs the outcome as a factor for classification
train_rf <- train %>%
  mutate(made_fac = factor(made, levels = c(0, 1), labels = c("miss", "make")))

rf_fit <- ranger(
  made_fac ~ shot_zone + shot_clock + contest + shot_type,
  data             = train_rf,
  num.trees        = 500,
  mtry             = 2,          
  min.node.size    = 10,
  importance       = "impurity", 
  probability      = TRUE,      
  seed             = 123
)

cat("\nRandom forest summary:\n")
print(rf_fit)


cat("\nVariable importance (Gini impurity):\n")
imp <- sort(rf_fit$variable.importance, decreasing = TRUE)
print(round(imp, 2))


barplot(
  imp,
  main   = "Random Forest — Variable Importance",
  col    = "#002147",
  horiz  = TRUE,
  las    = 1,
  xlab   = "Mean Gini Decrease"
)


test$p_rf <- predict(rf_fit, data = test)$predictions[, "make"]


# MODEL COMPARISON METRICS

cat("\n══ Model Comparison ═══════════════════════════════════════════════════\n")


model_metrics <- function(actual, probs, model_name, threshold = 0.5) {

  predicted <- factor(ifelse(probs >= threshold, 1, 0), levels = c(0, 1))
  actual_f  <- factor(actual, levels = c(0, 1))

  cm      <- confusionMatrix(predicted, actual_f, positive = "1")
  roc_obj <- roc(actual, probs, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))
  brier   <- mean((probs - actual)^2)
  log_loss <- -mean(actual * log(probs + 1e-9) +
                    (1 - actual) * log(1 - probs + 1e-9))

  tibble(
    Model     = model_name,
    AUC       = round(auc_val, 4),
    Accuracy  = round(cm$overall["Accuracy"], 4),
    Precision = round(cm$byClass["Precision"], 4),
    Recall    = round(cm$byClass["Recall"], 4),
    F1        = round(cm$byClass["F1"], 4),
    Brier     = round(brier, 4),
    LogLoss   = round(log_loss, 4)
  )
}

metrics_table <- bind_rows(
  model_metrics(test$made, test$p_glm,  "Logistic Regression"),
  model_metrics(test$made, test$p_tree, "Decision Tree (rpart)"),
  model_metrics(test$made, test$p_rf,   "Random Forest (ranger)")
)

cat("\nMetrics summary (test = game data):\n")
print(as.data.frame(metrics_table))


roc_glm  <- roc(test$made, test$p_glm,  quiet = TRUE)
roc_tree <- roc(test$made, test$p_tree, quiet = TRUE)
roc_rf   <- roc(test$made, test$p_rf,   quiet = TRUE)

plot(
  roc_glm,
  col  = "#CC0000", lwd = 2,
  main = "ROC Curves — Game Data",
  xlab = "False Positive Rate", ylab = "True Positive Rate"
)
plot(roc_tree, col = "#e8a838", lwd = 2, add = TRUE)
plot(roc_rf,   col = "#3aaa72", lwd = 2, add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "grey60")
legend(
  "bottomright",
  legend = c(
    paste0("Logistic Regression (AUC = ", round(auc(roc_glm),  3), ")"),
    paste0("Decision Tree       (AUC = ", round(auc(roc_tree), 3), ")"),
    paste0("Random Forest       (AUC = ", round(auc(roc_rf),   3), ")")
  ),
  col = c("#CC0000", "#e8a838", "#3aaa72"),
  lwd = 2, cex = 0.85
)


calibration_plot <- function(actual, probs, model_name, colour) {
  tibble(actual = actual, prob = probs) %>%
    mutate(bucket = ntile(prob, 10)) %>%
    group_by(bucket) %>%
    summarise(mean_pred = mean(prob),
              mean_act  = mean(actual),
              .groups = "drop") %>%
    mutate(model = model_name, colour = colour)
}

cal_all <- bind_rows(
  calibration_plot(test$made, test$p_glm,  "Logistic Regression",   "#CC0000"),
  calibration_plot(test$made, test$p_tree, "Decision Tree (rpart)", "#e8a838"),
  calibration_plot(test$made, test$p_rf,   "Random Forest (ranger)", "#3aaa72")
)

ggplot(cal_all, aes(x = mean_pred, y = mean_act, colour = model)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(
    values = c(
      "Logistic Regression"   = "#CC0000",
      "Decision Tree (rpart)" = "#e8a838",
      "Random Forest (ranger)"= "#3aaa72"
    )
  ) +
  labs(
    title   = "Calibration Plot — Predicted vs Actual FG%",
    subtitle = "Each point = one decile of predicted probability",
    x       = "Mean predicted probability",
    y       = "Actual FG%",
    colour  = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", colour = "#002147"),
    legend.position = "top"
  )


# PER-PLAYER PAE COMPARISON ACROSS MODELS

cat("\n══ Per-player PAE comparison ══════════════════════════════════════════\n")

player_pae <- test %>%
  mutate(
    exp_pts_glm  = p_glm  * shot_value,
    exp_pts_tree = p_tree * shot_value,
    exp_pts_rf   = p_rf   * shot_value,
    actual_pts   = made   * shot_value
  ) %>%
  group_by(Shooter) %>%
  summarise(
    attempts          = n(),
    actual_pts        = round(sum(actual_pts), 1),

    # GLM
    exp_pts_glm       = round(sum(exp_pts_glm), 1),
    pae_glm           = round(actual_pts - exp_pts_glm, 1),
    pae100_glm        = round(100 * pae_glm / attempts, 1),

    # Tree
    exp_pts_tree      = round(sum(exp_pts_tree), 1),
    pae_tree          = round(actual_pts - exp_pts_tree, 1),
    pae100_tree       = round(100 * pae_tree / attempts, 1),

    # RF
    exp_pts_rf        = round(sum(exp_pts_rf), 1),
    pae_rf            = round(actual_pts - exp_pts_rf, 1),
    pae100_rf         = round(100 * pae_rf / attempts, 1),

    .groups = "drop"
  ) %>%
  arrange(desc(pae100_rf))

cat("\nPAE per 100 shots by model (sorted by RF):\n")
print(as.data.frame(
  player_pae %>%
    select(Shooter, attempts, actual_pts,
           pae100_glm, pae100_tree, pae100_rf)
))


