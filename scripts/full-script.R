install.packages("readxl")

library(readxl)

df <- read_excel("/Users/nguyenthuyhien/Downloads/ISYS3447_ISYS3448_A3_IntelliAuto (2).xlsx")
View(df)
library(dplyr)

############################################
# DATA PREPROCESSING – R STUDIO SCRIPT
############################################

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)

############################################
# 1. Missing Values
############################################

# Total missing values
total_missing <- sum(is.na(df))

# Missing values by variable
missing_by_var <- colSums(is.na(df))

# Missing percentage
missing_pct <- round(colMeans(is.na(df)) * 100, 2)

############################################
# 2. Duplicate Observations
############################################

# Number of duplicate rows
num_duplicates <- sum(duplicated(df))

############################################
# 3. Skewness (Numeric Variables Only)
############################################

numeric_vars <- df %>% select(where(is.numeric))

skewness_values <- sapply(numeric_vars, skewness, na.rm = TRUE)

############################################
# 4. Outlier Detection (IQR Method)
############################################

detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
}

outlier_count <- sapply(numeric_vars, function(x) {
  sum(detect_outliers(x), na.rm = TRUE)
})

############################################
# 5. df Quality Summary Table
############################################

df_quality_summary <- data.frame(
  Variable = names(numeric_vars),
  Missing_Percent = missing_pct[names(numeric_vars)],
  Skewness = round(skewness_values, 2),
  Outliers = outlier_count
)

print(df_quality_summary)

############################################
# 6. Distribution Plots (Optional – Appendix)
############################################

# Histogram
numeric_vars %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~ name, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_blank()
  ) +
  labs(
    title = "Distribution of Numerical Variables"
  )


# Boxplot
numeric_vars %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(y = value, fill = name)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
  facet_wrap(~ name, scales = "free") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Boxplots of Numerical Variables with Outliers Highlighted"
  )


cat("Total missing values:", total_missing, "\n\n")

cat("Missing values by variable:\n")
print(missing_by_var)

cat("\nMissing percentage (%):\n")
print(missing_pct)

cat("\nNumber of duplicate rows:", num_duplicates, "\n\n")

cat("Data quality summary:\n")
print(df_quality_summary)

install.packages("DescTools")
library(DescTools)
num0 <- df %>% select(where(is.numeric))

# IQR outlier detector
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQRv <- Q3 - Q1
  (x < (Q1 - 1.5 * IQRv)) | (x > (Q3 + 1.5 * IQRv))
}

# Diagnostics BEFORE treatment
skew_before <- sapply(num0, skewness, na.rm = TRUE)
out_before  <- sapply(num0, function(x) sum(detect_outliers(x), na.rm = TRUE))

before_summary <- data.frame(
  Variable = names(num0),
  Skewness_Before = round(skew_before, 2),
  Outliers_Before = out_before
)

############################################
# OUTLIER HANDLING (Selected & Justified) – df
# - WorkHrs: winsorize (1% and 99%) to reduce extreme influence
# - NumPromo: log1p transform to reduce right-skew
# - EmpYears: keep original (avoid separation / preserve heterogeneity)
############################################
install.packages("DescTools")
library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)

############################################
# Robust Winsorize (Version-proof)
############################################
winsorize_pct <- function(x, p = c(0.01, 0.99)) {
  qs <- quantile(x, probs = p, na.rm = TRUE, type = 7)
  x[x < qs[1]] <- qs[1]
  x[x > qs[2]] <- qs[2]
  x
}

# IQR outlier detector
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQRv <- Q3 - Q1
  (x < (Q1 - 1.5 * IQRv)) | (x > (Q3 + 1.5 * IQRv))
}

############################################
# BEFORE diagnostics
############################################
num0 <- df %>% select(where(is.numeric))

before_summary <- data.frame(
  Variable = names(num0),
  Skewness_Before = round(sapply(num0, skewness, na.rm = TRUE), 2),
  Outliers_Before = sapply(num0, function(x) sum(detect_outliers(x), na.rm = TRUE))
)

############################################
# OUTLIER TREATMENT (Selected & Justified)
# - WorkHrs: winsorize (1% and 99%)
# - NumPromo: log1p transform
# - EmpYears: keep original
############################################
df <- df %>%
  mutate(
    WorkHrs_w    = if ("WorkHrs" %in% names(.)) winsorize_pct(.data$WorkHrs, p = c(0.01, 0.99)) else NA_real_,
    NumPromo_log = if ("NumPromo" %in% names(.)) log1p(.data$NumPromo) else NA_real_
  )

############################################
# AFTER diagnostics
############################################
num1 <- df %>%
  select(where(is.numeric)) %>%
  select(any_of(c(names(num0), "WorkHrs_w", "NumPromo_log")))

after_summary <- data.frame(
  Variable = names(num1),
  Skewness_After = round(sapply(num1, skewness, na.rm = TRUE), 2),
  Outliers_After = sapply(num1, function(x) sum(detect_outliers(x), na.rm = TRUE))
)

############################################
# COMPARISON TABLE
############################################
comparison <- full_join(before_summary, after_summary, by = "Variable") %>%
  arrange(Variable)

cat("\n===== OUTLIER HANDLING SUMMARY (BEFORE vs AFTER) =====\n")
print(comparison)

############################################
# QUICK CHECK (Original vs Treated)
############################################
cat("\n===== QUICK CHECK =====\n")
if ("WorkHrs" %in% names(df)) {
  cat("\nWorkHrs (original):\n"); print(summary(df$WorkHrs))
  cat("\nWorkHrs_w (winsorized 1%-99%):\n"); print(summary(df$WorkHrs_w))
}
if ("NumPromo" %in% names(df)) {
  cat("\nNumPromo (original):\n"); print(summary(df$NumPromo))
  cat("\nNumPromo_log (log1p):\n"); print(summary(df$NumPromo_log))
}

############################################
# PLOTS (Treated variables only)
############################################
treated_vars <- df %>%
  select(any_of(c("WorkHrs_w", "NumPromo_log"))) %>%
  select(where(is.numeric))

if (ncol(treated_vars) > 0) {
  
  # Histogram
  treated_vars %>%
    pivot_longer(cols = everything()) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    facet_wrap(~ name, scales = "free") +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_blank()
    ) +
    labs(title = "Distribution of Treated Variables")
  
  # Boxplot
  treated_vars %>%
    pivot_longer(cols = everything()) %>%
    ggplot(aes(y = value, fill = name)) +
    geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
    facet_wrap(~ name, scales = "free") +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.y = element_blank()
    ) +
    labs(title = "Boxplots of Treated Variables with Outliers Highlighted")
}

cat("\nNOTE:\n")
cat("- Use WorkHrs_w instead of WorkHrs (winsorized).\n")
cat("- Use NumPromo_log instead of NumPromo (log-transformed).\n")
cat("- Keep EmpYears unchanged to avoid separation issues.\n")

            # Create target variable: leaving in less than 3 years
            df$lessThan3 <- ifelse(df$EmpYears < 3, 1, 0)
            df$lessThan3 <- as.factor(df$lessThan3)
            
            # =========================
            # 1) Identify variable types
            # =========================
            # (Adjust these lists if your dataset has different columns)
            cont_vars <- c("WorkHrs", "Age", "Educ_Yrs", "WrkYears", "EmpYears", "NumPromo")
            cat_vars  <- c("Occup_n", "Sex", "MemUnion", "FutPromo", "SexPromo", "AwareI4")
            
            # Keep only variables that actually exist in df (avoid errors)
            cont_vars <- cont_vars[cont_vars %in% names(df)]
            cat_vars  <- cat_vars[cat_vars %in% names(df)]
            
            # =========================
            # 2) T-tests for continuous variables
            # =========================
            t_test_results <- lapply(cont_vars, function(v) {
              x0 <- df %>% filter(lessThan3 == 0) %>% pull(!!sym(v))
              x1 <- df %>% filter(lessThan3 == 1) %>% pull(!!sym(v))
              
              # Remove NA just in case
              x0 <- x0[!is.na(x0)]
              x1 <- x1[!is.na(x1)]
              
              # Run t-test (Welch by default)
              tt <- t.test(x1, x0)
              
              data.frame(
                variable = v,
                mean_leave1 = mean(x1),
                mean_leave0 = mean(x0),
                t_stat = unname(tt$statistic),
                p_value = tt$p.value,
                stringsAsFactors = FALSE
              )
            }) %>% bind_rows() %>% arrange(p_value)
            
            print(t_test_results)
            
            # =========================
            # 3) Chi-square tests for categorical variables
            # =========================
            chisq_results <- lapply(cat_vars, function(v) {
              
              # Build contingency table
              tab <- table(df[[v]], df$lessThan3, useNA = "no")
              
              # If any expected counts are too small, fall back to Fisher's Exact (optional)
              # Here we still run chisq.test with simulated p-value if needed
              test <- suppressWarnings(chisq.test(tab))
              
              data.frame(
                variable = v,
                chi_sq = unname(test$statistic),
                df = unname(test$parameter),
                p_value = test$p.value,
                stringsAsFactors = FALSE
              )
            }) %>% bind_rows() %>% arrange(p_value)
            
            print(chisq_results)
            
            # =========================
            # 4) Full logistic regression + AIC stepwise
            # =========================
            # Build a full model using all candidate predictors (continuous + categorical)
            candidate_vars <- c(cont_vars, cat_vars)
            
            # Ensure categoricals are factors
            for (v in cat_vars) df[[v]] <- as.factor(df[[v]])
            
            full_formula <- as.formula(
              paste("lessThan3 ~", paste(candidate_vars, collapse = " + "))
            )
            
            full_model <- glm(full_formula, data = df, family = binomial())
            
            # Stepwise selection by AIC (both directions)
            step_model <- step(full_model, direction = "both", trace = 0)
            
            summary(step_model)
            
            # Odds ratios (easier to interpret)
            odds_ratios <- exp(coef(step_model))
            odds_ratios
            
            # Optional: AIC values
            cat("Full model AIC:", AIC(full_model), "\n")
            cat("Stepwise model AIC:", AIC(step_model), "\n")`

            # Remove EmpYears from predictors
            model_B <- glm(
              lessThan3 ~ Age + WrkYears + NumPromo + WorkHrs +
                Occup_n + FutPromo + SexPromo,
              data = df,
              family = binomial()
            )
            
            summary(model_B)
            
            # Stepwise AIC
            model_B_step <- step(model_B, direction = "both", trace = 0)
            summary(model_B_step)
            
            # Predicted probabilities
            df$probability_B <- predict(model_B_step, type = "response")
            
            
            
            
            # =========================
            # FULL CODE: Interaction tests + stepwise interaction model
            # Variables: WrkYears, NumPromo, FutPromo, SexPromo
            # =========================
            
            library(dplyr)
            
            # 0) Ensure variable types
            df <- df %>%
              mutate(
                lessThan3 = factor(lessThan3, levels = c(0, 1)),
                FutPromo  = as.factor(FutPromo),
                SexPromo  = as.factor(SexPromo)
              )
            
            # -------------------------
            # 1) Base model (no interaction)
            # -------------------------
            m0 <- glm(
              lessThan3 ~ WrkYears + NumPromo + FutPromo + SexPromo,
              data = df,
              family = binomial()
            )
            
            cat("\n=== BASE MODEL (m0) ===\n")
            print(summary(m0))
            cat("\nAIC(m0):", AIC(m0), "\n")
            
            # -------------------------
            # 2) Test one interaction at a time (LRT + AIC)
            # -------------------------
            interaction_terms <- c(
              "WrkYears:NumPromo",
              "WrkYears:FutPromo",
              "WrkYears:SexPromo",
              "NumPromo:FutPromo",
              "NumPromo:SexPromo",
              "FutPromo:SexPromo"
            )
            
            results <- lapply(interaction_terms, function(term) {
              
              f1 <- as.formula(
                paste("lessThan3 ~ WrkYears + NumPromo + FutPromo + SexPromo +", term)
              )
              
              m1 <- glm(f1, data = df, family = binomial())
              
              lrt <- anova(m0, m1, test = "Chisq")
              
              data.frame(
                interaction = term,
                AIC_base = AIC(m0),
                AIC_with_interaction = AIC(m1),
                delta_AIC = AIC(m1) - AIC(m0),     # <0 means improves AIC
                LRT_p_value = lrt$`Pr(>Chi)`[2]    # <0.05 means significant improvement
              )
            })
            
            interaction_results <- bind_rows(results) %>%
              arrange(delta_AIC)
            
            cat("\n=== ONE-BY-ONE INTERACTION TESTS (LRT + AIC) ===\n")
            print(interaction_results)
            
            cat("\nRule of thumb: keep an interaction if delta_AIC < 0 AND LRT_p_value < 0.05\n")
            
            # -------------------------
            # 3) Full pairwise interaction model + stepwise backward AIC
            # -------------------------
            m_full_int <- glm(
              lessThan3 ~ (WrkYears + NumPromo + FutPromo + SexPromo)^2,
              data = df,
              family = binomial()
            )
            
            cat("\n=== FULL PAIRWISE INTERACTION MODEL (m_full_int) ===\n")
            cat("AIC(m_full_int):", AIC(m_full_int), "\n")
            
            m_step_int <- step(m_full_int, direction = "backward", trace = 0)
            
            cat("\n=== STEPWISE (BACKWARD) INTERACTION MODEL (m_step_int) ===\n")
            print(summary(m_step_int))
            cat("\nAIC(m_step_int):", AIC(m_step_int), "\n")
            cat("\nAIC(m0):", AIC(m0), "\n")
            
            # -------------------------
            # 4) Quick comparison: base vs stepwise interaction model
            # -------------------------
            cat("\n=== MODEL COMPARISON ===\n")
            cat("Base model AIC (m0):", AIC(m0), "\n")
            cat("Stepwise interaction model AIC (m_step_int):", AIC(m_step_int), "\n")
            
            if (AIC(m_step_int) < AIC(m0)) {
              cat("Conclusion: Interaction model improves AIC (better fit after penalty).\n")
            } else {
              cat("Conclusion: Base model is preferred (interactions do not improve AIC).\n")
            }
            


# Select any 20 employees with high probability
top20 <- df %>%
  arrange(desc(probability_B)) %>%
  slice_head(n = 20)

print(top20)

install.packages("writexl")   
library(writexl)
write_xlsx(top20, "top20_employees_probability_B.xlsx")


library(dplyr)

# Ensure categorical variables are factors
df <- df %>%
  mutate(
    lessThan3 = factor(lessThan3, levels = c(0,1)),
    SexPromo = as.factor(SexPromo),
    Occup_n  = as.factor(Occup_n),
    FutPromo = as.factor(FutPromo)
  )

vars <- c("SexPromo", "Occup_n", "FutPromo", "NumPromo", "WrkYears", "Age")

# Full model (Hướng B)
full_formula <- as.formula(paste("lessThan3 ~", paste(vars, collapse = " + ")))
m_full <- glm(full_formula, data = df, family = binomial())

AIC_full <- AIC(m_full)

# Drop-one-variable AIC test
aic_drop <- lapply(vars, function(v) {
  f_drop <- as.formula(paste("lessThan3 ~", paste(setdiff(vars, v), collapse = " + ")))
  m_drop <- glm(f_drop, data = df, family = binomial())
  data.frame(
    dropped = v,
    AIC_full = AIC_full,
    AIC_drop = AIC(m_drop),
    delta_AIC = AIC(m_drop) - AIC_full,   # <0 means dropping improves AIC
    improves_if_drop = AIC(m_drop) < AIC_full
  )
}) %>% bind_rows() %>% arrange(delta_AIC)

print(aic_drop)


final_model <- glm(
  lessThan3 ~ WrkYears + NumPromo + FutPromo + SexPromo,
  data = df,
  family = binomial()
)

summary(final_model)
AIC(final_model)


library(dplyr)

ids <- c(43,525,769,307,422,480,532,776,47,529,773,369,398,20,415,502,554,746,121,620)

df_prob <- df %>%
  filter(IdNum %in% ids) %>%
  select(IdNum, Occup_n, Sex, probability) %>%
  arrange(match(IdNum, ids))

print(df_prob, n = 20)

