## Pretty row labels in the order you want them to appear
var_order <- c(
  "Black_Frost"       = "Black Frost",
  "Heat"              = "Heat",
  "Spring_Drought"    = "Spring Drought",
  "Summer_Drought"    = "Summer Drought",
  "Spring_Waterlogging" = "Spring Waterlogging",
  "Summer_Waterlogging" = "Summer Waterlogging"
)

fmt_cell <- function(est, se) sprintf("%.4f (%.4f)", est, se)

## Map model names to column headers used in the table
model_lab <- c(
  "NUTS1"  = "NUTS-1",
  "NUTS2"  = "NUTS-2",
  "NUTS3 (probabilistic)"  = "NUTS-3 (probabilistic)",
  "Schmitt"= "Municipality (Schmitt et al.)"
)

## Bottom panel information (edit to your actual counts/settings)
obs_by_model <- c(
  "NUTS-1"          = "46,806",
  "NUTS-2"          = "46,806",
  "NUTS-3 (probabilistic)"          = "46,806",
  "Municipality (Schmitt et al.)"  = "165,602"
)
fe_text  <- "Farm & Year"
se_text  <- "by: farm & year"

## ---- build the top panel (variables × models) ----
top_panel <- all_results %>%
  mutate(
    term  = recode(term, !!!var_order),          # pretty labels
    Model = recode(Model, !!!model_lab),         # pretty model names
    cell  = fmt_cell(estimate, std.error)
  ) %>%
  filter(term %in% unname(var_order),            # just the variables you want
         Model %in% unname(model_lab)) %>%
  select(term, Model, cell) %>%
  # make sure rows are in the requested order
  mutate(term = factor(term, levels = unname(var_order))) %>%
  arrange(term) %>%
  pivot_wider(names_from = Model, values_from = cell) %>%
  # ensure all model columns exist even if missing in data
  {
    needed <- unname(model_lab)
    missing_cols <- setdiff(needed, names(.))
    if (length(missing_cols)) mutate(., !!!setNames(rep(list(character(nrow(.))), length(missing_cols)), missing_cols)) else .
  } %>%
  select(term, `NUTS-1`, `NUTS-2`, `NUTS-3 (probabilistic)`, `Municipality (Schmitt et al.)`) %>%
  rename(` ` = term)                                # blank header for row labels

## ---- build the bottom panel ----
bottom_panel <- tibble(
  ` `            = c("Observations", "Fixed-Effects:", "S.E.: Clustered"),
  `NUTS-1`       = c(obs_by_model[["NUTS-1"]],      fe_text, se_text),
  `NUTS-2`       = c(obs_by_model[["NUTS-2"]],      fe_text, se_text),
  `NUTS-3 (probabilistic)`       = c(obs_by_model[["NUTS-3 (probabilistic)"]],      fe_text, se_text),
  `Municipality (Schmitt et al.)`= c(obs_by_model[["Municipality (Schmitt et al.)"]], fe_text, se_text)
)

final_tbl <- bind_rows(top_panel, bottom_panel)

## ---- make the LaTeX table ----
last_var_row <- nrow(top_panel)

latex_code <- kbl(
  final_tbl,
  format   = "latex",
  booktabs = TRUE,
  align    = c("l", "r", "r", "r", "r"),
  caption  = "Table 2: Winter wheat - regression results",
  col.names = c("", "NUTS-1", "NUTS-2", "NUTS-3 (probabilistic)", "Municipality (Schmitt et al.)")
) %>%
  add_header_above(c(" " = 1, "Estimate (SE)" = 4)) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::row_spec(last_var_row, hline_after = TRUE)

