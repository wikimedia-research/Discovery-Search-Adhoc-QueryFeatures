# devtools::install_github("slowkow/ggrepel")
library(ggrepel)

variable_importance <- data.frame(Variable = rownames(importance(rf)), importance(rf)) %>%
  set_rownames(NULL)
logit_coefs <- data.frame(Variable = gsub("`", "", names(logistic_regression$coefficients)),
                          Coefficient = logistic_regression$coefficients) %>%
  set_rownames(NULL)
temp <- dplyr::left_join(variable_importance, logit_coefs, by = "Variable")

# http://stackoverflow.com/questions/32123288/position-ggplot-text-in-each-corner
annotations <- data.frame(
  xpos = c(-0.5,-0.5,1.1,1.1),
  ypos =  c(-2, 6.5,-2, 5),
  hjustvar = c(0,0,1,1),
  vjustvar = c(0,1,0,1),
  feature = c("less important\nzero results less likely",
              "less important\nzero results more likely",
              "more important\nzero results less likely",
              "more important\nzero results more likely"))
annotations_top <- dplyr::filter(annotations, ypos > 0)
annotations_bottom <- dplyr::filter(annotations, ypos < 0)

p <- temp %>%
  set_colnames(c("Variable",
                 "MDA specific to queries with some results",
                 "MDA specific to queries with zero results",
                 "MDA (across all queries)",
                 "MDI Gini", "Coefficient")) %>%
  tidyr::gather(metric, var.imp, 2:5) %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(var.imp = var.imp/max(var.imp)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Coefficient)) %>%
  dplyr::mutate(lbl.a = ifelse(var.imp > 0, "more important", "less important"),
                lbl.b = ifelse(Coefficient > 0, "zero results more likely", "zero results less likely"),
                feature = paste(lbl.a, lbl.b, sep = "\n")) %>%
  ggplot(aes(x = var.imp, y = Coefficient)) +
  geom_point(aes(color = feature)) +
  geom_text_repel(aes(label = Variable), nudge_y = 0.5) +
  scale_color_brewer(type = "qual", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_y_continuous(limits = c(-3, 7)) + scale_x_continuous(limits = c(-0.5, 1.25)) +
  facet_wrap(~metric, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data = annotations_top,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar,
                label = feature, color = feature)) +
  geom_text(data = annotations_bottom,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar,
                label = feature, color = feature)) +
  labs(title = "Variable Importance vs Logistic Regression Estimates", subtitle = fig_subtitle,
       x = "(Relative) Variable Importance via Mean Decrease Accuracy",
       y = "Logistic Regression Coefficient") +
  ggthemes::theme_few(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom")
print(p)
ggsave("mda_logitcoef.png", p, path = fig_path, units = "in", dpi = 300, height = 12, width = 16)
rm(temp, p)
