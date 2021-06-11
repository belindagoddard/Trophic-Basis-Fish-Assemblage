  library(rstatix)  # Need to load these packages onto your computer
  library(datarium)
  
  # Just read in the example dataset from the package
  Basal_Methods = read.csv("Supplementary Table 4- Table 1.csv", header=TRUE)
  
  summary(Basal_Methods) # Summary data
  Basal_Methods %>% sample_n_by(Method, Percentage, size = 1)
  
  # Run the ANOVA test
  res.aov <- Basal_Methods %>% anova_test(Percentage ~ Method * Source)
  res.aov 
 
## Post-hoc test for main effects - start by testing if there are differences among source for each location
model <- lm(Percentage ~ Method * Source, data = Basal_Methods)
Basal_Methods %>%
  group_by(Method) %>%
  anova_test(Percentage ~ Source, error = model)


library(emmeans)
posthoc1 <- Basal_Methods %>% 
  group_by(Method) %>%
  emmeans_test(Percentage ~ Source, p.adjust.method = "bonferroni") 
posthoc1

# Now the 'other side' of the interaction term, check for differences among location for each source type
model2 <- lm(Percentage ~ Method * Source, data = Basal_Methods)
Basal_Methods %>%
  group_by(Source) %>%
  anova_test(Percentage ~ Method, error = model2)

posthoc2 <- Basal_Methods %>% 
  group_by(Source) %>%
  emmeans_test(Percentage ~ Method, p.adjust.method = "bonferroni") 
posthoc2
