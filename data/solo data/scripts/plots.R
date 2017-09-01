library("ggplot2")
library("dplyr")

#TODO - clean up variable names

standarderrors <- read.csv(file.choose())
head(standarderrors)

byspecies_river2$se <- standarderrors$value

head(river)
plot(river$temp, river$count)

byspecies_river <- river[order(river$species),]
head(byspecies_river2)

speciesnames <- c("Baetis", "Caenis", "Elmidae.Larvae", "Gammarus", "Halesus",
                  "Heptageniidae", "Hydrophorus", "Leuctra", "Oligochaeta", 
                  "Platambus", "Potamophylax", "Serratella")

longernames <- rep(speciesnames, each=4)
head(longernames, n=10)

byspecies_river2 <- byspecies_river
byspecies_river2$species <- longernames
head(byspecies_river2)

#plots with same y-axis scale
samescale.plots = ggplot(data = byspecies_river2, aes(x = temperature , y = count)) + 
  geom_line() + facet_wrap(~species) +
  labs(x = "Temperature (degrees Celsius)",
       y = "Number of Individuals", 
       title    = "Change in species abundance with changing temperature",
       subtitle = "Fixed y-axis scale") +
  geom_errorbar(aes(ymin=count-se, ymax=count+se), width=.2,
                position=position_dodge(0.05))
                       
plots = byspecies_river2 %>%
  group_by(species) %>%
  do(plots = samescale.plots %+% . + facet_wrap(~species))

#plots with variable y-axis scale
freescale.plots = ggplot(data = byspecies_river2, aes(x = temperature , y = count)) + 
  geom_line() + facet_wrap(~species, scales = "free") +
  labs(x = "Temperature (degrees Celsius)", 
       y = "Number of Individuals",
       title    = "Change in taxa abundance with changing temperature",
       subtitle = "Variable y-axis scale") +
geom_errorbar(aes(ymin=count-se, ymax=count+se), width=.2,
              position=position_dodge(0.05))

plots = byspecies_river2 %>%
  group_by(species) %>%
  do(plots = freescale.plots %+% . + facet_wrap(~species))

#view plots
samescale.plots
freescale.plots
