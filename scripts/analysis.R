library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(tibble)
library(janitor)
library(ggplot2)
# install.packages("styler")
library(styler)
#install.packages("kableExtra")
library(kableExtra)

# styler::style_dir() #Fomrat nicely :) (but make sure it's saved first!)

census <- read.csv("Inputs/custom-filtered-census_b.csv") %>%
  transmute(
    age = as.numeric(substr(Age..61.categories., 6, 7)), # Note that 15 is 15 and under and 75 is 75 and over (shouldn't make a difference)
    sex = Sex..2.categories.,
    ethnicity = Ethnic.group..6.categories.,
    Tenure = Tenure.of.household..7.categories.,
    count = Observation
  ) %>%
  mutate(Tenure = case_when(
    Tenure == "Does not apply" ~ "NA",
    Tenure == "Owned: Owns outright" ~ "Owned.outright",
    Tenure == "Owned: Owns with a mortgage or loan or shared ownership" ~ "Buying.with.a.mortgage",
    Tenure == "Social rented: Rents from council or Local Authority" ~ "Council",
    Tenure == "Social rented: Other social rented" ~ "Registered.Social.Landlord",
    Tenure == "Private rented: Private landlord or letting agency" ~ "Rent.paid",
    Tenure == "Private rented: Other private rented or lives rent free" ~ "Rent.free",
    TRUE ~ Tenure
  ))

# Import household expense data.
exp.data <- read.csv("Inputs/expenditure.by.tenure.csv")

# Find proportions of rent.paid subgroups to calculate average - hardcoded to index position which is, frankly, unacceptable.
unfurnished.fact <- exp.data[2, 7] / (exp.data[2, 7] + exp.data[2, 8])
furnished.fact <- exp.data[2, 8] / (exp.data[2, 7] + exp.data[2, 8])

# Extract consumption data and transpose (should have done this later, but more effort to rewrite).
exp <- slice(exp.data, 5:17, 19)

exp.clean <- exp %>%
  mutate(Rent.paid = (Rent.paid.furnished * furnished.fact) + (Rent.paid.unfurnished * unfurnished.fact)) %>%
  select(-c("Rent.paid.furnished", "Rent.paid.unfurnished")) # Only keep relevant columns.

# Probably (definitely) not the best way to reformat the data!
exp_tran <- transpose(exp.clean)

rownames(exp_tran) <- colnames(exp.clean)
colnames(exp_tran) <- rownames(exp.clean)

exp_tran_2 <- tibble::rownames_to_column(exp_tran, "Tenure")
exp_tran_2 <- exp_tran_2 %>% row_to_names(row_number = 1)

# Here I see that there's no common trend amongst expenses across different tenure types, so this shouldn't be used to model health and education.
pived <- pivot_longer(exp_tran_2, cols = c(
  "Food & non-alcoholic drinks", "Alcoholic drinks, tobacco & narcotics",
  "Clothing & footwear", "Housing (net)?, fuel & power", "Household goods & services",
  "Transport", "Communication", "Recreation & culture", "Restaurants & hotels",
  "Miscellaneous goods & services", "Other expenditure items"
), names_to = "expense.type", values_to = "expense")
gg <- ggplot(pived, aes(x = Tenure, y = expense)) +
  geom_line(aes(group = expense.type, colour = expense.type))+
  theme(legend.position="none")

# Average used for all tenures
exp_tran_2$Education <- 5.1
exp_tran_2$Health <- 8.9

# Convert relevant columns to numeric.
exp_tran_2[, -1] <- lapply(exp_tran_2[, -1], as.numeric)

expenses <- exp_tran_2 %>%
  filter(Tenure != "All.tenures") %>%
  transmute(
    Tenure = Tenure, # Here I adjust the consumption to an individual level, making the assumption that each household member contributes equally.
    food.drink = `Food & non-alcoholic drinks` / `Weighted average number of persons per household`,
    drugs = `Alcoholic drinks, tobacco & narcotics` / `Weighted average number of persons per household`,
    clothes = `Clothing & footwear` / `Weighted average number of persons per household`,
    fuel = `Housing (net)?, fuel & power` / `Weighted average number of persons per household`,
    hhd.goods = `Household goods & services` / `Weighted average number of persons per household`,
    health = Health / `Weighted average number of persons per household`,
    transport = Transport / `Weighted average number of persons per household`,
    comms = Communication / `Weighted average number of persons per household`,
    culture = `Recreation & culture` / `Weighted average number of persons per household`,
    education = `Education` / `Weighted average number of persons per household`,
    restaurants = `Restaurants & hotels` / `Weighted average number of persons per household`,
    misc = `Miscellaneous goods & services` / `Weighted average number of persons per household`,
    other = `Other expenditure items` / `Weighted average number of persons per household`
  )

# Add in consumption emissions
conv.fact <- read.csv("Inputs/emissions.factors.csv") %>%
  filter(Product != "7.1.4 Animal drawn vehicles") %>% # Remove as this is empty and messes up average
  mutate(Product = str_replace(Product, "12.7.1 Other services n.e.c.", "13 Other services n.e.c.")) %>%
  mutate(prod.code = substr(Product, 1, 2)) %>%
  group_by(prod.code) %>%
  summarise(GHG = mean(as.numeric(GHG)), CO2 = mean(as.numeric(CO2))) %>%
  mutate(prod.grp = case_when(
    prod.code == "1." ~ "food.drink",
    prod.code == "2." ~ "drugs",
    prod.code == "3." ~ "clothes",
    prod.code == "4." ~ "fuel",
    prod.code == "5." ~ "hhd.goods",
    prod.code == "6." ~ "health",
    prod.code == "7." ~ "transport",
    prod.code == "8." ~ "comms",
    prod.code == "9." ~ "culture",
    prod.code == "10" ~ "education",
    prod.code == "11" ~ "restaurants",
    prod.code == "12" ~ "misc",
    prod.code == "13" ~ "other"
  ))

# This is a poor order (should have joined the emissions factors earlier but didn't have the wisdom) - pivoting long to join now and will pivot back to join to census data.
exp_long <- expenses %>% pivot_longer(cols = c("food.drink", "drugs", "clothes", "fuel", "hhd.goods", "health", "transport", "comms", "culture", "education", "restaurants", "misc", "other"), names_to = "prod.grp", values_to = "expenses")

emissions <- left_join(x = exp_long, y = conv.fact, by = "prod.grp") %>%
  transmute(
    Tenure = Tenure,
    prod.grp = prod.grp,
    ov.em = expenses * GHG,
    co2.em = expenses * CO2
  ) %>%
  pivot_wider(names_from = prod.grp, values_from = c(ov.em, co2.em))

census.em <- left_join(x = census, y = emissions, by = "Tenure") %>% mutate(
  ov.em.tot = rowSums(select(., starts_with("ov.em"))),
  co2.em.tot = rowSums(select(., starts_with("co2.em")))
)


yng <- census.em %>% filter(age >= 16 & age <= 30)
old <- census.em %>% filter(age >= 60)
wmn40em <- census.em %>% filter(sex == "Female" & age >= 40 & !(ethnicity %in% c("White", "Does not apply")))

censusin.na <- na.omit(census.em)

yng.narem <- censusin.na %>% filter(age >= 16 & age <= 30)
old.narem <- censusin.na %>% filter(age >= 60)
wmn40em.narem <- censusin.na %>% filter(sex == "Female" & age >= 40 & !(ethnicity %in% c("White", "Does not apply")))

# Calculate population totals, including Bristol overall
bri.pop <- sum(census.em$count)
yng.pop <- sum(yng$count)
old.pop <- sum(old$count)
wmn40em.pop <- sum(wmn40em$count)

bri.pop.narem <- sum(censusin.na$count)
yng.pop.narem <- sum(yng.narem$count)
old.pop.narem <- sum(old.narem$count)
wmn40em.pop.narem <- sum(wmn40em.narem$count)

# Calculate emissions totals for individual groups
yng.ov.em <- sum(yng$ov.em.tot, na.rm = TRUE)
old.ov.em <- sum(old$ov.em.tot, na.rm = TRUE)
wmn50em.ov.em <- sum(wmn40em$ov.em.tot, na.rm = TRUE)
bri.ov.em <- sum(census.em$ov.em.tot, na.rm = TRUE)

yng.co2.em <- sum(yng$co2.em.tot, na.rm = TRUE)
old.co2.em <- sum(old$co2.em.tot, na.rm = TRUE)
wmn40em.co2.em <- sum(wmn40em$co2.em.tot, na.rm = TRUE)
bri.co2.em <- sum(census.em$co2.em.tot, na.rm = TRUE)

# Per capita emissions - needs to account for na values in emissions.
yng.ov.cap <- yng.ov.em / yng.pop.narem
old.ov.cap <- old.ov.em / yng.pop.narem
wmn40em.ov.cap <- wmn50em.ov.em / old.pop.narem
bri.ov.cap <- bri.ov.em / bri.pop.narem

yng.co2.cap <- yng.co2.em / yng.pop.narem
old.co2.cap <- old.co2.em / old.pop.narem
wmn40em.co2.cap <- wmn40em.co2.em / old.pop.narem
bri.co2.cap <- bri.co2.em / bri.pop.narem

# Scale back up to population totals
yng.emissions.ov <- yng.ov.cap * yng.pop
old.emissions.ov <- old.ov.cap * old.pop
wmn40em.emissions.ov <- wmn40em.ov.cap * wmn40em.pop
bri.em.ov <- bri.ov.cap * bri.pop

yng.emissions.co2 <- yng.co2.cap * yng.pop
old.emissions.co2 <- old.co2.cap * old.pop
wmn40em.emissions.co2 <- wmn40em.co2.cap * wmn40em.pop
bri.em.co2 <- bri.co2.cap * bri.pop

cat <- c("Younger People", "Older people", "Women 40+ EM", "Bristol Overall")
pop <- c(yng.pop, old.pop, wmn40em.pop, bri.pop)
per.cap.fp.ov <- c(yng.ov.cap, old.ov.cap, wmn40em.ov.cap, bri.ov.cap)
per.cap.fp.co2 <- c(yng.co2.cap, old.co2.cap, wmn40em.co2.cap, bri.co2.cap)
tot.ghg <- c(yng.emissions.ov, old.emissions.ov, wmn40em.emissions.ov, bri.em.ov)
tot.co2 <- c(yng.emissions.co2, old.emissions.co2, wmn40em.emissions.co2, bri.em.co2)

fin.tab <- data.frame(
  Category = cat, Population = pop, "Per capita GHG emissions" = per.cap.fp.ov,
  "Per capita CO2 emissions" = per.cap.fp.co2, "Overall GHG emissions" = tot.ghg,
  "Overall CO2 emissions" = tot.co2
)%>%
  mutate(across(where(is.numeric), round, digits = 2))