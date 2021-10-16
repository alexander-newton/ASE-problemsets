defence_expenditure <- readr::read_csv("week1_AES_tidydata.csv")

#base units
defence_expenditure <- dplyr::mutate(defence_expenditure,
                                     GDP = GDP * 1E9,
                                     Population = Population * 1E6,
                                     `Military Expenditure` = `Military Expenditure` * 1E9,
                                     `Number in Armed Forces` = `Number in Armed Forces` * 1E3)

# Per capita GDP in dollars per person
defence_expenditure["GDP_per_capita"] <- defence_expenditure["GDP"] / defence_expenditure["Population"]

# Percentage share of GDP that is military expenditure
defence_expenditure["Military_per_GDP"] <- defence_expenditure["Military Expenditure"] / defence_expenditure["GDP"]
# Both rich and poor countries decreased their military expenditure across the period as a function of GDP

# Number of people in armed forces per 1000 population
defence_expenditure["forces_per_1000_pop"] <- defence_expenditure["Number in Armed Forces"] / 1000 / defence_expenditure["Population"]

# Military expenditure  per member of the armed forces for rich and poor countries in 1985 and 1995
defence_expenditure["expenditure_per_member_armed_forces"] <- defence_expenditure["Military Expenditure"] / defence_expenditure["Number in Armed Forces"]
