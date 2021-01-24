
# load package
  devtools::load_all()
  library(dplyr)
  library(DataExplorer)

# get data
  data <- vin::get_data()

# clean data
  str(data)
  data %>%
    mutate(priceValidFrom = lubridate::as_date(priceValidFrom)) %>%
    select(
      priceValidFrom
    ) %>%
    head()


# EDA
  data <- data %>%
    mutate(type = if_else(classification.productTypeName %in% "Rødvin", "Rødvin", "Annet") %>% as.factor())

  introduce(data) %>% t()
  plot_missing(data)
  plot_bar(data)
  plot_bar(data, by = "type")
  plot_histogram(data)
  plot_density(data)
  plot_qq(data)
  plot_correlation(data)
  plot_boxplot(data, by = "type")
  plot_scatterplot(data, by = "salesPrice")
  # plot_prcomp(data, maxcat = 3L)





# price distribution
  data %>%
    ggplot(
      aes(x = salesPrice)
    ) +
    geom_density(
      fill = 'forestgreen',
      alpha = .4
      )

# list wines based on priorities
  data %>%
    dplyr::filter(
      classification.subProductTypeName == "Rødvin" |
        classification.productTypeName == "Rødvin",
      basic.volume == 0.75,
      salesPrice < 200,
      origins.origin.country %in% c("Spania", "Italia", "Frankrike"),
      stringr::str_detect(properties.storagePotential, "Drikkeklar")
    ) %>%
    View()

# alcohol vs sugar
  data %>%
    mutate(ingredients.sugar = as.numeric(ingredients.sugar)) %>%
    ggplot() +
      aes(
        y = basic.alcoholContent,
        x = ingredients.sugar
        ) +
        geom_point() +
    geom_smooth()

# sugar vs sweetness


