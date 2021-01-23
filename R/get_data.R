
#' Get data
#'
#' @param request_url
#' @param primary_key
#'
#' @return
#' @export
#' @importFrom httr GET content add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr select
#'
#' @examples
get_data <- function(
  request_url = "https://apis.vinmonopolet.no/products/v0/details-normal",
  primary_key = "c8971627fe1d48b7bbb336a7ab924660") {

  json <-
    httr::GET(request_url,
              httr::add_headers('Ocp-Apim-Subscription-Key' = primary_key)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE)

  data <- tibble::as_tibble(json) %>%

    # unnest nested columns
    tidyr::unnest_wider(ingredients.grapes) %>%
    tidyr::unnest_wider(description.recommendedFood) %>%
    tidyr::unnest_wider(prices) %>%

    # select variables
    dplyr::select(
      basic.productId,
      basic.productShortName,
      basic.productLongName,
      priceValidFrom,
      salesPrice,
      salesPricePrLiter,
      basic.volume,
      basic.alcoholContent,
      basic.vintage,
      basic.ageLimit,
      basic.packagingMaterial,
      basic.volumType,
      basic.corkType,
      basic.bottlePerSalesUnit,
      basic.introductionDate,
      logistics.wholesalerName,
      logistics.vendorName,
      logistics.vendorValidFrom,
      logistics.manufacturerName,
      origins.origin.countryId,
      origins.origin.country,
      origins.origin.region,
      origins.origin.subRegion,
      properties.storagePotential,
      properties.organic,
      properties.biodynamic,
      properties.ethicallyCertified,
      properties.vintageControlled,
      properties.sweetWine,
      properties.freeOrLowOnGluten,
      properties.noAddedSulphur,
      properties.environmentallySmart,
      properties.productionMethodStorage,
      classification.mainProductTypeName,
      classification.subProductTypeName,
      classification.productTypeName,
      classification.productGroupName,
      grapeDesc,
      grapePct,
      ingredients.sugar,
      ingredients.acid,
      description.freshness,
      description.fullness,
      description.bitterness,
      description.sweetness,
      description.tannins,
      foodDesc,
      description.characteristics.colour,
      description.characteristics.odour,
      description.characteristics.taste,
      assortment.assortment,
      assortment.validFrom,
      assortment.listedFrom
    )


  return(data)

}
