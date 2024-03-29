#' Convertit une chaine de caracteres au format "XXX+YYY" en numerique
#'
#' Cette fonction prend une chaîne de caracteres representant un nombre au format "XXX+YYY",
#' ou "XXX" et "YYY" sont des chaînes representant des nombres entiers, et convertit cette chaine
#' en une valeur numerique en additionnant XXX à YYY/1000.
#'
#' @param X Une chaine de caracteres au format "XXX+YYY".
#' @return Un nombre numerique resultant de l'addition de XXX et YYY/1000.
#' Si la chaine n'est pas dans le format correct, la fonction renvoie une erreur.
#' Le format de la chaine doit etre 'XXX+YYY', où XXX est un nombre et YYY est un nombre de maximum 3 chiffres.
#' @examples
#' convertir_X_en_numerique("115+161")
#' convertir_X_en_numerique("003+045")
#' @export
convertir_X_en_numerique <- function(X) {
  if (!grepl("^\\d+\\+\\d{1,3}$", X)) {
    stop("Le format de la chaine doit etre 'XXX+YYY', ou XXX est un nombre et YYY est un nombre de maximum 3 chiffres.")
  }

  parties <- strsplit(X, "\\+")[[1]]

  part1 <- as.numeric(parties[1])
  part2 <- as.numeric(parties[2])

  if (is.na(part1) || is.na(part2)) {
    stop("Erreur lors de la conversion en numerique.")
  }

  return(part1 + part2 / 1000)
}



