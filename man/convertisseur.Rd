\name{convertir_X_en_numerique}
\alias{convertir_X_en_numerique}
\title{Convertit une chaîne de caractères au format "XXX+YYY" en un nombre décimal}
\usage{
  convertir_X_en_numerique(X)
}
\arguments{
  \item{X}{Une chaîne de caractères au format "XXX+YYY".}
}
\description{
  Cette fonction prend une chaîne de caractères représentant un nombre au format "XXX+YYY",
  où "XXX" et "YYY" sont des entiers, et convertit cette chaîne en un nombre décimal
  en additionnant XXX à YYY/1000.
}
\examples{
  convertir_X_en_numerique ("115+161")  # Renvoie 115.161
  convertir_X_en_numerique ("003+045")  # Renvoie 3.045
}

