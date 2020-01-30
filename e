-- Adapted from https://discourse.dhall-lang.org/t/working-with-rationals/47/4

let Prelude = ./Prelude

let Rational = ./Type

let e
    : Integer → Integer → Rational
    =   λ(coefficient : Integer)
      → λ(exponent : Integer)
      →       if Prelude.Integer.nonPositive exponent

        then  { numerator = coefficient
              , predenominator =
                  Natural/subtract
                    1
                    ( Natural/fold
                        (Integer/clamp (Integer/negate exponent))
                        Natural
                        (λ(i : Natural) → i * 10)
                        1
                    )
              }

        else  { numerator =
                  Natural/fold
                    (Integer/clamp exponent)
                    Integer
                    (λ(i : Integer) → Prelude.Integer.multiply i +10)
                    coefficient
              , predenominator = 0
              }

in  e
