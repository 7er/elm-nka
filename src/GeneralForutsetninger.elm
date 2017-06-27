module GeneralForutsetninger exposing (..)


drente =
    4 / 100


vekstrate =
    1.3 / 100


analysePeriode =
    40


nokPrMinPrSyklist =
    147.47 / 60.0


wtpSeparatSykkelveg =
    2.68


wtpKombinertGangOgSykkelveg =
    1.69


wtpSykkelfelt =
    1.42


sykkelParkeringUteNOK =
    11.75


sykkelParkeringInneNOK =
    13.58


garderobeFasiliteterNOK =
    21.49


skyggepris =
    0.2


helsegevinstOgEndretUlykkesrisikoNOKPerKm =
    26.019435971


miljoOgKlimaeffektNOKPerKm =
    0.315774159


koreduksjonSlitasjeDriftNOKPerKm =
    1.194014079


avgTripLengthKm =
    8.0


usageIncrease =
    { gangOgSykkelveg = 0.08
    , sykkelfelt = 0.05
    , separatSykkelveg = 0.1
    , sykkelParkeringInne = 0.05
    , sykkelParkeringUte = 0.05
    , garderobeFasiliteter = 0.05
    }


drenteVekst =
    (drente - vekstrate) / (1 + vekstrate)


afaktorVekst =
    (1 / drenteVekst) * (1 - (1 / ((1 + drenteVekst) ^ analysePeriode)))



{-
   afaktor: function () {
     return (1/this.drente) * (1 - (1/ Math.pow((1 + this.drente), this.analysePeriode)));
   }

-}
