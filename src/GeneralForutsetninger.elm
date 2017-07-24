module GeneralForutsetninger exposing (..)


drente : Float
drente =
    4 / 100


vekstrate : Float
vekstrate =
    1.3 / 100


drenteVekst : Float
drenteVekst =
    (drente - vekstrate) / (1 + vekstrate)


analysePeriode : number
analysePeriode =
    40


investeringsFaktor : Float -> Float
investeringsFaktor levetid =
    -- denne funksjonen virker relatert til afaktorCalculation
    let
        beregningsTekniskMellomregning =
            toFloat <| (analysePeriode // (truncate levetid)) + 1

        ledd1 =
            (1 - ((1 + drente) ^ ((negate levetid) * beregningsTekniskMellomregning)))
                / (1 - ((1 + drente) ^ (negate levetid)))

        ledd2 =
            (analysePeriode - (levetid * beregningsTekniskMellomregning))
                / (levetid * ((1 + drente) ^ analysePeriode))
    in
        ledd1 + ledd2


afaktorCalculation : Float -> Float
afaktorCalculation drenteValue =
    (1 / drenteValue) * (1 - (1 / ((1 + drenteValue) ^ analysePeriode)))


afaktorVekst : Float
afaktorVekst =
    afaktorCalculation drenteVekst


afaktor : Float
afaktor =
    afaktorCalculation drente


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


leskurPaaBussholdeplassenMedSitteplassNOK : Float
leskurPaaBussholdeplassenMedSitteplassNOK =
    5.81177152111594


skyggepris : Float
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
