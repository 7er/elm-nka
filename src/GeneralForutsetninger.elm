module GeneralForutsetninger exposing (..)


verdisettinger =
    -- alle verdier i NOK
    { opphoyetHoldeplass =
        -- pr min pr passasjer
        0.951535141202315
    , reisetidKollektivTransport =
        -- pr min pr passasjer
        1.02560673902046
    , reisetidBil =
        -- pr min pr bil
        1.53841010853069
    , operatoerKostnad =
        -- per minutt
        7.30197811291267
    , leskurPaaBussholdeplassenMedSitteplass =
        -- pr kollektivreise
        5.81177152111594
    , leskurPaaBussholdeplassenUtenSitteplass =
        -- pr kollektivreise
        3.55543669527093
    }


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


nokPrMinPrSyklist : Float
nokPrMinPrSyklist =
    147.47 / 60.0


wtpSeparatSykkelveg : Float
wtpSeparatSykkelveg =
    2.68


wtpKombinertGangOgSykkelveg : Float
wtpKombinertGangOgSykkelveg =
    1.69


wtpSykkelfelt : Float
wtpSykkelfelt =
    1.42


sykkelParkeringUteNOK : Float
sykkelParkeringUteNOK =
    11.75


sykkelParkeringInneNOK : Float
sykkelParkeringInneNOK =
    13.58


garderobeFasiliteterNOK : Float
garderobeFasiliteterNOK =
    21.49


skyggepris : Float
skyggepris =
    0.2


helsegevinstOgEndretUlykkesrisikoNOKPerKm : Float
helsegevinstOgEndretUlykkesrisikoNOKPerKm =
    26.019435971


miljoOgKlimaeffektNOKPerKm : Float
miljoOgKlimaeffektNOKPerKm =
    0.315774159


koreduksjonSlitasjeDriftNOKPerKm : Float
koreduksjonSlitasjeDriftNOKPerKm =
    1.194014079


avgTripLengthKm : Float
avgTripLengthKm =
    8.0


usageIncrease :
    { gangOgSykkelveg : Float
    , garderobeFasiliteter : Float
    , separatSykkelveg : Float
    , sykkelParkeringInne : Float
    , sykkelParkeringUte : Float
    , sykkelfelt : Float
    }
usageIncrease =
    { gangOgSykkelveg = 0.08
    , sykkelfelt = 0.05
    , separatSykkelveg = 0.1
    , sykkelParkeringInne = 0.05
    , sykkelParkeringUte = 0.05
    , garderobeFasiliteter = 0.05
    }
