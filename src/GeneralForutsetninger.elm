module GeneralForutsetninger exposing (..)


verdisettinger =
    -- alle verdier i NOK
    { reisetidKollektivTransport =
        -- pr min pr passasjer
        1.29
    , reisetidBil =
        -- pr min pr bil
        2.46
    , sitteplassPaaHpl =
        -- pr kollektivreise
        3.99
    , leskurPaaBussholdeplassenUtenSitteplass =
        -- pr kollektivreise
        7.87
    , leskurPaaBussholdeplassenMedSitteplass =
        -- pr kollektivreise
        10.08
    , renholdPaaHpl =
        -- pr kollektivreise
        8.61
    , fjerningAvIsSnoePaaHpl =
        -- pr kollektivreise
        12.67
    , opphoyetHoldeplass =
        -- pr kollektivreise
        1.04
    , rutekartPaaHpl =
        -- pr kollektivreise
        2.79
    , lokalkartPaaHpl =
        -- pr kollektivreise
        4.09
    , skiltingIBuss =
        -- pr kollektivreise
        5.05
    , destinasjonsSkiltPaaBuss =
        -- pr kollektivreise
        0.76
    , avviksinformasjonHoeyttaler =
        -- pr kollektivreise
        0.86
    , hplOpprop =
        -- pr kollektivreise
        3.52
    , bussrenhold =
        -- pr kollektivreise
        3.64
    , belysning =
        -- pr kollektivreise
        6.93
    , vektere =
        -- pr kollektivreise
        4.0
    , alarmsystemPaaHpl =
        -- pr kollektivreise
        4.08
    , lavgulvUtenTilpassetHoldeplass =
        -- pr kollektivreise
        2.08
    , lavgulvMedTilpassetHoldeplass =
        -- pr kollektivreise
        2.17
    , pakkeSkiltOgOppropBuss =
        -- pr kollektivreise
        5.24
    , operatoerKostnad =
        -- per minutt
        7.81
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


skyggepris : Float
skyggepris =
    0.2
