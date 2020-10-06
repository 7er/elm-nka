module GeneralForutsetninger exposing (..)


verdisettinger =
    -- alle verdier i NOK
    { reisetidKollektivTransport =
        -- pr min pr passasjer
        1.02560673902046
    , reisetidBil =
        -- pr min pr bil
        1.53841010853069
    , sitteplassPaaHpl =
        -- pr kollektivreise
        2.25633482584501
    , leskurPaaBussholdeplassenUtenSitteplass =
        -- pr kollektivreise
        3.55543669527093
    , leskurPaaBussholdeplassenMedSitteplass =
        -- pr kollektivreise
        5.81177152111594
    , renholdPaaHpl =
        -- pr kollektivreise
        4.12521821694896
    , fjerningAvIsSnoePaaHpl =
        -- pr kollektivreise
        5.66362832547965
    , opphoyetHoldeplass =
        -- pr kollektivreise
        0.951535141202315
    , rutekartPaaHpl =
        -- pr kollektivreise
        0.490012108643109
    , lokalkartPaaHpl =
        -- pr kollektivreise
        0.490012108643109
    , skiltingIBuss =
        -- pr kollektivreise
        4.18219636911676
    , destinasjonsSkiltPaaBuss =
        -- pr kollektivreise
        0.694635934120579
    , avviksinformasjonHoeyttaler =
        -- pr kollektivreise
        0.786298499915686
    , hplOpprop =
        -- pr kollektivreise
        4.12521821694896
    , bussrenhold =
        -- pr kollektivreise
        2.83411461121196
    , belysning =
        -- pr kollektivreise
        3.21356778226411
    , vektere =
        -- pr kollektivreise
        3.65378501347425
    , alarmsystemPaaHpl =
        -- pr kollektivreise
        1.94498061553762
    , lavgulvUtenTilpassetHoldeplass =
        -- pr kollektivreise
        1.90307028240463
    , lavgulvMedTilpassetHoldeplass =
        -- pr kollektivreise
        2.35889549974706
    , pakkeSkiltOgOppropBuss =
        -- pr kollektivreise
        4.78616478209548
    , operatoerKostnad =
        -- per minutt
        7.30197811291267
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
