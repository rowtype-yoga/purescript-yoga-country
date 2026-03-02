module Data.Country where

import Prelude

import Control.Monad.Except (throwError)
import Data.Enum (class Enum, class BoundedEnum, Cardinality(..), enumFromTo)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data Country
  = Andorra
  | UnitedArabEmirates
  | Afghanistan
  | AntiguaAndBarbuda
  | Anguilla
  | Albania
  | Armenia
  | Angola
  | Antarctica
  | Argentina
  | AmericanSamoa
  | Austria
  | Australia
  | Aruba
  | AlandIslands
  | Azerbaijan
  | BosniaAndHerzegovina
  | Barbados
  | Bangladesh
  | Belgium
  | BurkinaFaso
  | Bulgaria
  | Bahrain
  | Burundi
  | Benin
  | SaintBarthelemy
  | Bermuda
  | BruneiDarussalam
  | BoliviaPlurinationalStateOf
  | BonaireSintEustatiusAndSaba
  | Brazil
  | Bahamas
  | Bhutan
  | BouvetIsland
  | Botswana
  | Belarus
  | Belize
  | Canada
  | CocosKeelingIslands
  | CongoDemocraticRepublicOfThe
  | CentralAfricanRepublic
  | Congo
  | Switzerland
  | CoteDIvoire
  | CookIslands
  | Chile
  | Cameroon
  | China
  | Colombia
  | CostaRica
  | Cuba
  | CaboVerde
  | Curacao
  | ChristmasIsland
  | Cyprus
  | Czechia
  | Germany
  | Djibouti
  | Denmark
  | Dominica
  | DominicanRepublic
  | Algeria
  | Ecuador
  | Estonia
  | Egypt
  | WesternSahara
  | Eritrea
  | Spain
  | Ethiopia
  | Finland
  | Fiji
  | FalklandIslandsMalvinas
  | MicronesiaFederatedStatesOf
  | FaroeIslands
  | France
  | Gabon
  | UnitedKingdomOfGreatBritainAndNorthernIreland
  | Grenada
  | Georgia
  | FrenchGuiana
  | Guernsey
  | Ghana
  | Gibraltar
  | Greenland
  | Gambia
  | Guinea
  | Guadeloupe
  | EquatorialGuinea
  | Greece
  | SouthGeorgiaAndTheSouthSandwichIslands
  | Guatemala
  | Guam
  | GuineaBissau
  | Guyana
  | HongKong
  | HeardIslandAndMcdonaldIslands
  | Honduras
  | Croatia
  | Haiti
  | Hungary
  | Indonesia
  | Ireland
  | Israel
  | IsleOfMan
  | India
  | BritishIndianOceanTerritory
  | Iraq
  | IranIslamicRepublicOf
  | Iceland
  | Italy
  | Jersey
  | Jamaica
  | Jordan
  | Japan
  | Kenya
  | Kyrgyzstan
  | Cambodia
  | Kiribati
  | Comoros
  | SaintKittsAndNevis
  | KoreaDemocraticPeopleSRepublicOf
  | KoreaRepublicOf
  | Kuwait
  | CaymanIslands
  | Kazakhstan
  | LaoPeopleSDemocraticRepublic
  | Lebanon
  | SaintLucia
  | Liechtenstein
  | SriLanka
  | Liberia
  | Lesotho
  | Lithuania
  | Luxembourg
  | Latvia
  | Libya
  | Morocco
  | Monaco
  | MoldovaRepublicOf
  | Montenegro
  | SaintMartinFrenchPart
  | Madagascar
  | MarshallIslands
  | NorthMacedonia
  | Mali
  | Myanmar
  | Mongolia
  | Macao
  | NorthernMarianaIslands
  | Martinique
  | Mauritania
  | Montserrat
  | Malta
  | Mauritius
  | Maldives
  | Malawi
  | Mexico
  | Malaysia
  | Mozambique
  | Namibia
  | NewCaledonia
  | Niger
  | NorfolkIsland
  | Nigeria
  | Nicaragua
  | NetherlandsKingdomOfThe
  | Norway
  | Nepal
  | Nauru
  | Niue
  | NewZealand
  | Oman
  | Panama
  | Peru
  | FrenchPolynesia
  | PapuaNewGuinea
  | Philippines
  | Pakistan
  | Poland
  | SaintPierreAndMiquelon
  | Pitcairn
  | PuertoRico
  | PalestineStateOf
  | Portugal
  | Palau
  | Paraguay
  | Qatar
  | Reunion
  | Romania
  | Serbia
  | RussianFederation
  | Rwanda
  | SaudiArabia
  | SolomonIslands
  | Seychelles
  | Sudan
  | Sweden
  | Singapore
  | SaintHelenaAscensionAndTristanDaCunha
  | Slovenia
  | SvalbardAndJanMayen
  | Slovakia
  | SierraLeone
  | SanMarino
  | Senegal
  | Somalia
  | Suriname
  | SouthSudan
  | SaoTomeAndPrincipe
  | ElSalvador
  | SintMaartenDutchPart
  | SyrianArabRepublic
  | Eswatini
  | TurksAndCaicosIslands
  | Chad
  | FrenchSouthernTerritories
  | Togo
  | Thailand
  | Tajikistan
  | Tokelau
  | TimorLeste
  | Turkmenistan
  | Tunisia
  | Tonga
  | Turkiye
  | TrinidadAndTobago
  | Tuvalu
  | TaiwanProvinceOfChina
  | TanzaniaUnitedRepublicOf
  | Ukraine
  | Uganda
  | UnitedStatesMinorOutlyingIslands
  | UnitedStatesOfAmerica
  | Uruguay
  | Uzbekistan
  | HolySee
  | SaintVincentAndTheGrenadines
  | VenezuelaBolivarianRepublicOf
  | VirginIslandsBritish
  | VirginIslandsUS
  | VietNam
  | Vanuatu
  | WallisAndFutuna
  | Samoa
  | Yemen
  | Mayotte
  | SouthAfrica
  | Zambia
  | Zimbabwe

derive instance Eq Country
derive instance Ord Country

instance Show Country where
  show = toAlpha2

instance Bounded Country where
  bottom = Andorra
  top = Zimbabwe

instance Enum Country where
  succ = case _ of
    Andorra -> Just UnitedArabEmirates
    UnitedArabEmirates -> Just Afghanistan
    Afghanistan -> Just AntiguaAndBarbuda
    AntiguaAndBarbuda -> Just Anguilla
    Anguilla -> Just Albania
    Albania -> Just Armenia
    Armenia -> Just Angola
    Angola -> Just Antarctica
    Antarctica -> Just Argentina
    Argentina -> Just AmericanSamoa
    AmericanSamoa -> Just Austria
    Austria -> Just Australia
    Australia -> Just Aruba
    Aruba -> Just AlandIslands
    AlandIslands -> Just Azerbaijan
    Azerbaijan -> Just BosniaAndHerzegovina
    BosniaAndHerzegovina -> Just Barbados
    Barbados -> Just Bangladesh
    Bangladesh -> Just Belgium
    Belgium -> Just BurkinaFaso
    BurkinaFaso -> Just Bulgaria
    Bulgaria -> Just Bahrain
    Bahrain -> Just Burundi
    Burundi -> Just Benin
    Benin -> Just SaintBarthelemy
    SaintBarthelemy -> Just Bermuda
    Bermuda -> Just BruneiDarussalam
    BruneiDarussalam -> Just BoliviaPlurinationalStateOf
    BoliviaPlurinationalStateOf -> Just BonaireSintEustatiusAndSaba
    BonaireSintEustatiusAndSaba -> Just Brazil
    Brazil -> Just Bahamas
    Bahamas -> Just Bhutan
    Bhutan -> Just BouvetIsland
    BouvetIsland -> Just Botswana
    Botswana -> Just Belarus
    Belarus -> Just Belize
    Belize -> Just Canada
    Canada -> Just CocosKeelingIslands
    CocosKeelingIslands -> Just CongoDemocraticRepublicOfThe
    CongoDemocraticRepublicOfThe -> Just CentralAfricanRepublic
    CentralAfricanRepublic -> Just Congo
    Congo -> Just Switzerland
    Switzerland -> Just CoteDIvoire
    CoteDIvoire -> Just CookIslands
    CookIslands -> Just Chile
    Chile -> Just Cameroon
    Cameroon -> Just China
    China -> Just Colombia
    Colombia -> Just CostaRica
    CostaRica -> Just Cuba
    Cuba -> Just CaboVerde
    CaboVerde -> Just Curacao
    Curacao -> Just ChristmasIsland
    ChristmasIsland -> Just Cyprus
    Cyprus -> Just Czechia
    Czechia -> Just Germany
    Germany -> Just Djibouti
    Djibouti -> Just Denmark
    Denmark -> Just Dominica
    Dominica -> Just DominicanRepublic
    DominicanRepublic -> Just Algeria
    Algeria -> Just Ecuador
    Ecuador -> Just Estonia
    Estonia -> Just Egypt
    Egypt -> Just WesternSahara
    WesternSahara -> Just Eritrea
    Eritrea -> Just Spain
    Spain -> Just Ethiopia
    Ethiopia -> Just Finland
    Finland -> Just Fiji
    Fiji -> Just FalklandIslandsMalvinas
    FalklandIslandsMalvinas -> Just MicronesiaFederatedStatesOf
    MicronesiaFederatedStatesOf -> Just FaroeIslands
    FaroeIslands -> Just France
    France -> Just Gabon
    Gabon -> Just UnitedKingdomOfGreatBritainAndNorthernIreland
    UnitedKingdomOfGreatBritainAndNorthernIreland -> Just Grenada
    Grenada -> Just Georgia
    Georgia -> Just FrenchGuiana
    FrenchGuiana -> Just Guernsey
    Guernsey -> Just Ghana
    Ghana -> Just Gibraltar
    Gibraltar -> Just Greenland
    Greenland -> Just Gambia
    Gambia -> Just Guinea
    Guinea -> Just Guadeloupe
    Guadeloupe -> Just EquatorialGuinea
    EquatorialGuinea -> Just Greece
    Greece -> Just SouthGeorgiaAndTheSouthSandwichIslands
    SouthGeorgiaAndTheSouthSandwichIslands -> Just Guatemala
    Guatemala -> Just Guam
    Guam -> Just GuineaBissau
    GuineaBissau -> Just Guyana
    Guyana -> Just HongKong
    HongKong -> Just HeardIslandAndMcdonaldIslands
    HeardIslandAndMcdonaldIslands -> Just Honduras
    Honduras -> Just Croatia
    Croatia -> Just Haiti
    Haiti -> Just Hungary
    Hungary -> Just Indonesia
    Indonesia -> Just Ireland
    Ireland -> Just Israel
    Israel -> Just IsleOfMan
    IsleOfMan -> Just India
    India -> Just BritishIndianOceanTerritory
    BritishIndianOceanTerritory -> Just Iraq
    Iraq -> Just IranIslamicRepublicOf
    IranIslamicRepublicOf -> Just Iceland
    Iceland -> Just Italy
    Italy -> Just Jersey
    Jersey -> Just Jamaica
    Jamaica -> Just Jordan
    Jordan -> Just Japan
    Japan -> Just Kenya
    Kenya -> Just Kyrgyzstan
    Kyrgyzstan -> Just Cambodia
    Cambodia -> Just Kiribati
    Kiribati -> Just Comoros
    Comoros -> Just SaintKittsAndNevis
    SaintKittsAndNevis -> Just KoreaDemocraticPeopleSRepublicOf
    KoreaDemocraticPeopleSRepublicOf -> Just KoreaRepublicOf
    KoreaRepublicOf -> Just Kuwait
    Kuwait -> Just CaymanIslands
    CaymanIslands -> Just Kazakhstan
    Kazakhstan -> Just LaoPeopleSDemocraticRepublic
    LaoPeopleSDemocraticRepublic -> Just Lebanon
    Lebanon -> Just SaintLucia
    SaintLucia -> Just Liechtenstein
    Liechtenstein -> Just SriLanka
    SriLanka -> Just Liberia
    Liberia -> Just Lesotho
    Lesotho -> Just Lithuania
    Lithuania -> Just Luxembourg
    Luxembourg -> Just Latvia
    Latvia -> Just Libya
    Libya -> Just Morocco
    Morocco -> Just Monaco
    Monaco -> Just MoldovaRepublicOf
    MoldovaRepublicOf -> Just Montenegro
    Montenegro -> Just SaintMartinFrenchPart
    SaintMartinFrenchPart -> Just Madagascar
    Madagascar -> Just MarshallIslands
    MarshallIslands -> Just NorthMacedonia
    NorthMacedonia -> Just Mali
    Mali -> Just Myanmar
    Myanmar -> Just Mongolia
    Mongolia -> Just Macao
    Macao -> Just NorthernMarianaIslands
    NorthernMarianaIslands -> Just Martinique
    Martinique -> Just Mauritania
    Mauritania -> Just Montserrat
    Montserrat -> Just Malta
    Malta -> Just Mauritius
    Mauritius -> Just Maldives
    Maldives -> Just Malawi
    Malawi -> Just Mexico
    Mexico -> Just Malaysia
    Malaysia -> Just Mozambique
    Mozambique -> Just Namibia
    Namibia -> Just NewCaledonia
    NewCaledonia -> Just Niger
    Niger -> Just NorfolkIsland
    NorfolkIsland -> Just Nigeria
    Nigeria -> Just Nicaragua
    Nicaragua -> Just NetherlandsKingdomOfThe
    NetherlandsKingdomOfThe -> Just Norway
    Norway -> Just Nepal
    Nepal -> Just Nauru
    Nauru -> Just Niue
    Niue -> Just NewZealand
    NewZealand -> Just Oman
    Oman -> Just Panama
    Panama -> Just Peru
    Peru -> Just FrenchPolynesia
    FrenchPolynesia -> Just PapuaNewGuinea
    PapuaNewGuinea -> Just Philippines
    Philippines -> Just Pakistan
    Pakistan -> Just Poland
    Poland -> Just SaintPierreAndMiquelon
    SaintPierreAndMiquelon -> Just Pitcairn
    Pitcairn -> Just PuertoRico
    PuertoRico -> Just PalestineStateOf
    PalestineStateOf -> Just Portugal
    Portugal -> Just Palau
    Palau -> Just Paraguay
    Paraguay -> Just Qatar
    Qatar -> Just Reunion
    Reunion -> Just Romania
    Romania -> Just Serbia
    Serbia -> Just RussianFederation
    RussianFederation -> Just Rwanda
    Rwanda -> Just SaudiArabia
    SaudiArabia -> Just SolomonIslands
    SolomonIslands -> Just Seychelles
    Seychelles -> Just Sudan
    Sudan -> Just Sweden
    Sweden -> Just Singapore
    Singapore -> Just SaintHelenaAscensionAndTristanDaCunha
    SaintHelenaAscensionAndTristanDaCunha -> Just Slovenia
    Slovenia -> Just SvalbardAndJanMayen
    SvalbardAndJanMayen -> Just Slovakia
    Slovakia -> Just SierraLeone
    SierraLeone -> Just SanMarino
    SanMarino -> Just Senegal
    Senegal -> Just Somalia
    Somalia -> Just Suriname
    Suriname -> Just SouthSudan
    SouthSudan -> Just SaoTomeAndPrincipe
    SaoTomeAndPrincipe -> Just ElSalvador
    ElSalvador -> Just SintMaartenDutchPart
    SintMaartenDutchPart -> Just SyrianArabRepublic
    SyrianArabRepublic -> Just Eswatini
    Eswatini -> Just TurksAndCaicosIslands
    TurksAndCaicosIslands -> Just Chad
    Chad -> Just FrenchSouthernTerritories
    FrenchSouthernTerritories -> Just Togo
    Togo -> Just Thailand
    Thailand -> Just Tajikistan
    Tajikistan -> Just Tokelau
    Tokelau -> Just TimorLeste
    TimorLeste -> Just Turkmenistan
    Turkmenistan -> Just Tunisia
    Tunisia -> Just Tonga
    Tonga -> Just Turkiye
    Turkiye -> Just TrinidadAndTobago
    TrinidadAndTobago -> Just Tuvalu
    Tuvalu -> Just TaiwanProvinceOfChina
    TaiwanProvinceOfChina -> Just TanzaniaUnitedRepublicOf
    TanzaniaUnitedRepublicOf -> Just Ukraine
    Ukraine -> Just Uganda
    Uganda -> Just UnitedStatesMinorOutlyingIslands
    UnitedStatesMinorOutlyingIslands -> Just UnitedStatesOfAmerica
    UnitedStatesOfAmerica -> Just Uruguay
    Uruguay -> Just Uzbekistan
    Uzbekistan -> Just HolySee
    HolySee -> Just SaintVincentAndTheGrenadines
    SaintVincentAndTheGrenadines -> Just VenezuelaBolivarianRepublicOf
    VenezuelaBolivarianRepublicOf -> Just VirginIslandsBritish
    VirginIslandsBritish -> Just VirginIslandsUS
    VirginIslandsUS -> Just VietNam
    VietNam -> Just Vanuatu
    Vanuatu -> Just WallisAndFutuna
    WallisAndFutuna -> Just Samoa
    Samoa -> Just Yemen
    Yemen -> Just Mayotte
    Mayotte -> Just SouthAfrica
    SouthAfrica -> Just Zambia
    Zambia -> Just Zimbabwe
    Zimbabwe -> Nothing
  pred = case _ of
    Andorra -> Nothing
    UnitedArabEmirates -> Just Andorra
    Afghanistan -> Just UnitedArabEmirates
    AntiguaAndBarbuda -> Just Afghanistan
    Anguilla -> Just AntiguaAndBarbuda
    Albania -> Just Anguilla
    Armenia -> Just Albania
    Angola -> Just Armenia
    Antarctica -> Just Angola
    Argentina -> Just Antarctica
    AmericanSamoa -> Just Argentina
    Austria -> Just AmericanSamoa
    Australia -> Just Austria
    Aruba -> Just Australia
    AlandIslands -> Just Aruba
    Azerbaijan -> Just AlandIslands
    BosniaAndHerzegovina -> Just Azerbaijan
    Barbados -> Just BosniaAndHerzegovina
    Bangladesh -> Just Barbados
    Belgium -> Just Bangladesh
    BurkinaFaso -> Just Belgium
    Bulgaria -> Just BurkinaFaso
    Bahrain -> Just Bulgaria
    Burundi -> Just Bahrain
    Benin -> Just Burundi
    SaintBarthelemy -> Just Benin
    Bermuda -> Just SaintBarthelemy
    BruneiDarussalam -> Just Bermuda
    BoliviaPlurinationalStateOf -> Just BruneiDarussalam
    BonaireSintEustatiusAndSaba -> Just BoliviaPlurinationalStateOf
    Brazil -> Just BonaireSintEustatiusAndSaba
    Bahamas -> Just Brazil
    Bhutan -> Just Bahamas
    BouvetIsland -> Just Bhutan
    Botswana -> Just BouvetIsland
    Belarus -> Just Botswana
    Belize -> Just Belarus
    Canada -> Just Belize
    CocosKeelingIslands -> Just Canada
    CongoDemocraticRepublicOfThe -> Just CocosKeelingIslands
    CentralAfricanRepublic -> Just CongoDemocraticRepublicOfThe
    Congo -> Just CentralAfricanRepublic
    Switzerland -> Just Congo
    CoteDIvoire -> Just Switzerland
    CookIslands -> Just CoteDIvoire
    Chile -> Just CookIslands
    Cameroon -> Just Chile
    China -> Just Cameroon
    Colombia -> Just China
    CostaRica -> Just Colombia
    Cuba -> Just CostaRica
    CaboVerde -> Just Cuba
    Curacao -> Just CaboVerde
    ChristmasIsland -> Just Curacao
    Cyprus -> Just ChristmasIsland
    Czechia -> Just Cyprus
    Germany -> Just Czechia
    Djibouti -> Just Germany
    Denmark -> Just Djibouti
    Dominica -> Just Denmark
    DominicanRepublic -> Just Dominica
    Algeria -> Just DominicanRepublic
    Ecuador -> Just Algeria
    Estonia -> Just Ecuador
    Egypt -> Just Estonia
    WesternSahara -> Just Egypt
    Eritrea -> Just WesternSahara
    Spain -> Just Eritrea
    Ethiopia -> Just Spain
    Finland -> Just Ethiopia
    Fiji -> Just Finland
    FalklandIslandsMalvinas -> Just Fiji
    MicronesiaFederatedStatesOf -> Just FalklandIslandsMalvinas
    FaroeIslands -> Just MicronesiaFederatedStatesOf
    France -> Just FaroeIslands
    Gabon -> Just France
    UnitedKingdomOfGreatBritainAndNorthernIreland -> Just Gabon
    Grenada -> Just UnitedKingdomOfGreatBritainAndNorthernIreland
    Georgia -> Just Grenada
    FrenchGuiana -> Just Georgia
    Guernsey -> Just FrenchGuiana
    Ghana -> Just Guernsey
    Gibraltar -> Just Ghana
    Greenland -> Just Gibraltar
    Gambia -> Just Greenland
    Guinea -> Just Gambia
    Guadeloupe -> Just Guinea
    EquatorialGuinea -> Just Guadeloupe
    Greece -> Just EquatorialGuinea
    SouthGeorgiaAndTheSouthSandwichIslands -> Just Greece
    Guatemala -> Just SouthGeorgiaAndTheSouthSandwichIslands
    Guam -> Just Guatemala
    GuineaBissau -> Just Guam
    Guyana -> Just GuineaBissau
    HongKong -> Just Guyana
    HeardIslandAndMcdonaldIslands -> Just HongKong
    Honduras -> Just HeardIslandAndMcdonaldIslands
    Croatia -> Just Honduras
    Haiti -> Just Croatia
    Hungary -> Just Haiti
    Indonesia -> Just Hungary
    Ireland -> Just Indonesia
    Israel -> Just Ireland
    IsleOfMan -> Just Israel
    India -> Just IsleOfMan
    BritishIndianOceanTerritory -> Just India
    Iraq -> Just BritishIndianOceanTerritory
    IranIslamicRepublicOf -> Just Iraq
    Iceland -> Just IranIslamicRepublicOf
    Italy -> Just Iceland
    Jersey -> Just Italy
    Jamaica -> Just Jersey
    Jordan -> Just Jamaica
    Japan -> Just Jordan
    Kenya -> Just Japan
    Kyrgyzstan -> Just Kenya
    Cambodia -> Just Kyrgyzstan
    Kiribati -> Just Cambodia
    Comoros -> Just Kiribati
    SaintKittsAndNevis -> Just Comoros
    KoreaDemocraticPeopleSRepublicOf -> Just SaintKittsAndNevis
    KoreaRepublicOf -> Just KoreaDemocraticPeopleSRepublicOf
    Kuwait -> Just KoreaRepublicOf
    CaymanIslands -> Just Kuwait
    Kazakhstan -> Just CaymanIslands
    LaoPeopleSDemocraticRepublic -> Just Kazakhstan
    Lebanon -> Just LaoPeopleSDemocraticRepublic
    SaintLucia -> Just Lebanon
    Liechtenstein -> Just SaintLucia
    SriLanka -> Just Liechtenstein
    Liberia -> Just SriLanka
    Lesotho -> Just Liberia
    Lithuania -> Just Lesotho
    Luxembourg -> Just Lithuania
    Latvia -> Just Luxembourg
    Libya -> Just Latvia
    Morocco -> Just Libya
    Monaco -> Just Morocco
    MoldovaRepublicOf -> Just Monaco
    Montenegro -> Just MoldovaRepublicOf
    SaintMartinFrenchPart -> Just Montenegro
    Madagascar -> Just SaintMartinFrenchPart
    MarshallIslands -> Just Madagascar
    NorthMacedonia -> Just MarshallIslands
    Mali -> Just NorthMacedonia
    Myanmar -> Just Mali
    Mongolia -> Just Myanmar
    Macao -> Just Mongolia
    NorthernMarianaIslands -> Just Macao
    Martinique -> Just NorthernMarianaIslands
    Mauritania -> Just Martinique
    Montserrat -> Just Mauritania
    Malta -> Just Montserrat
    Mauritius -> Just Malta
    Maldives -> Just Mauritius
    Malawi -> Just Maldives
    Mexico -> Just Malawi
    Malaysia -> Just Mexico
    Mozambique -> Just Malaysia
    Namibia -> Just Mozambique
    NewCaledonia -> Just Namibia
    Niger -> Just NewCaledonia
    NorfolkIsland -> Just Niger
    Nigeria -> Just NorfolkIsland
    Nicaragua -> Just Nigeria
    NetherlandsKingdomOfThe -> Just Nicaragua
    Norway -> Just NetherlandsKingdomOfThe
    Nepal -> Just Norway
    Nauru -> Just Nepal
    Niue -> Just Nauru
    NewZealand -> Just Niue
    Oman -> Just NewZealand
    Panama -> Just Oman
    Peru -> Just Panama
    FrenchPolynesia -> Just Peru
    PapuaNewGuinea -> Just FrenchPolynesia
    Philippines -> Just PapuaNewGuinea
    Pakistan -> Just Philippines
    Poland -> Just Pakistan
    SaintPierreAndMiquelon -> Just Poland
    Pitcairn -> Just SaintPierreAndMiquelon
    PuertoRico -> Just Pitcairn
    PalestineStateOf -> Just PuertoRico
    Portugal -> Just PalestineStateOf
    Palau -> Just Portugal
    Paraguay -> Just Palau
    Qatar -> Just Paraguay
    Reunion -> Just Qatar
    Romania -> Just Reunion
    Serbia -> Just Romania
    RussianFederation -> Just Serbia
    Rwanda -> Just RussianFederation
    SaudiArabia -> Just Rwanda
    SolomonIslands -> Just SaudiArabia
    Seychelles -> Just SolomonIslands
    Sudan -> Just Seychelles
    Sweden -> Just Sudan
    Singapore -> Just Sweden
    SaintHelenaAscensionAndTristanDaCunha -> Just Singapore
    Slovenia -> Just SaintHelenaAscensionAndTristanDaCunha
    SvalbardAndJanMayen -> Just Slovenia
    Slovakia -> Just SvalbardAndJanMayen
    SierraLeone -> Just Slovakia
    SanMarino -> Just SierraLeone
    Senegal -> Just SanMarino
    Somalia -> Just Senegal
    Suriname -> Just Somalia
    SouthSudan -> Just Suriname
    SaoTomeAndPrincipe -> Just SouthSudan
    ElSalvador -> Just SaoTomeAndPrincipe
    SintMaartenDutchPart -> Just ElSalvador
    SyrianArabRepublic -> Just SintMaartenDutchPart
    Eswatini -> Just SyrianArabRepublic
    TurksAndCaicosIslands -> Just Eswatini
    Chad -> Just TurksAndCaicosIslands
    FrenchSouthernTerritories -> Just Chad
    Togo -> Just FrenchSouthernTerritories
    Thailand -> Just Togo
    Tajikistan -> Just Thailand
    Tokelau -> Just Tajikistan
    TimorLeste -> Just Tokelau
    Turkmenistan -> Just TimorLeste
    Tunisia -> Just Turkmenistan
    Tonga -> Just Tunisia
    Turkiye -> Just Tonga
    TrinidadAndTobago -> Just Turkiye
    Tuvalu -> Just TrinidadAndTobago
    TaiwanProvinceOfChina -> Just Tuvalu
    TanzaniaUnitedRepublicOf -> Just TaiwanProvinceOfChina
    Ukraine -> Just TanzaniaUnitedRepublicOf
    Uganda -> Just Ukraine
    UnitedStatesMinorOutlyingIslands -> Just Uganda
    UnitedStatesOfAmerica -> Just UnitedStatesMinorOutlyingIslands
    Uruguay -> Just UnitedStatesOfAmerica
    Uzbekistan -> Just Uruguay
    HolySee -> Just Uzbekistan
    SaintVincentAndTheGrenadines -> Just HolySee
    VenezuelaBolivarianRepublicOf -> Just SaintVincentAndTheGrenadines
    VirginIslandsBritish -> Just VenezuelaBolivarianRepublicOf
    VirginIslandsUS -> Just VirginIslandsBritish
    VietNam -> Just VirginIslandsUS
    Vanuatu -> Just VietNam
    WallisAndFutuna -> Just Vanuatu
    Samoa -> Just WallisAndFutuna
    Yemen -> Just Samoa
    Mayotte -> Just Yemen
    SouthAfrica -> Just Mayotte
    Zambia -> Just SouthAfrica
    Zimbabwe -> Just Zambia

instance BoundedEnum Country where
  cardinality = Cardinality 249
  toEnum = case _ of
    0 -> Just Andorra
    1 -> Just UnitedArabEmirates
    2 -> Just Afghanistan
    3 -> Just AntiguaAndBarbuda
    4 -> Just Anguilla
    5 -> Just Albania
    6 -> Just Armenia
    7 -> Just Angola
    8 -> Just Antarctica
    9 -> Just Argentina
    10 -> Just AmericanSamoa
    11 -> Just Austria
    12 -> Just Australia
    13 -> Just Aruba
    14 -> Just AlandIslands
    15 -> Just Azerbaijan
    16 -> Just BosniaAndHerzegovina
    17 -> Just Barbados
    18 -> Just Bangladesh
    19 -> Just Belgium
    20 -> Just BurkinaFaso
    21 -> Just Bulgaria
    22 -> Just Bahrain
    23 -> Just Burundi
    24 -> Just Benin
    25 -> Just SaintBarthelemy
    26 -> Just Bermuda
    27 -> Just BruneiDarussalam
    28 -> Just BoliviaPlurinationalStateOf
    29 -> Just BonaireSintEustatiusAndSaba
    30 -> Just Brazil
    31 -> Just Bahamas
    32 -> Just Bhutan
    33 -> Just BouvetIsland
    34 -> Just Botswana
    35 -> Just Belarus
    36 -> Just Belize
    37 -> Just Canada
    38 -> Just CocosKeelingIslands
    39 -> Just CongoDemocraticRepublicOfThe
    40 -> Just CentralAfricanRepublic
    41 -> Just Congo
    42 -> Just Switzerland
    43 -> Just CoteDIvoire
    44 -> Just CookIslands
    45 -> Just Chile
    46 -> Just Cameroon
    47 -> Just China
    48 -> Just Colombia
    49 -> Just CostaRica
    50 -> Just Cuba
    51 -> Just CaboVerde
    52 -> Just Curacao
    53 -> Just ChristmasIsland
    54 -> Just Cyprus
    55 -> Just Czechia
    56 -> Just Germany
    57 -> Just Djibouti
    58 -> Just Denmark
    59 -> Just Dominica
    60 -> Just DominicanRepublic
    61 -> Just Algeria
    62 -> Just Ecuador
    63 -> Just Estonia
    64 -> Just Egypt
    65 -> Just WesternSahara
    66 -> Just Eritrea
    67 -> Just Spain
    68 -> Just Ethiopia
    69 -> Just Finland
    70 -> Just Fiji
    71 -> Just FalklandIslandsMalvinas
    72 -> Just MicronesiaFederatedStatesOf
    73 -> Just FaroeIslands
    74 -> Just France
    75 -> Just Gabon
    76 -> Just UnitedKingdomOfGreatBritainAndNorthernIreland
    77 -> Just Grenada
    78 -> Just Georgia
    79 -> Just FrenchGuiana
    80 -> Just Guernsey
    81 -> Just Ghana
    82 -> Just Gibraltar
    83 -> Just Greenland
    84 -> Just Gambia
    85 -> Just Guinea
    86 -> Just Guadeloupe
    87 -> Just EquatorialGuinea
    88 -> Just Greece
    89 -> Just SouthGeorgiaAndTheSouthSandwichIslands
    90 -> Just Guatemala
    91 -> Just Guam
    92 -> Just GuineaBissau
    93 -> Just Guyana
    94 -> Just HongKong
    95 -> Just HeardIslandAndMcdonaldIslands
    96 -> Just Honduras
    97 -> Just Croatia
    98 -> Just Haiti
    99 -> Just Hungary
    100 -> Just Indonesia
    101 -> Just Ireland
    102 -> Just Israel
    103 -> Just IsleOfMan
    104 -> Just India
    105 -> Just BritishIndianOceanTerritory
    106 -> Just Iraq
    107 -> Just IranIslamicRepublicOf
    108 -> Just Iceland
    109 -> Just Italy
    110 -> Just Jersey
    111 -> Just Jamaica
    112 -> Just Jordan
    113 -> Just Japan
    114 -> Just Kenya
    115 -> Just Kyrgyzstan
    116 -> Just Cambodia
    117 -> Just Kiribati
    118 -> Just Comoros
    119 -> Just SaintKittsAndNevis
    120 -> Just KoreaDemocraticPeopleSRepublicOf
    121 -> Just KoreaRepublicOf
    122 -> Just Kuwait
    123 -> Just CaymanIslands
    124 -> Just Kazakhstan
    125 -> Just LaoPeopleSDemocraticRepublic
    126 -> Just Lebanon
    127 -> Just SaintLucia
    128 -> Just Liechtenstein
    129 -> Just SriLanka
    130 -> Just Liberia
    131 -> Just Lesotho
    132 -> Just Lithuania
    133 -> Just Luxembourg
    134 -> Just Latvia
    135 -> Just Libya
    136 -> Just Morocco
    137 -> Just Monaco
    138 -> Just MoldovaRepublicOf
    139 -> Just Montenegro
    140 -> Just SaintMartinFrenchPart
    141 -> Just Madagascar
    142 -> Just MarshallIslands
    143 -> Just NorthMacedonia
    144 -> Just Mali
    145 -> Just Myanmar
    146 -> Just Mongolia
    147 -> Just Macao
    148 -> Just NorthernMarianaIslands
    149 -> Just Martinique
    150 -> Just Mauritania
    151 -> Just Montserrat
    152 -> Just Malta
    153 -> Just Mauritius
    154 -> Just Maldives
    155 -> Just Malawi
    156 -> Just Mexico
    157 -> Just Malaysia
    158 -> Just Mozambique
    159 -> Just Namibia
    160 -> Just NewCaledonia
    161 -> Just Niger
    162 -> Just NorfolkIsland
    163 -> Just Nigeria
    164 -> Just Nicaragua
    165 -> Just NetherlandsKingdomOfThe
    166 -> Just Norway
    167 -> Just Nepal
    168 -> Just Nauru
    169 -> Just Niue
    170 -> Just NewZealand
    171 -> Just Oman
    172 -> Just Panama
    173 -> Just Peru
    174 -> Just FrenchPolynesia
    175 -> Just PapuaNewGuinea
    176 -> Just Philippines
    177 -> Just Pakistan
    178 -> Just Poland
    179 -> Just SaintPierreAndMiquelon
    180 -> Just Pitcairn
    181 -> Just PuertoRico
    182 -> Just PalestineStateOf
    183 -> Just Portugal
    184 -> Just Palau
    185 -> Just Paraguay
    186 -> Just Qatar
    187 -> Just Reunion
    188 -> Just Romania
    189 -> Just Serbia
    190 -> Just RussianFederation
    191 -> Just Rwanda
    192 -> Just SaudiArabia
    193 -> Just SolomonIslands
    194 -> Just Seychelles
    195 -> Just Sudan
    196 -> Just Sweden
    197 -> Just Singapore
    198 -> Just SaintHelenaAscensionAndTristanDaCunha
    199 -> Just Slovenia
    200 -> Just SvalbardAndJanMayen
    201 -> Just Slovakia
    202 -> Just SierraLeone
    203 -> Just SanMarino
    204 -> Just Senegal
    205 -> Just Somalia
    206 -> Just Suriname
    207 -> Just SouthSudan
    208 -> Just SaoTomeAndPrincipe
    209 -> Just ElSalvador
    210 -> Just SintMaartenDutchPart
    211 -> Just SyrianArabRepublic
    212 -> Just Eswatini
    213 -> Just TurksAndCaicosIslands
    214 -> Just Chad
    215 -> Just FrenchSouthernTerritories
    216 -> Just Togo
    217 -> Just Thailand
    218 -> Just Tajikistan
    219 -> Just Tokelau
    220 -> Just TimorLeste
    221 -> Just Turkmenistan
    222 -> Just Tunisia
    223 -> Just Tonga
    224 -> Just Turkiye
    225 -> Just TrinidadAndTobago
    226 -> Just Tuvalu
    227 -> Just TaiwanProvinceOfChina
    228 -> Just TanzaniaUnitedRepublicOf
    229 -> Just Ukraine
    230 -> Just Uganda
    231 -> Just UnitedStatesMinorOutlyingIslands
    232 -> Just UnitedStatesOfAmerica
    233 -> Just Uruguay
    234 -> Just Uzbekistan
    235 -> Just HolySee
    236 -> Just SaintVincentAndTheGrenadines
    237 -> Just VenezuelaBolivarianRepublicOf
    238 -> Just VirginIslandsBritish
    239 -> Just VirginIslandsUS
    240 -> Just VietNam
    241 -> Just Vanuatu
    242 -> Just WallisAndFutuna
    243 -> Just Samoa
    244 -> Just Yemen
    245 -> Just Mayotte
    246 -> Just SouthAfrica
    247 -> Just Zambia
    248 -> Just Zimbabwe
    _ -> Nothing
  fromEnum = case _ of
    Andorra -> 0
    UnitedArabEmirates -> 1
    Afghanistan -> 2
    AntiguaAndBarbuda -> 3
    Anguilla -> 4
    Albania -> 5
    Armenia -> 6
    Angola -> 7
    Antarctica -> 8
    Argentina -> 9
    AmericanSamoa -> 10
    Austria -> 11
    Australia -> 12
    Aruba -> 13
    AlandIslands -> 14
    Azerbaijan -> 15
    BosniaAndHerzegovina -> 16
    Barbados -> 17
    Bangladesh -> 18
    Belgium -> 19
    BurkinaFaso -> 20
    Bulgaria -> 21
    Bahrain -> 22
    Burundi -> 23
    Benin -> 24
    SaintBarthelemy -> 25
    Bermuda -> 26
    BruneiDarussalam -> 27
    BoliviaPlurinationalStateOf -> 28
    BonaireSintEustatiusAndSaba -> 29
    Brazil -> 30
    Bahamas -> 31
    Bhutan -> 32
    BouvetIsland -> 33
    Botswana -> 34
    Belarus -> 35
    Belize -> 36
    Canada -> 37
    CocosKeelingIslands -> 38
    CongoDemocraticRepublicOfThe -> 39
    CentralAfricanRepublic -> 40
    Congo -> 41
    Switzerland -> 42
    CoteDIvoire -> 43
    CookIslands -> 44
    Chile -> 45
    Cameroon -> 46
    China -> 47
    Colombia -> 48
    CostaRica -> 49
    Cuba -> 50
    CaboVerde -> 51
    Curacao -> 52
    ChristmasIsland -> 53
    Cyprus -> 54
    Czechia -> 55
    Germany -> 56
    Djibouti -> 57
    Denmark -> 58
    Dominica -> 59
    DominicanRepublic -> 60
    Algeria -> 61
    Ecuador -> 62
    Estonia -> 63
    Egypt -> 64
    WesternSahara -> 65
    Eritrea -> 66
    Spain -> 67
    Ethiopia -> 68
    Finland -> 69
    Fiji -> 70
    FalklandIslandsMalvinas -> 71
    MicronesiaFederatedStatesOf -> 72
    FaroeIslands -> 73
    France -> 74
    Gabon -> 75
    UnitedKingdomOfGreatBritainAndNorthernIreland -> 76
    Grenada -> 77
    Georgia -> 78
    FrenchGuiana -> 79
    Guernsey -> 80
    Ghana -> 81
    Gibraltar -> 82
    Greenland -> 83
    Gambia -> 84
    Guinea -> 85
    Guadeloupe -> 86
    EquatorialGuinea -> 87
    Greece -> 88
    SouthGeorgiaAndTheSouthSandwichIslands -> 89
    Guatemala -> 90
    Guam -> 91
    GuineaBissau -> 92
    Guyana -> 93
    HongKong -> 94
    HeardIslandAndMcdonaldIslands -> 95
    Honduras -> 96
    Croatia -> 97
    Haiti -> 98
    Hungary -> 99
    Indonesia -> 100
    Ireland -> 101
    Israel -> 102
    IsleOfMan -> 103
    India -> 104
    BritishIndianOceanTerritory -> 105
    Iraq -> 106
    IranIslamicRepublicOf -> 107
    Iceland -> 108
    Italy -> 109
    Jersey -> 110
    Jamaica -> 111
    Jordan -> 112
    Japan -> 113
    Kenya -> 114
    Kyrgyzstan -> 115
    Cambodia -> 116
    Kiribati -> 117
    Comoros -> 118
    SaintKittsAndNevis -> 119
    KoreaDemocraticPeopleSRepublicOf -> 120
    KoreaRepublicOf -> 121
    Kuwait -> 122
    CaymanIslands -> 123
    Kazakhstan -> 124
    LaoPeopleSDemocraticRepublic -> 125
    Lebanon -> 126
    SaintLucia -> 127
    Liechtenstein -> 128
    SriLanka -> 129
    Liberia -> 130
    Lesotho -> 131
    Lithuania -> 132
    Luxembourg -> 133
    Latvia -> 134
    Libya -> 135
    Morocco -> 136
    Monaco -> 137
    MoldovaRepublicOf -> 138
    Montenegro -> 139
    SaintMartinFrenchPart -> 140
    Madagascar -> 141
    MarshallIslands -> 142
    NorthMacedonia -> 143
    Mali -> 144
    Myanmar -> 145
    Mongolia -> 146
    Macao -> 147
    NorthernMarianaIslands -> 148
    Martinique -> 149
    Mauritania -> 150
    Montserrat -> 151
    Malta -> 152
    Mauritius -> 153
    Maldives -> 154
    Malawi -> 155
    Mexico -> 156
    Malaysia -> 157
    Mozambique -> 158
    Namibia -> 159
    NewCaledonia -> 160
    Niger -> 161
    NorfolkIsland -> 162
    Nigeria -> 163
    Nicaragua -> 164
    NetherlandsKingdomOfThe -> 165
    Norway -> 166
    Nepal -> 167
    Nauru -> 168
    Niue -> 169
    NewZealand -> 170
    Oman -> 171
    Panama -> 172
    Peru -> 173
    FrenchPolynesia -> 174
    PapuaNewGuinea -> 175
    Philippines -> 176
    Pakistan -> 177
    Poland -> 178
    SaintPierreAndMiquelon -> 179
    Pitcairn -> 180
    PuertoRico -> 181
    PalestineStateOf -> 182
    Portugal -> 183
    Palau -> 184
    Paraguay -> 185
    Qatar -> 186
    Reunion -> 187
    Romania -> 188
    Serbia -> 189
    RussianFederation -> 190
    Rwanda -> 191
    SaudiArabia -> 192
    SolomonIslands -> 193
    Seychelles -> 194
    Sudan -> 195
    Sweden -> 196
    Singapore -> 197
    SaintHelenaAscensionAndTristanDaCunha -> 198
    Slovenia -> 199
    SvalbardAndJanMayen -> 200
    Slovakia -> 201
    SierraLeone -> 202
    SanMarino -> 203
    Senegal -> 204
    Somalia -> 205
    Suriname -> 206
    SouthSudan -> 207
    SaoTomeAndPrincipe -> 208
    ElSalvador -> 209
    SintMaartenDutchPart -> 210
    SyrianArabRepublic -> 211
    Eswatini -> 212
    TurksAndCaicosIslands -> 213
    Chad -> 214
    FrenchSouthernTerritories -> 215
    Togo -> 216
    Thailand -> 217
    Tajikistan -> 218
    Tokelau -> 219
    TimorLeste -> 220
    Turkmenistan -> 221
    Tunisia -> 222
    Tonga -> 223
    Turkiye -> 224
    TrinidadAndTobago -> 225
    Tuvalu -> 226
    TaiwanProvinceOfChina -> 227
    TanzaniaUnitedRepublicOf -> 228
    Ukraine -> 229
    Uganda -> 230
    UnitedStatesMinorOutlyingIslands -> 231
    UnitedStatesOfAmerica -> 232
    Uruguay -> 233
    Uzbekistan -> 234
    HolySee -> 235
    SaintVincentAndTheGrenadines -> 236
    VenezuelaBolivarianRepublicOf -> 237
    VirginIslandsBritish -> 238
    VirginIslandsUS -> 239
    VietNam -> 240
    Vanuatu -> 241
    WallisAndFutuna -> 242
    Samoa -> 243
    Yemen -> 244
    Mayotte -> 245
    SouthAfrica -> 246
    Zambia -> 247
    Zimbabwe -> 248

toAlpha2 :: Country -> String
toAlpha2 = case _ of
  Andorra -> "AD"
  UnitedArabEmirates -> "AE"
  Afghanistan -> "AF"
  AntiguaAndBarbuda -> "AG"
  Anguilla -> "AI"
  Albania -> "AL"
  Armenia -> "AM"
  Angola -> "AO"
  Antarctica -> "AQ"
  Argentina -> "AR"
  AmericanSamoa -> "AS"
  Austria -> "AT"
  Australia -> "AU"
  Aruba -> "AW"
  AlandIslands -> "AX"
  Azerbaijan -> "AZ"
  BosniaAndHerzegovina -> "BA"
  Barbados -> "BB"
  Bangladesh -> "BD"
  Belgium -> "BE"
  BurkinaFaso -> "BF"
  Bulgaria -> "BG"
  Bahrain -> "BH"
  Burundi -> "BI"
  Benin -> "BJ"
  SaintBarthelemy -> "BL"
  Bermuda -> "BM"
  BruneiDarussalam -> "BN"
  BoliviaPlurinationalStateOf -> "BO"
  BonaireSintEustatiusAndSaba -> "BQ"
  Brazil -> "BR"
  Bahamas -> "BS"
  Bhutan -> "BT"
  BouvetIsland -> "BV"
  Botswana -> "BW"
  Belarus -> "BY"
  Belize -> "BZ"
  Canada -> "CA"
  CocosKeelingIslands -> "CC"
  CongoDemocraticRepublicOfThe -> "CD"
  CentralAfricanRepublic -> "CF"
  Congo -> "CG"
  Switzerland -> "CH"
  CoteDIvoire -> "CI"
  CookIslands -> "CK"
  Chile -> "CL"
  Cameroon -> "CM"
  China -> "CN"
  Colombia -> "CO"
  CostaRica -> "CR"
  Cuba -> "CU"
  CaboVerde -> "CV"
  Curacao -> "CW"
  ChristmasIsland -> "CX"
  Cyprus -> "CY"
  Czechia -> "CZ"
  Germany -> "DE"
  Djibouti -> "DJ"
  Denmark -> "DK"
  Dominica -> "DM"
  DominicanRepublic -> "DO"
  Algeria -> "DZ"
  Ecuador -> "EC"
  Estonia -> "EE"
  Egypt -> "EG"
  WesternSahara -> "EH"
  Eritrea -> "ER"
  Spain -> "ES"
  Ethiopia -> "ET"
  Finland -> "FI"
  Fiji -> "FJ"
  FalklandIslandsMalvinas -> "FK"
  MicronesiaFederatedStatesOf -> "FM"
  FaroeIslands -> "FO"
  France -> "FR"
  Gabon -> "GA"
  UnitedKingdomOfGreatBritainAndNorthernIreland -> "GB"
  Grenada -> "GD"
  Georgia -> "GE"
  FrenchGuiana -> "GF"
  Guernsey -> "GG"
  Ghana -> "GH"
  Gibraltar -> "GI"
  Greenland -> "GL"
  Gambia -> "GM"
  Guinea -> "GN"
  Guadeloupe -> "GP"
  EquatorialGuinea -> "GQ"
  Greece -> "GR"
  SouthGeorgiaAndTheSouthSandwichIslands -> "GS"
  Guatemala -> "GT"
  Guam -> "GU"
  GuineaBissau -> "GW"
  Guyana -> "GY"
  HongKong -> "HK"
  HeardIslandAndMcdonaldIslands -> "HM"
  Honduras -> "HN"
  Croatia -> "HR"
  Haiti -> "HT"
  Hungary -> "HU"
  Indonesia -> "ID"
  Ireland -> "IE"
  Israel -> "IL"
  IsleOfMan -> "IM"
  India -> "IN"
  BritishIndianOceanTerritory -> "IO"
  Iraq -> "IQ"
  IranIslamicRepublicOf -> "IR"
  Iceland -> "IS"
  Italy -> "IT"
  Jersey -> "JE"
  Jamaica -> "JM"
  Jordan -> "JO"
  Japan -> "JP"
  Kenya -> "KE"
  Kyrgyzstan -> "KG"
  Cambodia -> "KH"
  Kiribati -> "KI"
  Comoros -> "KM"
  SaintKittsAndNevis -> "KN"
  KoreaDemocraticPeopleSRepublicOf -> "KP"
  KoreaRepublicOf -> "KR"
  Kuwait -> "KW"
  CaymanIslands -> "KY"
  Kazakhstan -> "KZ"
  LaoPeopleSDemocraticRepublic -> "LA"
  Lebanon -> "LB"
  SaintLucia -> "LC"
  Liechtenstein -> "LI"
  SriLanka -> "LK"
  Liberia -> "LR"
  Lesotho -> "LS"
  Lithuania -> "LT"
  Luxembourg -> "LU"
  Latvia -> "LV"
  Libya -> "LY"
  Morocco -> "MA"
  Monaco -> "MC"
  MoldovaRepublicOf -> "MD"
  Montenegro -> "ME"
  SaintMartinFrenchPart -> "MF"
  Madagascar -> "MG"
  MarshallIslands -> "MH"
  NorthMacedonia -> "MK"
  Mali -> "ML"
  Myanmar -> "MM"
  Mongolia -> "MN"
  Macao -> "MO"
  NorthernMarianaIslands -> "MP"
  Martinique -> "MQ"
  Mauritania -> "MR"
  Montserrat -> "MS"
  Malta -> "MT"
  Mauritius -> "MU"
  Maldives -> "MV"
  Malawi -> "MW"
  Mexico -> "MX"
  Malaysia -> "MY"
  Mozambique -> "MZ"
  Namibia -> "NA"
  NewCaledonia -> "NC"
  Niger -> "NE"
  NorfolkIsland -> "NF"
  Nigeria -> "NG"
  Nicaragua -> "NI"
  NetherlandsKingdomOfThe -> "NL"
  Norway -> "NO"
  Nepal -> "NP"
  Nauru -> "NR"
  Niue -> "NU"
  NewZealand -> "NZ"
  Oman -> "OM"
  Panama -> "PA"
  Peru -> "PE"
  FrenchPolynesia -> "PF"
  PapuaNewGuinea -> "PG"
  Philippines -> "PH"
  Pakistan -> "PK"
  Poland -> "PL"
  SaintPierreAndMiquelon -> "PM"
  Pitcairn -> "PN"
  PuertoRico -> "PR"
  PalestineStateOf -> "PS"
  Portugal -> "PT"
  Palau -> "PW"
  Paraguay -> "PY"
  Qatar -> "QA"
  Reunion -> "RE"
  Romania -> "RO"
  Serbia -> "RS"
  RussianFederation -> "RU"
  Rwanda -> "RW"
  SaudiArabia -> "SA"
  SolomonIslands -> "SB"
  Seychelles -> "SC"
  Sudan -> "SD"
  Sweden -> "SE"
  Singapore -> "SG"
  SaintHelenaAscensionAndTristanDaCunha -> "SH"
  Slovenia -> "SI"
  SvalbardAndJanMayen -> "SJ"
  Slovakia -> "SK"
  SierraLeone -> "SL"
  SanMarino -> "SM"
  Senegal -> "SN"
  Somalia -> "SO"
  Suriname -> "SR"
  SouthSudan -> "SS"
  SaoTomeAndPrincipe -> "ST"
  ElSalvador -> "SV"
  SintMaartenDutchPart -> "SX"
  SyrianArabRepublic -> "SY"
  Eswatini -> "SZ"
  TurksAndCaicosIslands -> "TC"
  Chad -> "TD"
  FrenchSouthernTerritories -> "TF"
  Togo -> "TG"
  Thailand -> "TH"
  Tajikistan -> "TJ"
  Tokelau -> "TK"
  TimorLeste -> "TL"
  Turkmenistan -> "TM"
  Tunisia -> "TN"
  Tonga -> "TO"
  Turkiye -> "TR"
  TrinidadAndTobago -> "TT"
  Tuvalu -> "TV"
  TaiwanProvinceOfChina -> "TW"
  TanzaniaUnitedRepublicOf -> "TZ"
  Ukraine -> "UA"
  Uganda -> "UG"
  UnitedStatesMinorOutlyingIslands -> "UM"
  UnitedStatesOfAmerica -> "US"
  Uruguay -> "UY"
  Uzbekistan -> "UZ"
  HolySee -> "VA"
  SaintVincentAndTheGrenadines -> "VC"
  VenezuelaBolivarianRepublicOf -> "VE"
  VirginIslandsBritish -> "VG"
  VirginIslandsUS -> "VI"
  VietNam -> "VN"
  Vanuatu -> "VU"
  WallisAndFutuna -> "WF"
  Samoa -> "WS"
  Yemen -> "YE"
  Mayotte -> "YT"
  SouthAfrica -> "ZA"
  Zambia -> "ZM"
  Zimbabwe -> "ZW"

fromAlpha2 :: String -> Maybe Country
fromAlpha2 = case _ of
  "AD" -> Just Andorra
  "AE" -> Just UnitedArabEmirates
  "AF" -> Just Afghanistan
  "AG" -> Just AntiguaAndBarbuda
  "AI" -> Just Anguilla
  "AL" -> Just Albania
  "AM" -> Just Armenia
  "AO" -> Just Angola
  "AQ" -> Just Antarctica
  "AR" -> Just Argentina
  "AS" -> Just AmericanSamoa
  "AT" -> Just Austria
  "AU" -> Just Australia
  "AW" -> Just Aruba
  "AX" -> Just AlandIslands
  "AZ" -> Just Azerbaijan
  "BA" -> Just BosniaAndHerzegovina
  "BB" -> Just Barbados
  "BD" -> Just Bangladesh
  "BE" -> Just Belgium
  "BF" -> Just BurkinaFaso
  "BG" -> Just Bulgaria
  "BH" -> Just Bahrain
  "BI" -> Just Burundi
  "BJ" -> Just Benin
  "BL" -> Just SaintBarthelemy
  "BM" -> Just Bermuda
  "BN" -> Just BruneiDarussalam
  "BO" -> Just BoliviaPlurinationalStateOf
  "BQ" -> Just BonaireSintEustatiusAndSaba
  "BR" -> Just Brazil
  "BS" -> Just Bahamas
  "BT" -> Just Bhutan
  "BV" -> Just BouvetIsland
  "BW" -> Just Botswana
  "BY" -> Just Belarus
  "BZ" -> Just Belize
  "CA" -> Just Canada
  "CC" -> Just CocosKeelingIslands
  "CD" -> Just CongoDemocraticRepublicOfThe
  "CF" -> Just CentralAfricanRepublic
  "CG" -> Just Congo
  "CH" -> Just Switzerland
  "CI" -> Just CoteDIvoire
  "CK" -> Just CookIslands
  "CL" -> Just Chile
  "CM" -> Just Cameroon
  "CN" -> Just China
  "CO" -> Just Colombia
  "CR" -> Just CostaRica
  "CU" -> Just Cuba
  "CV" -> Just CaboVerde
  "CW" -> Just Curacao
  "CX" -> Just ChristmasIsland
  "CY" -> Just Cyprus
  "CZ" -> Just Czechia
  "DE" -> Just Germany
  "DJ" -> Just Djibouti
  "DK" -> Just Denmark
  "DM" -> Just Dominica
  "DO" -> Just DominicanRepublic
  "DZ" -> Just Algeria
  "EC" -> Just Ecuador
  "EE" -> Just Estonia
  "EG" -> Just Egypt
  "EH" -> Just WesternSahara
  "ER" -> Just Eritrea
  "ES" -> Just Spain
  "ET" -> Just Ethiopia
  "FI" -> Just Finland
  "FJ" -> Just Fiji
  "FK" -> Just FalklandIslandsMalvinas
  "FM" -> Just MicronesiaFederatedStatesOf
  "FO" -> Just FaroeIslands
  "FR" -> Just France
  "GA" -> Just Gabon
  "GB" -> Just UnitedKingdomOfGreatBritainAndNorthernIreland
  "GD" -> Just Grenada
  "GE" -> Just Georgia
  "GF" -> Just FrenchGuiana
  "GG" -> Just Guernsey
  "GH" -> Just Ghana
  "GI" -> Just Gibraltar
  "GL" -> Just Greenland
  "GM" -> Just Gambia
  "GN" -> Just Guinea
  "GP" -> Just Guadeloupe
  "GQ" -> Just EquatorialGuinea
  "GR" -> Just Greece
  "GS" -> Just SouthGeorgiaAndTheSouthSandwichIslands
  "GT" -> Just Guatemala
  "GU" -> Just Guam
  "GW" -> Just GuineaBissau
  "GY" -> Just Guyana
  "HK" -> Just HongKong
  "HM" -> Just HeardIslandAndMcdonaldIslands
  "HN" -> Just Honduras
  "HR" -> Just Croatia
  "HT" -> Just Haiti
  "HU" -> Just Hungary
  "ID" -> Just Indonesia
  "IE" -> Just Ireland
  "IL" -> Just Israel
  "IM" -> Just IsleOfMan
  "IN" -> Just India
  "IO" -> Just BritishIndianOceanTerritory
  "IQ" -> Just Iraq
  "IR" -> Just IranIslamicRepublicOf
  "IS" -> Just Iceland
  "IT" -> Just Italy
  "JE" -> Just Jersey
  "JM" -> Just Jamaica
  "JO" -> Just Jordan
  "JP" -> Just Japan
  "KE" -> Just Kenya
  "KG" -> Just Kyrgyzstan
  "KH" -> Just Cambodia
  "KI" -> Just Kiribati
  "KM" -> Just Comoros
  "KN" -> Just SaintKittsAndNevis
  "KP" -> Just KoreaDemocraticPeopleSRepublicOf
  "KR" -> Just KoreaRepublicOf
  "KW" -> Just Kuwait
  "KY" -> Just CaymanIslands
  "KZ" -> Just Kazakhstan
  "LA" -> Just LaoPeopleSDemocraticRepublic
  "LB" -> Just Lebanon
  "LC" -> Just SaintLucia
  "LI" -> Just Liechtenstein
  "LK" -> Just SriLanka
  "LR" -> Just Liberia
  "LS" -> Just Lesotho
  "LT" -> Just Lithuania
  "LU" -> Just Luxembourg
  "LV" -> Just Latvia
  "LY" -> Just Libya
  "MA" -> Just Morocco
  "MC" -> Just Monaco
  "MD" -> Just MoldovaRepublicOf
  "ME" -> Just Montenegro
  "MF" -> Just SaintMartinFrenchPart
  "MG" -> Just Madagascar
  "MH" -> Just MarshallIslands
  "MK" -> Just NorthMacedonia
  "ML" -> Just Mali
  "MM" -> Just Myanmar
  "MN" -> Just Mongolia
  "MO" -> Just Macao
  "MP" -> Just NorthernMarianaIslands
  "MQ" -> Just Martinique
  "MR" -> Just Mauritania
  "MS" -> Just Montserrat
  "MT" -> Just Malta
  "MU" -> Just Mauritius
  "MV" -> Just Maldives
  "MW" -> Just Malawi
  "MX" -> Just Mexico
  "MY" -> Just Malaysia
  "MZ" -> Just Mozambique
  "NA" -> Just Namibia
  "NC" -> Just NewCaledonia
  "NE" -> Just Niger
  "NF" -> Just NorfolkIsland
  "NG" -> Just Nigeria
  "NI" -> Just Nicaragua
  "NL" -> Just NetherlandsKingdomOfThe
  "NO" -> Just Norway
  "NP" -> Just Nepal
  "NR" -> Just Nauru
  "NU" -> Just Niue
  "NZ" -> Just NewZealand
  "OM" -> Just Oman
  "PA" -> Just Panama
  "PE" -> Just Peru
  "PF" -> Just FrenchPolynesia
  "PG" -> Just PapuaNewGuinea
  "PH" -> Just Philippines
  "PK" -> Just Pakistan
  "PL" -> Just Poland
  "PM" -> Just SaintPierreAndMiquelon
  "PN" -> Just Pitcairn
  "PR" -> Just PuertoRico
  "PS" -> Just PalestineStateOf
  "PT" -> Just Portugal
  "PW" -> Just Palau
  "PY" -> Just Paraguay
  "QA" -> Just Qatar
  "RE" -> Just Reunion
  "RO" -> Just Romania
  "RS" -> Just Serbia
  "RU" -> Just RussianFederation
  "RW" -> Just Rwanda
  "SA" -> Just SaudiArabia
  "SB" -> Just SolomonIslands
  "SC" -> Just Seychelles
  "SD" -> Just Sudan
  "SE" -> Just Sweden
  "SG" -> Just Singapore
  "SH" -> Just SaintHelenaAscensionAndTristanDaCunha
  "SI" -> Just Slovenia
  "SJ" -> Just SvalbardAndJanMayen
  "SK" -> Just Slovakia
  "SL" -> Just SierraLeone
  "SM" -> Just SanMarino
  "SN" -> Just Senegal
  "SO" -> Just Somalia
  "SR" -> Just Suriname
  "SS" -> Just SouthSudan
  "ST" -> Just SaoTomeAndPrincipe
  "SV" -> Just ElSalvador
  "SX" -> Just SintMaartenDutchPart
  "SY" -> Just SyrianArabRepublic
  "SZ" -> Just Eswatini
  "TC" -> Just TurksAndCaicosIslands
  "TD" -> Just Chad
  "TF" -> Just FrenchSouthernTerritories
  "TG" -> Just Togo
  "TH" -> Just Thailand
  "TJ" -> Just Tajikistan
  "TK" -> Just Tokelau
  "TL" -> Just TimorLeste
  "TM" -> Just Turkmenistan
  "TN" -> Just Tunisia
  "TO" -> Just Tonga
  "TR" -> Just Turkiye
  "TT" -> Just TrinidadAndTobago
  "TV" -> Just Tuvalu
  "TW" -> Just TaiwanProvinceOfChina
  "TZ" -> Just TanzaniaUnitedRepublicOf
  "UA" -> Just Ukraine
  "UG" -> Just Uganda
  "UM" -> Just UnitedStatesMinorOutlyingIslands
  "US" -> Just UnitedStatesOfAmerica
  "UY" -> Just Uruguay
  "UZ" -> Just Uzbekistan
  "VA" -> Just HolySee
  "VC" -> Just SaintVincentAndTheGrenadines
  "VE" -> Just VenezuelaBolivarianRepublicOf
  "VG" -> Just VirginIslandsBritish
  "VI" -> Just VirginIslandsUS
  "VN" -> Just VietNam
  "VU" -> Just Vanuatu
  "WF" -> Just WallisAndFutuna
  "WS" -> Just Samoa
  "YE" -> Just Yemen
  "YT" -> Just Mayotte
  "ZA" -> Just SouthAfrica
  "ZM" -> Just Zambia
  "ZW" -> Just Zimbabwe
  _ -> Nothing

allCountries :: Array Country
allCountries = enumFromTo bottom top

instance WriteForeign Country where
  writeImpl = toAlpha2 >>> writeImpl

instance ReadForeign Country where
  readImpl f = do
    let parse s = case fromAlpha2 s of
          Just c -> pure c
          Nothing -> throwError $ pure $ ForeignError $ "Unknown country: " <> s
    s <- readImpl f
    parse s

