library(tidyverse)
library(zmisc)

mapping_tbl <- list()

# Define a tibble with Icelandic character mappings
mapping_tbl$is <- tribble(
  ~lang, ~org, ~ascii,
  "is", "Á", "A",
  "is", "á", "a",
  "is", "Ð", "D",
  "is", "ð", "d",
  "is", "É", "E",
  "is", "é", "e",
  "is", "Í", "I",
  "is", "í", "i",
  "is", "Ó", "O",
  "is", "ó", "o",
  "is", "Ú", "U",
  "is", "ú", "u",
  "is", "Ý", "Y",
  "is", "ý", "y",
  "is", "Þ", "Th",
  "is", "þ", "th",
  "is", "Æ", "Ae",
  "is", "æ", "ae",
  "is", "Ö", "O",
  "is", "ö", "o"
)

# Define a tibble with Swedish character mappings
mapping_tbl$se <- tribble(
  ~lang, ~org, ~ascii,
  "se", "Å", "A",
  "se", "å", "a",
  "se", "Ä", "A",
  "se", "ä", "a",
  "se", "Ö", "O",
  "se", "ö", "o"
)

# Define a tibble with Danish character mappings
mapping_tbl$dk <- tribble(
  ~lang, ~org, ~ascii,
  "dk", "Æ", "Ae",
  "dk", "æ", "ae",
  "dk", "Ø", "O",
  "dk", "ø", "o",
  "dk", "Å", "Aa",
  "dk", "å", "aa"
)

# Define a tibble with Norwegian character mappings
mapping_tbl$no <- tribble(
  ~lang, ~org, ~ascii,
  "no", "Æ", "Ae",
  "no", "æ", "ae",
  "no", "Ø", "O",
  "no", "ø", "o",
  "no", "Å", "Aa",
  "no", "å", "aa"
)

# Define a tibble with Finnish character mappings
mapping_tbl$fi <- tribble(
  ~lang, ~org, ~ascii,
  "fi", "Ä", "A",
  "fi", "ä", "a",
  "fi", "Ö", "O",
  "fi", "ö", "o"
)

# Define a tibble with German character mappings
mapping_tbl$de <- tribble(
  ~lang, ~org, ~ascii,
  "de", "ß", "ss"
)

mapping_tbl$fr <- tribble(
  ~lang, ~org, ~ascii,
  "fr", "À", "A",
  "fr", "à", "a",
  "fr", "Â", "A",
  "fr", "â", "a",
  "fr", "Ç", "C",
  "fr", "ç", "c",
  "fr", "È", "E",
  "fr", "è", "e",
  "fr", "É", "E",
  "fr", "é", "e",
  "fr", "Ê", "E",
  "fr", "ê", "e",
  "fr", "Î", "I",
  "fr", "î", "i",
  "fr", "Ï", "I",
  "fr", "ï", "i",
  "fr", "Ô", "O",
  "fr", "ô", "o",
  "fr", "Œ", "OE",
  "fr", "œ", "oe",
  "fr", "Ù", "U",
  "fr", "ù", "u",
  "fr", "Û", "U",
  "fr", "û", "u",
  "fr", "Ü", "U",
  "fr", "ü", "u",
  "fr", "Ÿ", "Y",
  "fr", "ÿ", "y"
)
mapping_tbl$es <- tribble(
  ~lang, ~org, ~ascii,
  "es", "Á", "A",
  "es", "á", "a",
  "es", "É", "E",
  "es", "é", "e",
  "es", "Í", "I",
  "es", "í", "i",
  "es", "Ñ", "N",
  "es", "ñ", "n",
  "es", "Ó", "O",
  "es", "ó", "o",
  "es", "Ú", "U",
  "es", "ú", "u",
  "es", "Ü", "U",
  "es", "ü", "u"
)

mapping_tbl$it <- tribble(
  ~lang, ~org, ~ascii,
  "it", "À", "A",
  "it", "à", "a",
  "it", "È", "E",
  "it", "è", "e",
  "it", "É", "E'",
  "it", "é", "e'",
  "it", "Ì", "I",
  "it", "ì", "i",
  "it", "Ò", "O",
  "it", "ò", "o",
  "it", "Ù", "U",
  "it", "ù", "u",
  "it", "Ô", "O'",
  "it", "ô", "o'",
  "it", "Î", "I'",
  "it", "î", "i'"
)
# Define a tibble with Estonian character mappings
mapping_tbl$ee <- tribble(
  ~lang, ~org, ~ascii,
  "ee", "Õ", "O",
  "ee", "õ", "o",
  "ee", "Ä", "A",
  "ee", "ä", "a",
  "ee", "Ö", "O",
  "ee", "ö", "o",
  "ee", "Ü", "U",
  "ee", "ü", "u"
)

# Define a tibble with Latvian character mappings
mapping_tbl$lv <- tribble(
  ~lang, ~org, ~ascii,
  "lv", "Ā", "A",
  "lv", "ā", "a",
  "lv", "Ē", "E",
  "lv", "ē", "e",
  "lv", "Ģ", "G",
  "lv", "ģ", "g",
  "lv", "Ī", "I",
  "lv", "ī", "i",
  "lv", "Ķ", "K",
  "lv", "ķ", "k",
  "lv", "Ļ", "L",
  "lv", "ļ", "l",
  "lv", "Ņ", "N",
  "lv", "ņ", "n",
  "lv", "Ū", "U",
  "lv", "ū", "u",
  "lv", "Č", "C",
  "lv", "č", "c",
  "lv", "Š", "S",
  "lv", "š", "s",
  "lv", "Ž", "Z",
  "lv", "ž", "z"
)

# Define a tibble with Lithuanian character mappings
mapping_tbl$lt <- tribble(
  ~lang, ~org, ~ascii,
  "lt", "Ą", "A",
  "lt", "ą", "a",
  "lt", "Č", "C",
  "lt", "č", "c",
  "lt", "Ę", "E",
  "lt", "ę", "e",
  "lt", "Ė", "E",
  "lt", "ė", "e",
  "lt", "Į", "I",
  "lt", "į", "i",
  "lt", "Š", "S",
  "lt", "š", "s",
  "lt", "Ų", "U",
  "lt", "ų", "u",
  "lt", "Ū", "U",
  "lt", "ū", "u",
  "lt", "Ž", "Z",
  "lt", "ž", "z"
)

# Define a tibble with Polish character mappings
mapping_tbl$pl <- tribble(
  ~lang, ~org, ~ascii,
  "pl", "Ą", "A",
  "pl", "ą", "a",
  "pl", "Ć", "C",
  "pl", "ć", "c",
  "pl", "Ę", "E",
  "pl", "ę", "e",
  "pl", "Ł", "L",
  "pl", "ł", "l",
  "pl", "Ń", "N",
  "pl", "ń", "n",
  "pl", "Ó", "O",
  "pl", "ó", "o",
  "pl", "Ś", "S",
  "pl", "ś", "s",
  "pl", "Ź", "Z",
  "pl", "ź", "z",
  "pl", "Ż", "Z",
  "pl", "ż", "z"
)

# Define a tibble with Hungarian character mappings
mapping_tbl$hu <- tribble(
  ~lang, ~org, ~ascii,
  "hu", "Á", "A",
  "hu", "á", "a",
  "hu", "É", "E",
  "hu", "é", "e",
  "hu", "Í", "I",
  "hu", "í", "i",
  "hu", "Ó", "O",
  "hu", "ó", "o",
  "hu", "Ö", "O",
  "hu", "ö", "o",
  "hu", "Ő", "O",
  "hu", "ő", "o",
  "hu", "Ú", "U",
  "hu", "ú", "u",
  "hu", "Ü", "U",
  "hu", "ü", "u",
  "hu", "Ű", "U",
  "hu", "ű", "u"
)

# Define a tibble with Czech character mappings
mapping_tbl$cz <- tribble(
  ~lang, ~org, ~ascii,
  "cz", "Á", "A",
  "cz", "á", "a",
  "cz", "Č", "C",
  "cz", "č", "c",
  "cz", "Ď", "D",
  "cz", "ď", "d",
  "cz", "É", "E",
  "cz", "é", "e",
  "cz", "Ě", "E",
  "cz", "ě", "e",
  "cz", "Í", "I",
  "cz", "í", "i",
  "cz", "Ň", "N",
  "cz", "ň", "n",
  "cz", "Ó", "O",
  "cz", "ó", "o",
  "cz", "Ř", "R",
  "cz", "ř", "r",
  "cz", "Š", "S",
  "cz", "š", "s",
  "cz", "Ť", "T",
  "cz", "ť", "t",
  "cz", "Ú", "U",
  "cz", "ú", "u",
  "cz", "Ů", "U",
  "cz", "ů", "u",
  "cz", "Ý", "Y",
  "cz", "ý", "y",
  "cz", "Ž", "Z",
  "cz", "ž", "z"
)

# Define a tibble with Slovak character mappings
mapping_tbl$sk <- tribble(
  ~lang, ~org, ~ascii,
  "sk", "Á", "A",
  "sk", "á", "a",
  "sk", "Ä", "A",
  "sk", "ä", "a",
  "sk", "Č", "C",
  "sk", "č", "c",
  "sk", "Ď", "D",
  "sk", "ď", "d",
  "sk", "É", "E",
  "sk", "é", "e",
  "sk", "Í", "I",
  "sk", "í", "i",
  "sk", "Ĺ", "L",
  "sk", "ĺ", "l",
  "sk", "Ľ", "L",
  "sk", "ľ", "l",
  "sk", "Ň", "N",
  "sk", "ň", "n",
  "sk", "Ó", "O",
  "sk", "ó", "o",
  "sk", "Ô", "O",
  "sk", "ô", "o",
  "sk", "Ŕ", "R",
  "sk", "ŕ", "r",
  "sk", "Š", "S",
  "sk", "š", "s",
  "sk", "Ť", "T",
  "sk", "ť", "t",
  "sk", "Ú", "U",
  "sk", "ú", "u",
  "sk", "Ý", "Y",
  "sk", "ý", "y",
  "sk", "Ž", "Z",
  "sk", "ž", "z"
)

# Define a tibble with Maltese character mappings
mapping_tbl$mt <- tribble(
  ~lang, ~org, ~ascii,
  "mt", "Ċ", "C",
  "mt", "ċ", "c",
  "mt", "Ġ", "G",
  "mt", "ġ", "g",
  "mt", "Ħ", "H",
  "mt", "ħ", "h"
)

# Define a tibble with Slovenian character mappings
mapping_tbl$si <- tribble(
  ~lang, ~org, ~ascii,
  "si", "Č", "C",
  "si", "č", "c",
  "si", "Š", "S",
  "si", "š", "s",
  "si", "Ž", "Z",
  "si", "ž", "z"
)

# Define a tibble with Romanian character mappings
mapping_tbl$ro <- tribble(
  ~lang, ~org, ~ascii,
  "ro", "Ă", "A",
  "ro", "ă", "a",
  "ro", "Â", "A",
  "ro", "â", "a",
  "ro", "Î", "I",
  "ro", "î", "i",
  "ro", "Ș", "S",
  "ro", "ș", "s",
  "ro", "Ț", "T",
  "ro", "ț", "t"
)

# Define a tibble with Albanian character mappings
mapping_tbl$al <- tribble(
  ~lang, ~org, ~ascii,
  "al", "Ç", "C",
  "al", "ç", "c",
  "al", "Ë", "E",
  "al", "ë", "e"
)

# Define a tibble with Croatian character mappings
mapping_tbl$hr <- tribble(
  ~lang, ~org, ~ascii,
  "hr", "Č", "C",
  "hr", "č", "c",
  "hr", "Ć", "C",
  "hr", "ć", "c",
  "hr", "Đ", "D",
  "hr", "đ", "d",
  "hr", "Š", "S",
  "hr", "š", "s",
  "hr", "Ž", "Z",
  "hr", "ž", "z"
)

mapping_tbl$other <- tribble(
  ~lang, ~org, ~ascii,
  "zz", "Ï", "I",
  "zz", "ï", "i"
)

d.mapping.all <- do.call(bind_rows, mapping_tbl)

d.mapping.summarized <- d.mapping.all |>
  #arrange(lang, ascii, org) |>
  summarise(langs = str_c(lang, collapse=", "),
            .by=c(org, ascii))


d.mapping.dup_report <- d.mapping.summarized |>
  mutate(ascii_m = str_c(ascii, collapse=" | "),
         langs_m = str_c(langs, collapse=" | "),
         z = zeq(1,n()),
         .by=org)

d.mapping <- d.mapping.dup_report |>
  filter(z==1) |>
  select(org,ascii)

d.mapping$org |> dput()
d.mapping$org |>
  stringi::stri_escape_unicode() |>
  dput()

d.mapping$ascii |> dput()
