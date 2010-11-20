{-# LANGUAGE UnicodeSyntax #-}
module Main (
    main

  -- *
  , Verb (..)

  -- * Grammatical categories.
  , Modus (Indikativ, Potentialis, Imperativ, Konditionalis)
  , Tempus (Presens, Imperfekt, Perfekt, Pluskvamperfekt)
  , Mood (Positiv, Negativ)
  , Voice (..)
  , Persona (..)
  , Numerus (Singularis, Pluralis)

  -- * Alphabet.
  , vowels             -- :: [Char]
  , isVowel            -- :: Char -> Bool
  , isRoundVowel       -- :: Char -> Bool
  , isNeutralVowel     -- :: Char -> Bool
  , isBackVowel        -- :: Char -> Bool
  , isFrontVowel       -- :: Char -> Bool
  , consonants         -- :: [Char]
  , isConsonant        -- :: Char -> Bool

  -- * Sounds
  , endsWithShortVowel -- :: String -> Bool
  , endsWithLongVowel  -- :: String -> Bool
  , endsWithDiftong    -- :: String -> Bool
  , endsWithLongSound  -- :: String -> Bool

  , syllabels          -- :: String -> [String]
  ) where

import qualified System.IO.UTF8 as IO

------------------------------------------------------------------------------
data Verb = Verb {
    theWord :: String
  , modus   :: Modus
  , tempus  :: Tempus
  , voice   :: Voice
  , mood    :: Mood
  }

data Modus =
    Indikativ
  | Potentialis
  | Imperativ
  | Konditionalis
  deriving (Eq, Show)

data Tempus = -- / tense
    Presens
  | Imperfekt -- / preteritum
  | Perfekt
  | Pluskvamperfekt
  deriving (Eq, Show)

data Mood =
    Positiv
  | Negativ
  deriving (Eq, Show)

data Voice =
    Active (Persona)
  | Passive
  deriving (Eq, Show)

data Persona = Persona Int Numerus
  deriving (Eq, Show)

data Numerus =
    Singularis
  | Pluralis
  deriving (Eq, Show)

-------------------------------------------------------------------------------
change :: String -> String
change stam =
    let xs = stav stam
        ([a,b]) = takeLast 2 xs
        p1 = case (last a:take 2 b) of
                "lke" -> "lje"
                "hke" -> "hje"
                "rke" -> "rje"
                _     -> ""
        p2 = case [last a, head b] of
                "pp" -> "p"
                "tt" -> "t"
                "kk" -> "k"
                "ht" -> "hd"
                "mp" -> "mm"
                "nt" -> "nn"
                "nk" -> "ng"
                "lt" -> "ll"
                "rt" -> "rr"
                _    -> ""
        p3 = case [head b] of
                "p" -> "v"
                "t" -> "d"
                "k" -> ""
                _   -> ""
        kuk = if p1 /= "" then                  dropLast 1 a ++ p1 ++ drop 2 b
                          else if p2 /= "" then dropLast 1 a ++ p2 ++ drop 1 b
                                           else a            ++ p3 ++ drop 1 b
    in concat (dropLast 2 xs ++ [kuk])

change2 stam
  | endsWithShortVowel stam && isRoundVowel (last stam)
    = stam
  | endsWithLongVowel stam
    = dropLast 1 stam
  | stam `endsWithEither` ["ie", "uo", "yö"]
    = dropLast 2 stam ++ takeLast 1 stam
  | endsWithDiftong stam  && last stam == 'i'
    = dropLast 1 stam
  | endsWithShortVowel stam && last stam == 'e'
    = dropLast 1 stam
  | endsWithShortVowel stam && last stam == 'i'
    = dropLast 1 stam -- (FIXME)
  | otherwise
    = stam

------------------------------------------------------------------------------
-- | Check if consonant gradation is needed.
stadie
    (Verb { theWord = v, modus = modus, voice = voice, mood = mood
          , tempus = tempus}) ending
  = and [
     -- Basic;
      contains "ptk" . take 1 . last $ stv
    , -- A(g);
      length stv > 1
--    , -- A(f);

    ] && or [
      -- A(a);
      length ending == 1 && isConsonant (head ending)
    , -- A(b);
      length ending >= 2 && all isConsonant (take 2 ending)
    , -- B(b);
      modus == Imperativ && voice == Active (Persona 2 Singularis)
    , -- B(c);
      mood == Negativ && tempus == Presens && modus == Indikativ
    ]
  where
    stm = tr $ stam v
    stv = stav stm

tr (a,b,c) = c

sampleVerb s = Verb s Indikativ Presens (Active $ Persona 1 Singularis) Positiv

------------------------------------------------------------------------------

stadieVaxla :: Verb -> String -> String
stadieVaxla w e = (if stadie w e then '+' : change root else root) ++ e
  where (_, _, root) = stam . theWord $ w

w `space` n = w ++ " " ++ n

------------------------------------------------------------------------------
instance Show Verb where
    --  CORNER CASES
    show (Verb {theWord = "olla", voice = Active (Persona 3 num), mood = Positiv})
      | num == Singularis = "on"
      | num == Pluralis   = "ovat"
    -- Neknings verb
    show w@(Verb {theWord = "e", voice = voice})
      | voice == Active (Persona 3 Singularis) = "ei"
      | voice == Active (Persona 3 Pluralis)   = "eivät"
      | otherwise = 'e' : form w
    --   Normal cases
    show w@(Verb {modus = Indikativ, tempus = Presens})
        = let word = theWord w
              (l, a, b) = stam word
              e = form w
          in case (voice w, mood w) of
                (Active p, Positiv) -> stadieVaxla w e ++ e
                (Active p, Negativ) -> show (ei p) ++ " " ++ stadieVaxla w ""
                -- FIXME
                (Passive, Positiv)
                  -> case l of
                       1 -> word ++ if isBackVowel b then "taan" else "tään"
                       _ -> word ++ if isBackVowel word then "an" else "än"
                (Passive, Negativ)
                  -> "hej"
  -- ***********
    show w@(Verb {modus = Konditionalis, tempus = Presens})
        = let word = theWord w
              (l, a, b) = stam word
              e = form w -- FIXME
              Active p = voice w
          in case mood w of
               Positiv -> change2 b ++ "isi" ++ e
               Negativ -> show (ei p) ++ " " ++ change2 b ++ "isi"
  -- ***********
    show w@(Verb {modus = Imperativ, tempus = Presens, voice = Active (Persona 1 Singularis)})
        = "<0>"
    show w@(Verb {modus = Imperativ, tempus = Presens, voice = Active (Persona 2 Singularis)})
        = case mood w of
            Positiv -> stadieVaxla w "n"
            Negativ -> "älä " ++ stadieVaxla w "n"
    show w@(Verb {modus = Imperativ, tempus = Presens, voice = Active p})
        = let word = theWord w
              (l, a, b) = stam word
              ends = case p of
                        Persona 3 Singularis -> ("koon", "köön")
                        Persona 1 Pluralis   -> ("kaamme", "käämme")
                        Persona 2 Pluralis   -> ("kaa", "kää")
                        Persona 3 Pluralis   -> ("koot", "kööt")
          in case mood w of
                Positiv -> a `addEnding` ends
                Negativ -> "äl" `addEnding` ends ++ " "
                           ++ a `addEnding` ("ko", "kö")
  -- ***********
    show w@(Verb {modus = Potentialis, tempus = Presens})
        = let word = theWord w
              (l, a, b) = stam word
              e = form w
              end = last b
              word' = case end of
                't' -> dropLast 1 a ++ "n" ++ "ne"
                v | [v] `contains` "lrs" -> a ++ (v:"e")
                  | otherwise -> a ++ "ne"
          in word' ++ e

ei p = (sampleVerb "e") {voice = Active p}

allVoice = [Active (Persona p n) | n <- [Singularis, Pluralis]
                                 , p <- [1,2,3]]


-- | Add a ending depending on the harmoni of the word.
s `addEnding` (endBack, endFront)
  | isBackVowel s = s ++ endBack
  | otherwise   = s ++ endFront

-----------------------------------------------------------------------------

main = sequence_ [ IO.putStrLn (" *** " ++ s ++ " ***"
                                  ++ "\n(Indikativ; Presens; Aktiv)\n"
                                  ++ "------------------\n"
                                  ++ unlines (ind s Indikativ allVoice)
                                  ++ "\n(Indikativ; Presens; Passiv)\n"
                                  ++ "------------------\n"
                                  ++ unlines (ind s Indikativ [Passive])
                                  ++ "\n(Konditionalis; Presens; Aktiv)\n"
                                  ++ "------------------\n"
                                  ++ unlines (ind s Konditionalis allVoice)
                                  ++ "\n(Konditionalis; Presens; Passiv)\n"
                                  ++ "------------------\n"
                                  ++ ""
                                  ++ "\n(Imperativ; Presens; Aktiv)\n"
                                  ++ "------------------\n"
                                  ++ unlines (ind s Imperativ allVoice)
                                  ++ "\n(Imperativ; Presens; Passiv)\n"
                                  ++ "------------------\n"
                                  ++ ""
                                  ++ "\n(Potentialis; Presens; Aktiv)\n"
                                  ++ "------------------\n"
                                  ++ unlines (ind s Potentialis allVoice)
                                  ++ "\n(Potentialis; Presens; Passiv)\n"
                                  ++ "------------------\n"
                                  ++ ""
                                  ++ "=================================="
                               ) | s <- verbs ]

  where
    ind s m vs = zipWith (\p n -> p ++ " \t " ++ n)
                  [ show $ Verb s m Presens v Positiv | v <- vs]
                  [ show $ Verb s m Presens v Negativ | v <- vs]


verbs = concat [verbType1, verbType2, verbType3, verbType4, verbType5]
verbType1 = [ "maksaa", "säästää", "katsoa", "kysyä", "lukea" , "tietää"
            , "ymmärtää", "lentää", "oppia", "antaa", "puhua", "unohtaa"
            , "elää"]
verbType2 = [ "juoda", "syödä", "saada", "myydä", "voida", "nähdä", "tehdä"]
verbType3 = [ "tulla", "opiskella", "kuulla", "luulla", "hymyillä", "purra"
            , "mennä" , "pestä", "olla", "nousta"]
verbType4 = [ "pelätä", "haluta", "avata"]
verbType5 = [ "tarvita" ]

stam :: String -> (Int, String, String)
stam v
  -- special cases
  | v == "tehdä" = (2, "teh", "teke")
  | v == "nähdä" = (2, "näh", "näke")
  -- normal cases
  | v `endsWithEither` ["ta", "tä"]
    = case charFromEnd 2 v of
        'i' -> (5, dropLast 2 v
                , dropLast 2 v ++ "tse")
        's' -> (3, dropLast 2 v
                , dropLast 2 v ++ "e")
        _   -> (4, dropLast 2 v
                , dropLast 2 v ++ if isBackVowel v then "a" else "ä")
  | v `endsWithEither` ["la", "lä", "na", "nä", "ra"]
    = (3, dropLast 2 v, dropLast 2 v ++ "e")
  | v `endsWithEither` ["da", "dä"]
    = (2, dropLast 2 v, dropLast 2 v)
  | v `endsWithEither` ["a", "ä"]
    = (1, dropLast 1 v, dropLast 1 v)
  | otherwise = error "verb you say?"

form :: Verb -> String
form (Verb {theWord = v, voice = Active p}) = case p of
    Persona 1 Singularis -> "n"
    Persona 2 Singularis -> "t"
    Persona 3 Singularis -> if endsWithLongSound stm then "" else [last stm]
    Persona 1 Pluralis   -> "mme"
    Persona 2 Pluralis   -> "tte"
    Persona 3 Pluralis   -> if isBackVowel stm then "vat" else "vät"
  where
    stm = tr $ stam v

------------------------------------------------------------------------------
-- String management

endsWith, startsWith :: Eq a => [a] -> [a] -> Bool
v `endsWith` s = takeLast (length s) v == s
v `startsWith` s = take (length s) v == s

endsWithEither, startsWithEither :: Eq a => [a] -> [[a]] -> Bool
endsWithEither   = any . endsWith
startsWithEither = any . startsWith

dropLast, takeLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse
takeLast n = reverse . take n . reverse

charFromEnd :: Int -> [a] -> a
charFromEnd n s = reverse s !! n

contains :: Eq a => [a] -> [a] -> Bool
contains xs ys = or [ x == y | x <- xs, y <- ys]

------------------------------------------------------------------------------
-- SOUND
-- Vokalharmoni

isFrontVowel :: String -> Bool
isFrontVowel = contains "äöy"

isBackVowel :: String -> Bool
isBackVowel = contains "aou"

isNeutralVowel :: String -> Bool
isNeutralVowel = contains "ei"

isRoundVowel :: Char -> Bool
isRoundVowel = contains "oöuy" . return

-- | The different vowels.
vowels :: [Char]
vowels = ['a' , 'e' , 'i' , 'o' , 'u' , 'y' , 'ä' , 'ö']

-- | Check if a character is a vowel.
isVowel :: Char -> Bool
isVowel = contains vowels . return

-- | The different consonants.
consonants :: [Char]
consonants = [ 'b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q'
             , 'r', 's', 't', 'v', 'w', 'x', 'z']

-- | Check if a character is a consonant.
isConsonant :: Char -> Bool
isConsonant = contains consonants . return

isDiftong :: String -> Bool
isDiftong = contains diftongs . return
  where diftongs = [ "ei", "äi", "ui", "ai", "oi", "öi", "yi"
                   , "au", "ou", "eu", "iu"
                   , "äy", "öy"
                   , "ie"
                   , "yö"
                   , "uo"]

endsWithLongSound :: String -> Bool
endsWithLongSound s = endsWithDiftong s || endsWithLongVowel s

-- Diphthong
endsWithDiftong :: String -> Bool
endsWithDiftong = isDiftong . takeLast 2

endsWithLongVowel :: String -> Bool
endsWithLongVowel s = let [a, b] = takeLast 2 s
                      in isVowel a && a == b

endsWithShortVowel :: String -> Bool
endsWithShortVowel s = let [a, b] = takeLast 2 s
                       in isVowel b && not (isDiftong [a,b]) && a /= b

syllabels = stav
stav :: String -> [String]
stav s = case stavelser s [] of
    ("" : xs) -> xs
    xs        -> xs

stavelser []  u = [reverse u]
stavelser [x] u
  | isVowel x && length u > 0 && head u /= x && not (isDiftong [head u, x])
    = reverse u : stavelser [] [x]
  | otherwise = stavelser [] (x:u)
stavelser (x:y:s) u
  | isConsonant x && isVowel y
    = endStav : stavelser s [y,x]
  | isVowel x && length u > 0 && head u /= x && not (isDiftong [head u, x])
    = endStav : stavelser (y:s) [x]
  | isVowel x && isVowel y && not (isDiftong [x,y])
    = reverse (x:u) : stavelser s [y]
  | otherwise  = stavelser (y:s) (x:u)
  where
    endStav = reverse u
