-- | We provide a format-string-like way of describing how to call particular
-- tools.  Thus the input is
-- (1) a particular format string
-- (2) a partial map from upper-case letters to strings; we call these strings
--     the _insert_ strings.
-- We map the format string to an output string in which combinations
-- of the form
-- %[upper-case-letter]
-- in the format string are replaced by the corresponding insert string; if no
-- such string exists this is an error.
--
-- We also provide a mechanism for "escaping" the insert strings.
-- Specifically, there is a fixed partial map from lower-case letters to
-- functions :: String -> String; these functions we call the transformers.
-- For a combination of the form
-- %[lower-case-letter-1]...[lower-case-letter-n][upper-case-letter]
-- we take the insert string corresponding to upper-case-letter, and then
-- pass it through the transformers corresponding to lower-case-letter-n,
-- and so on down to the transformer corresponding to lower-case-letter-1.
--
-- Instead of [upper-case-letter] we may also write "%" in which case the
-- insert string is just "%"; thus "%%" transforms to "%".
--
-- Sections of the input string not containing % are left untouched.
--
-- Defined transformers with their corresponding letters:
--    b  transformer suitable for escaping bash strings quoted with ".
--    e  transformer suitable for escaping emacs lisp strings quoted with ".
-- None of these transformers insert the closing or end quotes, allowing you
-- to use them in the middle of strings.
--
-- Other transformers will be added as the need arises.
module Util.CommandStringSub(
   CompiledFormatString,
      -- This represents a format string in which all the transformers and
      -- escapes (apart from escaped upper-case letters) have been parsed.

   -- compileFormatString and runFormatString split the computation into
   -- two stages so we can save a bit of time if the same format string is
   -- used more than once.
   compileFormatString,
      -- :: String -> WithError CompiledFormatString
   runFormatString,
      -- :: CompiledFormatString -> (Char -> Maybe String) -> WithError String

   -- doFormatString does everything at once, throwing an error if necessary.
   doFormatString,
      -- :: String -> (Char -> Maybe String) -> String


   -- Some transformers we export for use as simple Haskell functions
   -- NB - these do not delimit the input strings.
   emacsEscape, -- :: String -> String
   bashEscape, -- :: String -> String
   ) where

import Data.Char

import Util.Computation

-- --------------------------------------------------------------------------
-- The datatypes
-- --------------------------------------------------------------------------

data FormatItem =
      Unescaped String
   |  Escaped (String -> String) Char

newtype CompiledFormatString = CompiledFormatString [FormatItem]

-- --------------------------------------------------------------------------
-- compileFormatString
-- --------------------------------------------------------------------------

compileFormatString :: String -> WithError CompiledFormatString
compileFormatString str =
   case splitToDollar str of
      Nothing -> hasValue (prependLiteral str (CompiledFormatString []))
      Just (s1,s2) ->
         mapWithError'
            (\ (ch,transformer,withError) ->
               mapWithError
                  (\ (CompiledFormatString l) ->
                     prependLiteral s1
                        (CompiledFormatString ((Escaped transformer ch):l))
                     )
                  withError
               )
            (compileFromEscape s2)

-- | Return portion up to (not including) first %, and portion after it.
splitToDollar :: String -> Maybe (String,String)
splitToDollar "" = Nothing
splitToDollar ('%':rest) = Just ("",rest)
splitToDollar (c:rest) = fmap (\ (s1,s2) -> (c:s1,s2)) (splitToDollar rest)

prependLiteral :: String -> CompiledFormatString -> CompiledFormatString
prependLiteral "" compiledFormatString = compiledFormatString
prependLiteral s (CompiledFormatString l) =
   CompiledFormatString (Unescaped s:l)

compileFromEscape :: String
   -> WithError (Char,String -> String,WithError CompiledFormatString)
compileFromEscape "" = hasError "Format string ends unexpectedly"
compileFromEscape (c:rest) =
   if isUpper c || c == '%' then hasValue (c,id,compileFormatString rest)
   else case c of
      'e' -> mapEscapeFunction emacsEscape rest
      'b' -> mapEscapeFunction bashEscape rest
      _ ->
         let
            compiledRest = compileFormatString rest
            e = error "Attempt to run bad format string"
            restFaked = hasValue (e,e,compiledRest)
            message = if isLower c then
               "Transformer character " ++ [c] ++ " not recognised."
               else "Unexpected character "++ show c ++ " in format string."
         in
            mapWithError snd (pairWithError (hasError message) restFaked)

mapEscapeFunction :: (String -> String) -> String ->
   WithError (Char,String -> String,WithError CompiledFormatString)
mapEscapeFunction escapeFunction s =
   mapWithError
      (\ (ch,transformer,rest) -> (ch,escapeFunction . transformer,rest))
      (compileFromEscape s)

-- --------------------------------------------------------------------------
-- The escape functions
-- --------------------------------------------------------------------------

mkEscapeFunction :: (Char -> String) -> (String -> String)
mkEscapeFunction chEscape str =
   concat (map chEscape str)

bashEscape :: String -> String
bashEscape = mkEscapeFunction chbashEscape

chbashEscape :: Char -> String
chbashEscape ch =
   case ch of
      '\\' -> "\\\\"
      '\"' -> "\\\""
      '$' -> "\\$"
      '`' -> "\\`"
      _ -> [ch]

emacsEscape :: String -> String
emacsEscape = mkEscapeFunction chEmacsEscape

chEmacsEscape :: Char -> String
chEmacsEscape ch =
   case ch of
      '\n' -> "\\n"
      '\t' -> "\\t"
      '\r' -> "\\r"
      '\f' -> "\\f"
      '\b' -> "\\b"
      '\\' -> "\\\\"
      '\"' -> "\\\""
      ch -> if isPrint ch then [ch] else "\\"++to3Oct ch
   where
      -- Converts to octal representation padded to 3 digits.
      to3Oct :: Char -> String
      to3Oct ch =
         let
            chOct = toOctal ch
         in
            case chOct of
               "" -> "000"
               [_] -> "00"++chOct
               [_,_] -> "0" ++ chOct
               [_,_,_] -> chOct
               _ -> error
                  "Character with enormous character code can't be emacs-escaped"


-- | Converts character to representation.
toOctal :: Char -> String
toOctal ch =
   -- We can't use Numeric.showOpt because GHC5.02.1 doesn't
   -- implement it!!!
   let
      toOct :: Int -> String
      toOct i =
         let
            (q,r) = divMod i 8
            e = [intToDigit r]
         in
            if q==0 then e else toOct q++e
   in
      toOct (ord ch)

-- --------------------------------------------------------------------------
-- runFormatString
-- --------------------------------------------------------------------------

runFormatString :: CompiledFormatString -> (Char -> Maybe String)
   -> WithError String
runFormatString (CompiledFormatString l) lookup =
   let
      withErrors =
         map
           (\ formatItem -> case formatItem of
              Unescaped str -> hasValue str
              Escaped transformer '%' -> hasValue "%"
              Escaped transformer ch -> case lookup ch of
                 Nothing -> hasError ("%"++[ch]++" not defined")
                 Just str -> hasValue (transformer str)
              )
           l
      appendWithError we1 we2 = mapWithError (uncurry (++))
         (pairWithError we1 we2)
   in
      foldr appendWithError (hasValue "") withErrors


-- --------------------------------------------------------------------------
-- doFormatString
-- --------------------------------------------------------------------------

  -- doFormatString does everything at once, throwing an error if necessary.
doFormatString :: String -> (Char -> Maybe String) -> String
doFormatString format lookup =
   let
      we1 = compileFormatString format
      we2 = mapWithError'
         (\ compiled -> runFormatString compiled lookup)
         we1
   in
      coerceWithError we2
