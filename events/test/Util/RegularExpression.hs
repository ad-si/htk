-- | RegularExpression is the interface between Expect and whatever
-- regular expression engine it uses.  Currently Text.Regex.
--
-- The current implementation (ghc's RegexString, which in turn
-- calls ghc's Regex, which in turn calls GHC regex version 1.2
-- (source-code included with ghc) uses the following syntax flags:
--
--  (RE_BACKSLASH_ESCAPE_IN_LISTS    | RE_CONTEXT_INDEP_ANCHORS         \
-- | RE_CONTEXT_INDEP_OPS                                               \
-- | RE_INTERVALS                    | RE_NO_BK_BRACES                  \
-- | RE_NO_BK_PARENS                 | RE_NO_BK_VBAR)
--
-- Consequences:
--   "." will NOT match newline or null.
--   "*" preceded by nothing will only match \"\" (but not give an error)
--   "+" means "one or more"
--   "?" means "nought or one"
--   "{M,N}" matches between M and N occurrences.  "{M,}" and "{M}" are
--       also defined.
--   "|" means alternation
--   "\\" quotes the next character in lists.
--   character classes do NOT work.
--   "(" and ")" represent the grouping operators.
--   "\\DIGIT" matches something the same as the regular expressions
--      DIGIT'th group, which should precede where we are now.
--      (DIGIT should be between 1 and 9.)
--   "^" matches the beginning of the string, or after a newline character.
--   "$" matches the end of the string, or before a newline character.
--   "\\b" matches word boundaries.
--   "\B" matches inside a word.
--   "\\<" matches the start of a word.
--   "\\>" matches the end of a word.
--   "\\w" matches any word-constituent character.
--   \"\\W\" is the opposite of \"\\w\".
--   \"\`\" matches the start of the string
--   "\'" ditto, end.
-- Matching is also case-sensitive.
module Util.RegularExpression(
   RegularExpression, -- compiled regular expression
   compile, -- compile regular expression
   MatchResult, -- result of a successful match
   matchString, -- match a regular expression
   getSubStrings, -- substrings in regular expression
   getSubString, -- get one substring.
   getAfter, -- get remaining unmatched string
   getMatched, -- get matched string.
   escapeString -- given a string, return string for regular expression
                -- that matches that string.
   ) where

import Text.Regex

import Util.Dynamics

-- Since the regular expression implementation is now changing for the
-- third time, we provide the following interface.
newtype RegularExpression = RegularExpression Regex
-- compiled regular expression

compile :: String -> RegularExpression
compile str = RegularExpression(mkRegex str)

data MatchResult = MatchResult String String String [String]
   deriving (Show,Typeable)
-- Strings are before,matched portion,after, and the
-- list $1,$2,... corresponding to matched subexpressions.

matchString :: RegularExpression -> String -> Maybe MatchResult
matchString (RegularExpression regEx) str =
   case matchRegexAll regEx str of
      Nothing -> Nothing
      Just (before,matched,after,subStrings) ->
         Just (MatchResult before matched after subStrings)

getSubStrings :: MatchResult -> [String]
getSubStrings (MatchResult _ _ _ subStrings) = subStrings

getSubString :: MatchResult -> Int -> String
getSubString result i = (getSubStrings result) !! i

getAfter :: MatchResult -> String
getAfter (MatchResult _ _ after _ ) = after

getMatched :: MatchResult -> String
getMatched (MatchResult _ matched _  _ ) = matched

escapeString :: String -> String
escapeString [] = []
escapeString (c:rest) =
   if special c
      then '\\':c:(escapeString rest)
      else c:(escapeString rest)
   where
      special c = elem c ".*+?{}|\\[]()^$"





