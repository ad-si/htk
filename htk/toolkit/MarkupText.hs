-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module MarkupText (

-- type
  MarkupText,

-- combinators
  prose,
  MarkupText.font,
  newline,
  bold,
  MarkupText.underline,
  italics,
  spaces,
  MarkupText.offset,
  colour,
  bgcolour,
  flipcolour,
  flipunderline,
  action,
  rangeaction,
  clipup,
  leftmargin,
  wrapmargin,
  rightmargin,
  href,
  window,

-- special characters
  alpha,
  beta,
  chi,
  delta,
  epsilon,
  phi,
  gamma,
  eta,
  varphi,
  iota,
  kappa,
  lambda,
  mu,
  nu,
  omikron,
  MarkupText.pi,
  theta,
  vartheta,
  rho,
  sigma,
  varsigma,
  tau,
  upsilon,
  varpi,
  omega,
  xi,
  psi,
  zeta,
  _Alpha,
  _Beta,
  _Chi,
  _Delta,
  _Eps,
  _Phi,
  _Gamma,
  _Eta,
  _Iota,
  _Kappa,
  _Lambda,
  _Mu,
  _Nu,
  _Omikron,
  _Pi,
  _Theta,
  _Rho,
  _Sigma,
  _Tau,
  _Upsilon,
  _Omega,
  _Xi,
  _Psi,
  _Zeta,
  forall,
  exists,
  _Forall,
  _Exists,
  existsone,
  MarkupText.not,
  MarkupText.and,
  MarkupText.or,
  times,
  MarkupText.sum,
  prod,
  comp,
  bullet,
  tensor,
  otimes,
  oplus,
  bot,
  rightarrow,
  _Rightarrow,
  longrightarrow,
  _Longrightarrow,
  leftrightarrow,
  _Leftrightarrow,
  _Downarrow,
  _Uparrow,
  vline,
  hline,
  rbrace1,
  rbrace2,
  rbrace3,
  emptyset,
  _in,
  notin,
  intersect,
  union,
  subset,
  subseteq,
  setminus,
  powerset,
  inf,
  _Intersect,
  _Union,
  equiv,
  neq,
  leq,
  grteq,
  lsem,
  rsem,
  dots,
  copyright,


-- container class for markup texts
  HasMarkupText(..)

) where

import HTk
import GUIObject
import Configuration
import Editor
import TextTag
import Font
import Editor
import Object
import ReferenceVariables
import PrelBase
import Char
import IOExts(unsafePerformIO)
import Object
import Wish


-- -----------------------------------------------------------------------
-- state
-- -----------------------------------------------------------------------

unbinds :: Ref [(ObjectID, [IO ()])]
unbinds = unsafePerformIO (newRef [])

addToState :: Editor String -> [IO ()] -> IO ()
addToState ed acts =
  do
    let GUIOBJECT oid _ = toGUIObject ed
    ub <- getRef unbinds
    setRef unbinds ((oid, acts) : ub)


-- -----------------------------------------------------------------------
-- types
-- -----------------------------------------------------------------------

---
-- The <code>MarkupText</code> datatype.
data MarkupText =
    MarkupText [MarkupText]
  | MarkupProse String
  | MarkupSpecialChar Font Int
  | MarkupFont Font [MarkupText]
  | MarkupNewline
  | MarkupBold [MarkupText]
  | MarkupItalics [MarkupText]
  | MarkupOffset Int [MarkupText]
  | MarkupColour Colour [MarkupText]
  | MarkupBgColour Colour [MarkupText]
  | MarkupFlipColour Colour Colour [MarkupText]
  | MarkupFlipUnderline [MarkupText]
  | MarkupUnderline [MarkupText]
  | MarkupAction (IO ()) [MarkupText]
  | MarkupClipUp [MarkupText] [MarkupText]
  | MarkupRangeAction (Maybe (IO ())) (Maybe (IO ())) [MarkupText]
  | MarkupLeftMargin Int [MarkupText]
  | MarkupWrapMargin Int [MarkupText]
  | MarkupRightMargin Int [MarkupText]
  | MarkupHRef [MarkupText] [MarkupText]
  | forall w . Widget w => MarkupWindow w

type TagFun =
  Editor String -> BaseIndex -> BaseIndex -> IO (TextTag String)

type Tag = (Position, Position, TagFun)

type EmbWindowFun =
  Editor String -> BaseIndex -> IO EmbeddedTextWin

type EmbWindow = (Position, EmbWindowFun)


-- ----------------------------------------------------------------------
-- combinators
-- -----------------------------------------------------------------------

prose :: String -> MarkupText
prose str = MarkupProse str

font :: FontDesignator f => f -> [MarkupText] -> MarkupText
font f = MarkupFont (toFont f)

newline :: MarkupText
newline = MarkupNewline

bold :: [MarkupText] -> MarkupText
bold = MarkupBold

underline :: [MarkupText] -> MarkupText
underline = MarkupUnderline

italics :: [MarkupText] -> MarkupText
italics = MarkupItalics

offset :: Int-> [MarkupText]-> MarkupText
offset = MarkupOffset

colour :: ColourDesignator c => c -> [MarkupText] -> MarkupText
colour c = MarkupColour (toColour c)

bgcolour :: ColourDesignator c => c -> [MarkupText] -> MarkupText
bgcolour c = MarkupBgColour (toColour c)

spaces :: Int -> MarkupText
spaces n = MarkupProse (replicate n ' ')

flipcolour :: ColourDesignator c => c -> c -> [MarkupText] -> MarkupText
flipcolour c1 c2 = MarkupFlipColour (toColour c1) (toColour c2)

flipunderline :: [MarkupText] -> MarkupText
flipunderline = MarkupFlipUnderline

action :: IO () -> [MarkupText] -> MarkupText
action = MarkupAction

rangeaction :: Maybe (IO ()) -> Maybe (IO ()) -> [MarkupText] ->
               MarkupText
rangeaction = MarkupRangeAction

clipup :: [MarkupText] -> [MarkupText] -> MarkupText
clipup = MarkupClipUp

leftmargin :: Int -> [MarkupText] -> MarkupText
leftmargin = MarkupLeftMargin

wrapmargin :: Int -> [MarkupText] -> MarkupText
wrapmargin = MarkupWrapMargin

rightmargin :: Int -> [MarkupText] -> MarkupText
rightmargin = MarkupRightMargin

window :: Widget w => w -> MarkupText
window = MarkupWindow

href :: [MarkupText] -> [MarkupText] -> MarkupText
href = MarkupHRef


-- -----------------------------------------------------------------------
-- special characters
-- -----------------------------------------------------------------------

-- grk letters, lowercase
alpha :: MarkupText
alpha = symbchr 97

beta :: MarkupText
beta = symbchr 98

chi ::MarkupText
chi = symbchr 99

delta :: MarkupText
delta = symbchr 100

epsilon :: MarkupText
epsilon = symbchr 101

phi :: MarkupText
phi = symbchr 102

gamma :: MarkupText
gamma = symbchr 103

eta :: MarkupText
eta = symbchr 104

varphi :: MarkupText
varphi = symbchr 106

iota :: MarkupText
iota = symbchr 105

kappa :: MarkupText
kappa = symbchr 107

lambda :: MarkupText
lambda = symbchr 108

mu :: MarkupText
mu = symbchr 109

nu :: MarkupText
nu = symbchr 110

omikron :: MarkupText
omikron = symbchr 111 

pi :: MarkupText
pi = symbchr 112

theta :: MarkupText
theta = symbchr 113 

vartheta :: MarkupText
vartheta = symbchr 74 

rho :: MarkupText
rho = symbchr 114

sigma :: MarkupText
sigma = symbchr 115

varsigma :: MarkupText
varsigma = symbchr 86

tau :: MarkupText
tau = symbchr 116

upsilon :: MarkupText
upsilon = symbchr 117

varpi :: MarkupText
varpi = symbchr 118

omega :: MarkupText
omega = symbchr 119

xi :: MarkupText
xi = symbchr 120

psi :: MarkupText
psi = symbchr 121

zeta :: MarkupText
zeta = symbchr 122


-- grk letters, uppercase
_Alpha :: MarkupText
_Alpha = symbchr 65

_Beta :: MarkupText
_Beta = symbchr 66

_Chi :: MarkupText
_Chi = symbchr 67

_Delta :: MarkupText
_Delta = symbchr 68

_Eps :: MarkupText
_Eps = symbchr 69

_Phi :: MarkupText
_Phi = symbchr 70

_Gamma :: MarkupText
_Gamma = symbchr 71 

_Eta :: MarkupText
_Eta = symbchr 72

_Iota :: MarkupText
_Iota = symbchr 73 

_Kappa :: MarkupText
_Kappa = symbchr 75 

_Lambda :: MarkupText
_Lambda = symbchr 76

_Mu :: MarkupText
_Mu = symbchr 77

_Nu :: MarkupText
_Nu = symbchr 78

_Omikron :: MarkupText
_Omikron = symbchr 79

_Pi :: MarkupText
_Pi = symbchr 80

_Theta :: MarkupText
_Theta = symbchr 81

_Rho :: MarkupText
_Rho = symbchr 82

_Sigma :: MarkupText
_Sigma = symbchr 83

_Tau :: MarkupText
_Tau = symbchr 84

_Upsilon :: MarkupText
_Upsilon = symbchr 85 

_Omega :: MarkupText
_Omega = symbchr 87

_Xi :: MarkupText
_Xi = symbchr 88

_Psi :: MarkupText
_Psi = symbchr 89

_Zeta :: MarkupText
_Zeta = symbchr 90


-- quantifiers and junctors
forall :: MarkupText
forall = symbchr 34

exists :: MarkupText
exists = symbchr 36

_Forall :: MarkupText
_Forall = bigsymbchr 34

_Exists :: MarkupText
_Exists = bigsymbchr 36

existsone :: MarkupText
existsone = symbstr [36, 33]

not :: MarkupText
not = symbchr 216

and :: MarkupText
and = symbchr 217

or :: MarkupText
or = symbchr 218


-- other operations
times :: MarkupText
times = symbchr 180

sum :: MarkupText
sum = symbchr 229

prod :: MarkupText
prod = symbchr 213

comp :: MarkupText
comp = symbchr 183

bullet :: MarkupText
bullet = symbchr 183

tensor :: MarkupText
tensor = symbchr 196

otimes :: MarkupText
otimes = symbchr 196

oplus :: MarkupText
oplus = symbchr 197

bot :: MarkupText
bot = symbchr 94


-- arrows
rightarrow :: MarkupText
rightarrow = symbchr 174

_Rightarrow :: MarkupText
_Rightarrow = symbchr 222

longrightarrow :: MarkupText
longrightarrow = symbstr [190, 174]

_Longrightarrow :: MarkupText
_Longrightarrow = symbstr [61, 222]

leftrightarrow :: MarkupText
leftrightarrow = symbchr 171

_Leftrightarrow :: MarkupText
_Leftrightarrow = symbchr 219

_Downarrow :: MarkupText
_Downarrow = symbchr 223

_Uparrow :: MarkupText
_Uparrow = symbchr 221

vline :: MarkupText
vline = symbchr 189

hline :: MarkupText
hline = symbchr 190

rbrace1 :: MarkupText
rbrace1 = symbchr 236

rbrace2 :: MarkupText
rbrace2 = symbchr 237

rbrace3 :: MarkupText
rbrace3 = symbchr 238


-- set operations
emptyset :: MarkupText
emptyset = symbchr 198

_in :: MarkupText
_in = symbchr 206

notin :: MarkupText
notin = symbchr 207

intersect :: MarkupText
intersect = symbchr 199

union :: MarkupText
union = symbchr 200

subset :: MarkupText
subset = symbchr 204

subseteq :: MarkupText
subseteq = symbchr 205

setminus :: MarkupText
setminus = symbchr 164

powerset :: MarkupText
powerset = symbchr 195

inf :: MarkupText
inf = symbchr 165

_Intersect :: MarkupText
_Intersect = bigsymbchr 199

_Union :: MarkupText
_Union = bigsymbchr 200

-- relations
equiv :: MarkupText
equiv = symbchr 186

neq :: MarkupText
neq = symbchr 185

leq :: MarkupText
leq = symbchr 163

grteq :: MarkupText
grteq = symbchr 179

lsem :: MarkupText
lsem = symbstr [91, 91]

rsem :: MarkupText
rsem = symbstr [93, 93]


-- misc other symbols
dots :: MarkupText
dots = symbchr 188

copyright :: MarkupText
copyright = symbchr 227

-- aux
symbchr :: Int -> MarkupText
symbchr i = MarkupSpecialChar
              (Font "-*-symbol-medium-r-normal-*-14-*-*-*-*-*-*-*") i

bigsymbchr :: Int -> MarkupText
bigsymbchr i = MarkupSpecialChar
                 (Font "-*-symbol-medium-r-normal-*-18-*-*-*-*-*-*-*") i

symbstr :: [Int] -> MarkupText
symbstr is = MarkupText (map symbchr is)


-- -----------------------------------------------------------------------
-- parse markup text structures
-- -----------------------------------------------------------------------

checkfont :: Font -> Bool -> Bool -> Font
checkfont f@(Font str) bold italics =
  let xf = read str
  in case (bold, italics) of
       (True, True) -> toFont xf {weight = Just Bold, slant = Just Italic}
       (True, False) -> toFont xf {weight = Just Bold}
       (False, True) -> toFont xf {slant = Just Italic}
       _ -> f

clipact :: Editor String -> Mark String -> Mark String -> Ref Bool ->
           Ref [TextTag String] -> String -> [Tag] -> IO ()
clipact ed mark1 mark2 open settags txt tags =
  do
    b <- getRef open
    setRef open (PrelBase.not b)
    (if b then
       do
         tags' <- getRef settags
         st <- getState ed
         if st == Disabled then ed # state Normal >> done else done
         mapM destroy tags'
         deleteTextRange ed mark1 mark2
         ed # state st -- restore state
         done
     else
       do
         st <- getState ed
         if st == Disabled then ed # state Normal >> done else done
         insertText ed mark1 txt
         tags' <- insertTags tags
         ed # state st -- restore state
         setRef settags tags')
  where insertTags :: [Tag] -> IO [TextTag String]
        insertTags (((l1,c1), (l2,c2), f) : ts) =
          do
            pos1 <- getBaseIndex ed
                      (mark1, [ForwardLines (fromDistance l1),
                               ForwardChars (fromDistance c1)])
            pos2 <- getBaseIndex ed
                      (mark1, [ForwardLines (fromDistance l2),
                               ForwardChars (fromDistance c2)])
            tag <- f ed pos1 pos2
            tags <- insertTags ts
            return (tag : tags)
        insertTags _ = return []

parseMarkupText :: [MarkupText] -> Font -> IO (String, [EmbWindow], [Tag])
parseMarkupText m f =
  do
    (ret, _) <- parseMarkupText' m [] [] [] (1,0) False False f
    return ret
  where
    simpleProperty :: [MarkupText] -> [MarkupText] -> String ->
                      [Tag] -> [EmbWindow] -> Position -> Bool -> Bool ->
                      Font -> [Config (TextTag String)] ->
                      IO ((String, [EmbWindow], [Tag]), Position)
    simpleProperty ms m' txt tags wins (line, char) bold italics
                   current_font cnf =
      do
        ((txt', wins', tags'), (line', char')) <-
          parseMarkupText' m' txt tags wins (line, char) bold italics
                           current_font
        let tag = ((line, char), (line', char'),
                   \ed pos1 pos2 ->
                     createTextTag ed pos1 pos2 cnf)
        parseMarkupText' ms txt' (tag : tags') wins' (line', char') bold
                         italics current_font

    parseMarkupText' :: [MarkupText] -> String -> [Tag] -> [EmbWindow] -> 
                        Position -> Bool -> Bool -> Font ->
                        IO ((String, [EmbWindow], [Tag]), Position)
    parseMarkupText' (m : ms) txt tags wins (line, char) bold italics
                     current_font =
      case m of

        MarkupText m' -> parseMarkupText' (m' ++ ms) txt tags wins
                                          (line, char) bold italics
                                          current_font

        MarkupProse str -> parseMarkupText' ms (txt ++ str) tags wins
                             (line, char + Distance (length str)) bold
                             italics current_font

        MarkupSpecialChar f i ->
          parseMarkupText' (MarkupFont f [prose [chr i]] : ms) txt tags
                           wins (line, char) bold italics current_font

        MarkupNewline -> parseMarkupText' ms (txt ++ "\n") tags wins
                                          (line + 1, 0) bold italics
                                          current_font

        MarkupColour c m' -> simpleProperty ms m' txt tags wins
                               (line, char) bold italics current_font
                               [fg c]

        MarkupOffset i m' -> simpleProperty ms m' txt tags wins
                               (line, char) bold italics current_font
                               [TextTag.offset (Distance i)]

        MarkupBgColour c m' -> simpleProperty ms m' txt tags wins
                                 (line, char) bold italics current_font
                                 [bg c]

        MarkupLeftMargin i m' ->
          simpleProperty ms m' txt tags wins (line, char)
                         bold italics current_font [lmargin1 (Distance i)]

        MarkupWrapMargin i m' ->
          simpleProperty ms m' txt tags wins (line, char)
                         bold italics current_font [lmargin2 (Distance i)]

        MarkupRightMargin i m' ->
          simpleProperty ms m' txt tags wins (line, char)
                         bold italics current_font [rmargin (Distance i)]

        MarkupUnderline m' ->
          simpleProperty ms m' txt tags wins (line, char)
                         bold italics current_font [underlined On]

        MarkupFont f m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold italics
                               f

            let (Font fstr) = f
--            putStrLn fstr

            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         createTextTag ed pos1 pos2
                           [Configuration.font
                              (checkfont f bold italics)])
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold italics current_font

        MarkupBold m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) True italics
                               current_font

            let (Font fstr) = current_font
--            putStrLn fstr

            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         createTextTag ed pos1 pos2
                           [Configuration.font
                              (checkfont current_font True italics)])
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold italics current_font

        MarkupItalics m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold True
                               current_font

            let (Font fstr) = current_font
--            putStrLn fstr

            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         createTextTag ed pos1 pos2
                           [Configuration.font
                              (checkfont current_font bold True)])
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold italics current_font

        MarkupFlipColour c1 c2 m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold italics
                               current_font
            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         do
                           tag <- createTextTag ed pos1 pos2 []
                           tag # fg c1
                           (entered, u_entered) <- bindSimple tag Enter
                           (left, u_left) <- bindSimple tag Leave
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (entered >>
                                       (always (tag # fg c2) >>
                                        listenTag))
                                 +> (left >>
                                       (always (tag # fg c1) >>
                                        listenTag))
                                 +> receive death
                           spawnEvent listenTag
                           addToState ed [u_entered, u_left,
                                          syncNoWait(send death ())]
                           return tag)
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold italics current_font

        MarkupFlipUnderline m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold italics
                               current_font
            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         do
                           tag <- createTextTag ed pos1 pos2 []
                           (entered, u_entered) <- bindSimple tag Enter
                           (left, u_left) <- bindSimple tag Leave
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (entered >>
                                       (always (tag # underlined On) >>
                                        listenTag))
                                 +> (left >>
                                       (always (tag # underlined Off) >>
                                        listenTag))
                                 +> receive death
                           spawnEvent listenTag
                           addToState ed [u_entered, u_left,
                                          syncNoWait (send death ())]
                           return tag)
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold italics current_font

        MarkupAction act m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold italics
                               current_font
            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         do
                           tag <- createTextTag ed pos1 pos2 []
                           (click, u_click) <-
                             bindSimple tag (ButtonPress (Just (BNo 1)))
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (click >> always act >> listenTag)
                                 +> receive death
                           spawnEvent listenTag
                           addToState ed [u_click,
                                          syncNoWait (send death ())]
                           return tag)
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold italics current_font

        MarkupRangeAction menteract mleaveact m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold italics
                               current_font
            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         do
                           tag <- createTextTag ed pos1 pos2 []
                           (enter, enter_u) <- bindSimple tag Enter
                           (leave, leave_u) <- bindSimple tag Leave
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                 (enter >> always (case menteract of
                                                     Just act -> act
                                                     Nothing -> done) >>
                                  listenTag) +>
                                 (leave >> always (case mleaveact of
                                                     Just act -> act
                                                     Nothing -> done) >>
                                  listenTag) +>
                                 receive death
                           spawnEvent listenTag
                           addToState ed [enter_u, leave_u,
                                          syncNoWait (send death ())]
                           return tag)
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold italics current_font

        MarkupClipUp m' cliptext ->
          do
            let pos = (if char > 0 then line + 1 else line, 0)
                s = if char > 0 then "\n" else ""
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' (s ++ txt) tags wins pos bold italics
                               current_font
            let tag = (pos, (line', char'),
                       \ed pos1 pos2 ->
                         do
                           ((txt', wins', tags'), (line', char')) <-
                             parseMarkupText' (cliptext ++ [newline]) ""
                                              [] [] (0, 0) bold italics f
                           oid1 <- newObject
                           mark1 <- createMark ed ("m" ++ show oid1)
                                               (pos1, [ForwardLines 1])
                           setMarkGravity mark1 ToLeft
                           oid2 <- newObject
                           mark2 <- createMark ed ("m" ++ show oid2)
                                               (pos1, [ForwardLines 1])
                           tag <- createTextTag ed pos1 pos2 []
                           (click, u_click) <-
                             bindSimple tag (ButtonPress (Just (BNo 1)))
                           open <- newRef False
                           settags <- newRef []
-- windows erzeugen!
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (click >>
                                     always (clipact ed mark1 mark2 open
                                               settags txt' tags') >>
                                    listenTag)
                                 +> receive death
                           spawnEvent listenTag
                           addToState ed [u_click,
                                          syncNoWait (send death ())]
                           return tag)
            parseMarkupText' ms (txt' ++ "\n") (tag : tags') wins'
                             (line' + 1, 0) bold italics current_font

        MarkupHRef m' linktext ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold italics
                               current_font
            let tag = ((line, char), (line', char'),
                       \ed pos1 pos2 ->
                         do
                           tag <- createTextTag ed pos1 pos2 []
                           (click, u_click) <-
                             bindSimple tag (ButtonPress (Just (BNo 1)))
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (click >>
                                     always (ed # clear >>
                                             ed # new linktext) >>
                                     listenTag)
                                 +> receive death
                           spawnEvent listenTag
                           addToState ed [u_click,
                                          syncNoWait (send death ())]
                           return tag)
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold
                             italics current_font

        MarkupWindow wid ->
          let win = ((line, char),
                     \ed pos -> do
                                  w <- createEmbeddedTextWin ed pos wid []
                                  addToState ed [destroy w]
                                  return w)
          in parseMarkupText' ms txt tags (win : wins) (line, char)
                              bold italics current_font

    parseMarkupText' _ txt tags wins (line, char) _ _ _ =
      return ((txt, wins, tags), (line, char))


-- -----------------------------------------------------------------------
-- class HasMarkupText
-- -----------------------------------------------------------------------

class HasMarkupText w where
  new :: [MarkupText] -> w -> IO w
  insertAt :: [MarkupText] -> Position -> Config w
  clear :: Config w

instance HasMarkupText (Editor String) where
  new m ed =
    do
      st <- getState ed
      if st == Disabled then ed # state Normal >> done else done
      f <- getFont ed
      (txt, wins, tags) <- parseMarkupText m f
      ed # value txt
      mapM (\ (pos1, pos2, f) -> do
                                   pos1' <- getBaseIndex ed pos1
                                   pos2' <- getBaseIndex ed pos2
                                   f ed pos1' pos2')
           tags
      mapM (\ (pos, f) -> do
                            pos' <- getBaseIndex ed pos
                            ew <- f ed pos'
                            addToState ed [destroy ew])
           wins
      ed # state st -- restore state
      return ed

  insertAt m pos@(line, char) ed =
    do
      f <- getFont ed
      (txt, wins, tags) <- parseMarkupText m f
      l <- getTextLine ed pos
      st <- getState ed
      if st == Disabled then ed # state Normal >> done else done
      insertText ed pos (replicate (fromDistance char - length l) ' ' ++
                         txt)
      let tags' = shiftTags pos tags
      mapM (\ (pos1, pos2, f) -> do
                                   pos1' <- getBaseIndex ed pos1
                                   pos2' <- getBaseIndex ed pos2
                                   f ed pos1' pos2')
           tags'
      ed # state st -- restore state
      return ed
    where
      shiftTags :: Position -> [Tag] -> [Tag]
      shiftTags p tags = map (shiftTag p) tags

      shiftTag :: Position -> Tag -> Tag
      shiftTag (line, char) (p1@(line1, char1), p2@(line2, char2), tag) =
        ((shiftLine line line1, shiftChar char p1),
         (shiftLine line line2, shiftChar char p2), tag)

      shiftLine :: Distance -> Distance -> Distance
      shiftLine pline line = pline + (line - 1)

      shiftChar :: Distance -> Position -> Distance
      shiftChar pchar (line, char) =
        if line == 1 then char + pchar else char

  clear ed =
    do
      let obj@(GUIOBJECT oid _) = toGUIObject ed
      unbinds' <- getRef unbinds
      mapM (\ (oid', ubs) -> if oid == oid' then
                               (mapM (\ act -> act) ubs) >> done
                             else done)
           unbinds'
      deleteTextRange ed (IndexPos (1,0)) EndOfText
      nm <- getObjectName obj
      execCmd (show nm ++ " tag delete [" ++ show nm ++ " tag names]")
      return ed

fromDistance :: Distance -> Int
fromDistance (Distance i) = i
