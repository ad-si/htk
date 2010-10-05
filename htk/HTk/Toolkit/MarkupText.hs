{-# LANGUAGE ExistentialQuantification #-}

-- | A simple markup language for convenient writing into an editor widget.
module HTk.Toolkit.MarkupText (

-- type
  MarkupText,

-- combinators
  prose,
  font,
  newline,
  bold,
  underline,
  italics,
  spaces,
  offset,
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
  centered,
  flushright,
  flushleft,
  href,
  window,
  window1,

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
  pi,
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
  aalpha,
  bbeta,
  cchi,
  ddelta,
  eeps,
  pphi,
  ggamma,
  eeta,
  iiota,
  kkappa,
  llambda,
  mmu,
  nnu,
  oomikron,
  ppi,
  ttheta,
  rrho,
  ssigma,
  ttau,
  uupsilon,
  oomega,
  xxi,
  ppsi,
  zzeta,
  forallsmall,
  exists,
  forallbig,
  eexists,
  existsone,
  not,
  and,
  bigand,
  or,
  times,
  sum,
  prod,
  comp,
  bullet,
  tensor,
  otimes,
  oplus,
  bot,
  rightarrow,
  rrightarrow,
  longrightarrow,
  llongrightarrow,
  leftrightarrow,
  lleftrightarrow,
  ddownarrow,
  uuparrow,
  vline,
  hline,
  rbrace1,
  rbrace2,
  rbrace3,
  emptyset,
  inset,
  notin,
  intersect,
  union,
  subset,
  subseteq,
  setminus,
  powerset,
  inf,
  iintersect,
  uunion,
  equiv,
  neq,
  leq,
  grteq,
  lsem,
  rsem,
  dots,
  copyright,


-- container class for markup texts
  HasMarkupText(..),


  scrollMarkupText,
) where

import Data.Char
import Prelude hiding (pi, not, and, or, sum)
import qualified Prelude (not)

import System.IO.Unsafe

import Util.Object
import Util.Computation

import Events.Channels
import Events.Events

import Reactor.ReferenceVariables

import HTk.Toplevel.HTk hiding (font, underline, offset)
import HTk.Kernel.GUIObject
import qualified HTk.Kernel.Configuration as Configuration (font)
import qualified HTk.Textitems.TextTag as TextTag (offset)
import HTk.Kernel.Font

-- -----------------------------------------------------------------------
-- state
-- -----------------------------------------------------------------------

unbinds :: Ref [(ObjectID, [IO ()])]
unbinds = unsafePerformIO (newRef [])
{-# NOINLINE unbinds #-}

addToState :: Editor -> [IO ()] -> IO ()
addToState ed acts =
  do
    let GUIOBJECT oid _ = toGUIObject ed
    ub <- getRef unbinds
    setRef unbinds ((oid, acts) : ub)


-- -----------------------------------------------------------------------
-- types
-- -----------------------------------------------------------------------

-- | The @MarkupText@ datatype.
data MarkupText =
    MarkupText [MarkupText]
  | MarkupProse [String]
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
  | MarkupJustify Justify [MarkupText]
  | MarkupAction (IO ()) [MarkupText]
  | MarkupClipUp [MarkupText] [MarkupText]
  | MarkupRangeAction (Maybe (IO ())) (Maybe (IO ())) [MarkupText]
  | MarkupLeftMargin Int [MarkupText]
  | MarkupWrapMargin Int [MarkupText]
  | MarkupRightMargin Int [MarkupText]
  | MarkupHRef [MarkupText] [MarkupText]
  | forall w . Widget w => MarkupWindow (Editor -> IO (w, IO()))

type TagFun = Editor -> BaseIndex -> BaseIndex -> IO TextTag

type Tag = (Position, Position, TagFun)

type EmbWindowFun =
  Editor -> BaseIndex -> IO EmbeddedTextWin

type EmbWindow = (Position, EmbWindowFun)


-- ----------------------------------------------------------------------
-- combinators
-- -----------------------------------------------------------------------

-- | The markup prose combinator.
prose :: String -> MarkupText
prose str = MarkupProse (lines str)

-- | The markup font combinator.
font :: FontDesignator f => f -> [MarkupText] -> MarkupText
font f = MarkupFont (toFont f)

-- | The markup newline combinator.
newline :: MarkupText
newline = MarkupNewline

-- | The markup bold combinator.
bold :: [MarkupText] -> MarkupText
bold = MarkupBold

-- | The markup underline combinator.
underline :: [MarkupText] -> MarkupText
underline = MarkupUnderline

-- | Center this part of the text
centered :: [MarkupText]-> MarkupText
centered = MarkupJustify JustCenter

-- | Flush this part of the against the left margin
flushleft :: [MarkupText]-> MarkupText
flushleft = MarkupJustify JustLeft

----
-- Flush this part of the against the right margin
flushright :: [MarkupText]-> MarkupText
flushright = MarkupJustify JustRight

-- | The markup italics combinator.
italics :: [MarkupText] -> MarkupText
italics = MarkupItalics

-- | The markup baseline offset combinator.
offset :: Int-> [MarkupText]-> MarkupText
offset = MarkupOffset

-- | The markup foreground colour combinator.
colour :: ColourDesignator c => c -> [MarkupText] -> MarkupText
colour c = MarkupColour (toColour c)

-- | The markup background colour combinator.
bgcolour :: ColourDesignator c => c -> [MarkupText] -> MarkupText
bgcolour c = MarkupBgColour (toColour c)

-- | The markup space combinator (a number of space characters).
spaces :: Int -> MarkupText
spaces n = MarkupProse [replicate n ' ']

-- | The markup flipcolour combinator (flips the colour when the mouse
-- is over this text segment).
flipcolour :: ColourDesignator c => c -> c -> [MarkupText] -> MarkupText
flipcolour c1 c2 = MarkupFlipColour (toColour c1) (toColour c2)

-- | The markup flipunderline combinator (underlines this text segment when
-- the mouse is over this segment).
flipunderline :: [MarkupText] -> MarkupText
flipunderline = MarkupFlipUnderline

-- | The markup action combinator (binds an action for mouse clicks on this
-- text segment).
action :: IO () -> [MarkupText] -> MarkupText
action = MarkupAction

-- | The markup range action combinator (binds actions for entering and\/or
-- leaving this text segment with the mouse cursor).
rangeaction :: Maybe (IO ()) -> Maybe (IO ()) -> [MarkupText] ->
               MarkupText
rangeaction = MarkupRangeAction

-- | The markup clipup combinator (clips up a text segment on a mouse
-- click).
clipup :: [MarkupText] -> [MarkupText] -> MarkupText
clipup = MarkupClipUp

-- | The markup left margin combinator (normal left intend for a line).
leftmargin :: Int -> [MarkupText] -> MarkupText
leftmargin = MarkupLeftMargin

-- | The markup wrap margin combinator (intend for a part of a line
-- that gets wrapped).
wrapmargin :: Int -> [MarkupText] -> MarkupText
wrapmargin = MarkupWrapMargin

-- | The markup right margin combinator.
rightmargin :: Int -> [MarkupText] -> MarkupText
rightmargin = MarkupRightMargin


-- | The markup window combinator (a widget container inside the editor
-- widget).
window1 :: Widget w => (Editor -> IO (w, IO())) -> MarkupText
window1 = MarkupWindow

window :: Widget w => IO (w, IO()) -> MarkupText
window act = window1 (const act)

-- | The markup href combinator (a link to another markup text).
href :: [MarkupText] -> [MarkupText] -> MarkupText
href = MarkupHRef


-- -----------------------------------------------------------------------
-- special characters
-- -----------------------------------------------------------------------

-- grk letters, lowercase

-- | Special character.
alpha :: MarkupText
alpha = symbchr 97

-- | Special character.
beta :: MarkupText
beta = symbchr 98

-- | Special character.
chi ::MarkupText
chi = symbchr 99

-- | Special character.
delta :: MarkupText
delta = symbchr 100

-- | Special character.
epsilon :: MarkupText
epsilon = symbchr 101

-- | Special character.
phi :: MarkupText
phi = symbchr 102

-- | Special character.
gamma :: MarkupText
gamma = symbchr 103

-- | Special character.
eta :: MarkupText
eta = symbchr 104

-- | Special character.
varphi :: MarkupText
varphi = symbchr 106

-- | Special character.
iota :: MarkupText
iota = symbchr 105

-- | Special character.
kappa :: MarkupText
kappa = symbchr 107

-- | Special character.
lambda :: MarkupText
lambda = symbchr 108

-- | Special character.
mu :: MarkupText
mu = symbchr 109

-- | Special character.
nu :: MarkupText
nu = symbchr 110

-- | Special character.
omikron :: MarkupText
omikron = symbchr 111

-- | Special character.
pi :: MarkupText
pi = symbchr 112

-- | Special character.
theta :: MarkupText
theta = symbchr 113

-- | Special character.
vartheta :: MarkupText
vartheta = symbchr 74

-- | Special character.
rho :: MarkupText
rho = symbchr 114

-- | Special character.
sigma :: MarkupText
sigma = symbchr 115

-- | Special character.
varsigma :: MarkupText
varsigma = symbchr 86

-- | Special character.
tau :: MarkupText
tau = symbchr 116

-- | Special character.
upsilon :: MarkupText
upsilon = symbchr 117

-- | Special character.
varpi :: MarkupText
varpi = symbchr 118

-- | Special character.
omega :: MarkupText
omega = symbchr 119

-- | Special character.
xi :: MarkupText
xi = symbchr 120

-- | Special character.
psi :: MarkupText
psi = symbchr 121

-- | Special character.
zeta :: MarkupText
zeta = symbchr 122



-- grk letters, uppercase

-- | Special character (uppercase).
aalpha :: MarkupText
aalpha = symbchr 65

-- | Special character (uppercase).
bbeta :: MarkupText
bbeta = symbchr 66

-- | Special character (uppercase).
cchi :: MarkupText
cchi = symbchr 67

-- | Special character (uppercase).
ddelta :: MarkupText
ddelta = symbchr 68

-- | Special character (uppercase).
eeps :: MarkupText
eeps = symbchr 69

-- | Special character (uppercase).
pphi :: MarkupText
pphi = symbchr 70

-- | Special character (uppercase).
ggamma :: MarkupText
ggamma = symbchr 71

-- | Special character (uppercase).
eeta :: MarkupText
eeta = symbchr 72

-- | Special character (uppercase).
iiota :: MarkupText
iiota = symbchr 73

-- | Special character (uppercase).
kkappa :: MarkupText
kkappa = symbchr 75

-- | Special character (uppercase).
llambda :: MarkupText
llambda = symbchr 76

-- | Special character (uppercase).
mmu :: MarkupText
mmu = symbchr 77

-- | Special character (uppercase).
nnu :: MarkupText
nnu = symbchr 78

-- | Special character (uppercase).
oomikron :: MarkupText
oomikron = symbchr 79

-- | Special character (uppercase).
ppi :: MarkupText
ppi = symbchr 80

-- | Special character (uppercase).
ttheta :: MarkupText
ttheta = symbchr 81

-- | Special character (uppercase).
rrho :: MarkupText
rrho = symbchr 82

-- | Special character (uppercase).
ssigma :: MarkupText
ssigma = symbchr 83

-- | Special character (uppercase).
ttau :: MarkupText
ttau = symbchr 84

-- | Special character (uppercase).
uupsilon :: MarkupText
uupsilon = symbchr 85

-- | Special character (uppercase).
oomega :: MarkupText
oomega = symbchr 87

-- | Special character (uppercase).
xxi :: MarkupText
xxi = symbchr 88

-- | Special character (uppercase).
ppsi :: MarkupText
ppsi = symbchr 89

-- | Special character (uppercase).
zzeta :: MarkupText
zzeta = symbchr 90



-- quantifiers and junctors

-- | Special character.
forallsmall :: MarkupText
forallsmall = symbchr 34

-- | Special character.
exists :: MarkupText
exists = symbchr 36

-- | Special character.
forallbig :: MarkupText
forallbig = bigsymbchr 34

-- | Special character.
eexists :: MarkupText
eexists = bigsymbchr 36

-- | Special character.
existsone :: MarkupText
existsone = symbstr [36, 33]

-- | Special character.
not :: MarkupText
not = symbchr 216

-- | Special character.
and :: MarkupText
and = symbchr 217

-- | Special character.
bigand :: MarkupText
bigand = bigsymbchr 217

-- | Special character.
or :: MarkupText
or = symbchr 218


-- other operations

-- | Special character.
times :: MarkupText
times = symbchr 180

-- | Special character.
sum :: MarkupText
sum = symbchr 229

-- | Special character.
prod :: MarkupText
prod = symbchr 213

-- | Special character.
comp :: MarkupText
comp = symbchr 183

-- | Special character.
bullet :: MarkupText
bullet = symbchr 183

-- | Special character.
tensor :: MarkupText
tensor = symbchr 196

-- | Special character.
otimes :: MarkupText
otimes = symbchr 196

-- | Special character.
oplus :: MarkupText
oplus = symbchr 197

-- | Special character.
bot :: MarkupText
bot = symbchr 94


-- arrows

-- | Special character.
rightarrow :: MarkupText
rightarrow = symbchr 174

-- | Special character.
rrightarrow :: MarkupText
rrightarrow = symbchr 222

-- | Special character.
longrightarrow :: MarkupText
longrightarrow = symbstr [190, 174]

-- | Special character.
llongrightarrow :: MarkupText
llongrightarrow = symbstr [61, 222]

-- | Special character.
leftrightarrow :: MarkupText
leftrightarrow = symbchr 171

-- | Special character.
lleftrightarrow :: MarkupText
lleftrightarrow = symbchr 219

-- | Special character.
ddownarrow :: MarkupText
ddownarrow = symbchr 223

-- | Special character.
uuparrow :: MarkupText
uuparrow = symbchr 221

-- | Special character.
vline :: MarkupText
vline = symbchr 189

-- | Special character.
hline :: MarkupText
hline = symbchr 190

-- | Special character.
rbrace1 :: MarkupText
rbrace1 = symbchr 236

-- | Special character.
rbrace2 :: MarkupText
rbrace2 = symbchr 237

-- | Special character.
rbrace3 :: MarkupText
rbrace3 = symbchr 238


-- set operations

-- | Special character.
emptyset :: MarkupText
emptyset = symbchr 198

-- | Special character.
inset :: MarkupText
inset = symbchr 206

-- | Special character.
notin :: MarkupText
notin = symbchr 207

-- | Special character.
intersect :: MarkupText
intersect = symbchr 199

-- | Special character.
union :: MarkupText
union = symbchr 200

-- | Special character.
subset :: MarkupText
subset = symbchr 204

-- | Special character.
subseteq :: MarkupText
subseteq = symbchr 205

-- | Special character.
setminus :: MarkupText
setminus = symbchr 164

-- | Special character.
powerset :: MarkupText
powerset = symbchr 195

-- | Special character.
inf :: MarkupText
inf = symbchr 165

-- | Special character.
iintersect :: MarkupText
iintersect = bigsymbchr 199

-- | Special character.
uunion :: MarkupText
uunion = bigsymbchr 200


-- relations

-- | Special character.
equiv :: MarkupText
equiv = symbchr 186

-- | Special character.
neq :: MarkupText
neq = symbchr 185

-- | Special character.
leq :: MarkupText
leq = symbchr 163

-- | Special character.
grteq :: MarkupText
grteq = symbchr 179

-- | Special character.
lsem :: MarkupText
lsem = symbstr [91, 91]

-- | Special character.
rsem :: MarkupText
rsem = symbstr [93, 93]


-- misc other symbols

-- | Special character.
dots :: MarkupText
dots = symbchr 188

-- | Special character.
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
  in case xf of
       XFontAlias _ -> f
       _ ->
         case (bold, italics) of
           (True, True) -> toFont xf {weight = Just Bold,
                                      slant = Just Italic}
           (True, False) -> toFont xf {weight = Just Bold}
           (False, True) -> toFont xf {slant = Just Italic}
           _ -> f

clipact :: Editor -> Mark -> Mark -> Ref Bool -> Ref [TextTag] ->
           String -> [Tag] -> IO ()
clipact ed mark1 mark2 open settags txt tags =
  do
    b <- getRef open
    setRef open (Prelude.not b)
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
  where insertTags :: [Tag] -> IO [TextTag]
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
                      Font -> [Config TextTag] ->
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

        MarkupProse [str] -> parseMarkupText' ms
                                      (txt ++ str) tags wins
                                      (line, char + Distance (length str))
                                      bold italics current_font
        MarkupProse (l:rest) -> parseMarkupText' (MarkupProse rest:ms)
                                      (txt++ l++ "\n") tags wins
                                      (line+ 1, 0)
                                      bold italics current_font
        MarkupProse [] -> parseMarkupText' ms txt tags wins
                                      (line, char) bold italics current_font

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

        MarkupJustify j m' ->
          simpleProperty ms m' txt tags wins (line, char)
                         bold italics current_font [justify j]

        MarkupFont f m' ->
          do
            ((txt', wins', tags'), (line', char')) <-
              parseMarkupText' m' txt tags wins (line, char) bold italics
                               f

            let (Font fstr) = f

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
                           _ <- spawnEvent listenTag
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
                           _ <- spawnEvent listenTag
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
                             bindSimple tag (ButtonPress (Just 1))
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (click >> always act >> listenTag)
                                 +> receive death
                           _ <- spawnEvent listenTag
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
                           _ <- spawnEvent listenTag
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
                             bindSimple tag (ButtonPress (Just 1))
                           open <- newRef False
                           settags <- newRef []
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (click >>
                                     always (clipact ed mark1 mark2 open
                                               settags txt' tags') >>
                                    listenTag)
                                 +> receive death
                           _ <- spawnEvent listenTag
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
                             bindSimple tag (ButtonPress (Just 1))
                           death <- newChannel
                           let listenTag :: Event ()
                               listenTag =
                                    (click >>
                                     always (ed # clear >>
                                             ed # new linktext) >>
                                     listenTag)
                                 +> receive death
                           _ <- spawnEvent listenTag
                           addToState ed [u_click,
                                          syncNoWait (send death ())]
                           return tag)
            parseMarkupText' ms txt' (tag : tags') wins' (line', char')
                             bold
                             italics current_font

        MarkupWindow iowid ->
          let win = ((line, char),
                     \ed pos -> do
                                  (wid, cleanup) <- iowid ed
                                  w <- createEmbeddedTextWin ed pos wid []
                                  addToState ed [cleanup]
                                  return w)
          in parseMarkupText' ms txt tags (win : wins) (line, char)
                              bold italics current_font

    parseMarkupText' _ txt tags wins (line, char) _ _ _ =
      return ((txt, wins, tags), (line, char))


-- -----------------------------------------------------------------------
-- class HasMarkupText
-- -----------------------------------------------------------------------

-- | Widgets that can contain markup text instantiate the
-- @class HasMarkupText@.
class HasMarkupText w where
  -- Clears the editor widget and inserts the given markup text.
  new :: [MarkupText] -> w -> IO w
  -- Inserts the given markup text at the specified position.
  insertAt :: [MarkupText] -> Position -> Config w
  -- Clears the editor widget.
  clear :: Config w

-- | An editor widget is a container for markup text.
instance HasMarkupText Editor where
  -- Clears the editor widget and inserts the given markup text.
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
  -- Inserts the given markup text at the specified position.
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
  -- Clears the editor widget.
  clear ed =
    do
      let obj@(GUIOBJECT oid _) = toGUIObject ed
      unbinds' <- getRef unbinds
      mapM (\ (oid', ubs) -> if oid == oid' then
                               (mapM (\ act -> act) ubs) >> done
                             else done)
           unbinds'
      setRef unbinds []
      return ed

fromDistance :: Distance -> Int
fromDistance (Distance i) = i

-- -----------------------------------------------------------------------
-- A utility for putting a scroll-bar around MarkupText.
-- -----------------------------------------------------------------------

scrollMarkupText :: Size -> [MarkupText] -> MarkupText
scrollMarkupText size1 markups =
   let
      action :: Editor -> IO (Frame,IO ())
      action editor =
         do
            editorFrame <- newFrame editor []
            editorFrame2 <- newFrame editorFrame []

            editor <- newEditor editorFrame2 [wrap NoWrap,disable,new markups,size size1]
            scrollBar1 <- newScrollBar editorFrame2 [orient Vertical]
            scrollBar2 <- newScrollBar editorFrame [orient Horizontal]

            editor # scrollbar Vertical scrollBar1
            editor # scrollbar Horizontal scrollBar2

            pack editor [Side AtRight,Fill Both]
            pack scrollBar1 [Side AtRight,Fill Y,Expand On]
            pack editorFrame2 [Side AtTop]
            pack scrollBar2 [Side AtTop,Fill X]
            return (editorFrame,destroy editorFrame)

   in
      window1 action
