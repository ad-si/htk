{-# This module encapsulates the basic CVS functionality we need,
    calling CVSBasic to issue the commands, looking at the output,
    and returning what we need to know in a Haskell-friendly form.
    #-}
module CVSHigh(
   CVSReturn(..), 
      -- The best value to receive is CVSSuccess, which indicates
      -- that CVS returned with no complaints, and without producing
      -- unexpected output.
   CVSLoc,
      -- CVSLoc points to the repository and the client's working directory,
      -- plus any other information that stays the same between CVS comamnds.
   newCVSLoc, -- :: String -> String -> IO CVSLoc
      -- First string is repository; second working directory.
   cvsAdd, -- :: CVSLoc -> CVSFile -> IO CVSReturn
      -- cvsAdd adds a new directory or file to the repository.
      -- It needs to be done before a commit operation.   
   cvsUpdate, -- :: CVSLoc -> CVSFile -> CVSVersion -> IO CVSReturn
      -- cvsUpdate retrieves a particular version of a file from the
      -- repository to the client's working directory.
   cvsCommit, 
      -- :: CVSLoc -> CVSFile -> (Maybe CVSVersion) -> 
      --   IO (Maybe CVSVersion,CVSReturn)
      -- cvsCommit creates a new version of a file (first argument)
      -- in the repository, taking it from the client's working directory.
      -- The second argument, if given, is a parent version of which this
      -- is a revision.
      -- In the event of a successful return, the String is the version
      -- assigned to the new version.
   cvsListVersions
      -- :: CVSLoc -> CVSFile -> IO (Maybe [CVSVersion],CVSReturn)
      -- If successful, returns all versions known so far of the file 
      -- argument.
   ) where

import System

import Posix
import Exception

import Maybes
import Dynamics

import RegularExpression
import Expect
import SIM

import CVSBasic

--------------------------------------------------------------
-- CVSReturn and CVSLoc types- 
--------------------------------------------------------------

data CVSReturn = 
      CVSSuccess 
   |  CVSProblem (Maybe String) ToolStatus

-- This is how we make CVSReturn values
toCVSReturn :: (Maybe String) -> ToolStatus -> CVSReturn
toCVSReturn Nothing (Just (Exited ExitSuccess)) = CVSSuccess
toCVSReturn message toolStatus = CVSProblem message toolStatus
   
instance Show CVSReturn where
   showsPrec 0 value acc =
      case value of
         CVSSuccess -> acc
         CVSProblem message toolStatus ->
            let
               acc2 =
                  case toolStatus of
                     Just(Exited ExitSuccess) -> acc
                     Just other ->
                        (show other)++"\n"++acc
                     Nothing -> "CVS ToolStatus is Nothing\n"++acc
               acc3 =
                  case message of
                     Nothing -> acc2
                     Just something -> something++"\n"++acc2
            in 
               acc3

newtype CVSLoc = CVSLoc GlobalOptions

newCVSLoc :: String -> String -> IO CVSLoc
newCVSLoc cvsRoot workingDir =
   return(CVSLoc(GlobalOptionsSimple
      {workingDir = workingDir,cvsRoot=workingDir}))

--------------------------------------------------------------
--- Structuring Expect events.
--------------------------------------------------------------
-- A key and new part of this CVS encapsulation is the novel
-- means of wrapping up Expect events.  Key features:
-- (1) We use (for the first time) the fact that IA events
--     are an instance of Monad, to write them using do expressions.
-- (2) To indicate errors we throw a Dynamic exception
--     encoding the error as a String.

--------------------------------------------------------------
-- Errors and Error Handling
-- The cvsError function throws errors; the tryCVS function
-- detects them and wraps them up.
-------------------------------------------------------------

newtype CVSError = CVSError String
instance Typeable CVSError where
   typeOf _ = tag_CVSError

tag_CVSError :: TypeTag
tag_CVSError = mkTypeTag (mkTyCon "CVSHigh" "CVSError") []

cvsError :: String -> IO a
cvsError mess = throw(DynException(toDyn(CVSError mess)))

-- tryCVS is wrapped around the entire processing of a CVS process
-- represented by Expect and returns the result or CVS error as
-- appropriate.
tryCVS :: Expect -> IA a -> IO (Maybe a,CVSReturn)
tryCVS exp event =
   do
      result <- tryIO isCVSError (sync event)
      status <- getToolStatus exp
      case result of
         Left errorMess -> 
            return (Nothing,toCVSReturn (Just errorMess) status)
         Right value ->
            return (Just value,toCVSReturn Nothing status)
   where
      isCVSError :: Exception -> Maybe String
      isCVSError =
         (\ dynamic ->
            fmap (\ (CVSError mess) -> mess) (fromDyn dynamic)
            )
         `compMaybe`
         justDynExceptions


--------------------------------------------------------------
-- Expect priorities
-------------------------------------------------------------

-- high is used for correct parses
high :: String -> Pattern 
high ptn = toPattern (ptn,1::Int)

-- low is used for errors
low :: String -> Pattern
low ptn = toPattern (ptn,0::Int)

--------------------------------------------------------------
-- Events which if matched cause fatal errors.
-- As all these cause fatal errors, they also kill the Expect instance.
-------------------------------------------------------------

-- This matches any line with 0 priority and raises an error
noLineHere :: Expect -> IA a
noLineHere exp =    
   (matchLine exp >>>=
      (\ line -> 
         do
            destroy exp
            cvsError("Couldn't parse: "++(show line))
         )
      )

-- This matches EOF and raises an error
noEOFHere :: Expect -> IA a
noEOFHere exp =
   (matchEOF exp >>>
      do
         destroy exp
         cvsError "Unexpected EOF"
      )

-- A common idiom to guard against both unparsed lines and EOF
guard :: Expect -> IA a -> IA a
guard exp event =
   event +> (noLineHere exp) +> (noEOFHere exp)

-- This returns harmlessly if EOF and raises an error otherwise
mustEOFHere :: Expect -> IA ()
mustEOFHere exp = (matchEOF exp) +> (noLineHere exp)

cvsAdd :: CVSLoc -> CVSFile -> IO CVSReturn
cvsAdd (CVSLoc globalOptions) file =
   do
      exp <- callCVS globalOptions
         (Add{file=file})
      
      (_,result) <- tryCVS exp
         (do
            guard exp ((match exp fileAdded) +> (match exp directoryAdded))
            mustEOFHere exp
            )
      return result
   where
      fileAdded = high
         "\\`cvs server: use 'cvs commit' to add this file permanently\\'"
      directoryAdded = high
         "\\`Directory .* added to the repository\\'"

cvsCommit :: CVSLoc -> CVSFile -> (Maybe CVSVersion) -> 
      IO (Maybe CVSVersion,CVSReturn)
cvsCommit (CVSLoc globalOptions) file maybeVersion =
   do
      exp <- callCVS globalOptions 
         (CommitSimple {revision'=maybeVersion,files=[file]})
      let 
         mat ptn = match exp (high ptn)
{- Typical output from cvs commit:
   (1) for the first commit of a file:
RCS file: /repository/unitest/1/3,v
done
Checking in 1/3;
/repository/unitest/1/3,v  <--  3
initial revision: 1.1
done
   -}
         event1 :: IA CVSVersion =
            do
               mat "\\`RCS file: "
               guard exp (mat "\\`done\\'")
               guard exp (mat "\\`Checking in .*;\\'")
               guard exp (mat "")
               revision <- 
                  guard exp (mat "\\`initial revision: (.*)\\'" >>>=
                     (\ matcher ->
                        return (getSubString matcher 0)
                        )
                     )
               guard exp (mat "\\`done\\'")
               mustEOFHere exp
               return (CVSVersion revision)         
{-
   (2) for a commit of a revision of a file:
Checking in 1/1;
/repository/unitest/1/1,v  <--  1
new revision: 1.2; previous revision: 1.1
done
   -}
         event2 :: IA CVSVersion = 
            do
               mat "\\`Checking in .*;\\'"
               guard exp (mat "")
               revision <- 
                  guard exp (mat "\\`new revision: (.*); previous revision'" >>>=
                     (\ matcher ->
                        return (head (getSubStrings matcher))
                        )
                     )
               guard exp (mat "\\`done\\'")
               mustEOFHere exp
               return (CVSVersion revision)

      -- (back to main "do" in cvsCommit function)
      tryCVS exp (guard exp (event1 +> event2))

cvsUpdate :: CVSLoc -> CVSFile -> CVSVersion -> IO CVSReturn
cvsUpdate (CVSLoc globalOptions) file version =
   do
      exp <- callCVS globalOptions 
         (UpdateSimple {revision=version,files=[file]})
      -- The output from cvsUpdate should either be
      -- nothing (if the file is already in the directory in this version)
      --   or
      -- P (filename)
      (_,result) <- tryCVS exp(
            (do
               match exp (high "\\`P ")
               mustEOFHere exp
               ) 
         +> mustEOFHere exp
         )

      return result

cvsListVersions :: CVSLoc -> CVSFile -> IO (Maybe [CVSVersion],CVSReturn)
      -- If successful, returns all versions known so far of the file 
      -- argument.
cvsListVersions (CVSLoc globalOptions) file =
   do
      exp <- callCVS globalOptions 
         (LogSimple {file=file})

      let
         mat ptn = match exp (high ptn)

         preamble :: IA () =
            -- skip everything until we get to a line beginning "total revisions"
               (do
                  mat "\\`total revisions "
                  guard exp (mat "\\`description\\'")
                  )
            +> (do
                  matchLine exp
                  preamble
                  )
            +> noEOFHere exp
       
         entry :: IA CVSVersion =
            (do
               guard exp (mat "\\`----------------------------\\'")
               revision <- 
                  guard exp (mat "\\`revision (.*)\\'" >>>=
                     \ matcher -> return(getSubString matcher 1) 
                     )
               guard exp (mat "\\`date")
               guard exp (mat "\\`X\\'")
               return (CVSVersion revision)
               )

         postamble :: IA () =
            do
               mat "\\`=============================================================================\\'"
               mustEOFHere exp

         body :: [CVSVersion] -> IA [CVSVersion]
         body acc =
               (do
                  postamble
                  return (reverse acc)
                  )
            +> (do
                  this <- entry
                  body (this:acc)
                  )

         logOutput :: IA [CVSVersion]
         logOutput =
            do
               preamble
               body []

      tryCVS exp logOutput
      



