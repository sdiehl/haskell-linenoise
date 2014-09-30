{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Console.Repl (
  getInputLine,
  addHistory,
  clearScreen,
  stifleHistory,
  historySave,
  historyLoad,
  printKeycodes,
  setMultiline,
  setCompletion,

  replIO,

  ReplT(unReplT),
  Repl,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types(CInt(..), CChar, CSize)

import Data.String
import System.Console.Monad

{-
typedef struct linenoiseCompletions {
  size_t len;
  char **cvec;
} linenoiseCompletions;
-}

foreign import ccall "linenoise.h linenoise"
  linenoise :: CString -> IO CString
foreign import ccall "linenoise.h linenoiseHistoryAdd"
  linenoiseHistoryAdd :: Ptr CChar -> IO CInt
foreign import ccall "linenoise.h linenoiseHistorySetMaxLen"
  linenoiseHistorySetMaxLen :: CInt -> IO CInt
foreign import ccall "linenoise.h linenoiseHistorySave"
  linenoiseHistorySave :: CString -> IO ()
foreign import ccall "linenoise.h linenoiseHistoryLoad"
  linenoiseHistoryLoad :: CString -> IO ()
foreign import ccall "linenoise.h linenoiseClearScreen"
  linenoiseClearScreen :: IO ()
foreign import ccall "linenoise.h linenoiseSetMultiLine"
  linenoiseSetMultiLine :: CInt -> IO ()
foreign import ccall "linenoise.h linenoisePrintKeyCodes"
  linenoisePrintKeyCodes :: IO ()
foreign import ccall "linenoise.h linenoiseSetCompletionCallback"
  linenoiseSetCompletionCallback :: FunPtr CompleteFunc -> IO ()
foreign import ccall "linenoise.h linenoiseAddCompletion"
  linenoiseAddCompletion :: Completion -> CString -> IO ()

foreign import ccall "wrapper"
    makeFunPtr :: CompleteFunc -> IO (FunPtr CompleteFunc)

data CompletionType = CompletionType CSize (Ptr (Ptr CChar))
  deriving (Show, Eq)

type Completion = Ptr CompletionType

instance Storable CompletionType where
  sizeOf _ = 8
  alignment = sizeOf
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    return (CompletionType a b)

{-
void completion(const char *buf, linenoiseCompletions *lc) {
    if (buf[0] == 'h') {
        linenoiseAddCompletion(lc,"hello");
        linenoiseAddCompletion(lc,"hello there");
    }
}
-}

type CompleteFunc = (CString -> Completion -> IO ())

-- Make a completion function pointer.
makeCompletion :: (String -> [String]) -> (CString -> Completion -> IO ())
makeCompletion f buf lc = do
  line <- peekCString buf
  let comps = f line
  cstrs <- mapM newCString comps
  mapM_ (linenoiseAddCompletion lc) cstrs

-- Run the prompt, yielding a polymorphic string ( String, Text, ByteString ).
getInputLine :: IsString a => String -> IO (Maybe a)
getInputLine prompt = do
  str <- newCString prompt
  ptr <- linenoise str
  res <- maybePeek peekCString ptr
  free str
  return (fmap fromString res)

-- Add history
addHistory :: String -> IO ()
addHistory line = do
  str <- newCString line
  _ <- linenoiseHistoryAdd str
  return ()

stifleHistory :: Int -> IO ()
stifleHistory len = do
  _ <- linenoiseHistorySetMaxLen $ fromIntegral len
  return ()

historySave :: FilePath -> IO ()
historySave fname = do
  str <- newCString fname
  _ <- linenoiseHistorySave str
  return ()

historyLoad :: FilePath -> IO ()
historyLoad fname = do
  str <- newCString fname
  _ <- linenoiseHistoryLoad str
  return ()

clearScreen :: IO ()
clearScreen = linenoiseClearScreen

setMultiline :: Bool -> IO ()
setMultiline = linenoiseSetMultiLine . fromBool

printKeycodes :: IO ()
printKeycodes = linenoisePrintKeyCodes

setCompletion :: (String -> [String]) -> IO ()
setCompletion f = do
  cb <- makeFunPtr (makeCompletion f)
  linenoiseSetCompletionCallback cb

replIO :: String               -- ^ Prompt
     -> (String -> IO a)       -- ^ Action
     -> (String -> [String])   -- ^ Completion
     -> IO ()
replIO prompt action comp = do
  setCompletion comp
  res <- getInputLine prompt
  case res of
    Nothing   -> return ()
    Just line -> do
      _ <- action line
      addHistory line
      replIO prompt action comp
