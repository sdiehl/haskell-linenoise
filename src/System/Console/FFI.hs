{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Console.FFI (
  getInputLine,
  outputStr,
  outputStrLn,

  addHistory,
  clearScreen,
  stifleHistory,
  historySave,
  historyLoad,
  printKeycodes,
  setMultiline,
  setCompletion,
) where

import Foreign
import Foreign.C.String
import Foreign.C.Types(CInt(..), CChar, CSize)

import Data.String (IsString(..))

outputStr :: String -> IO ()
outputStr = putStr

outputStrLn :: String -> IO ()
outputStrLn = putStrLn

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
  poke = error "no poke"

-- Completion C callback
type CompleteFunc = (CString -> Completion -> IO ())

-- Make a completion function pointer.
makeCompletion :: (String -> IO [String]) -> (CString -> Completion -> IO ())
makeCompletion f = \buf lc -> do
  line <- peekCString buf
  comps <- f line
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

-- | Add to current history.
addHistory :: String -> IO ()
addHistory line = do
  str <- newCString line
  _ <- linenoiseHistoryAdd str
  return ()

-- | Limit the maximum history length.
stifleHistory :: Int -> IO ()
stifleHistory len = do
  _ <- linenoiseHistorySetMaxLen $ fromIntegral len
  return ()

-- | Save history to a file.
historySave :: FilePath -> IO ()
historySave fname = do
  str <- newCString fname
  linenoiseHistorySave str

-- | Load history from a file.
historyLoad :: FilePath -> IO ()
historyLoad fname = do
  str <- newCString fname
  linenoiseHistoryLoad str

-- | Clear the screen
clearScreen :: IO ()
clearScreen = linenoiseClearScreen

-- | Enable/Disable multiline input
setMultiline :: Bool -> IO ()
setMultiline = linenoiseSetMultiLine . fromBool

printKeycodes :: IO ()
printKeycodes = linenoisePrintKeyCodes

-- | Set the current completion function
setCompletion :: (String -> IO [String]) -> IO ()
setCompletion f = do
  cb <- makeFunPtr (makeCompletion f)
  linenoiseSetCompletionCallback cb
