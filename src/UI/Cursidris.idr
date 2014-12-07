module Cursidris

{- TODO: public functions:

initCurses,     -- :: IO () -> IO ()
resetParams,    -- :: IO ()

stdScr,         -- :: Window
endWin,         -- :: IO ()

keypad,         -- :: Window -> Bool -> IO ()
scrSize,        -- :: IO (Int, Int)
refresh,        -- :: IO ()
getCh,          -- :: IO Char

-- * Line drawing
waddnstr,       -- :: Window -> CString -> CInt -> IO CInt
bkgrndSet,      -- :: Attr -> Pair -> IO ()
clrToEol,       -- :: IO ()
wMove,          -- :: Window -> Int -> Int -> IO ()

-- * Key codes
keyBackspace, keyUp, keyDown, keyNPage, keyHome, keyPPage, keyEnd,
keyLeft, keyRight,

{-# LINE 67 "UI/Nanocurses/Curses.hsc" #-}
keyResize,

{-# LINE 69 "UI/Nanocurses/Curses.hsc" #-}

-- * Cursor
CursorVisibility(..),
cursSet,        -- :: CInt -> IO CInt
                                                                                getYX,          -- :: Window -> IO (Int, Int)

-- * Colours
Pair(..), Color,
initPair,           -- :: Pair -> Color -> Color -> IO ()
color,              -- :: String -> Maybe Color
hasColors,          -- :: IO Bool

-- * Attributes
Attr,
attr0, setBold, setReverse,
attrSet,
attrPlus,           -- :: Attr -> Attr -> Attr

-- * error handling
throwIfErr_,    -- :: Num a => String -> IO a -> IO ()
-}
