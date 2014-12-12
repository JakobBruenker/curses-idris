module Cursidris

%include C "ncurses.h"
%include C "cursesrun.h"
%link C "cursesrun.o"
%lib C "ncurses"

%access private

data Window = MkWindow Ptr

||| Use this for setGetChMode
public
data GetChMode : Type where
  WaitForever           : GetChMode
  WaitForeverRaw        : GetChMode
  Wait                  : (k : Nat) -> GetChMode
  WaitForeverLinebuf    : GetChMode
  WaitForeverLinebufRaw : GetChMode

cBool : Bool -> Int
cBool True  = 1
cBool False = 0

idrBool : Int -> Bool
idrBool = (/= 0)

||| The standard screen
stdScr : IO Window
stdScr = map MkWindow $ mkForeign (FFun "stdScr" [] FPtr)

||| > The nodelay option causes getch to be a non-blocking call.
||| > If  no input is ready, getch returns ERR.  If disabled (bf
||| > is FALSE), getch waits until a key is pressed.
abstract
noDelay : Bool -> IO ()
noDelay bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "nodelay" [FPtr, FInt] FUnit) scr (cBool bf)

abstract
halfDelay : Int -> IO ()
halfDelay delay = mkForeign (FFun "halfdelay" [FInt] FUnit) delay

||| Enables or disables the reading of function keys, arrow keys, and so
||| on.
||| @bf  if `True`, enables reading, if `False`, disables it
abstract
keypad : (bf : Bool) -> IO ()
keypad bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "keypad" [FPtr, FInt] FUnit) scr (cBool bf)

meta : Bool -> IO ()
meta bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "meta" [FPtr, FInt] FUnit) scr (cBool bf)

||| > Normally, the hardware cursor is left at the  location  of
||| > the  window  cursor  being  refreshed.  The leaveok option
||| > allows the cursor to be left wherever the  update  happens
||| > to leave it.  It is useful for applications where the cur-
||| > sor is not used, since it  reduces  the  need  for  cursor
||| > motions.   If  possible, the cursor is made invisible when
||| > this option is enabled.
leaveOk : Bool -> IO Int
leaveOk bf = do
  (MkWindow scr) <- stdScr
  mkForeign (FFun "leaveok" [FPtr, FInt] FInt) scr (cBool bf)

||| > The  nl  and  nonl routines control whether the underlying
||| > display device translates the return key into  newline  on
||| > input,  and  whether it translates newline into return and
||| > line-feed on output (in either case, the call  addch('\n')
||| > does the equivalent of return and line feed on the virtual
||| > screen).  Initially, these translations do occur.  If  you
||| > disable  them using nonl, curses will be able to make bet-
||| > ter use of the line-feed capability, resulting  in  faster
||| > cursor  motion.   Also, curses will then be able to detect
||| > the return key.
nl : Bool -> IO ()
nl True  = mkForeign (FFun "nl"   [] FUnit)
nl False = mkForeign (FFun "nonl" [] FUnit)

||| > The  echo  and  noecho routines control whether characters
||| > typed by the user are echoed by getch as they  are  typed.
||| > Echoing  by  the  tty  driver is always disabled, but ini-
||| > tially getch is in echo  mode,  so  characters  typed  are
||| > echoed.  Authors of most interactive programs prefer to do
||| > their own echoing in a controlled area of the  screen,  or
||| > not  to  echo  at  all, so they disable echoing by calling
||| > noecho.  [See curs_getch(3) for a discussion of how  these
||| > routines interact with cbreak and nocbreak.]
abstract
echo : Bool -> IO ()
echo False = mkForeign (FFun "noecho" [] FUnit)
echo True  = mkForeign (FFun "echo"   [] FUnit)

||| > The cbreak routine
||| > disables line buffering and erase/kill  character-process-
||| > ing  (interrupt  and  flow  control  characters  are unaf-
||| > fected), making characters typed by the  user  immediately
||| > available  to  the  program.  The nocbreak routine returns
||| > the terminal to normal (cooked) mode.
abstract
cBreak : Bool -> IO ()
cBreak True  = mkForeign (FFun "cbreak"   [] FUnit)
cBreak False = mkForeign (FFun "nocbreak" [] FUnit)

raw : Bool -> IO ()
raw True  = mkForeign (FFun "raw"   [] FUnit)
raw False = mkForeign (FFun "noraw" [] FUnit)

||| A bunch of settings we need
abstract
resetParams : IO ()
resetParams = do
  cBreak True
  echo False
  nl True
  leaveOk True
  meta True
  keypad True
  noDelay False
  return ()

||| The use_default_colors() and assume_default_colors() func-
|||  tions are extensions to the curses library.  They are used
|||  with terminals that support ISO 6429 color, or equivalent.
|||
||| use_default_colors() tells the  curses library  to  assign terminal
||| default foreground/background colors to color number  -1.
useDefaultColors : IO ()
useDefaultColors = return ()

||| Initialise the color settings, also sets the screen on the default
||| colors (white on black)
startColor : IO ()
startColor = mkForeign (FFun "start_color" [] FUnit)

hasColors : IO Bool
hasColors = map idrBool (mkForeign (FFun "has_colors" [] FInt))

initScr : IO Window
initScr = map MkWindow $ mkForeign (FFun "initscr" [] FPtr)

-- ||| Start it all up
-- abstract
-- initCurses : IO ()
-- initCurses = do
--   initScr
--   b <- hasColors
--   when b $ startColor $> useDefaultColors
--   resetParams

||| > The program must call endwin for each terminal being used before
||| > exiting from curses.
abstract
endWin : IO ()
endWin = mkForeign (FFun "endwin" [] FUnit)

||| get the dimensions of the screen
abstract
scrSize : IO (Int, Int)
scrSize = [| MkPair (mkForeign (FFun "getLines" [] FInt))
                    (mkForeign (FFun "getCols"  [] FInt)) |]

||| refresh curses windows and lines. curs_refresh(3)
abstract
refresh : IO ()
refresh = mkForeign (FFun "refresh" [] FUnit)

public
data Pair = MkPair Int

public
data Color = MkColor Int

abstract
color : String -> Color
color "black"   = MkColor 0
color "red"     = MkColor 1
color "green"   = MkColor 2
color "yellow"  = MkColor 3
color "blue"    = MkColor 4
color "magenta" = MkColor 5
color "cyan"    = MkColor 6
color "white"   = MkColor 7
color _         = MkColor 8

||| > curses support color attributes  on  terminals  with  that
||| > capability.   To  use  these  routines start_color must be
||| > called, usually right after initscr.   Colors  are  always
||| > used  in pairs (referred to as color-pairs).  A color-pair
||| > consists of a foreground  color  (for  characters)  and  a
||| > background color (for the blank field on which the charac-
||| > ters are displayed).  A programmer  initializes  a  color-
||| > pair  with  the routine init_pair.  After it has been ini-
||| > tialized, COLOR_PAIR(n), a macro  defined  in  <curses.h>,
||| > can be used as a new video attribute.
|||
||| > If  a  terminal  is capable of redefining colors, the pro-
||| > grammer can use the routine init_color to change the defi-
||| > nition   of   a   color.
|||
||| > The init_pair routine changes the definition of  a  color-
||| > pair.   It takes three arguments: the number of the color-
||| > pair to be changed, the foreground color number,  and  the
||| > background color number.  For portable applications:
|||
||| > -  The value of the first argument must be between 1 and
||| >    COLOR_PAIRS-1.
|||
||| > -  The value of the second and third arguments  must  be
||| >    between  0  and  COLORS (the 0 color pair is wired to
||| >    white on black and cannot be changed).
abstract
initPair : Pair -> Color -> Color -> IO ()
initPair (MkPair p) (MkColor f) (MkColor b) =
  mkForeign (FFun "init_pair" [FInt, FInt, FInt] FUnit) p f b

public
data Attr = MkAttr Int

colorPair : Int -> IO Int
colorPair x = mkForeign (FFun "get_color_pair" [FInt] FInt) x

abstract
attrSet : Attr -> Pair -> IO ()
attrSet (MkAttr attr) (MkPair p) = do
  pair <- colorPair p
  mkForeign (FFun "attrset" [FInt] FUnit) (prim__orInt attr pair)

abstract
attr0 : Attr
attr0 = MkAttr 0

abstract
attrOn : Attr -> IO ()
attrOn (MkAttr attr) = mkForeign (FFun "attron" [FInt] FUnit) attr

abstract
attrOff : Attr -> IO ()
attrOff (MkAttr attr) = mkForeign (FFun "attroff" [FInt] FUnit) attr

||| bitwise combination of attributes
setAttr : Attr -> Attr -> Bool -> Attr
setAttr (MkAttr b) (MkAttr a) False = MkAttr $ prim__andInt a $ prim__complInt b
setAttr (MkAttr b) (MkAttr a) True  = MkAttr $ prim__orInt  a b

abstract
setBold : Attr -> Bool -> Attr
setBold = setAttr $ MkAttr 2097152

abstract
setReverse : Attr -> Bool -> Attr
setReverse = setAttr $ MkAttr 262144

abstract
attrPlus : Attr -> Attr -> Attr
attrPlus (MkAttr a) (MkAttr b) = MkAttr $ prim__orInt a b

abstract
bkgrndSet : Attr -> Pair -> IO ()
bkgrndSet (MkAttr a) (MkPair p) = colorPair p >>= \pair => 
  bkgdset (prim__orInt (ord ' ') (prim__orInt ored pair))
  where
    bkgdset : Int -> IO ()
    bkgdset x = mkForeign (FFun "bkgdset" [FInt] FUnit) x
    testZero : Int -> Int
    testZero x = if prim__andInt a x /= 0 then x else 0
    ored = foldr1 prim__orInt (map testZero (with List [4194304
                                                       ,524288
                                                       ,2097152
                                                       ,1048576
                                                       ,8388608
                                                       ,16777216
                                                       ,262144
                                                       ,65536
                                                       ,131072
                                                       ]))

||| Write a String to the standard screen at current cursor position.
||| The cursor will be advanced after writing.
||| @s        The String that will be written
||| @maxChars Specifies the maximum number of characters that will be printed
abstract
addNStr : (s : String) -> (maxChars : Nat) -> IO ()
addNStr s n = mkForeign (FFun "addnstr" [FString, FInt] FUnit) s (toIntNat n)

abstract
addStr : String -> IO ()
addStr s = mkForeign (FFun "addstr" [FString] FUnit) s

abstract
addCh : Char -> IO ()
addCh c = mkForeign (FFun "addch" [FChar] FUnit) c

abstract
clrToEol : IO ()
clrToEol = mkForeign (FFun "clrtoeol" [] FUnit)

||| >    move the cursor associated with the window
||| >    to line y and column x.  This routine does  not  move  the
||| >    physical  cursor  of the terminal until refresh is called.
||| >    The position specified is relative to the upper  left-hand
||| >    corner of the window, which is (0,0).
abstract
move : Int -> Int -> IO ()
move y x = mkForeign (FFun "move" [FInt, FInt] FUnit) y x

curs_set : Int -> IO Int
curs_set x = mkForeign (FFun "curs_set" [FInt] FInt) x

||| Set the cursor state
|||
||| >       The curs_set routine sets  the  cursor  state  is  set  to
||| >       invisible, normal, or very visible for visibility equal to
||| >       0, 1, or 2 respectively.  If  the  terminal  supports  the
||| >       visibility   requested,   the  previous  cursor  state  is
||| >       returned; otherwise, ERR is returned.
abstract
cursSet : Int -> IO Int
cursSet 0 = leaveOk True  $> curs_set 0
cursSet x = leaveOk False $> curs_set x

||| Get the current cursor coordinates
abstract
getYX : IO (Int, Int)
getYX = do
  (MkWindow scr) <- stdScr
  [| MkPair (mkForeign (FFun "getY" [FPtr] FInt) scr)
            (mkForeign (FFun "getX" [FPtr] FInt) scr) |]

abstract
getMaxYX : IO (Int, Int)
getMaxYX = scrSize >>= \(row, col) => return (row - 1, col - 1)

||| >      The getch, wgetch, mvgetch and mvwgetch, routines read a
||| >      character  from the window.
getch : IO Int
getch = mkForeign (FFun "getch" [] FInt)

-- Some constants for easy symbolic manipulation.
-- NB we don't map keys to an abstract type anymore, as we can't use
-- Alex lexers then.

abstract
keyDown : Char
keyDown = chr 258

abstract
keyUp : Char
keyUp = chr 259

abstract
keyLeft : Char
keyLeft = chr 260

abstract
keyRight : Char
keyRight = chr 261

abstract
keyHome : Char
keyHome = chr 262

abstract
keyBackspace : Char
keyBackspace = chr 263

abstract
keyNPage : Char
keyNPage = chr 338

abstract
keyPPage : Char
keyPPage = chr 339

abstract
keyEnd : Char
keyEnd = chr 360

abstract
keyResize : Char
keyResize = chr 410

abstract
clear : IO ()
clear = mkForeign (FFun "clear" [] FUnit)

abstract
moveNextCh : IO ()
moveNextCh = do
  (maxY, maxX) <- getMaxYX
  (y, x) <- getYX
  let (newY, newX) = if x >= maxX
    then if y >= maxY then (maxY, maxX) else (y + 1, 0)
    else (y, x + 1)
  move newY newX

abstract
movePrevCh : IO ()
movePrevCh = do
  (maxY, maxX) <- getMaxYX
  (y, x) <- getYX
  let (newY, newX) = if x <= 0
    then if y <= 0 then (0, 0) else (y - 1, maxX)
    else (y, x - 1)
  move newY newX

||| read a character from the window
|||
||| When 'ESC' followed by another key is pressed before the ESC timeout,
||| that second character is not returned until a third character is
||| pressed. wtimeout, nodelay and timeout don't appear to change this
||| behaviour.
|||
||| Be warned, getCh will block the whole process without noDelay
abstract
getCh : IO (Maybe Char)
getCh = do
  refresh
  v <- getch
  case v of
    (-1) => return Nothing
    c    => return . return $ chr c

abstract
forceCh : IO Char
forceCh = do
  refresh
  v <- getch
  case v of
    (-1) => forceCh
    c    => return $ chr c

abstract
getStr : (useEcho : Bool) -> (setEcho : Bool) -> IO String
getStr useEcho setEcho = do
    echo useEcho
    (y, x) <- getYX
    (maxY, maxX) <- scrSize
    str <- map reverse $ getRawStr y x ""
    echo setEcho
    return str
  where
    getRawStr : Int -> Int -> String -> IO String
    getRawStr initY initX str = do
      (preY, preX) <- getYX
      c <- forceCh
      (y, x) <- getYX
      case c of
        '\n'   => move preY preX $> return str
        '\263' => if y <= initY && x < initX
                    then move preY preX $> getRawStr initY initX str
                    else do addStr " "
                            move y x
                            getRawStr initY initX $ strTail str
        char   => getRawStr initY initX $ strCons char str

abstract
setGetChMode : GetChMode -> IO ()
setGetChMode WaitForever           = raw False $> cBreak True  $> noDelay False
setGetChMode WaitForeverRaw        = raw True                  $> noDelay False
setGetChMode (Wait Z)              = raw False $> cBreak True  $> noDelay True
setGetChMode (Wait (S k))          = halfDelay (toIntNat k)    $> noDelay False
setGetChMode WaitForeverLinebuf    = raw False $> cBreak False $> noDelay False
setGetChMode WaitForeverLinebufRaw = raw True  $> cBreak False $> noDelay False

||| Use this function to start curses
abstract
initCurses : (getChMode : GetChMode) -> IO ()
initCurses getChMode = do
  initScr
  echo False
  keypad True
  meta True
  setGetChMode getChMode
