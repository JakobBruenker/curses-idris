module Cursidris

%include C "curses.h"
%access private

||| A window
abstract
Window : Type
Window = Ptr

cBool : Bool -> Int
cBool True  = 1
cBool False = 0

idrBool : Int -> Bool
idrBool = (/= 0)

||| > The nodelay option causes getch to be a non-blocking call.
||| > If  no input is ready, getch returns ERR.  If disabled (bf
||| > is FALSE), getch waits until a key is pressed.
noDelay : Window -> Bool -> IO ()
noDelay win bf = mkForeign (FFun "nodelay" [FPtr, FInt] FUnit) win (cBool bf)

||| Enable the keypad of the user's terminal
abstract
keypad : Window -> Bool -> IO ()
keypad win bf = mkForeign (FFun "keypad" [FPtr, FInt] FUnit) win (cBool bf)

||| The standard screen
abstract
stdScr : IO Window
stdScr = mkForeign (FFun "static &stdscr" [] FPtr)

meta : Window -> Bool -> IO ()
meta win bf = mkForeign (FFun "meta" [FPtr, FInt] FUnit) win (cBool bf)

||| > Normally, the hardware cursor is left at the  location  of
||| > the  window  cursor  being  refreshed.  The leaveok option
||| > allows the cursor to be left wherever the  update  happens
||| > to leave it.  It is useful for applications where the cur-
||| > sor is not used, since it  reduces  the  need  for  cursor
||| > motions.   If  possible, the cursor is made invisible when
||| > this option is enabled.
leaveOk : Bool -> IO Int
leaveOk bf = mkForeign (FFun "leaveok" [FInt] FInt) (cBool bf)

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
echo : Bool -> IO ()
echo False = mkForeign (FFun "noecho" [] FUnit)
echo True  = mkForeign (FFun "echo"   [] FUnit)

||| > The cbreak routine
||| > disables line buffering and erase/kill  character-process-
||| > ing  (interrupt  and  flow  control  characters  are unaf-
||| > fected), making characters typed by the  user  immediately
||| > available  to  the  program.  The nocbreak routine returns
||| > the terminal to normal (cooked) mode.
cBreak : Bool -> IO ()
cBreak True  = mkForeign (FFun "cbreak"   [] FUnit)
cBreak False = mkForeign (FFun "nocbreak" [] FUnit)

||| A bunch of settings we need
abstract
resetParams : IO ()
resetParams = do
  cBreak True
  echo False
  nl True
  leaveOk True
  scr <- stdScr
  meta scr True
  keypad scr True
  noDelay scr False
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

||| initscr is normally the first curses routine to call when
||| initializing a program. curs_initscr(3):
|||
||| > To initialize the routines, the routine initscr or newterm
||| > must be called before any of the other routines that  deal
||| > with  windows  and  screens  are used.
|||
||| > The initscr code determines the terminal type and initial-
||| > izes all curses data structures.  initscr also causes  the
||| > first  call  to  refresh  to  clear the screen.  If errors
||| > occur, initscr writes  an  appropriate  error  message  to
||| > standard error and exits; otherwise, a pointer is returned
||| > to stdscr
initScr : IO Window
initScr = mkForeign (FFun "init_screen" [] FPtr)

||| Start it all up
abstract
initCurses : IO ()
initCurses = do
  initScr
  b <- hasColors
  when b $ startColor $> useDefaultColors
  resetParams

||| > The program must call endwin for each terminal being used before
||| > exiting from curses.
abstract
endWin : IO ()
endWin = mkForeign (FFun "endwin" [] FUnit)

||| get the dimensions of the screen
abstract
scrSize : IO (Int, Int)
scrSize = [| MkPair (mkForeign (FFun "LINES" [] FInt))
                    (mkForeign (FFun "COLS"  [] FInt)) |]

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
    ored = foldr1 prim__orInt (map testZero (the (List Int) [4194304
                                                            ,524288
                                                            ,2097152
                                                            ,1048576
                                                            ,8388608
                                                            ,16777216
                                                            ,262144
                                                            ,65536
                                                            ,131072
                                                            ]))

abstract
waddnstr : Window -> String -> Int -> IO Int
waddnstr w s x = mkForeign (FFun "waddnstr" [FPtr, FString, FInt] FInt) w s x

abstract
clrToEol : IO ()
clrToEol = mkForeign (FFun "clrtoeol" [] FUnit)

||| >    move the cursor associated with the window
||| >    to line y and column x.  This routine does  not  move  the
||| >    physical  cursor  of the terminal until refresh is called.
||| >    The position specified is relative to the upper  left-hand
||| >    corner of the window, which is (0,0).
abstract
wMove : Window -> Int -> Int -> IO ()
wMove win y x = mkForeign (FFun "wmove" [FPtr, FInt, FInt] FUnit) win y x

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
getYX : Window -> IO (Int, Int)
getYX win = [| MkPair (mkForeign (FFun "getY" [FPtr] FInt) win) --TODO: write C
                      (mkForeign (FFun "getX" [FPtr] FInt) win) |] -- function

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

||| read a character from the window
|||
||| When 'ESC' followed by another key is pressed before the ESC timeout,
||| that second character is not returned until a third character is
||| pressed. wtimeout, nodelay and timeout don't appear to change this
||| behaviour.
|||
||| Be warned, getCh will block the whole process without noDelay
abstract
getCh : IO Char
getCh = do
  v <- getch
  case v of
    (-1) => getCh
    x    => return $ chr x

-- XXX Do we need this? cursesSigWinch : Signal
