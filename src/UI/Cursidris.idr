module Cursidris

%access private

||| Start it all up
abstract
initCurses : IO () -> IO ()
initCurses x = ?initCurses_rhs

||| A bunch of settings we need
abstract
resetParams : IO ()
resetParams = ?resetParams_rhs

||| Throw error if result of given action yields true
|||
||| Execute an IO action, throwing a userError if the predicate yields True when
||| applied to the result returned by the IO action. If no exception is raised, 
||| return the result of the computation. 
throwIf : (a -> Bool) -> (a -> String) -> IO a -> IO a
throwIf f x x1 = ?throwIf_rhs

||| Arbitrary test
throwIfErr : Num a => (a -> String) -> IO a -> IO a
throwIfErr x x1 = ?throwIfErr_rhs

||| Discard result
abstract
throwIfErr_ : Num a => (a -> String) -> IO a -> IO ()
throwIfErr_ f x = ?throwIfErr__rhs

||| Throw error if given Pointer is a Nullpointer
throwIfNull : String -> IO Ptr -> IO Ptr
throwIfNull x x1 = ?throwIfNull_rhs

||| A window
abstract
Window : Type
Window = Ptr

||| The standard screen
abstract
stdScr : Window
stdScr = ?stdScr_rhs

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
initScr = ?initScr_rhs

||| > The cbreak routine
||| > disables line buffering and erase/kill  character-process-
||| > ing  (interrupt  and  flow  control  characters  are unaf-
||| > fected), making characters typed by the  user  immediately
||| > available  to  the  program.  The nocbreak routine returns
||| > the terminal to normal (cooked) mode.
cBreak : Bool -> IO ()
cBreak x = ?cBreak_rhs

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
echo x = ?echo_rhs

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
nl x = ?nl_rhs

||| Enable the keypad of the user's terminal
abstract
keypad : Window -> Bool -> IO ()
keypad x x1 = ?keypad_rhs

||| > The nodelay option causes getch to be a non-blocking call.
||| > If  no input is ready, getch returns ERR.  If disabled (bf
||| > is FALSE), getch waits until a key is pressed.
noDelay : Window -> Bool -> IO ()
noDelay x x1 = ?noDelay_rhs

||| > Normally, the hardware cursor is left at the  location  of
||| > the  window  cursor  being  refreshed.  The leaveok option
||| > allows the cursor to be left wherever the  update  happens
||| > to leave it.  It is useful for applications where the cur-
||| > sor is not used, since it  reduces  the  need  for  cursor
||| > motions.   If  possible, the cursor is made invisible when
||| > this option is enabled.
leaveOk : Bool -> IO Int
leaveOk x = ?leaveOk_rhs

||| The use_default_colors() and assume_default_colors() func-
|||  tions are extensions to the curses library.  They are used
|||  with terminals that support ISO 6429 color, or equivalent.
|||
||| use_default_colors() tells the  curses library  to  assign terminal
||| default foreground/background colors to color number  -1.
useDefaultColors : IO ()
useDefaultColors = return ()

||| > The program must call endwin for each terminal being used before
||| > exiting from curses.
abstract
endWin : IO ()
endWin = ?endWin_rhs

||| get the dimensions of the screen
abstract
scrSize : IO (Int, Int)
scrSize = ?scrSize_rhs

||| refresh curses windows and lines. curs_refresh(3)
abstract
refresh : IO ()
refresh = ?refresh_rhs

hasColors : IO Bool
hasColors = ?hasColors_rhs

||| Initialise the color settings, also sets the screen on the default
||| colors (white on black)
startColor : IO ()
startColor = ?startColor_rhs

public
data IntPair = CIntPair  Int

public
data Color   = CColor    Int

abstract
color : String -> Maybe Color
color x = ?color_rhs

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
initIntPair : IntPair -> Color -> Color -> IO ()
initIntPair x x1 x2 = ?initIntPair_rhs

public
data Attr = CAttr Int

abstract
attrSet : Attr -> IntPair -> IO ()
attrSet x x1 = ?attrSet_rhs

abstract
attr0 : Attr
attr0 = ?attr0_rhs

abstract
setBold : Attr -> Bool -> Attr
setBold x x1 = ?setBold_rhs

abstract
setReverse : Attr -> Bool -> Attr
setReverse x x1 = ?setReverse_rhs

||| bitwise combination of attributes
setAttr : Attr -> Attr -> Bool -> Attr
setAttr x x1 x2 = ?setAttr_rhs

abstract
attrPlus : Attr -> Attr -> Attr
attrPlus x x1 = ?attrPlus_rhs

abstract
bkgrndSet : Attr -> IntPair -> IO ()
bkgrndSet x x1 = ?bkgrndSet_rhs

abstract
waddnstr : Window -> String -> Int -> IO Int
waddnstr x x1 x2 = ?waddnstr_rhs

abstract
clrToEol : IO ()
clrToEol = ?clrToEol_rhs

||| >    move the cursor associated with the window
||| >    to line y and column x.  This routine does  not  move  the
||| >    physical  cursor  of the terminal until refresh is called.
||| >    The position specified is relative to the upper  left-hand
||| >    corner of the window, which is (0,0).
abstract
wMove : Window -> Int -> Int -> IO ()
wMove x x1 x2 = ?wMove_rhs

public
data CursorVisibility = CursorInvisible | CursorVisible | CursorVeryVisible

||| Set the cursor state
|||
||| >       The curs_set routine sets  the  cursor  state  is  set  to
||| >       invisible, normal, or very visible for visibility equal to
||| >       0, 1, or 2 respectively.  If  the  terminal  supports  the
||| >       visibility   requested,   the  previous  cursor  state  is
||| >       returned; otherwise, ERR is returned.
abstract
cursSet : Int -> IO Int
cursSet x = ?cursSet_rhs

||| Get the current cursor coordinates
abstract
getYX : Window -> IO (Int, Int)
getYX x = ?getYX_rhs

||| Get the current cursor coords, written into the two argument ints.
|||
||| >    The getyx macro places the current cursor position of the given
||| >    window in the two integer variables y and x.
|||
||| >    void getyx(WINDOW *win, int y, int x);
nomacro_gityx : Window -> Ptr -> Ptr -> IO ()
nomacro_gityx = ?nomacro_gityx_rhs

||| >      The getch, wgetch, mvgetch and mvwgetch, routines read a
||| >      character  from the window.
getch : IO Int
getch = ?getch_rhs

||| Map keys to real chars. The lexer will like this.
decodeKey : Int -> Char

-- Some constants for easy symbolic manipulation.
-- NB we don't map keys to an abstract type anymore, as we can't use
-- Alex lexers then.

abstract
keyDown : Char
keyDown = ?keyDown_rhs

abstract
keyUp : Char
keyUp = ?keyUp_rhs

abstract
keyLeft : Char
keyLeft = ?keyLeft_rhs

abstract
keyRight : Char
keyRight = ?keyRight_rhs

abstract
keyHome : Char
keyHome = ?keyHome_rhs

abstract
keyBackspace : Char
keyBackspace = ?keyBackspace_rhs

abstract
keyNPage : Char
keyNPage = ?keyNPage_rhs

abstract
keyPPage : Char
keyPPage = ?keyPPage_rhs

abstract
keyEnd : Char
keyEnd = ?keyEnd_rhs

abstract
keyResize : Char
keyResize = ?keyResize_rhs

meta : Window -> Bool -> IO ()
meta x x1 = ?meta_rhs

||| read a character from the window
|||
||| When 'ESC' followed by another key is pressed before the ESC timeout,
||| that second character is not returned until a third character is
||| pressed. wtimeout, nodelay and timeout don't appear to change this
||| behaviour.
|||
||| On emacs, we really would want Alt to be our meta key, I think.
|||
||| Be warned, getCh will block the whole process without noDelay
abstract
getCh : IO Char
getCh = ?getCh_rhs

-- XXX Do we need this? cursesSigWinch : Signal
