#include "cursesrun.h"

WINDOW * stdScr()
{
  return stdscr;
}

int getLines()
{
  return LINES;
}

int getCols()
{
  return COLS;
}

int getY(WINDOW *win)
{
  int y;
  int x;
  getyx(win, y, x);
  return y;
}

int getX(WINDOW *win)
{
  int y;
  int x;
  getyx(win, y, x);
  return x;
}
