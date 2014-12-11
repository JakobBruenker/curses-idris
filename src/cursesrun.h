#ifndef RUNCURSES_H
#define RUNCURSES_H

#include <ncurses.h>

WINDOW * stdScr();
int getLines();
int getCols();
int getY();
int getX();

#endif
