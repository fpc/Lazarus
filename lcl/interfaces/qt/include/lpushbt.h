 /***************************************************************************
                             lzwidget.h
                             -------------------
                          LButton Widget Class Header
                   Initial Revision  : Sat Apr 7 2000


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include <qpushbt.h>


class LPushButton : public QPushButton{
    Q_OBJECT;
 public:
    LPushButton (QWidget* parent=0, const char* name=0);
    LPushButton (const char* text, QWidget* parent=0, const char* name=0);
 protected:
    void    mousePressEvent( QMouseEvent *);
    void    mouseMoveEvent( QMouseEvent *);
    void    mouseReleaseEvent( QMouseEvent *);
 signals:
    void    mousePressed(int button, int x, int y, int state);
    void    mouseMoved(int button, int x, int y, int state);
    void    mouseReleased(int button, int x, int y, int state);
};