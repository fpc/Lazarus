 /***************************************************************************
                             lzwidget.h
                             -------------------
                          LZ Widget Class Header
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
#include <qwidget.h>

typedef void (*mousepressedptr)(int,int,int,int,int);//simple event trigger pointer

class LZWidget : public QObject{
   Q_OBJECT;
 public:
   QWidget *lwidget;  //widget reference holder
   mousepressedptr lmousepressedptr; //simple mouse press event pointer
   int lzWid;
   int lzWtype;
   void *lData;
 public slots:
   void CallMousePressedEvent(int button,int x, int y, int state);

};