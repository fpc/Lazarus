 /***************************************************************************
                             lzwidget.cpp
                             -------------------
                          LZ Widget Class Implementation
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

#include <lzwidget.h>

void LZWidget::CallMousePressedEvent(int button,int x, int y, int state){
  //pass back the widget stack id also
  if (lmousepressedptr != NULL){
    lmousepressedptr(lzWid,button,x,y,state);
  }
}


