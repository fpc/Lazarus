 /***************************************************************************
                             lpushbt.cpp
                             -------------------
                          LPushButton Widget Class Implementation
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

#include <lpushbt.h>

LPushButton::LPushButton (QWidget* parent, const char* name)
    : QPushButton(parent,name)
{
 initMetaObject();
}


LPushButton::LPushButton (const char* text, QWidget* parent, const char* name)
    :QPushButton(text,parent,name)
{
 initMetaObject();
}


void    LPushButton::mousePressEvent( QMouseEvent *event){
 QPushButton::mousePressEvent(event);
 QMouseEvent *e = (QMouseEvent*)event;
 emit mousePressed(e->button(), e->pos().x(), e->pos().y(), e->state());
}


void    LPushButton::mouseMoveEvent( QMouseEvent *event){
 QPushButton::mouseMoveEvent(event);
 QMouseEvent *e = (QMouseEvent*)event;
 emit mouseMoved(e->button(), e->pos().x(), e->pos().y(), e->state());
}

void    LPushButton::mouseReleaseEvent( QMouseEvent *event){
 QPushButton::mouseReleaseEvent(event);
 QMouseEvent *e = (QMouseEvent*)event;
 emit mouseReleased(e->button(), e->pos().x(), e->pos().y(), e->state());
}


