 /***************************************************************************
                             qtengine.cpp
                             -------------------
                          QT Engine Class Implementation
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
/////////////////
//STL
#include <vector>
#include <stdio.h>

/////////////////
//USER DEFINED
#include <qtengine.h>
#include <lzwidget.h>
#include <lztypes.h>

/////////////////
//QT INCLUDES
#include <qobject.h>
#include <qapplication.h>
#include <qbutton.h>
#include <lpushbt.h>
#include <qchkbox.h>
#include <qradiobt.h>
#include <qdialog.h>
#include <qfiledlg.h>
#include <qmsgbox.h>
#include <qtabdlg.h>
#include <qframe.h>
#include <qgrpbox.h>
#include <qbttngrp.h>
#include <qlcdnum.h>
#include <qlabel.h>
#include <qmenubar.h>
#include <qtablevw.h>
#include <qlistbox.h>
#include <qmlined.h>
#include <qpopmenu.h>
#include <qlined.h>
#include <qscrbar.h>
#include <qtabbar.h>
#include <qwindow.h>


static QApplication *app;
vector <LZWidget*> wlist;

/////////////////////////////////////////////////
// Initialize a app object
void QTEngine::InitializeEngine(){
  int argc;
  char **argv;
  argc = 0;
  app = new QApplication( argc, argv );

}

/////////////////////////////////////////////////
//Create a widget on the stack
int QTEngine::CreateWidget(int qwtype){

QWidget *oWidget;
int rCount;

  rCount = -1;

  switch(qwtype){
   case WIDGET:
     oWidget = new QWidget();
     break;

   case WIDGET_BUTTON:
     oWidget = new QButton();
     break;

   case WIDGET_PUSH_BUTTON:
     oWidget = new LPushButton();
     break;

   case WIDGET_CHECK_BOX:
     oWidget = new QCheckBox();
     break;

   case WIDGET_RADIO_BUTTON:
     oWidget = new QRadioButton();
     break;

  case WIDGET_DIALOG:
     oWidget = new QDialog();
     break;

   case WIDGET_DIALOG_FILE:
     oWidget = new QFileDialog();
     break;

   case WIDGET_MESSAGE_BOX:
     oWidget = new QMessageBox();
     break;

   case WIDGET_TAB_DIALOG:
     oWidget = new QTabDialog();
     break;

   case WIDGET_FRAME:
     oWidget = new QFrame();
     break;

    case WIDGET_GROUP_BOX:
     oWidget = new QGroupBox();
     break;

    case WIDGET_BUTTON_GROUP:
     oWidget = new QButtonGroup();
     break;

    case WIDGET_LCD_NUMBER:
     oWidget = new QLCDNumber();
     break;

    case WIDGET_LABEL:
     oWidget = new QLabel();
     break;

    case WIDGET_MENU_BAR:
     oWidget = new QMenuBar();
     break;

    case WIDGET_LISTBOX:
     oWidget = new QListBox();
     break;

    case WIDGET_MULTI_LINE_EDIT:
     oWidget = new QMultiLineEdit();
     break;

    case WIDGET_POPUP_MENU:
     oWidget = new QPopupMenu();
     break;

    case WIDGET_LINE_EDIT:
     oWidget = new QLineEdit();
     break;

    case WIDGET_SCROLL_BAR:
     oWidget = new QScrollBar();
     break;

    case WIDGET_WINDOW:
     oWidget = new QWindow();
     break;
  }

 /* create a new lzwidget */
 if (oWidget != NULL){
   LZWidget *oLZWidget = new LZWidget();
   oLZWidget->lwidget = oWidget;
   rCount = wlist.size();
   oLZWidget->lzWid = rCount;
   oLZWidget->lzWtype = qwtype;

   /* push widget to back of  vector stack */
   wlist.push_back(oLZWidget);
   }

 return rCount;
}

/////////////////////////////////////////////////
// Set application main widget
void QTEngine::SetMainWidget(int qwid){
 LZWidget *oLZWidget;
 QWidget *oWidget;

 oLZWidget = wlist[qwid];
 oWidget = oLZWidget->lwidget;
 app->setMainWidget(oWidget);
}

/////////////////////////////////////////////////
//Show a widget
void QTEngine::ShowWidget(int qwid){
 LZWidget *oLZWidget;
 QWidget *oWidget;

 oLZWidget = wlist[qwid];
 oWidget = oLZWidget->lwidget;
 oWidget->show();
}

/////////////////////////////////////////////////
//Reparent a widget
void QTEngine::ReparentWidget(int qwidparent, int qwidchild){
 LZWidget *opLZWidget;
 LZWidget *ocLZWidget;
 QWidget *opWidget;
 QWidget *ocWidget;
 QPoint apt(1,1);
 opLZWidget = wlist[qwidparent];
 opWidget = opLZWidget->lwidget;

 ocLZWidget = wlist[qwidchild];
 ocWidget = ocLZWidget->lwidget;

 ocWidget->recreate(opWidget,0, apt,true);
}


/////////////////////////////////////////////////
//Move a widget
void QTEngine::MoveWidget(int qwid, int x, int y){
 LZWidget *oLZWidget;
 QWidget *oWidget;

 oLZWidget = wlist[qwid];
 oWidget = oLZWidget->lwidget;
 oWidget->move(x,y);
}

/////////////////////////////////////////////////
//Resize a widget
void QTEngine::ResizeWidget(int qwid,int h, int w){
 LZWidget *oLZWidget;
 QWidget *oWidget;

 oLZWidget = wlist[qwid];
 oWidget = oLZWidget->lwidget;
 oWidget->resize(h,w);

}

/////////////////////////////////////////////////
//Set widget text property
void QTEngine::SetWidgetText(int qwid,char *wtext){
 LZWidget *oLZWidget;
 QWidget *oWidget;
 QButton *oButton;
 int qwtype;

 oLZWidget = wlist[qwid];
 oWidget = oLZWidget->lwidget;
 qwtype = oLZWidget->lzWtype;

 switch(qwtype){
   case WIDGET_PUSH_BUTTON:
     oButton = (LPushButton *)oWidget;
     oButton->setText(wtext);
     break;

   case WIDGET:
     oWidget->setCaption(wtext);
     break;
  }

}

/////////////////////////////////////////////////
//Hook to a simple click event
void QTEngine::HookMousePressedEvent(int qwid, void *fptr){
 LZWidget *oLZWidget;
 QWidget *oWidget;
 int qwtype;
 oLZWidget = wlist[qwid];
 oWidget = oLZWidget->lwidget;
 qwtype = oLZWidget->lzWtype;



 switch(qwtype){
   case WIDGET_PUSH_BUTTON:
     if (oLZWidget->lmousepressedptr == NULL){
     LPushButton *oButton = (LPushButton *)oWidget;
     QObject::connect( oButton, SIGNAL(mousePressed(int,int,int,int)), oLZWidget, SLOT(CallMousePressedEvent(int,int,int,int)) );
     oLZWidget->lmousepressedptr = fptr;
     break;
     }
  }

}

void QTEngine::SetData(int qwid, void *data){
 LZWidget *oLZWidget;
 oLZWidget = wlist[qwid];
 oLZWidget->lData = data;
}

void *QTEngine::GetData(int qwid){
 LZWidget *oLZWidget;
 oLZWidget = wlist[qwid];
 return oLZWidget->lData;
}

/////////////////////////////////////////////////
//Shutdown the QT application
void QTEngine::Shutdown(){
 app->quit();
}

/////////////////////////////////////////////////
//Run QT Main event loop
void QTEngine::MainLoop(){
 int rtn;
 rtn = app->exec();
}
