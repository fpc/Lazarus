 /***************************************************************************
                               lzqt.h
                             -------------------
                    QT Shared Library Exports Header
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

extern "C"
{
 // Initialize Lazarus QT Engine
 void InitializeEngine();
  //Create a widget of given type
 int CreateWidget(int wtype);
 //set app main widget
 void SetMainWidget(int qwid);
 //Show a widget
 void ShowWidget(int wid);
 //reparent a widget
 void ReparentWidget(int qwidparent, int qwidchild);
 //move widget
 void MoveWidget(int qwid, int x, int y);
 //resize widget
 void ResizeWidget(int qwid, int h, int w);
 //set widget text
 void SetWidgetText(int qwid,char *wtext);
 //hook simple click event
 void HookMousePressedEvent(int qwid, void *fptr);
 //attach data pointer to widget
 void SetData(int qwid, void *data);
  //function to get data pointer from widget
 void *GetData(int qwid);
  //shutdown the qt engine
 void Shutdown();
 // Run Engine Main Event Loop
 void MainLoop();
}