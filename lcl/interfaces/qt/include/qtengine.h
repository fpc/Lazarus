 /***************************************************************************
                             qtengine.h
                             -------------------
                            QT Engine Class Header
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

class QTEngine{

 public:
 void InitializeEngine();
 void SetMainWidget(int qwid);
 int CreateWidget(int qwtype);
 void ShowWidget(int qwid);
 void ReparentWidget(int qwidparent, int qwidchild);
 void MoveWidget(int qwid, int x, int y);
 void ResizeWidget(int qwid, int h, int w);
 void SetWidgetText(int qwid,char *wtext);
 void HookMousePressedEvent(int qwid, void *fptr);
 void SetData(int qwid, void *data);
 void *GetData(int qwid);
 void Shutdown();
 void MainLoop();
};