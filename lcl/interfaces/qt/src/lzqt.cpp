 /***************************************************************************
                               lzqt.cpp
                             -------------------
                    QT Shared Library implementation
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
#include <lzqt.h>
#include <qtengine.h>

QTEngine *lazengine;


// Routine to create a singular instance of the lazarus qt engine
void InitializeEngine(){
 lazengine = new QTEngine();
 lazengine->InitializeEngine();
}

//Set app main widget
void SetMainWidget(int qwid){
 lazengine->SetMainWidget(qwid);
}

// Create a widget of given type on the engine stack
int CreateWidget(int wtype){
 return lazengine->CreateWidget(wtype);
}

// Show the widget by its id
void ShowWidget(int wid){
  lazengine->ShowWidget(wid);
}

// reparent the child onto the parent
void ReparentWidget(int qwidparent, int qwidchild){
  lazengine->ReparentWidget(qwidparent,qwidchild);
}

void MoveWidget(int qwid,int x, int y){
 lazengine->MoveWidget(qwid,x,y);
}

void ResizeWidget(int qwid, int h, int w){
 lazengine->ResizeWidget(qwid,h,w);
}

void SetWidgetText(int qwid,char *wtext){
 lazengine->SetWidgetText(qwid,wtext);
}

void HookMousePressedEvent(int qwid, void *fptr){
 lazengine->HookMousePressedEvent(qwid, fptr);
}

void SetData(int qwid, void *data){
 lazengine->SetData(qwid,data);
}

void *GetData(int qwid){
 return lazengine->GetData(qwid);
}

void Shutdown(){
 lazengine->Shutdown();
}

// Run Engine Main Loop
void MainLoop(){
 lazengine->MainLoop();
}

