{ $Id$}
{
 *****************************************************************************
 *                              TestMockWSMenus.pp                           *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit TestMockWSMenus;
{$mode objfpc}{$H+}

interface

uses
  LCLType, Graphics, GraphType, ImgList, Menus, Forms,
  WSMenus, WSLCLClasses, WSProc;

type

  { TTestMockWSMenuItem }

  TTestMockWSMenuItem = class(TWSMenuItem)
  published
//    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
//    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
  end;

  { TTestMockWSMenu }

  TTestMockWSMenu = class(TWSMenu)
  published
//    class function CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TTestMockWSMainMenu }

  TTestMockWSMainMenu = class(TWSMainMenu)
  published
  end;

  { TTestMockWSPopupMenu }

  TTestMockWSPopupMenu = class(TWSPopupMenu)
  published
//    class function CreateHandle(const AMenu: TMenu): HMENU; override;
//    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;

implementation


end.
