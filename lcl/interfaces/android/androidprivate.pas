{
                    -----------------------------------------
                    androidprivate.pas
                    -----------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit androidprivate;

{$mode objfpc}{$H+}

interface

// defines
//{$I carbondefines.inc}

uses
  // libs
  android_all,
  // wdgetset
  WSLCLClasses, LCLClasses,
  // LCL + RTL
  Types, Classes, SysUtils, Controls, LCLType, LCLProc, Graphics, Math, Contnrs,
  AVL_Tree, LMessages, LCLMessageGlue, stdctrls, Forms;

type

  TAndroidView = class
  public
    LCLObject: TWinControl;
    ParentGroupView: TViewGroup;
    MainView: TView;
  end;

  TAndroidViewGroup = class(TAndroidView)
  public
    mainviewgroup: TViewGroup;
  end;

  { TAndroidButton }

  TAndroidButton = class(TAndroidView)
  public
    btn: android_all.TButton;
    params: TAbsoluteLayout_LayoutParams;
    constructor Create(const AObject: TCustomButton; const AParams: TCreateParams);
    destructor Destroy; override;
  end;

  { TAndroidWindow }
  
  TAndroidWindow = class(TAndroidViewGroup)
  public
    layout: TAbsoluteLayout;
    scroller: TScrollView;
    constructor Create(const AObject: TCustomForm; const AParams: TCreateParams);
    destructor Destroy; override;
  end;

//function CheckHandle(const AWinControl: TWinControl; const AClass: TClass; const DbgText: String): Boolean;
//function CheckWidget(const Handle: HWND; const AMethodName: String; AParamName: String = ''): Boolean;
//function CheckWidget(const Handle: HWND; const AMethodName: String; AClass: TClass): Boolean;

implementation

{ TAndroidButton }

constructor TAndroidButton.Create(const AObject: TCustomButton;
  const AParams: TCreateParams);
var
  Str: string;
begin
  LCLObject := AObject;
  ParentGroupView := TAndroidViewGroup(AObject.Parent.Handle).mainviewgroup;

  btn := android_all.TButton.Create;
  Str := AObject.Caption;
  btn.setText(Str);
{  btn.setOnClickListener(buttonClickCallback);}
  params := TAbsoluteLayout_LayoutParams.Create(AObject.Width, AObject.Height, AObject.Left, AObject.Top);
  ParentGroupView.addView(TView(btn), TViewGroup_LayoutParams(params));
  params.Free;

  MainView := btn;
end;

destructor TAndroidButton.Destroy;
begin
  inherited Destroy;
end;

{ TCarbonWidget }

constructor TAndroidWindow.Create(const AObject: TCustomForm;
  const AParams: TCreateParams);
begin
  LCLObject := AObject;

  layout := TAbsoluteLayout.Create;
//  scroller := TScrollView.Create;
//  scroller.addView(layout);

  mainviewgroup := layout;
  MainView := layout;//scroller;
end;

destructor TAndroidWindow.Destroy;
begin
  inherited Destroy;
end;

end.
