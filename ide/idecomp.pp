{
 /***************************************************************************
                              idecomp.pp
                              ----------
                             TIDEComponent


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit IDEComp;

{$mode objfpc}{$H+}

{ $DEFINE DATABASE}
{ $DEFINE INTERBASE}

interface

uses
  Classes, LclLinux, StdCtrls, Forms, Buttons, Menus, ComCtrls,Arrow,
  Spin, SysUtils, Controls, CompReg, Graphics, ExtCtrls, Dialogs, Calendar,
  ImgList, LResources
  {$IFDEF DATABASE}
  ,db
  {$ENDIF}
  {$IFDEF INTERBASE}
  ,interbase
  {$ENDIF}
  ;

const
  ComponentPaletteBtnWidth  = 25;
  ComponentPaletteBtnHeight = 25;

type
   {--------------------------------------------
   Created by Shane Miller
   This class is used for adding controls to the toolbar to be
   dropped onto a form
   ---------------------------------------------}
   TIDEComponent = class(TObject)
   private
     {The speedbutton that's displayed on the IDE control bar}
     FSpeedButton : TSpeedButton;
     {This is the @link(TRegisteredComponent) from compreg.pp.}
     FRegisteredComponent : TRegisteredComponent;
   protected
     {Loads the image (from a resource) into a @link(TPixMap)}
     Function LoadImageintoPixmap : TPixmap;
   public
     constructor Create;
     destructor Destroy; override;
     {Public access to create the Speedbutton.}
     Function _Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton;
     {Public access to @link(FSpeedbutton)}
     property SpeedButton : TSpeedButton  read FSpeedButton write FSPeedbutton;
     {Public access to @link(FRegisteredComponent)}
     property RegisteredComponent : TRegisteredComponent
        read FRegisteredComponent write FRegisteredComponent;
   end;

   {-------------------------------------------
   Created by Shane Miller
   This class keeps a list of TIDEComponents
   --------------------------------------------}
   TIDECompList = Class(TObject)
   private
     {The list of @link(TIdeComponent)s used in the IDE.}
     FItems : TList;
     {Used to count the @link(TIdeComponent)s used in the IDE.  Checks FItems.Count}
     Function GetCount : Integer;
   public
     constructor Create;
     destructor Destroy; override;

     {You can pass the Speedbutton and find the @link(TIdeComponent).
      This can be used when the Speedbutton is clicked and you want to find out
      what the @link(TRegisteredComponent) is.}
     function FindCompbySpeedbutton(Value : TSpeedButton) : TIDEComponent;

     {You can pass the index and find the @link(TIdeComponent).
      This is used because the tag of the speedbutton stores it's index
      in this list}
     function FindCompbyIndex(Value : Integer) : TIDEComponent;

     {You can pass the @link(TRegisteredComponent) and it'll return the @link(TIdeComponent).
      This can be used if you are running through the list of RegisteredComponents and
      want to find the speedbutton associated with it.}
     function FindCompbyRegComponent(Value : TRegisteredComponent) : TIDEComponent;

     {You can pass the Class and find the @link(TIdeComponent).}
     function FindCompbyClass(Value: TComponentClass): TIdeComponent;

     {This is used to add a @link(TIdeComponent) to the @link(FItems).}
     function Add(Value : TObject) : Integer;

     {This is used to delete a @link(TIdeComponent) from the @link(FItems).}
     function Delete(Value : TObject) : Boolean;

     {Calls @link(GetCount)}
     property Count : Integer read GetCount;

     procedure OnGetNonVisualCompIconCanvas(Sender: TObject;
        AComponent: TComponent; var IconCanvas: TCanvas;
        var IconWidth, IconHeight: integer);
   end;

   {-------------------------------------------
   These are the default components
   --------------------------------------------}


var
  IDECompList : TIDECompList;

procedure InitIDEComponents;


implementation



{ TIDECompList }

constructor TIDECompList.Create;
begin
  inherited create;
  FItems := TList.Create;
end;

destructor TIDECompList.Destroy;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do FindCompbyIndex(i).Free;
  FItems.Free;
  inherited Destroy;
end;

function TIdeCompList.GetCount : Integer;
Begin
  Result := FItems.Count;
end;

function TIDECompList.FindCompbyIndex(Value : Integer) : TIDEComponent;
Begin
  if Value < FItems.Count then
    Result := TIDEComponent(FITems[Value])
  else
    Result := nil;
end;

function TIDECompList.FindCompbySpeedbutton(Value : TSpeedButton) : TIDEComponent;
var
  I : Integer;
Begin
  for I := 0 to Count-1 do
  Begin
    Result := TIDEComponent(FItems[i]);
    if (Result.SpeedButton = Value) then exit;
  end;
  Result := nil;
end;

function TIDECompList.FindCompbyRegComponent(
  Value : TRegisteredComponent) : TIDEComponent;
var
  I : Integer;
Begin
  for I := 0 to Count-1 do
  Begin
    Result := TIDEComponent(FItems[i]);
    if (Result.RegisteredComponent = Value) then exit;
  end;
  Result := nil;
end;

function TIDECompList.FindCompbyClass(Value: TComponentClass): TIdeComponent;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do begin
    Result := TIDEComponent(FItems[i]);
    if (Result.RegisteredComponent.ComponentClass = Value) then exit;
  end;
  Result := nil;
end;

procedure TIDECompList.OnGetNonVisualCompIconCanvas(Sender: TObject;
  AComponent: TComponent; var IconCanvas: TCanvas;
  var IconWidth, IconHeight: integer);
var
  AnIDEComp: TIdeComponent;
  APixmap: TPixmap;
begin
  AnIDEComp:=IDECompList.FindCompbyClass(TComponentClass(AComponent.ClassType));
  if AnIDEComp<>nil then begin
    APixmap:=TPixmap(AnIDEComp.SpeedButton.Glyph);
    IconCanvas:=APixmap.Canvas;
    IconWidth:=APixmap.Width;
    IconHeight:=APixmap.Height;
  end;
end;

function TIdeCompList.Add(Value : TObject) : Integer;
Begin
  Result := FItems.Add(Value);
end;

function TIdeCompList.Delete(Value : TObject) : Boolean;
var i: integer;
Begin
  i:=FItems.IndexOf(Value);
  Result := (i >= 0);
  if Result then FItems.Delete(i);
end;

{ TIDEComponent }

constructor TIDEComponent.Create;
begin
  inherited Create;
end;

destructor TIDEComponent.Destroy;
begin
  inherited Destroy;
end;

Function TIDEComponent._Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
var
  Pixmap1 : TPixmap;
Begin
  Pixmap1 := LoadImageIntoPixmap;

  FSpeedButton := TSpeedButton.Create(AOwner);
  with FSpeedButton do
   Begin
    Parent := nParent;
    Flat := True;
    SetBounds((FRegisteredComponent.IndexInPage+1)*27,Top,
              ComponentPaletteBtnWidth,ComponentPaletteBtnHeight);
    Enabled := True;
    Glyph := Pixmap1;
    Visible := True;
   end;
  Result := FSpeedButton;
end;

function TIDEComponent.LoadImageIntoPixmap: TPixmap;

  function LoadResource(ResourceName:string; PixMap:TPixMap):boolean;
  var 
    ms:TMemoryStream;
    res:TLResource;
  begin
    Result:=false;
    res:=LazarusResources.Find(ResourceName);
    if (res <> nil) then begin
      if res.ValueType='XPM' then begin
        ms:=TMemoryStream.Create;
        try
          ms.Write(res.Value[1],length(res.Value));
          ms.Position:=0;
          PixMap.LoadFromStream(ms);
          Result:=true;
        finally
          ms.Free;
        end;
      end;
    end;
  end;

begin
  Result:=TPixMap.Create;
  Result.TransparentColor:=clBtnFace;
  if not LoadResource(FRegisteredComponent.ComponentClass.ClassName,Result) then
  begin
    LoadResource('default',Result);
  end;
end;


{--------------------------------------------------}

procedure RegisterStandardComponents(
  ARegisteredComponentList:TRegisteredComponentList);

  procedure RegisterComponents(const Page,UnitName:ShortString;
    ComponentClasses: array of TComponentClass);
  begin
    ARegisteredComponentList.RegisterComponents(
       Page,UnitName,ComponentClasses);
  end;

begin
  RegisterComponentsProc:=@RegisterComponents;
  
  RegisterComponents('Standard','Menus',[TMainMenu,TPopupMenu]);
  RegisterComponents('Standard','Buttons',[TButton]);
  RegisterComponents('Standard','StdCtrls',[TEdit,TLabel,TMemo,TCheckBox,
          TListBox,TRadioButton,TComboBox,TScrollBar,TGroupBox,TToggleBox]);
  RegisterComponents('Standard', 'ExtCtrls',[TRadioGroup,TPanel]);
  RegisterComponents('Additional','Buttons',[TBitBtn,TSpeedButton]);
  RegisterComponents('Additional','ExtCtrls',[TNoteBook,TPaintBox,
          TBevel,TImage]);
  RegisterComponents('Additional','ComCtrls',[TStatusBar,TListView,TTreeView,
          TProgressBar,TToolBar,TTrackbar,TScrollBox,TUpDown]);
  RegisterComponents('Additional','ImgList',[TImageList]);

  RegisterComponents('Misc','Calendar',[TCalendar]);
  RegisterComponents('Misc','Arrow',[TArrow]);

  RegisterComponents('System','ExtCtrls',[TTimer]);
  RegisterComponents('Dialogs','Dialogs',[TOpenDialog,TSaveDialog,
          TColorDialog,TFontDialog]);

  RegisterComponents('Samples','Spin',[TSpinEdit]);

{$IFDEF DATABASE}
  RegisterComponents('Data Access','Db',[TDatasource,TDatabase]);
{$ENDIF}
{$IFDEF INTERBASE}
  RegisterComponents('Interbase Data Access','Interbase',[TIBStoredProc,
          TIBQuery,TIBDatabase]);
{$ENDIF}

  // unselectable components
  // components that are streamed but not selectable in the IDE
  RegisterComponents('','ExtCtrls',[TPage]);
  RegisterComponents('','ComCtrls',[TToolbutton]);
  RegisterComponents('','menus', [TMenuItem]);

  RegisterComponentsProc:=nil;
end;

procedure InitIDEComponents;
begin
  RegisterStandardComponents(RegCompList);
  IdeCompList := TIDECompList.Create;
end;

initialization

{$I images/components_images.lrs}


finalization
  IdeCompList.Free;

end.
