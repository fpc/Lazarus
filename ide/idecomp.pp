{
 /***************************************************************************
                          idecomp.pp  -
                             -------------------
                   TIDEComponent


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{$H+}
unit idecomp;

{$mode objfpc}

interface

uses
  classes,LclLinux,stdctrls,forms,buttons,menus,comctrls,
  Spin, sysutils,Controls,compreg,Graphics,extctrls;


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
     property RegisteredComponent : TRegisteredComponent read FRegisteredComponent write FRegisteredComponent;

   end;

   {-------------------------------------------
   Created by Shane Miller
   This class keeps a list of TIDeComponents
   --------------------------------------------}
   TIdeCompList = Class(TObject)
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

     {This is used to add a @link(TIdeComponent) to the @link(FItems).}
     function Add(Value : TObject) : Integer;

     {This is used to delete a @link(TIdeComponent) from the @link(FItems).}
     function Delete(Value : TObject) : Boolean;

     {Calls @link(GetCount)}
     property Count : Integer read GetCount;
   end;

   {-------------------------------------------
   These are the default components
   --------------------------------------------}


var
IDECompList : TIDECompList;
RegCompList:TRegisteredComponentList;

implementation

uses Project,LResources;


{ TIDECompList }

constructor TIDECompList.Create;
begin
inherited create;
FItems := TList.Create;
end;

destructor TIDECompList.destroy;
begin
FItems.Destroy;
inherited;
end;

function TIdeCompList.GetCount : Integer;
Begin
Result := FItems.Count;
end;

function TIDECompList.FindCompbyIndex(Value : Integer) : TIDEComponent;
Begin
if Value < FItems.Count then
   Result := TIDEComponent(FITems[Value]) else
   Result := nil;

end;


function TIDECompList.FindCompbySpeedbutton(Value : TSpeedButton) : TIDEComponent;
var
I : Integer;
Begin
for I := 0 to Count-1 do
   Begin
   Result := TIDeComponent(FItems[i]);
   if (Result.SpeedButton = Value) then exit;
   end;
   Result := nil;

end;

function TIDECompList.FindCompbyRegComponent(Value : TRegisteredComponent) : TIDEComponent;
var
I : Integer;
Begin
for I := 0 to Count-1 do
   Begin
   Result := TIDeComponent(FItems[i]);
   if (Result.RegisteredComponent = Value) then exit;
   end;
   Result := nil;

end;


function TIdeCompList.Add(Value : TObject) : Integer;
Begin
Result := FItems.Add(Value);
end;

function TIdeCompList.Delete(Value : TObject) : Boolean;
Begin
result := (FItems.IndexOf(Value) >= 0);
if Result then FItems.Delete(FItems.IndexOf(Value));
end;

{ TIDECOMPONENT }

constructor TIDEComponent.Create;
begin
inherited create;
IDECompList.Add(self);
end;

destructor TIDEComponent.destroy;
begin
IDECompList.Delete(self);
inherited;
end;

Function TIDEComponent._Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
var
  Pixmap1 : TPixmap;
Begin
pixmap1 := LoadImageintoPixmap;

  FSpeedButton := TSpeedButton.Create(aowner);
  with FSpeedButton do
   Begin
    Parent := nParent;
    Flat := True;
    SetBounds((FRegisteredComponent.IndexInPage+1)*26,Top,Width,Height);
    Enabled := True;
    Glyph := Pixmap1;
    Visible := True;

   end;
result := FSpeedButton;
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
    ARegisteredComponentList.RegisterComponents(Page,UnitName,ComponentClasses);
  end;

begin

  RegisterComponentsProc:=@RegisterComponents;
  RegisterComponents('Standard','Menus',[TMenu,TPopupMenu]);
  RegisterComponents('Standard','Buttons',[TButton]);
  RegisterComponents('Standard','StdCtrls',[TEdit,TLabel,TMemo,TCheckBox
          ,TListBox,TRadioButton,TComboBox,TScrollBar,TGroupBox,TToggleBox]);
  RegisterComponents('Additional','Buttons',[TBitBtn,TSpeedButton]);
  RegisterComponents('Additional','ExtCtrls',[TNoteBook,TPaintBox
          ,TBevel,TRadioGroup]);
  RegisterComponents('Additional','ComCtrls',[TStatusBar,TListView,TProgressBar
          ,TToolBar,TToolbutton,TTrackbar]);

  RegisterComponents('Samples','Spin',[TSpinEdit]);
  RegisterComponents('System','ExtCtrls',[TTimer]);
  RegisterComponents('','ExtCtrls',[TPage]);

  RegisterComponentsProc:=nil;


end;




initialization
{$I designer/lazarus_control_images.lrs}
RegCompList := TRegisteredComponentList.Create;
RegisterStandardComponents(RegCompList);

IdeCompList := TIDECompList.Create;


finalization
IdeCompList.Destroy;
RegCompList.Destroy;
end.
