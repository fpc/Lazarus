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
   This class is used for adding controls to the toolbar to be
   dropped onto a form
   ---------------------------------------------}
   TIDEComponent = class(TObject)
   private
     FSpeedButton : TSpeedButton;
     FRegisteredComponent : TRegisteredComponent;
   protected
     Function LoadImageintoPixmap : TPixmap;
   public
     constructor Create;
     destructor Destroy; override;
     Function _Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; Virtual;
     property SpeedButton : TSpeedButton  read FSpeedButton write FSPeedbutton;
     property RegisteredComponent : TRegisteredComponent read FRegisteredComponent write FRegisteredComponent;

   end;

   {-------------------------------------------
   This class keeps a list of TIDeComponents
   --------------------------------------------}
   TIdeCompList = Class(TObject)
   private
     FItems : TList;
     FCount : Integer;
     Function GetCount : Integer;
   public
     constructor Create;
     destructor Destroy; override;

     function FindCompbySpeedbutton(Value : TSpeedButton) : TIDEComponent;
     function FindCompbyIndex(Value : Integer) : TIDEComponent;
     function FindCompbyRegComponent(Value : TRegisteredComponent) : TIDEComponent;

     function Add(Value : TObject) : Integer;
     function Delete(Value : TObject) : Boolean;
     property Count : Integer read GetCount;
   end;

   {-------------------------------------------
   These are the default components
   --------------------------------------------}


var
IDECompList : TIDECompList;
RegCompList:TRegisteredComponentList;

implementation

uses Project,global,LResources;


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
    Left := (FRegisteredComponent.IndexInPage+1)*26;
    Width := 25;
    Height := 25;
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
    res:LResource;
  begin
    Result:=false;
    res:=LazarusResources.Find(ResourceName);
    if (res.Value<>'') then begin
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
  if not LoadResource
    (FRegisteredComponent.ComponentClass.ClassName,Result) then
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

// RegisterStandardComponents

  RegisterComponentsProc:=@RegisterComponents;
  RegisterComponents('Standard','Menus',[TMenu,TPopupMenu]);
  RegisterComponents('Standard','StdCtrls',[TEdit,TLabel,TMemo,TCheckBox
          ,TListBox,TRadioButton,TComboBox,TScrollBar,TGroupBox,TToggleBox]);
  RegisterComponents('Standard','Buttons',[TButton,TBitBtn,TSpeedButton]);
  RegisterComponents('Additional','ExtCtrls',[TNoteBook,TPaintBox
          ,TBevel,TRadioGroup]);
  RegisterComponents('Additional','ComCtrls',[TStatusBar,TListView,TProgressBar
          ,TToolBar,TToolButton,TTrackBar]);

  RegisterComponents('Samples','Spin',[TSpinEdit]);
  RegisterComponents('System','ExtCtrls',[TTimer]);

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
