{
 /***************************************************************************
                          idecomp.pp  -
                             -------------------
                   TMain is the application toolbar window.


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
  Spin, sysutils,Controls,graphics,extctrls, Dialogs,dlgMEssage;


type
   {--------------------------------------------
   This class is used for adding controls to the toolbar to be
   dropped onto a form
   ---------------------------------------------}
   TIDEComponent = class(TObject)
   private
     FImage : String;
     FClassName : String; //hold the main types classname
     FClassType : TClass;
     procedure SetImage(Value : String);
   protected
     Function LoadImageintoPixmap : TPixmap;
   public
     constructor Create; virtual;
     function CreateMethod(AOwner : TComponent) :Tcontrol; virtual;
     procedure ClickMethod(sender : TObject); Virtual;
     procedure DblClickMethod(sender : TObject); Virtual;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; Virtual;
     property image : string read FImage write SetImage;
     property ClassName : String read FClassName write FClassName;
     property ClassType : TClass read FClassType write FClassType;

   end;

   {-------------------------------------------
   This class keeps a list of TIDeComponents
   --------------------------------------------}
   TIdeCompList = TList;

   {-------------------------------------------
   These are the default components
   --------------------------------------------}

TIDEMouse = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEMenu = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEPopup = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEEdit = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDELabel = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEButton = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEMemo = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDECheckbox = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDERadioButton = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEListbox = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEComboBox = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDEBitBtn = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDESpeedbutton = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDESpinedit = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;

TIDENotebook = class(TIdeComponent)
   public
     function CreateMethod(AOwner : TComponent): TControl; override;
     procedure ClickMethod(sender: TObject); override;
     procedure DblClickMethod(sender: TObject); override;
     constructor Create; override;
     Function Speedbutton(AOwner : TComponent; nParent: TWinControl): TSpeedButton; override;
   end;


var
IDECompList : TIDECompList;

implementation

uses Project,global;

constructor TIDEComponent.Create;
begin
inherited create;
Fimage := 'images/mouse.xpm';
FClassName := 'TIDECOMPONENT';
end;

Procedure TIDEComponent.SetImage(value : String);
begin
if FileExists(value) then
   fimage := Value;
end;

Procedure TIDEComponent.ClickMethod(sender:TObject);
begin
end;

Procedure TIDEComponent.DblClickMethod(sender : TObject);
begin
end;

Function TIDEComponent.CreateMethod(AOwner : TComponent): TControl;
begin
result := nil;
end;

Function TIDEComponent.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
var
Speedbtn : TSpeedbutton;
Pixmap1 : TPixmap;
Begin
pixmap1 := LoadImageintoPixmap;

  SpeedBtn := TSpeedButton.Create(aowner);
  with SpeedBtn do
   Begin
    Parent := nParent;
    Flat := True;
    Width := 25;
    Height := 25;
    Enabled := True;
    Glyph := Pixmap1;
    Visible := True;

   end;
Result := Speedbtn;

end;

Function TIDeComponent.LoadImageintoPixmap : TPixmap;
var
S : TStream;
Begin
    S := TFileStream.Create(image, fmOpenRead);
    try
      Result := TPixmap.Create;
      Result.TransparentColor := clBtnFace;
      Result.LoadFromStream(S);
    finally
      S.Free;
    end;

end;


{---------------------------------------}
constructor TIDEMouse.create;
begin
inherited create;
FImage := 'images/mouse.xpm';
ClassName := 'TMOUSE';  //not really the classname for a mouse
//ClassType := TComponent;
end;

function TIDEMouse.CreateMethod(AOwner : TComponent): TControl;
begin
//this doesn't create any control on the form
result := nil;
end;

Procedure TIDEMouse.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEMouse.DblClickMethod(sender : TObject);
Begin
//need to add the form.create method here.
{   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;

 }
end;

Function TIDEMouse.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
begin
result := inherited Speedbutton(aowner,nparent);
Result.hint := 'Mouse Pointer';
end;
{--------------------------------------------------}



{---------------------------------------}
constructor TIDEMenu.create;
begin
inherited create;
FImage := 'images/menu.xpm';
FClassName := 'TMenu';
FClassType := TMenu;
end;

function TIDEMenu.CreateMethod(AOwner : TComponent): TControl;
begin
//return another speedbutton to drop on the form
result := SpeedButton(AOwner, TWinControl(AOwner));
end;

Procedure TIDEMenu.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEMenu.DblClickMethod(sender : TObject);
Begin
//need to create a menu editor

end;

Function TIDEMenu.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
Result.hint := 'Menu';
end;
{--------------------------------------------------}


{---------------------------------------}
constructor TIDEPopup.create;
begin
inherited create;
FImage := 'images/popup.xpm';
FClassName := 'TPopup';
end;

function TIDEPopup.CreateMethod(AOwner : TComponent): TControl;
begin
//return another speedbutton to drop on the form
result := SpeedButton(AOwner, TWinControl(AOwner));
end;

Procedure TIDEPopup.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEPopup.DblClickMethod(sender : TObject);
Begin
//need to create a popup menu editor

end;

Function TIDEPopup.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;
{--------------------------------------------------}




{---------------------------------------}
constructor TIDEEdit.create;
begin
inherited create;
FImage := 'images/editbox.xpm';
FClassName := 'TEdit';
end;

function TIDEEdit.CreateMethod(AOwner : TComponent): TControl;
begin
result := TEdit.Create(AOwner);
end;

Procedure TIDEEdit.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEEdit.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T' + TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDEEdit.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}




{---------------------------------------}
constructor TIDELabel.create;
begin
inherited create;
FImage := 'images/label.xpm';
FClassName := 'TLabel';
end;

function TIDELabel.CreateMethod(AOwner : TComponent): TControl;
begin
result := TLabel.Create(AOwner);
end;

Procedure TIDELabel.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDELabel.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDELabel.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}




{---------------------------------------}
constructor TIDEButton.create;
begin
inherited create;
FImage := 'images/button.xpm';
FClassName := 'TButton';
end;

function TIDEButton.CreateMethod(AOwner : TComponent): TControl;
begin
result := TButton.Create(AOwner);
end;

Procedure TIDEButton.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEButton.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDEButton.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}




{---------------------------------------}
constructor TIDEMemo.create;
begin
inherited create;
FImage := 'images/memo.xpm';
FClassName := 'TMemo';
end;

function TIDEMemo.CreateMethod(AOwner : TComponent): TControl;
begin
result := TMemo.Create(AOwner);
end;

Procedure TIDEMemo.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEMemo.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDEMemo.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}


{---------------------------------------}
constructor TIDECheckbox.create;
begin
inherited create;
FImage := 'images/checkbox.xpm';
FClassName := 'TCheckbox';
end;

function TIDECheckbox.CreateMethod(AOwner : TComponent): TControl;
begin
result := TCheckbox.Create(AOwner);
end;

Procedure TIDECheckbox.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDECheckbox.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDECheckbox.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}




{---------------------------------------}
constructor TIDERadioButton.create;
begin
inherited create;
FImage := 'images/radiobutton.xpm';
FClassName := 'TRadioButton';
end;

function TIDERadioButton.CreateMethod(AOwner : TComponent): TControl;
begin
result := TRadioButton.Create(AOwner);
end;

Procedure TIDERadioButton.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDERadioButton.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDERadioButton.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}




{---------------------------------------}
constructor TIDEListbox.create;
begin
inherited create;
FImage := 'images/listbox.xpm';
FClassName := 'TListbox';
end;

function TIDEListbox.CreateMethod(AOwner : TComponent): TControl;
begin
result := TListbox.Create(AOwner);
end;

Procedure TIDEListbox.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEListbox.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDEListbox.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}




{---------------------------------------}
constructor TIDEComboBox.create;
begin
inherited create;
FImage := 'images/combobox.xpm';
FClassName := 'TCombobox';
end;

function TIDEComboBox.CreateMethod(AOwner : TComponent): TControl;
begin
result := TComboBox.Create(AOwner);
end;

Procedure TIDEComboBox.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEComboBox.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDEComboBox.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}





{---------------------------------------}
constructor TIDEBitbtn.create;
begin
inherited create;
FImage := 'images/bitbtn.xpm';
FClassName := 'TBitbtn';
end;

function TIDEBitBtn.CreateMethod(AOwner : TComponent): TControl;
begin
result := TBitBtn.Create(AOwner);
end;

Procedure TIDEBitBtn.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDEBitBtn.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDEBitBtn.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}





{---------------------------------------}
constructor TIDESpeedbutton.create;
begin
inherited create;
FImage := 'images/speedbutton.xpm';
FClassName := 'TSpeedbutton';
end;

function TIDESpeedbutton.CreateMethod(AOwner : TComponent): TControl;
var
S : TStream;
Pixmap1 :TPixmap;
begin
result := TSpeedbutton.Create(AOwner);
  S := TFileStream.Create('./images/default.xpm', fmOpenRead);
  try
    Pixmap1 := TPixmap.Create;
    Pixmap1.TransparentColor := clBtnFace;
    Pixmap1.LoadFromStream(S);
  finally
    S.Free;
  end;

with TSpeedbutton(result) do
   Begin
    Glyph := Pixmap1;
    Flat := False;
  end;

end;

Procedure TIDESpeedbutton.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDESpeedbutton.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDESpeedButton.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}





{---------------------------------------}
constructor TIDESpinedit.create;
begin
inherited create;
FImage := 'images/spinedit.xpm';
FClassName := 'TSpinEdit';
end;

function TIDESpinedit.CreateMethod(AOwner : TComponent): TControl;
begin
Writeln('TIDESPinEdit.CreateMethod1');
result := TSpinedit.Create(AOwner);
Writeln('TIDESPinEdit.CreateMethod2');
end;

Procedure TIDESpinedit.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDESpinedit.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDESpinedit.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}





{---------------------------------------}
constructor TIDENotebook.create;
begin
inherited create;
FImage := 'images/notebook.xpm';
FClassName := 'TNotebook';
end;

function TIDENotebook.CreateMethod(AOwner : TComponent): TControl;
begin
result := TNotebook.Create(AOwner);
end;

Procedure TIDENotebook.ClickMethod(sender : TObject);
begin
inherited ClickMethod(Sender);
end;

Procedure TIDENotebook.DblClickMethod(sender : TObject);
var
I : Integer;
NewLine : String;
Texts : String;
Begin
//need to add the onclick to the source.
   for i := 0 to Project1.Unitlist.Count-1 do
       begin
       if TUnitInfo(Project1.Unitlist.items[i]).Formname = tform(tcontrol(sender).parent).name then break;
       end;

   if I < Project1.Unitlist.Count then
     Begin
     NewLine := 'procedure '+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddControlLine(NewLine);
     texts := 'T'+TUnitInfo(Project1.Unitlist.items[i]).FormName;
     NewLine := 'Procedure '+Texts+'.'+tcontrol(sender).name+'click(Sender : TObject);';
     TUnitInfo(Project1.Unitlist.items[i]).AddProcedureLine(NewLine);
     end;


end;

Function TIDENotebook.Speedbutton(aowner : TComponent; nParent : TWinControl): TSpeedButton;
Begin
result := inherited Speedbutton(aowner,nparent);
end;

{--------------------------------------------------}





initialization
IdeCompList := TList.Create;

finalization
IdeCompList.Destroy;


end.
