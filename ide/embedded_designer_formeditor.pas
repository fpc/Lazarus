{**********************************************************************
          Copyright (c) PilotLogic Software House
                   All rights reserved

 This file is part of CodeTyphon Studio (https://www.pilotlogic.com/)

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************}

unit embedded_designer_formeditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons,
  Math, EnvironmentOpts,
  Embedded_Designer_Basics, IDEWindowIntf, Toolwin,
  Graphics, Dialogs,
  ComCtrls, LMessages,
  LCLIntf,
  LCLType,
  LCLClasses,
  LazLoggerBase,
  intfgraphics, StdCtrls, Menus, ComponentEditors,
  dateutils, types, typinfo;

type

TEmbedHandlerOption=(ehoSendBefore,ehoSendAfter,ehoSendToForm);
TEmbedHandlerOptions=Set of TEmbedHandlerOption;

TEmbedTabSheet = class;
TEmbedFormEditor = class;

TEmbedMessageHandler = class(TPersistent)
  protected
    FHookControl: TControl;
    InheritedWndMethod: TWndMethod;
  public
    FOptions:TEmbedHandlerOptions;
    OnBeforeMessage,
    OnAfterMessage  : TEmbedMessageHandlerEvent;
    constructor Create;
    destructor Destroy; override;
    procedure HookControl(AControl: TControl);
    procedure UnHookControl;
    function  HasHookControl: Boolean;
    procedure SelfWndMethod(var TheMessage: TLMessage);
    Procedure SetDefaulOptions;
    Property TheHookControl: TControl read FHookControl;
  end;

TTEmbedFakeMainMenu = class(TToolBar)
  protected
    FMainMenu:TMainMenu;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    Procedure MainMenuAssign(aMainMenu:TMainMenu);
    Procedure MainMenuClear;
    Procedure MainMenuUpdate;
end;

TEmbedDesignPanel = class(TPanel)
  protected
    FHasLoadedForm: Boolean;
    FLoadedForm: TCustomForm;
    FFakeWindow: TPanel;
    FFakeWindow_MouseDown: Boolean;
    FFakeWindow_MousePos: TPoint;
    FFakeWindowCaption: TPanel;
    FMessagesHandler:TEmbedMessageHandler;
    FFakeMainMenu:TTEmbedFakeMainMenu;
    FMouseArea:integer;
    FExecuteBeforeMessage: Boolean;
    FIniOK:boolean;
    FFormEditor: TFrame;
    OnFormLoad: TNotifyEvent;
    OnLoadedFormChangeBounds: TNotifyEvent;

    FZeroOriginPoint:Tpoint;
    FDeltaPointMousePos: TPoint;

    dsParentPage:TEmbedTabSheet;
    dsParentFormEditor: TEmbedFormEditor;

    procedure InitializeWnd; override;
    procedure HideWindowDesign(aForm: TCustomForm);
    function  FormIsValid(aForm: TCustomForm): Boolean;
    function  LoadedFormIsValid: Boolean;
    function  FormZeroOrigin(aForm: TCustomForm): TPoint;
    Function  CheckForFakeMainMenu(aForm: TCustomForm):Boolean;
    Procedure UpdateFakeMainMenu;
    procedure AddForm(aForm: TCustomForm);
  public
    constructor Create(TheOwner: TComponent; AFormEditor: TFrame); reintroduce;
    destructor Destroy; override;
    procedure EmbeddedForm(Sender: TObject; aForm: TCustomForm);
    procedure Form_BeforeMessage(Sender: TObject; SenderForm: TControl; Msg: TLMessage);
    procedure Form_AfterMessage(Sender: TObject; SenderForm: TControl; Msg: TLMessage);
    procedure FFakeWindowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FFakeWindowMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FFakeWindowMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FFakeWindowResize(Sender: TObject);
    procedure FFakeWindowCaptionPaint(Sender: TObject);
    Function  FindMouseArea(const X,Y:integer):integer;
    procedure UnLoadLoadedForm;

    property IniOK:boolean read FIniOK write FIniOK;
    property LoadedForm: TCustomForm read FLoadedForm;
  end;

  { TEmbedFormEditor }

  TEmbedFormEditor = class(TFrame)
    imgWindowScreenPreview: TImage;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelW: TLabel;
    LabelH: TLabel;
    pnShowHide: TPanel;
    RootPanel: TPanel;
    pnInfoPanel: TPanel;
    pnScreenPreviewBtns: TPanel;
    pnCaptionWindowScreenPreview: TPanel;
    pnScreenPreview: TPanel;
    pnWindowScreenPreview: TPanel;
    pnDesignBox: TScrollBox;
    sb_hide: TSpeedButton;
    sb_zero: TSpeedButton;
    sb_up: TSpeedButton;
    sb_bottom: TSpeedButton;
    sb_left: TSpeedButton;
    sb_right: TSpeedButton;
    sb_update: TSpeedButton;
    procedure imgWindowScreenPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure imgWindowScreenPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure imgWindowScreenPreviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sb_upClick(Sender: TObject);
    procedure sb_hideClick(Sender: TObject);
  private
    FPreviewMoving: boolean;
    FPreviewMovingPoint: TPoint;
    FDesignAreaViewMoving: boolean;
    FDesignAreaViewMovingPoint: TPoint;
  public
    FDesignPanel: TEmbedDesignPanel;
    function  PixelPerc(vl, perc: single): integer;
    procedure DesignPanel_OnLoadForm(Sender: TObject);
    procedure DesignPanel_OnLoadedFormChangeBounds(Sender: TObject);
    procedure SetupPreviewFormDesign(_Form: TCustomForm);
    Procedure ActivatedDesigner;
  end;

 TEmbedTabSheet = class(TTabSheet)
  private
    FOnToggleFormOrSource:TNotifyEvent;
    Function GetLoadedForm:TCustomForm;
  public
    FEditPages : TPageControl;
    FEditPageForSource : TTabSheet;
    FEditPageForDesigner : TTabSheet;

    FFormEditor: TEmbedFormEditor;
    FSynEditor : TCustomControl;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoOnTabControlChange(Sender: TObject);

    procedure ShowSourceTab;
    procedure ShowFormEditorTab;

    procedure EditPages_Create;
    procedure EditPages_Free;

    procedure FormEditorsInit;
    Procedure FormEditorDelete;
    Procedure FormEditorPause;

    Procedure FormEditorAdd;
    Procedure EmbeddedForm(Sender: TObject; aForm: TCustomForm);

    Procedure UpdateEmbedFormDsgSettings;

    property LoadedForm: TCustomForm read GetLoadedForm;

    property  OnToggleFormOrSource:TNotifyEvent read FOnToggleFormOrSource write FOnToggleFormOrSource;
  end;

implementation

{$R *.lfm}

var
  EMBEDDED_FORM_DESIGNER: PLazLoggerLogGroup;

// ==============================================================================
// =========================== TEmbedFormEditor ===================================
// ==============================================================================

Procedure TEmbedFormEditor.ActivatedDesigner;
begin
  if Embedded_Updating then exit;

  if FDesignPanel=nil then Exit;
  if FDesignPanel.LoadedFormIsValid=false then Exit;
  if FDesignPanel.FLoadedForm=nil then exit;

//------------------------------------------
//Fix FDesignPanel Geometry
  if FDesignPanel.FIniOK=false then
   begin
     FDesignPanel.Visible:=true;
     self.Visible:=true;

     FDesignPanel.FLoadedForm.SetBounds(FDesignPanel.FLoadedForm.Left,
                                        FDesignPanel.FLoadedForm.Top,
                                        FDesignPanel.FLoadedForm.Width+1,
                                        FDesignPanel.FLoadedForm.Height);
     FDesignPanel.FLoadedForm.SetBounds(FDesignPanel.FLoadedForm.Left,
                                        FDesignPanel.FLoadedForm.Top,
                                        FDesignPanel.FLoadedForm.Width-1,
                                        FDesignPanel.FLoadedForm.Height);

     FDesignPanel.FIniOK:=true;

     LCLIntf.ShowWindow(FDesignPanel.FLoadedForm.Handle,SW_SHOWNORMAL);
   end;

//--------------------------------------
// Activate Embedded Form Designer
// Call main.pp procedure TMainIDE.OnDesignerActivated(Sender: TObject);

  if FDesignPanel.FLoadedForm.Designer<>nil then
    TComponentEditorDesigner(FDesignPanel.FLoadedForm.Designer).IsActive:=true;

end;

procedure TEmbedFormEditor.SetupPreviewFormDesign(_Form: TCustomForm);
begin

  Self.DoubleBuffered := True;
  if (FPreviewMoving) then  Exit;

  with pnWindowScreenPreview do
  begin
    Width  := PixelPerc(_Form.Width , CN_PREVIEW_FACTOR);
    Height := PixelPerc(_Form.Height, CN_PREVIEW_FACTOR);
    Top    := PixelPerc(_Form.Top   , CN_PREVIEW_FACTOR);
    Left   := PixelPerc(_Form.Left  , CN_PREVIEW_FACTOR);
  end;

  with pnCaptionWindowScreenPreview do
  begin
    Height := PixelPerc(Parent.Height, 15);
  end;

end;

procedure TEmbedFormEditor.DesignPanel_OnLoadForm(Sender: TObject);
begin
  //-----------------------
end;

procedure TEmbedFormEditor.DesignPanel_OnLoadedFormChangeBounds(Sender: TObject);
begin
  if FDesignPanel.LoadedFormIsValid then
  begin
    SetupPreviewFormDesign(FDesignPanel.FLoadedForm);
  end;

end;

function TEmbedFormEditor.PixelPerc(vl, perc: single): integer;
begin
  Result := Round((vl / 100) * perc);
end;


//------------------------ imgWindowScreenPreview Mouse Events ----------------------
procedure TEmbedFormEditor.imgWindowScreenPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FDesignPanel=nil then Exit;
  if FDesignPanel.LoadedFormIsValid=false then Exit;

  SetCapture(pnWindowScreenPreview.Handle);
  FPreviewMoving := True;
  FPreviewMovingPoint.X := x;
  FPreviewMovingPoint.Y := Y;

  pnInfoPanel.Visible:=True;
end;

procedure TEmbedFormEditor.imgWindowScreenPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
 Var R:TRect;
begin
  if FPreviewMoving=false then exit;

  pnWindowScreenPreview.SetBounds(pnWindowScreenPreview.Left - (FPreviewMovingPoint.x - X),
                                  pnWindowScreenPreview.Top  - (FPreviewMovingPoint.Y - Y),
                                  pnWindowScreenPreview.Width,
                                  pnWindowScreenPreview.Height  );

  //............................................................
  R:=Rect(round(((max(pnWindowScreenPreview.Left - (FPreviewMovingPoint.X - X), 0)) / pnWindowScreenPreview.Parent.Width)  * Screen.Width ),
          round(((max(pnWindowScreenPreview.Top  - (FPreviewMovingPoint.Y - Y), 0)) / pnWindowScreenPreview.Parent.Height) * Screen.Height),
          FDesignPanel.FLoadedForm.Width,
          FDesignPanel.FLoadedForm.Height );

  LabelX.Caption:='X: '+IntToStr(R.Left);
  LabelY.Caption:='Y: '+IntToStr(R.Top);
  LabelW.Caption:='W: '+IntToStr(R.Right);
  LabelH.Caption:='H: '+IntToStr(R.Bottom);
end;

procedure TEmbedFormEditor.imgWindowScreenPreviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FPreviewMoving then
  begin
    ReleaseCapture;
    FPreviewMoving := False;
    pnInfoPanel.Visible:=False;

    with pnWindowScreenPreview do
    begin
      Left := max(Left - (FPreviewMovingPoint.X - X), 0);
      Top :=  max(Top -  (FPreviewMovingPoint.Y - Y), 0);

      if FDesignPanel.LoadedForm<>nil then
      begin
        FDesignPanel.FLoadedForm.SetBounds(round((Left / Parent.Width)  * Screen.Width ),
                                           round((Top  / Parent.Height) * Screen.Height),
                                           FDesignPanel.FLoadedForm.Width,
                                           FDesignPanel.FLoadedForm.Height );

      end;
    end;
  end;
end;

//------------------------ Mouse Events ----------------------

procedure TEmbedFormEditor.sb_upClick(Sender: TObject);
begin
  if FDesignPanel=nil then Exit;
  if FDesignPanel.LoadedFormIsValid=false then Exit;

  imgWindowScreenPreviewMouseDown(nil,mbLeft, [ssLeft], 0, 0);

  if Sender = sb_up then pnWindowScreenPreview.Top  := pnWindowScreenPreview.Top  - 5;
  if Sender = sb_bottom then pnWindowScreenPreview.Top  := pnWindowScreenPreview.Top  + 5;
  if Sender = sb_left then pnWindowScreenPreview.Left := pnWindowScreenPreview.Left - 5;
  if Sender = sb_right then pnWindowScreenPreview.Left := pnWindowScreenPreview.Left + 5;
  if Sender = sb_zero then
                                begin
                                   pnWindowScreenPreview.Top  := CN_BORDER_PX;
                                   pnWindowScreenPreview.Left := CN_BORDER_PX;
                                end;

  if Sender = sb_update then  // Update button 5555
                                begin
                                   ActivatedDesigner;
                                   DesignPanel_OnLoadForm(nil);
                                   DesignPanel_OnLoadedFormChangeBounds(nil);
                                   FDesignPanel.Paint;
                                   FDesignPanel.FFakeWindowCaption.Repaint;
                                end;

  imgWindowScreenPreviewMouseUp( nil,mbLeft, [ssLeft], 0, 0);
end;

procedure TEmbedFormEditor.sb_hideClick(Sender: TObject);
begin
  if pnScreenPreview.Visible then
    begin
      pnScreenPreview.Visible:=False;
      pnScreenPreviewBtns.Visible:=False;
    end else
    begin
      pnScreenPreview.Visible:=True;
      pnScreenPreviewBtns.Visible:=True;
    end;
end;

//===========================================================================
//======================= TEmbedDesignPanel ====================================
//===========================================================================

constructor TEmbedDesignPanel.Create(TheOwner: TComponent; AFormEditor: TFrame);
begin
  inherited Create(TheOwner);
  FLoadedForm:=nil;
  FFakeMainMenu:=nil;
  dsParentPage:=nil;
  dsParentFormEditor:=nil;
  FFormEditor := AFormEditor;

  SetInitialBounds(0,0,screen.Width,Screen.Height);

  BevelInner := bvNone;
  BevelOuter := bvNone;

  FHasLoadedForm := False;
  FIniOK:=false;

  FFakeWindow_MouseDown := false;

  FExecuteBeforeMessage := True;

  FMessagesHandler:=TEmbedMessageHandler.Create;

  FFakeWindow := TPanel.Create(Self);
  with FFakeWindow do
  begin
    BorderWidth := CN_BORDER_PX;
    OnResize    := @FFakeWindowResize;

    OnMouseDown := @FFakeWindowMouseDown;
    OnMouseMove := @FFakeWindowMouseMove;
    OnMouseUp   := @FFakeWindowMouseUp;
  end;

  FFakeWindowCaption := TPanel.Create(FFakeWindow);
  with FFakeWindowCaption do
  begin
    Parent := FFakeWindow;
    Align := alTop;
    Height := CN_CAPTION_HEIGHT;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    ParentColor := False;
    Color := clActiveCaption;
    OnPaint := @FFakeWindowCaptionPaint;
  end;
end;

destructor TEmbedDesignPanel.Destroy;
begin
  if FormIsValid(FLoadedForm) then
    UnLoadLoadedForm;
  FMessagesHandler.Free;
  inherited Destroy;
end;

procedure TEmbedDesignPanel.EmbeddedForm(Sender: TObject; aForm: TCustomForm);
begin
  if EnvironmentOptions.UseEmbeddedDesigner = False then
    Exit;
  if Embed_IsDesignForm(aForm) then
    AddForm(aForm);
end;

procedure TEmbedDesignPanel.AddForm(AForm: TCustomForm);
begin
  if AForm=nil then
    Exit;
  if FormIsValid(FLoadedForm) then
    Exit;
  if (aForm.ParentWindow = Self.Handle) or (aForm.Parent = Self) then
    Exit;
  FLoadedForm := AForm;
  HideWindowDesign(FLoadedForm);
  if dsParentPage <> nil then
    dsParentPage.FormEditorAdd;
  FMessagesHandler.UnHookControl;
  FMessagesHandler.HookControl(FLoadedForm);
  FMessagesHandler.OnBeforeMessage := @Self.Form_BeforeMessage;
  FMessagesHandler.OnAfterMessage  := @Self.Form_AfterMessage;
  FHasLoadedForm := True;
end;

procedure TEmbedDesignPanel.UnLoadLoadedForm;
begin
  if LoadedFormIsValid=False then Exit;
  try
    if FFakeWindow<>nil then FFakeWindow.Visible := false;
    if FMessagesHandler.HasHookControl then FMessagesHandler.UnHookControl;
    FLoadedForm:=nil;
  finally
    FHasLoadedForm := False;
  end;
end;

function TEmbedDesignPanel.FormIsValid(aForm: TCustomForm): Boolean;
begin
  Result := (Assigned(aForm)) and (aForm <> Nil);
end;

function TEmbedDesignPanel.LoadedFormIsValid: Boolean;
begin
  Result := FormIsValid(FLoadedForm);
end;

procedure TEmbedDesignPanel.HideWindowDesign(aForm: TCustomForm);
begin
  if (aForm = Nil) or (not Assigned(aForm)) then Exit;

  ShowWindow(aForm.Handle, SW_HIDE);
end;

function TEmbedDesignPanel.FormZeroOrigin(aForm: TCustomForm): TPoint;

//------------------------------------------
  procedure _FindFormBorder;
    var
        w: TWinControl;
        rc1, rc2: TRect;
   begin
    if CN_REMOVE_FORM_BORDER=False then exit;

    try
      w := TWinControl.Create(Self);
      w.Left := 0;
      w.Top := 0;
      w.Parent := aForm;

      GetWindowRect(w.Handle, rc1);
      GetWindowRect(aForm.Handle, rc2);

      Result.x := Max(rc1.Left - rc2.Left, 0);
      Result.y := Max(rc1.Top  - rc2.Top,  0);
    finally
      w.free;
    end;
   end;

//------------------------------------------
  procedure _FindCompSizes;
    var i: Integer;
   begin
     if CN_MUST_FIND_MAINMENU_HEIGHT=False then Exit;

     for i := 0 to aForm.ComponentCount - 1 do
      begin
        if aForm.Components[i] is TMainMenu then
         if TMainMenu(aForm.Components[i]).Items.Count>0 then // 8888  Don't add Height of TmainMenu with No Items
          begin
            Result.y := Result.y - LCLIntf.GetSystemMetrics(SM_CYMENU);
            Exit;
          end;
      end;
   end;

//-----------------------------------------
begin
Result := Point(0,0);

if FormIsValid(aForm)=false then exit;

if FFakeWindow_MouseDown then
begin
 result:=FZeroOriginPoint;
 EXIT;
end;

_FindFormBorder;
_FindCompSizes;

FZeroOriginPoint:=Result;
end;

Function  TEmbedDesignPanel.CheckForFakeMainMenu(aForm: TCustomForm):Boolean;
    var i: Integer;
        xMainMenu:TMainMenu;
begin

Result:=False;

if FormIsValid(aForm)=False then Exit;
if Embed_IsDesignForm(aform)=false then Exit;
if CN_USE_FAKE_MAINMENU=False then Exit;
if FFakeWindow_MouseDown then EXIT;

xMainMenu:=nil;

//..... Find Form MainMenu ...
for i := 0 to aForm.ComponentCount - 1 do
  begin
    if aForm.Components[i] is TMainMenu then
      if TMainMenu(aForm.Components[i]).Items.Count>0 then
       begin
        xMainMenu:=TMainMenu(aForm.Components[i]);
       end;
  end;

Result:=(xMainMenu<>nil);

//............................
if xMainMenu=nil then
begin
  if FFakeMainMenu<>nil then
    begin
      FFakeMainMenu.Free;
      FFakeWindowCaption.Height:=CN_CAPTION_HEIGHT;
    end;
    FFakeMainMenu:=nil;
end else
begin
  if FFakeWindow=nil then Exit;

  if FFakeMainMenu=nil then
  begin
     FFakeMainMenu:=TTEmbedFakeMainMenu.Create(FFakeWindowCaption);
     FFakeMainMenu.Parent:=FFakeWindowCaption;
     FFakeMainMenu.MainMenuAssign(xMainMenu);
     FFakeWindowCaption.Height:=CN_CAPTION_HEIGHT+FFakeMainMenu.Height;
  end;
end;
end;

Procedure TEmbedDesignPanel.UpdateFakeMainMenu;
begin
if CN_USE_FAKE_MAINMENU=False then Exit;
if FFakeMainMenu=nil then Exit;

FFakeMainMenu.MainMenuUpdate;
end;

//=============================== Before LoadedForm Message =====================
procedure TEmbedDesignPanel.Form_BeforeMessage(Sender: TObject; SenderForm: TControl; Msg: TLMessage);
var
  pRect: TRect;
//..................................................................
  procedure _AdjustSelfPosition(aForm: TCustomForm);
  begin

    if aForm.ParentWindow <> Self.Handle then
    begin
      Self.SetBounds(0,0,Self.Width,Self.Height);
    end;

    GetWindowRect(Self.Handle, pRect);

    Self.SetBounds(-(pRect.Left - Self.Left),
                   -(pRect.Top  - Self.Top),
                     Self.Width,
                     Self.Height  );
  end;
//..................................................................
begin
if NOT FExecuteBeforeMessage     then Exit;
if NOT (Sender=FMessagesHandler) then Exit;
if LoadedFormIsValid=false       then Exit;
if TCustomForm(SenderForm)<>FLoadedForm then Exit;

//debugln('TEmbedDesignPanel.Form_BeforeMessage : '+GetMessageName(Msg.msg)); //--- ct7777 -----

//--------------------------------------------------------

           if NOT (csDestroying in FLoadedForm.ComponentState) then
            if ((CN_FORM_DESIGNER_PARENT_TYPE=ctptParentWindow) and (FLoadedForm.ParentWindow <> Self.Handle)) or
                 ((CN_FORM_DESIGNER_PARENT_TYPE=ctptParent) and (FLoadedForm.Parent <> Self)) or
                 ( CN_FORM_DESIGNER_PARENT_TYPE=ctptDock) or
                 ( CN_FORM_DESIGNER_PARENT_TYPE=ctptNone) then
               begin
                   try
                         FExecuteBeforeMessage := False;
                         FLoadedForm.BeginUpdateBounds;
                         pRect.Left   := FLoadedForm.Left;
                         pRect.Top    := FLoadedForm.Top;
                         pRect.Right  := FLoadedForm.Left + FLoadedForm.Width;
                         pRect.Bottom := FLoadedForm.Top  + FLoadedForm.Height;

                         case CN_FORM_DESIGNER_PARENT_TYPE of
                           ctptParentWindow  : FLoadedForm.ParentWindow := Self.Handle;
                           ctptParent        : FLoadedForm.Parent := Self;
                           ctptDock          : FLoadedForm.Dock(Self, pRect);
                           ctptNone          : ;
                         end;

                    finally
                           FLoadedForm.EndUpdateBounds;
                           FExecuteBeforeMessage := True;
                    end;

               if Assigned(OnFormLoad) then OnFormLoad(Self);
               if Assigned(OnLoadedFormChangeBounds) then  OnLoadedFormChangeBounds(Self);
              end;

 //--------------------------------------------------------
end;

//=============================== After LoadedForm Message =====================
procedure TEmbedDesignPanel.Form_AfterMessage(Sender: TObject; SenderForm: TControl; Msg: TLMessage);
var pRect: TRect;

//..................................................................
  procedure _UpdateFakeWindow(aForm: TCustomForm);
   var zoPoint:TPoint;
  begin
      FFakeWindow.Visible := ((aForm <> Nil) and (Assigned(aForm)));

      if FFakeWindow.Visible then
      begin
        zoPoint:=FormZeroOrigin(aForm);
        FFakeWindow.SetBounds(
                              ((aForm.Left)  - (FFakeWindowCaption.Left)) + zoPoint.x,
                              ((aForm.Top )  - (FFakeWindowCaption.Top + FFakeWindowCaption.Height)) + zoPoint.y,
                                aForm.Width  + (FFakeWindowCaption.Left * 2),
                                aForm.Height + (FFakeWindowCaption.Top + FFakeWindowCaption.Height) + FFakeWindowCaption.Top
                              );
      end;

      if FFakeWindow_MouseDown then exit;
      FFakeWindowResize(FFakeWindow);
  end;
//..................................................................
  procedure _UpdateLoadedFormBorder(aForm: TCustomForm);
  var
    xRegion: TRegion;
  begin
    if CN_REMOVE_FORM_BORDER=false then Exit;

    with aForm, FormZeroOrigin(aForm) do
    begin
      xRegion := TRegion.Create;
       try
         xRegion.AddRectangle(  x,
                                y,
                                x + Width,
                                y + Height );

         aForm.SetShape(xRegion);
       finally
         xRegion.Free;
      end;

    end;
  end;
//..................................................................
  procedure _UpdateFormDesigner(aForm: TCustomForm);
  begin
    with TCustomForm(aForm) do UpdateControlState;
  end;
//..................................................................
  procedure _UpdateSelf;
    var R:TRect;
   begin

     if FFakeWindow.Visible then
         R:=Rect((-FFakeWindow.Left) + 5,(-FFakeWindow.Top)  + 5, Screen.Width, Screen.Height) else
         R:=Rect((-FLoadedForm.Left) + 5,(-FLoadedForm.Top)  + 5, Screen.Width, Screen.Height);

      Self.SetBounds(R.Left, R.Top, R.Right, R.Bottom);
   end;
//..................................................................
begin
if FHasLoadedForm=false then exit;
if NOT (Sender=FMessagesHandler) then Exit;
if LoadedFormIsValid=false then Exit;
if TCustomForm(SenderForm)<>FLoadedForm  then Exit;

if (csDestroying in FLoadedForm.ComponentState) then Exit;
if (csFreeNotification in FLoadedForm.ComponentState) then Exit;
if (csLoading in FLoadedForm.ComponentState) then Exit;
if (csUpdating in FLoadedForm.ComponentState) then Exit;
if (csReading in FLoadedForm.ComponentState) then Exit;
if (csWriting in FLoadedForm.ComponentState) then Exit;


//--------------------------------
case Msg.msg of
LM_SHOWWINDOW:
              begin
              if TLMShowWindow(Msg).Show then
                begin

                  if CN_USE_FAKE_MAINMENU then
                    if CheckForFakeMainMenu(FLoadedForm) then
                       UpdateFakeMainMenu;

                  _UpdateFakeWindow(FLoadedForm);
                  _UpdateLoadedFormBorder(FLoadedForm);
                  _UpdateSelf;
                end;
              end;

LM_ACTIVATE,
LM_SETFOCUS,
LM_MOVE,
LM_SIZE:
             begin
                 _UpdateFakeWindow(FLoadedForm);
                 _UpdateLoadedFormBorder(FLoadedForm);
                 _UpdateSelf;
                 _UpdateFormDesigner(FLoadedForm);
                 if Assigned(OnLoadedFormChangeBounds) then  OnLoadedFormChangeBounds(Self);
             end;

CM_TEXTCHANGED:    //Change Stored Form/Module/Frame Caption/Name
             begin
                 FFakeWindowCaption.Repaint;
             end;

CM_MENUCHANGED:   // for Fake MainMemu added
             begin
                if CN_USE_FAKE_MAINMENU=false then exit;

                if FFakeMainMenu=nil then   // Used Only for Init Greate FakeMainMenu
                 if CheckForFakeMainMenu(FLoadedForm) then
                   begin
                    UpdateFakeMainMenu;
                    _UpdateFakeWindow(FLoadedForm);
                    _UpdateLoadedFormBorder(FLoadedForm);
                    _UpdateSelf;
                    if Assigned(OnLoadedFormChangeBounds) then  OnLoadedFormChangeBounds(Self);
                   end;

             end;

LM_ERASEBKGND,LM_PAINT:        // for Fake MainMemu Deleted
             begin
                if CN_USE_FAKE_MAINMENU=false then exit;
                if FFakeMainMenu=nil then exit;    //Used Only for Init Greate FakeMainMenu

                if CheckForFakeMainMenu(FLoadedForm) then   //  FakeMainMenu Exists so Update Items
                 begin
                   UpdateFakeMainMenu;
                 end else                                   //  FakeMainMenu Deleted so Update DesignPanel
                 begin
                   _UpdateFakeWindow(FLoadedForm);
                   _UpdateLoadedFormBorder(FLoadedForm);
                   _UpdateSelf;
                   if Assigned(OnLoadedFormChangeBounds) then  OnLoadedFormChangeBounds(Self);
                 end;
             end;

end;
end;

//Result=0 Mouse is in LoadedForm
//Result=1 Mouse in Bottom-Righr Corner
//Result=2 Mouse in Righr Bar
//Result=3 Mouse in Bottom Bar
Function TEmbedDesignPanel.FindMouseArea(const X,Y:integer):integer;
begin
   result:=0;

   if (x < FFakeWindow.Width   - 4*CN_BORDER_PX) and
      (y >= FFakeWindow.Height - CN_BORDER_PX)
    then  begin result:=3; exit; end;   // Bottom Bar

   if (x >= FFakeWindow.Width  - CN_BORDER_PX) and
      (y <  FFakeWindow.Height - 4*CN_BORDER_PX)
    then  begin result:=2; exit; end;   // Right Bar

   if (x >= FFakeWindow.Width  - CN_BORDER_PX) or
      (y >= FFakeWindow.Height - CN_BORDER_PX)
    then begin result:=1; exit; end;    // Bottom-Righr Corner

end;

procedure TEmbedDesignPanel.FFakeWindowMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if LoadedFormIsValid=false then Exit;

  FFakeWindow_MouseDown := false;

  if
    (x >= FFakeWindow.Width  - CN_BORDER_PX) or
    (y >= FFakeWindow.Height - CN_BORDER_PX) then
  begin
    FFakeWindow_MouseDown := True;
    GetCursorPos(FFakeWindow_MousePos);

    FDeltaPointMousePos:=Point(FFakeWindowCaption.Left-X,FFakeWindowCaption.Top-Y);

    dsParentFormEditor.pnInfoPanel.Visible:=True;
  end;

end;

procedure TEmbedDesignPanel.FFakeWindowMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
const
  minWidth = 120;
  minHeight = 50;
var
  newPos: TPoint;
  frmPoint : TPoint;
  w, h: Integer;
begin

if Sender= nil then Exit;

with TWinControl(Sender) do
begin

  if FFakeWindow_MouseDown=false then  // NO Resize Mode
    begin
     FMouseArea:=FindMouseArea(x,y);
     case FMouseArea of
       1: Cursor := crSizeNWSE; // Bottom-Right Corner
       2: Cursor := crSizeWE;   // Right Bar
       3: Cursor := crSizeNS;   // Bottom Bar
     else
       Cursor := crDefault;
     end;
  end else                            // Resize Mode
  begin
      GetCursorPos(newPos);
      frmPoint := ScreenToClient(Mouse.CursorPos);

      if CN_REMOVE_FORM_BORDER then
       begin
         frmPoint.X:=frmPoint.X-FZeroOriginPoint.X;
         frmPoint.Y:=frmPoint.Y-FZeroOriginPoint.Y;
       end else
       begin
         frmPoint.X:=frmPoint.X-CN_BORDER_PX;
         frmPoint.Y:=frmPoint.Y-CN_BORDER_PX-FFakeWindowCaption.Height;
       end;

      if frmPoint.X<minWidth  then frmPoint.X:=minWidth;
      if frmPoint.Y<minHeight then frmPoint.Y:=minHeight;


       dsParentFormEditor.LabelX.Caption:='X: '+IntToStr(FLoadedForm.Left);
       dsParentFormEditor.LabelY.Caption:='Y: '+IntToStr(FLoadedForm.Top);
       dsParentFormEditor.LabelW.Caption:='W: '+IntToStr(frmPoint.X);
       dsParentFormEditor.LabelH.Caption:='H: '+IntToStr(frmPoint.Y);


       if LoadedForm<>nil then     // Resize Only if Loaded Form is Valid
       begin
          case FMouseArea of
             1: begin
                 FLoadedForm.SetBounds(FLoadedForm.Left, FLoadedForm.Top, frmPoint.X, frmPoint.Y);
                end;
             2: begin
                 FLoadedForm.SetBounds(FLoadedForm.Left, FLoadedForm.Top, frmPoint.X, FLoadedForm.Height);
                end;
             3: begin
                 FLoadedForm.SetBounds(FLoadedForm.Left, FLoadedForm.Top, FLoadedForm.Width, frmPoint.Y);
                end;
          end;
       end;
    end;
end;
end;

procedure TEmbedDesignPanel.FFakeWindowMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFakeWindow_MouseDown then
  begin
    FFakeWindow_MouseDown := False;
    dsParentFormEditor.pnInfoPanel.Visible:=False;
  end;
end;

procedure TEmbedDesignPanel.FFakeWindowResize(Sender: TObject);
var
  fk_rgn, fm_rgn, Region, Region2 : HRGN;
  fk_rc, fm_rc: TRect;
  xRegion : TRegion;
begin

  fk_rc.Left   := 0;
  fk_rc.Top    := 0;
  fk_rc.Right  := FFakeWindow.Width;
  fk_rc.Bottom := FFakeWindow.Height;

  with fm_rc do
  begin
    Left   := FFakeWindowCaption.Left;
    Top    := FFakeWindowCaption.Top + FFakeWindowCaption.Height;
    Right  := Left + FFakeWindowCaption.Width;
    Bottom := FFakeWindow.Height - (FFakeWindowCaption.Top);
  end;

  fk_rgn := CreateRectRgn(fk_rc.Left, fk_rc.Top, fk_rc.Right, fk_rc.Bottom);
  fm_rgn := CreateRectRgn(fm_rc.Left, fm_rc.Top, fm_rc.Right, fm_rc.Bottom);

  CombineRgn(fk_rgn, fk_rgn, fm_rgn, RGN_DIFF);

  //........................................
  GetRgnBox(fk_rgn,PRect(@fk_rc));

  xRegion := TRegion.Create;
       try
         xRegion.AddRectangle(
                                fk_rc.Left,
                                fk_rc.Top,
                                fk_rc.Right,
                                fk_rc.Bottom);

         FFakeWindow.SetShape(xRegion);
       finally
         xRegion.Free;
      end;
  FFormEditor.Update;
end;

procedure TEmbedDesignPanel.FFakeWindowCaptionPaint(Sender: TObject);
var
  R: TRect;
begin
  if LoadedFormIsValid=false then Exit;
  FFakeWindowCaption.Canvas.Font.Color := clHighlightText;
  R := FFakeWindowCaption.ClientRect;
  R.Left := R.Left + 5;
  DrawText(FFakeWindowCaption.Canvas.Handle, PChar(FLoadedForm.Caption), -1, R, DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
end;

procedure TEmbedDesignPanel.InitializeWnd;
var fm: TForm;
begin
  inherited InitializeWnd;

  with FFakeWindow do
  begin
    Parent := Self;
    ParentColor := False;
    Color := clForm;
  end;

end;

//===========================================================================
//======================== TEmbedMessageHandler =============================
//===========================================================================

constructor TEmbedMessageHandler.Create;
begin
  inherited Create;
  SetDefaulOptions;
  FHookControl    := nil;
  OnBeforeMessage := nil;
  OnAfterMessage  := nil;
end;

destructor TEmbedMessageHandler.Destroy;
begin
  UnHookControl;
  inherited Destroy;
end;

Procedure TEmbedMessageHandler.SetDefaulOptions;
begin
 FOptions:=[ehoSendBefore,ehoSendAfter,ehoSendToForm];
end;

procedure TEmbedMessageHandler.HookControl(AControl: TControl);
begin
  SetDefaulOptions;

  if AControl=nil then exit;
  FHookControl := AControl;
  InheritedWndMethod :=  FHookControl.WindowProc; //Save Control WindowProc
  FHookControl.WindowProc := @SelfWndMethod;      //Change Control WindowProc
end;

procedure TEmbedMessageHandler.UnHookControl;
begin
  OnBeforeMessage := nil;
  OnAfterMessage  := nil;
  if NOT HasHookControl then Exit;
  FHookControl.WindowProc:=InheritedWndMethod;   //Restore Control WindowProc
  FHookControl:=nil;
  InheritedWndMethod:=nil;

  SetDefaulOptions;
end;

function TEmbedMessageHandler.HasHookControl: Boolean;
begin
  Result := (Assigned(FHookControl)) and (FHookControl <> Nil);
end;

procedure TEmbedMessageHandler.SelfWndMethod(var TheMessage: TLMessage);
begin
  if (not HasHookControl) then Exit;

// debugln('TEmbedMessageHandler.SelfWndMethod : '+GetMessageName(TheMessage.msg)); //--- ct7777 -----

  case TheMessage.msg of
     LM_SHOWWINDOW,
     LM_ACTIVATE,
     LM_SETFOCUS,
     LM_MOVE,
     LM_SIZE,
     LM_ERASEBKGND,
     LM_PAINT,
     CM_MENUCHANGED,
     CM_TEXTCHANGED
                :begin
                  if (ehoSendBefore in Foptions) and Assigned(OnBeforeMessage)    then OnBeforeMessage(Self, FHookControl, TheMessage);
                  if (ehoSendToForm in Foptions) and Assigned(InheritedWndMethod) then InheritedWndMethod(TheMessage);
                  if (ehoSendAfter in Foptions)  and Assigned(OnAfterMessage)     then OnAfterMessage(Self, FHookControl, TheMessage);
                 end;
  else
     if (ehoSendToForm in Foptions) and Assigned(InheritedWndMethod) then InheritedWndMethod(TheMessage);
  end;

end;


//===========================================================================
//======================= TTEmbedFakeMainMenu ===============================
//===========================================================================

constructor TTEmbedFakeMainMenu.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMainMenu:=nil;
  Color:=clMenuBar;
  EdgeBorders:=[ebTop,ebBottom];
  ShowCaptions:=true;
  Align:=alBottom;
  List:=True;
end;

destructor TTEmbedFakeMainMenu.Destroy;
begin
  MainMenuClear;
  inherited Destroy;
end;

Procedure TTEmbedFakeMainMenu.MainMenuAssign(aMainMenu:TMainMenu);
begin
 FMainMenu:=aMainMenu;
 if FMainMenu=nil then exit;

 self.Images:=FMainMenu.Images;
end;

Procedure TTEmbedFakeMainMenu.MainMenuClear;
  var i:Integer;
begin

 for i := ButtonCount - 1 downto 0 do
   begin
     TToolButton(Buttons[i]).MenuItem:=nil;
     TToolButton(Buttons[i]).Free;
   end;

 ButtonList.Clear;
end;

Procedure TTEmbedFakeMainMenu.MainMenuUpdate;
  var i:Integer;
      xMenu    :TMenuItem;
      xButton  :TToolButton;

  Procedure _SetMenuItem(const val:integer);
  begin
      xMenu:=FMainMenu.Items[i];

      if xMenu<>nil then
       if xMenu.Visible then
       begin
        xButton:=TToolButton.Create(self);
        xButton.Parent:=self;
        xButton.Name:=xMenu.Name;
        xButton.Caption :=xMenu.Caption;
        xButton.MenuItem:=xMenu;
       end;
    end;

begin
if FMainMenu=nil then exit;

MainMenuClear;

if FMainMenu.IsRightToLeft=false then
begin
  for i:=FMainMenu.Items.Count-1 Downto 0 do _SetMenuItem(i);
end else
begin
  for i:=0 to FMainMenu.Items.Count-1     do _SetMenuItem(i);
end;

end;

//===========================================================================
//========================== TEmbedTabSheet =================================
//===========================================================================

constructor TEmbedTabSheet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditPages:=nil;
  FEditPageForSource:=nil;
  FEditPageForDesigner:=nil;
  FFormEditor:=nil;
  FSynEditor:=nil;
end;

destructor TEmbedTabSheet.Destroy;
begin
  inherited Destroy;
end;


procedure TEmbedTabSheet.EditPages_Create;
begin

  if FEditPages<>nil then exit;

  Embedded_Updating:=True;

  //-----------------
  FEditPages:= TPageControl.Create(self);
  FEditPages.Parent:=self;
  FEditPages.Align:=alClient;//alBottom;
  FEditPages.TabPosition:=tpBottom;
  FEditPages.ShowTabs := EnvironmentOptions.UseEmbeddedDesigner;

  //--- page for Source code SynEdit ----
  FEditPageForSource:=FEditPages.AddTabSheet;
  FEditPageForSource.Name:='EditPageForSource1';
  FEditPageForSource.Caption:='Source';
  FEditPageForSource.TabVisible:=true;

  //--- page for FormDesigner -----------
  FEditPageForDesigner:=FEditPages.AddTabSheet;
  FEditPageForDesigner.Name:='EditPageForDesigner1';
  FEditPageForDesigner.Caption:='Designer';
  FEditPageForDesigner.TabVisible:=false;


  FEditPages.OnChange:=@DoOnTabControlChange;

  Embedded_Updating:=False;
end;

procedure TEmbedTabSheet.EditPages_Free;
begin
  if FEditPages=nil then exit;

  Embedded_Updating:=True;

  FormEditorDelete;
  FEditPages.Free;

  FEditPages:=nil;
  FEditPageForSource:=nil;
  FEditPageForDesigner:=nil;

  Embedded_Updating:=False;
end;

procedure TEmbedTabSheet.DoOnTabControlChange(Sender: TObject);
begin
  if Embedded_Updating then exit;
  if FEditPages=nil then exit;

  if FEditPages.ActivePage=FEditPageForDesigner then
    if FFormEditor<>nil then
      FFormEditor.ActivatedDesigner;
end;

procedure TEmbedTabSheet.ShowSourceTab;
begin
  if Embedded_Updating then exit;

 if (FEditPages.ActivePage<>FEditPageForSource) then
     FEditPages.ActivePage:=FEditPageForSource;

 if FSynEditor<>nil then FSynEditor.SetFocus;
end;

procedure TEmbedTabSheet.ShowFormEditorTab;
 var xform:TCustomForm;
begin
 if Embedded_Updating then exit;

 if FEditPages=nil then exit;

//------------------------------------- Check FFormEditor
 xform:=nil;

  if FFormEditor<>nil then
    if FFormEditor.FDesignPanel<>nil then
       if FFormEditor.FDesignPanel.FHasLoadedForm=false then
       begin
         xform:=FFormEditor.FDesignPanel.FLoadedForm;   // Get LoadedForm
         FormEditorDelete;                              // Delete OLD FormEditor

         if xform<>nil then
         begin

           if (CN_FORM_DESIGNER_PARENT_TYPE=ctptParentWindow) then
             xForm.ParentWindow:=0;                     // Reset LoadedForm ParentWindow

           xForm.Parent:=nil;                           // Reset LoadedForm Parent
           ShowWindow(xform.Handle, SW_HIDE);           // Hide  LoadedForm
           EmbeddedForm(nil,xform);                     // Embedded LoadedForm
         end;
       end;

//--------------------------------------

  if FEditPageForDesigner.TabVisible then               // Only if Tab is visible for "Clone" Forms
   if (FEditPages.ActivePage<>FEditPageForDesigner) then
    begin

     FEditPages.ActivePage:=FEditPageForDesigner;
     if FFormEditor<>nil then FFormEditor.ActivatedDesigner;

    end;
end;

Procedure TEmbedTabSheet.UpdateEmbedFormDsgSettings;
begin
  if NOT Assigned(FFormEditor) then exit;

  Embedded_Updating:=True;

  FFormEditor.RootPanel.ParentColor := false;
  FFormEditor.RootPanel.Color := EnvironmentOptions.UseEmbeddedDesignerBackColor;

  FFormEditor.pnDesignBox.ParentColor := false;
  FFormEditor.pnDesignBox.Color := EnvironmentOptions.UseEmbeddedDesignerBackColor;

  FFormEditor.pnInfoPanel.Color := EnvironmentOptions.UseEmbeddedInfopanelColor;

  FFormEditor.pnScreenPreview.Color := EnvironmentOptions.UseEmbeddedSreenPreviewBackColor;
  FFormEditor.pnScreenPreview.Visible := EnvironmentOptions.UseEmbeddedScreenPreview;

  FFormEditor.pnScreenPreviewBtns.Color  := EnvironmentOptions.UseEmbeddedDesignerBackColor;
  FFormEditor.pnScreenPreviewBtns.Visible:= EnvironmentOptions.UseEmbeddedScreenPreview;

  Embedded_Updating:=False;
end;

procedure TEmbedTabSheet.FormEditorsInit;
 var R:TRect;
begin
  if EnvironmentOptions.UseEmbeddedDesigner = False then
    Exit;
  Embedded_Updating := True;
  if FFormEditor<>nil then FFormEditor.Free;

  FFormEditor:=TEmbedFormEditor.Create(FEditPageForDesigner);
  FFormEditor.Parent:=FEditPageForDesigner;
  FFormEditor.Visible:=false;
  FFormEditor.Align:=alClient;

  //........... Info Panel ..........................
  FFormEditor.pnInfoPanel.Visible:=False;
  FFormEditor.pnInfoPanel.SetBounds(FFormEditor.Width-FFormEditor.pnInfoPanel.Width-(LCLIntf.GetSystemMetrics(SM_CYVSCROLL)+4),
                                    4,
                                    FFormEditor.pnInfoPanel.Width,FFormEditor.pnInfoPanel.Height);

  //........... Screen Preview Panel.....................
  R:=Rect(FFormEditor.pnScreenPreview.Left,
          FFormEditor.pnScreenPreview.top,
          FFormEditor.PixelPerc(Screen.Width,  CN_PREVIEW_FACTOR),
          FFormEditor.PixelPerc(Screen.Height, CN_PREVIEW_FACTOR)
          );
  R.Left:=FFormEditor.Width -(R.Right +LCLIntf.GetSystemMetrics(SM_CYVSCROLL)+4);
  R.Top :=FFormEditor.Height-(R.Bottom+LCLIntf.GetSystemMetrics(SM_CXHSCROLL)+4);

  FFormEditor.pnScreenPreview.SetBounds(R.Left, R.top, R.Right, R.Bottom);

  //.......... Show Hide Panel ..........................
  R:=Rect(FFormEditor.pnShowHide.Left,
          FFormEditor.pnShowHide.top,
          FFormEditor.pnShowHide.Width,
          FFormEditor.pnShowHide.Height
          );

  R.Left:=FFormEditor.pnScreenPreview.Left + (FFormEditor.pnScreenPreview.Width-R.Right);
  R.Top :=FFormEditor.pnScreenPreview.Top  -  FFormEditor.pnShowHide.Height - 4;

  FFormEditor.pnShowHide.SetBounds(R.Left, R.top, R.Right, R.Bottom);

   //.......... Screen Preview Buttons Panel .............
  R:=Rect(FFormEditor.pnScreenPreviewBtns.Left,
          FFormEditor.pnScreenPreviewBtns.top,
          FFormEditor.pnScreenPreviewBtns.Width,
          FFormEditor.pnScreenPreviewBtns.Height
          );
  R.Left:=FFormEditor.pnScreenPreview.Left + (FFormEditor.pnScreenPreview.Width-R.Right-FFormEditor.pnShowHide.Width-4);
  R.Top :=FFormEditor.pnScreenPreview.Top  -  FFormEditor.pnScreenPreviewBtns.Height - 4;

  FFormEditor.pnScreenPreviewBtns.SetBounds(R.Left, R.top, R.Right, R.Bottom);


  //........... Design Panel .......................
  FFormEditor.FDesignPanel := TEmbedDesignPanel.Create(FFormEditor.pnDesignBox, FFormEditor);
  FFormEditor.FDesignPanel.Parent := FFormEditor.pnDesignBox;
  FFormEditor.FDesignPanel.Visible:=false;
  FFormEditor.FDesignPanel.dsParentPage:=self;
  FFormEditor.FDesignPanel.dsParentFormEditor:=FFormEditor;
  FFormEditor.FDesignPanel.OnFormLoad := @FFormEditor.DesignPanel_OnLoadForm;
  FFormEditor.FDesignPanel.OnLoadedFormChangeBounds := @FFormEditor.DesignPanel_OnLoadedFormChangeBounds;

  UpdateEmbedFormDsgSettings;

  FEditPageForDesigner.TabVisible:=true;

  Embedded_Updating := False;
end;

Procedure TEmbedTabSheet.FormEditorDelete;
begin
  if EnvironmentOptions.UseEmbeddedDesigner = False then
    Exit;
  if FFormEditor <> nil then
    FFormEditor.free;
  FFormEditor := nil;
  if FEditPageForDesigner<> nil then
    FEditPageForDesigner.TabVisible:=false;
end;

Procedure TEmbedTabSheet.FormEditorAdd;
begin
 if EnvironmentOptions.UseEmbeddedDesigner = False then
   Exit;
 if FEditPageForDesigner <> nil then
   FEditPageForDesigner.TabVisible := True;
end;


Function TEmbedTabSheet.GetLoadedForm:TCustomForm;
begin
 Result:=nil;

 if FFormEditor<>nil then
   if FFormEditor.FDesignPanel<>nil then
     if FFormEditor.FDesignPanel.LoadedForm<>nil then
        Result:=FFormEditor.FDesignPanel.LoadedForm;
end;

Procedure TEmbedTabSheet.EmbeddedForm(Sender: TObject; aForm: TCustomForm);
begin
  if FEditPages=nil then exit;
  if FFormEditor<>nil then exit;     // if has form then exit...

  FormEditorsInit;

  if FFormEditor=nil then exit;
  if FFormEditor.FDesignPanel=nil then exit;

  Embedded_Updating := True;
  FFormEditor.FDesignPanel.EmbeddedForm(Sender,aForm);

  if FEditPageForDesigner<> nil then FEditPageForDesigner.TabVisible:=true;

  FFormEditor.FDesignPanel.IniOK:=false;
  FFormEditor.ActivatedDesigner;

  Embedded_Updating := False;

  if aForm=nil then
    DebugLn(EMBEDDED_FORM_DESIGNER,'TEmbedTabSheet.EmbeddedForm : NIL') else
    DebugLn(EMBEDDED_FORM_DESIGNER,'TEmbedTabSheet.EmbeddedForm :'+aForm.Name);
end;


//================================================================== SOS SOS SOS
// This Is for Problem of "Docking Embedded Form Designer"
// The GlassDocking send LM_DESTROY msg to FLoadedForm
// On Windows give Error 1400 and crash the IDE
// On GTK2    give Error and crash the IDE
// We don't know why, must find this and remove the next code...
//==================================================================

Procedure TEmbedTabSheet.FormEditorPause;
begin
  if EnvironmentOptions.UseEmbeddedDesigner = False then
    Exit;
  if FEditPages=nil then exit;
  if FFormEditor=nil then exit;     // if has form then exit...
  if FFormEditor.FDesignPanel=nil then exit;
  if FFormEditor.FDesignPanel.FHasLoadedForm=false then exit;

  DebugLn(EMBEDDED_FORM_DESIGNER,'TEmbedTabSheet.FormEditorPause START');

 Embedded_Updating:=True;
{$IF (NOT DEFINED(LCLGTK2))}
// Must on LCLWin32, LCLWin64
// Must NOT on LCLGTK2
  if (CN_FORM_DESIGNER_PARENT_TYPE=ctptParentWindow) then
      FFormEditor.FDesignPanel.FLoadedForm.Parent:=FFormEditor.FDesignPanel;

{$ENDIF}

  FFormEditor.FDesignPanel.FMessagesHandler.UnHookControl;
  FFormEditor.FDesignPanel.FHasLoadedForm := False;


  if FEditPageForDesigner<> nil then FEditPageForDesigner.TabVisible:=false;
  Embedded_Updating:=False;
end;

//====================================

initialization
  EMBEDDED_FORM_DESIGNER := DebugLogger.RegisterLogGroup('EMBEDDED_FORM_DESIGNER' {$IFDEF EMBEDDED_FORM_DESIGNER} , True {$ENDIF} );

end.




