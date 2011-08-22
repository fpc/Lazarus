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
  android_all, androidpipescomm, androidstringlists,
  // wdgetset
  WSLCLClasses, LCLClasses,
  // LCL + RTL
  Types, Classes, SysUtils, Controls, LCLType, LCLProc, Graphics, Math, Contnrs,
  AVL_Tree, LMessages, LCLMessageGlue, stdctrls, Forms;

type
  { TAndroidView }

  TAndroidView = class
  public
    LCLObject: TWinControl;
    ParentGroupView: TViewGroup;
    MainView: TView;
    function DeliverMessage(var Msg; const AIsInputEvent: Boolean = False): LRESULT;
  end;

  TAndroidViewGroup = class(TAndroidView)
  public
    mainviewgroup: TViewGroup;
  end;

  { TAndroidEdit }

  TAndroidEdit = class(TAndroidView)
  public
    edittext: android_all.TEditText;
    params: TAbsoluteLayout_LayoutParams;
    constructor Create(const AObject: TCustomEdit; const AParams: TCreateParams);
    destructor Destroy; override;
    function GetText: string;
    procedure SetText(AText: string);
  end;

  { TAndroidButton }

  TAndroidButton = class(TAndroidView)
  public
    btn: android_all.TButton;
    params: TAbsoluteLayout_LayoutParams;
    constructor Create(const AObject: TCustomButton; const AParams: TCreateParams);
    destructor Destroy; override;
    procedure buttonClickCallback(v: TView);
  end;

  { TAndroidCheckBox }

  TAndroidCheckBox = class(TAndroidView)
  public
    checkbox: android_all.TCheckBox;
    params: TAbsoluteLayout_LayoutParams;
    constructor Create(const AObject: TCustomCheckBox; const AParams: TCreateParams);
    destructor Destroy; override;
    function GetState: TCheckBoxState;
    procedure SetState(const AState: TCheckBoxState);
  end;

  { TAndroidStaticText }

  TAndroidStaticText = class(TAndroidView)
  public
    textview: android_all.TTextView;
    params: TAbsoluteLayout_LayoutParams;
    constructor Create(const AObject: TCustomStaticText; const AParams: TCreateParams);
    destructor Destroy; override;
  end;

  { TAndroidComboBox }

  TAndroidComboBox = class(TAndroidView)
  public
    spinner: android_all.TSpinner;
    Adapter: TArrayAdapter_String_;
    params: TAbsoluteLayout_LayoutParams;
    FList: TAndroidComboBoxStrings;
    constructor Create(const AObject: TCustomComboBox; const AParams: TCreateParams);
    destructor Destroy; override;
    procedure UpdateItems();
    procedure Put(Index: Integer; const S: string);
    procedure InsertItem(Index: Integer; const S: string);
    procedure Clear;
    procedure Delete(Index: Integer);
  end;

  { TAndroidWindow }
  
  TAndroidWindow = class(TAndroidViewGroup)
  public
    layout: TAbsoluteLayout;
    scroller: TScrollView;
    FVertScrollable: Boolean;
    constructor Create(const AObject: TCustomForm; const AParams: TCreateParams);
    destructor Destroy; override;
    function GetText: string;
    procedure SetText(AText: string);
  end;

//function CheckHandle(const AWinControl: TWinControl; const AClass: TClass; const DbgText: String): Boolean;
//function CheckWidget(const Handle: HWND; const AMethodName: String; AParamName: String = ''): Boolean;
//function CheckWidget(const Handle: HWND; const AMethodName: String; AClass: TClass): Boolean;

implementation

{ TAndroidView }

function TAndroidView.DeliverMessage(var Msg; const AIsInputEvent: Boolean
  ): LRESULT;
begin
  Result := LRESULT(AIsInputEvent);
  if LCLObject = nil then Exit;

  try
    if LCLObject.HandleAllocated then
    begin
      LCLObject.WindowProc(TLMessage(Msg));
      Result := TLMessage(Msg).Result;
    end;
  except
    {if AIsInputEvent and (LCLObject = nil) and (PtrUInt(Widget) = 0) and
      QtWidgetSet.IsValidHandle(HWND(Self)) then
    begin
      raise Exception.CreateFmt('%s.DeliverMessage(): error in input event %d ',
        [ClassName, TLMessage(Msg).Msg]);
    end else}
      Application.HandleException(nil);
  end;
end;

{ TAndroidComboBox }

constructor TAndroidComboBox.Create(const AObject: TCustomComboBox;
  const AParams: TCreateParams);
begin
  inherited Create;

  LCLObject := AObject;
  ParentGroupView := TAndroidViewGroup(AObject.Parent.Handle).mainviewgroup;

  spinner := android_all.TSpinner.Create;
  MainView := spinner;
  params := TAbsoluteLayout_LayoutParams.Create(AObject.Width, WRAP_CONTENT, AObject.Left, AObject.Top);
  ParentGroupView.addView(MainView, TViewGroup_LayoutParams(params));
  params.Free;

  Adapter := TArrayAdapter_String_.Create(simple_spinner_item);
  Spinner.setAdapter(Adapter);
  {$ifdef LCL_ANDROID_STDCTRLS_VERBOSE}
  vAndroidPipesComm.Log(Format('[TAndroidComboBox.Create] AObject=%P Self=%P Adapter=%P Index=%X', [@AObject, @Self, @Adapter, Adapter.Index]));
  {$endif}
end;

destructor TAndroidComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TAndroidComboBox.UpdateItems();
begin

end;

// Put substitutes an existing value
procedure TAndroidComboBox.Put(Index: Integer; const S: string);
var
  lStr: string;
begin
  lStr := TCustomComboBox(LCLObject).Items.Strings[Index];
  Adapter.remove(lStr);
  InsertItem(Index, S);
end;

procedure TAndroidComboBox.InsertItem(Index: Integer; const S: string);
begin
  {$ifdef LCL_ANDROID_STDCTRLS_VERBOSE}
  vAndroidPipesComm.Log(Format('[TAndroidComboBox.InsertItem] Self=%P Adapter=%P Index=%X', [@Self, @Adapter, Adapter.Index]));
  {$endif}
  Adapter.insert(S, Index);
  Spinner.setAdapter(Adapter);
end;

procedure TAndroidComboBox.Clear;
begin
  {$ifdef LCL_ANDROID_STDCTRLS_VERBOSE}
  vAndroidPipesComm.Log(Format('[TAndroidComboBox.Clear] Self=%P Adapter=%P Index=%X', [@Self, @Adapter, Adapter.Index]));
  {$endif}
  Adapter.Clear();
end;

procedure TAndroidComboBox.Delete(Index: Integer);
var
  lStr: string;
begin
  lStr := TCustomComboBox(LCLObject).Items.Strings[Index];
  Adapter.remove(lStr);
end;

{ TAndroidStaticText }

constructor TAndroidStaticText.Create(const AObject: TCustomStaticText;
  const AParams: TCreateParams);
var
  Str: string;
begin
  LCLObject := AObject;
  ParentGroupView := TAndroidViewGroup(AObject.Parent.Handle).mainviewgroup;

  textview := android_all.TTextView.Create;
  MainView := textview;
  Str := AObject.Caption;
  textview.setText(Str);
  params := TAbsoluteLayout_LayoutParams.Create(AObject.Width, WRAP_CONTENT, AObject.Left, AObject.Top);
  ParentGroupView.addView(MainView, TViewGroup_LayoutParams(params));
  params.Free;
end;

destructor TAndroidStaticText.Destroy;
begin
  inherited Destroy;
end;

{ TAndroidCheckBox }

constructor TAndroidCheckBox.Create(const AObject: TCustomCheckBox;
  const AParams: TCreateParams);
var
  Str: string;
begin
  LCLObject := AObject;
  ParentGroupView := TAndroidViewGroup(AObject.Parent.Handle).mainviewgroup;

  checkbox := android_all.TCheckBox.Create;
  MainView := checkbox;
  Str := AObject.Caption;
  checkbox.setText(Str);
  params := TAbsoluteLayout_LayoutParams.Create(AObject.Width, WRAP_CONTENT, AObject.Left, AObject.Top);
  ParentGroupView.addView(MainView, TViewGroup_LayoutParams(params));
  params.Free;
end;

destructor TAndroidCheckBox.Destroy;
begin
  inherited Destroy;
end;

function TAndroidCheckBox.GetState: TCheckBoxState;
begin
  if checkbox.isChecked() then Result := cbChecked
  else Result := cbUnchecked;
end;

procedure TAndroidCheckBox.SetState(const AState: TCheckBoxState);
begin
  case AState of
  cbUnchecked: checkbox.setChecked(False);
  cbChecked:   checkbox.setChecked(True);
  cbGrayed:    checkbox.setChecked(True);// Android does not support cbGrayed
  end;
end;

{ TAndroidEdit }

constructor TAndroidEdit.Create(const AObject: TCustomEdit;
  const AParams: TCreateParams);
var
  Str: string;
begin
  LCLObject := AObject;
  ParentGroupView := TAndroidViewGroup(AObject.Parent.Handle).mainviewgroup;

  edittext := android_all.TEditText.Create;
  MainView := edittext;
  Str := AObject.Caption;
  edittext.setText(Str);
  params := TAbsoluteLayout_LayoutParams.Create(AObject.Width, WRAP_CONTENT, AObject.Left, AObject.Top);
  ParentGroupView.addView(MainView, TViewGroup_LayoutParams(params));
  params.Free;
end;

destructor TAndroidEdit.Destroy;
begin
  inherited Destroy;
end;

function TAndroidEdit.GetText: string;
begin
  Result := edittext.GetText();
end;

procedure TAndroidEdit.SetText(AText: string);
begin
  edittext.SetText(AText);
end;

{ TAndroidButton }

constructor TAndroidButton.Create(const AObject: TCustomButton;
  const AParams: TCreateParams);
var
  Str: string;
begin
  LCLObject := AObject;
  ParentGroupView := TAndroidViewGroup(AObject.Parent.Handle).mainviewgroup;

  btn := android_all.TButton.Create;
  MainView := btn;
  Str := AObject.Caption;
  btn.setText(Str);
  btn.setOnClickListener(@buttonClickCallback);
  params := TAbsoluteLayout_LayoutParams.Create(AObject.Width, WRAP_CONTENT{AObject.Height}, AObject.Left, AObject.Top);
  ParentGroupView.addView(MainView, TViewGroup_LayoutParams(params));
  params.Free;
end;

destructor TAndroidButton.Destroy;
begin
  inherited Destroy;
end;

procedure TAndroidButton.buttonClickCallback(v: TView);
var
  Msg: TLMessage;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  Msg.Msg := LM_CLICKED;
  DeliverMessage(Msg);
end;

{ TCarbonWidget }

constructor TAndroidWindow.Create(const AObject: TCustomForm;
  const AParams: TCreateParams);
begin
  LCLObject := AObject;

  FVertScrollable := AObject.VertScrollBar.Visible;

  if FVertScrollable then
  begin
    layout := TAbsoluteLayout.Create;
    scroller := TScrollView.Create;
    scroller.addView(layout);

    mainviewgroup := layout;
    MainView := scroller;
  end
  else
  begin
    layout := TAbsoluteLayout.Create;

    mainviewgroup := layout;
    MainView := layout;
  end;
end;

destructor TAndroidWindow.Destroy;
begin
  inherited Destroy;
end;

function TAndroidWindow.GetText: string;
begin
  Result := activity.getTitle();
end;

procedure TAndroidWindow.SetText(AText: string);
begin
  activity.setTitle(AText);
end;

end.
