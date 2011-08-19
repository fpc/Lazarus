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
  TAndroidComboBoxStrings = class;

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
    procedure InsertItem(Index: Integer; const S: string);
    procedure Clear;
    procedure Delete(Index: Integer);
  end;

  { TAndroidWindow }
  
  TAndroidWindow = class(TAndroidViewGroup)
  public
    layout: TAbsoluteLayout;
    scroller: TScrollView;
    constructor Create(const AObject: TCustomForm; const AParams: TCreateParams);
    destructor Destroy; override;
  end;

  // Now StringLists

  { TAndroidComboBoxStrings }

  TAndroidComboBoxStrings = class(TStringList)
  private
    FWinControl: TWinControl;
    FOwner: TAndroidComboBox;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create(AWinControl: TWinControl; AOwner: TAndroidComboBox);
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Sort; override;
    procedure Exchange(AIndex1, AIndex2: Integer); override;
  public
    property Owner: TAndroidComboBox read FOwner;
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

{ TAndroidComboBoxStrings }

procedure TAndroidComboBoxStrings.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  //FOwner.BeginUpdate;
//  FOwner.setItemText(Index, S);
//  FOwner.EndUpdate;
end;

procedure TAndroidComboBoxStrings.InsertItem(Index: Integer; const S: string);
var
  FSavedIndex: Integer;
  FSavedText: WideString;
begin
  inherited InsertItem(Index, S);
{  //FOwner.BeginUpdate;
  FSavedText := FOwner.getText;
  FSavedIndex := FOwner.currentIndex;}
  FOwner.insertItem(Index, S);
{  if FOwner.getEditable then
  begin
    if (FSavedIndex <> FOwner.currentIndex) then
      FOwner.setCurrentIndex(FSavedIndex);
    FOwner.setText(FSavedText);
  end;
  FOwner.EndUpdate;}
end;

procedure TAndroidComboBoxStrings.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited InsertItem(Index, S, O);
end;

constructor TAndroidComboBoxStrings.Create(AWinControl: TWinControl;
  AOwner: TAndroidComboBox);
begin
  inherited Create;
  FWinControl := AWinControl;
  FOwner := AOwner;
end;

procedure TAndroidComboBoxStrings.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TAndroidComboBoxStrings.Clear;
begin
  inherited Clear;
  FOwner.clear;
end;

procedure TAndroidComboBoxStrings.Delete(Index: Integer);
begin
  inherited Delete(Index);
  FOwner.delete(Index);
end;

procedure TAndroidComboBoxStrings.Sort;
begin
  inherited Sort;
end;

procedure TAndroidComboBoxStrings.Exchange(AIndex1, AIndex2: Integer);
begin
  inherited Exchange(AIndex1, AIndex2);
end;

{ TAndroidComboBox }

constructor TAndroidComboBox.Create(const AObject: TCustomComboBox;
  const AParams: TCreateParams);
var
  Str: string;
begin
  LCLObject := AObject;
  ParentGroupView := TAndroidViewGroup(AObject.Parent.Handle).mainviewgroup;

  spinner := android_all.TSpinner.Create;
  MainView := spinner;
  params := TAbsoluteLayout_LayoutParams.Create(AObject.Width, WRAP_CONTENT, AObject.Left, AObject.Top);
  ParentGroupView.addView(MainView, TViewGroup_LayoutParams(params));
  params.Free;

  Adapter := TArrayAdapter_String_.Create(simple_spinner_dropdown_item);
  Spinner.setAdapter(Adapter);
end;

destructor TAndroidComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TAndroidComboBox.UpdateItems();
begin

end;

procedure TAndroidComboBox.InsertItem(Index: Integer; const S: string);
begin
  Adapter.insert(S, Index);
end;

procedure TAndroidComboBox.Clear;
begin
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
