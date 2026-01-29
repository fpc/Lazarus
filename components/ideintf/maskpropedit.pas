{ Copyright (C) 2005

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Lagunov Aleksey

  Abstract:
    Property Editors for TMaskEdit.EditMask of FCL and LCL.
}

unit MaskPropEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ButtonPanel, MaskEdit,
  // LazUtils
  LazUTF8, LazFileUtils,
  // IdeIntf
  LazIDEIntf, PropEdits, ComponentEditors, ObjInspStrConsts, IDEWindowIntf, PropEditConfig;

type

  { TMaskEditorForm }

  TMaskEditorForm = class(TForm)
  private
  type
    TParsedSample = record
      Caption, Example, FormattedExample, Mask: String;
    end;
    TParsedSamples = array of TParsedSample;
  published
    ButtonPanel1: TButtonPanel;
    EnableSetsCheckBox: TCheckBox;
    LoadSampleMasksButton: TButton;
    UnloadSampleMasksButton: TButton;
    SaveLiteralCheckBox: TCheckBox;
    InputMaskEdit: TEdit;
    CharactersForBlanksEdit: TEdit;
    InputMaskLabel: TLabel;
    SampleMasksLabel: TLabel;
    CharactersForBlanksLabel: TLabel;
    TestInputLabel: TLabel;
    SampleMasksListBox: TListBox;
    TestMaskEdit: TMaskEdit;
    OpenDialog1: TOpenDialog;
    TestInputPanel: TPanel;
    procedure EnableSetsCheckBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure LoadSampleMasksButtonClick(Sender: TObject);
    procedure UnloadSampleMasksButtonClick(Sender: TObject);
    procedure SampleMasksListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure SaveLiteralCheckBoxClick(Sender: TObject);
    procedure InputMaskEditChange(Sender: TObject);
    procedure CharactersForBlankEditChange(Sender: TObject);
    procedure SampleMasksListBoxClick(Sender: TObject);
    procedure MaskEditorFormCreate(Sender: TObject);
  private
    FEnableSets: Boolean;
    ParsedSamples: TParsedSamples;
    FSampleEditMaskFilename: String;
    function ConstructEditmask: String;
    function GetEditMask: string;
    function MaskDoFormatText(const EditMask: string; const Value: string; EnableSets: Boolean): String;
    function ParseMaskLineDelphi(Line: String; EnableSets: Boolean; out aCaption, aExample, aFormattedExample, aMask: String): Boolean;
    function ParseMaskLineLazarus(Line: String; EnableSets: Boolean; out aCaption, aExample, aFormattedExample, aMask: String): Boolean;
    procedure LoadAndCleanSampleFile(Fn: String; EnableSets: Boolean; List: TStrings; out AParsedSamples: TParsedSamples);  //out list of record?
    procedure ReConstructEditmask;
    procedure SetEditMask(AValue: string);
    procedure SetEnableSets(AValue: Boolean);
    procedure UpdateTestEditor;
  public
    property EditMask: string read GetEditMask write SetEditMask;
    property EnableSets: Boolean write SetEnableSets;
  end; 

  { TEditMaskProperty }

  TEditMaskProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetMaskEdit: TCustomMaskEdit;
  end;

  { TMaskEditEditor }
  TCustomMaskEditAccess = class(TCustomMaskEdit)
  end; //Hack to get access to EditMask

  TMaskEditEditor = class(TDefaultComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function MaskEdit: TCustomMaskEditAccess; virtual;
  end;



implementation

{$R *.lfm}


{ TMaskEditorForm }

procedure TMaskEditorForm.MaskEditorFormCreate(Sender: TObject);
begin
  Caption := oisInputMaskEditor;
  LoadSampleMasksButton.Caption := oisMasks;
  UnloadSampleMasksButton.Caption := oisUnload;
  UnloadSampleMasksButton.Hint := oisUnloadHint;  // alternatively have a longer caption?
  SaveLiteralCheckBox.Caption := oisSaveLiteralCharacters;
  InputMaskLabel.Caption := oisInputMask;
  SampleMasksLabel.Caption := oisSampleMasks;
  CharactersForBlanksLabel.Caption := oisCharactersForBlanks;
  TestInputLabel.Caption := oisTestInput;

  EnableSetsCheckBox.Hint := oisEnableSetsHint;
  OpenDialog1.Filter := oisMaskSampleFilter;
  OpenDialog1.Title := oisSelectInputMaskSample;

  if not Assigned(PropEditConfigs) then
    PropEditConfigs := TPropEditConfigs.Create;
  PropEditConfigs.Load;
  FSampleEditMaskFilename := PropEditConfigs.SampleEditMaskFilename;
  FSampleEditMaskFilename := CleanAndExpandFilename(FSampleEditMaskFilename);
  if (FSampleEditMaskFilename <> '') and FileExistsUTF8(FSampleEditMaskFilename) then
    LoadAndCleanSampleFile(FSampleEditMaskFilename, FEnableSets, SampleMasksListBox.Items, ParsedSamples);
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TMaskEditorForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PropEditConfigs.SampleEditMaskFilename := FSampleEditMaskFilename;
  PropEditConfigs.Save;
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TMaskEditorForm.UnloadSampleMasksButtonClick(Sender: TObject);
begin
  SampleMasksListBox.Clear;
  ParsedSamples := nil;
  FSampleEditMaskFilename := '';
end;

procedure TMaskEditorForm.EnableSetsCheckBoxClick(Sender: TObject);
begin
  SetEnableSets(EnableSetsCheckBox.Checked);
end;

procedure TMaskEditorForm.LoadSampleMasksButtonClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFileDir(FSampleEditMaskFilename);
  if OpenDialog1.Execute then
    LoadAndCleanSampleFile(OpenDialog1.Filename, FEnableSets, SampleMasksListBox.Items, ParsedSamples);
end;

procedure TMaskEditorForm.SampleMasksListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  OldBrushStyle: TBrushStyle;
  OldTextStyle: TTextStyle;
  NewTextStyle: TTextStyle;
  ListBox: TListBox absolute Control;
  R1, R2: TRect;
begin
  ListBox.Canvas.FillRect(ARect);
  if (Index >= 0) and (Index < ListBox.Items.Count) then
  begin
    OldBrushStyle := ListBox.Canvas.Brush.Style;
    ListBox.Canvas.Brush.Style := bsClear;

    OldTextStyle := ListBox.Canvas.TextStyle;
    NewTextStyle := OldTextStyle;
    NewTextStyle.Layout := tlCenter;
    ListBox.Canvas.TextStyle := NewTextStyle;

    R1 := ARect;
    R2 := ARect;
    R1.Right := (R1.Left + R1.Right) div 2;
    R2.Left := R1.Right + 1;
    ListBox.Canvas.TextRect(R1, R1.Left + 2, R1.Top, ParsedSamples[Index].Caption);
    ListBox.Canvas.TextRect(R2, R2.Left + 2, R2.Top, ParsedSamples[Index].FormattedExample);
    ListBox.Canvas.MoveTo(R2.Left - 1, R2.Top);
    ListBox.Canvas.LineTo(R2.Left - 1, R2.Bottom);
    ListBox.Canvas.Brush.Style := OldBrushStyle;
    ListBox.Canvas.TextStyle := OldTextStyle;
  end;
end;

procedure TMaskEditorForm.SaveLiteralCheckBoxClick(Sender: TObject);

begin
  ReconstructEditMask;
end;

procedure TMaskEditorForm.InputMaskEditChange(Sender: TObject);
begin
  UpdateTestEditor;
end;

procedure TMaskEditorForm.CharactersForBlankEditChange(Sender: TObject);
var
  S:string;
  SL: Boolean;
  BC: Char;
begin
  SplitEditMask(InputMaskEdit.Text, S, SL, BC);
  if (CharactersForBlanksEdit.Text<>'') and (Length(S) > 0) then
    begin
      BC := CharactersForBlanksEdit.Text[1];
      if SL then InputMaskEdit.Text:=S + MaskFieldSeparator + '1' + MaskFieldSeparator + BC
        else InputMaskEdit.Text:=S + MaskFieldSeparator + MaskNoSave + MaskFieldSeparator + BC;
    end
    else
      ReConstructEditMask
end;


Function TMaskEditorForm.ConstructEditmask : String;

Var
  S : String;
  B : Char;
  L : Boolean;

begin
  SplitEditMask(InputMaskEdit.Text,S,L,B);
  If (CharactersForBlanksEdit.Text<>'') then
    B:=CharactersForBlanksEdit.Text[1];
  if (Length(S) = 0) then
    Result := ''
  else
    Result:=Format('%s'+MaskFieldSeparator+'%d'+MaskFieldSeparator+'%s',[S,ord(SaveLiteralCheckBox.checked),B]);
end;

procedure TMaskEditorForm.ReConstructEditmask;

begin
  InputMaskEdit.Text:=ConstructEditMask;
  UpdateTestEditor;
end;

procedure TMaskEditorForm.SampleMasksListBoxClick(Sender: TObject);
begin
  if (SampleMasksListBox.Items.Count > 0) then
  begin
    TestMaskEdit.Text := '';
    EditMask := ParsedSamples[SampleMasksListBox.ItemIndex].Mask;
  end;
end;

function TMaskEditorForm.GetEditMask: string;
begin
  Result:=ConstructEditMask;
end;

function TMaskEditorForm.MaskDoFormatText(const EditMask: string; const Value: string; EnableSets: Boolean): String;
var
  P: Integer;
  S, MaskPart: String;
  MaskSave: Boolean;
  SpaceChar: Char;
begin
  // cheat maskutils while it has no its own MaskDoFormatText
  SplitEditMask(EditMask, MaskPart, MaskSave, SpaceChar);
  // in order to construct the displayed string from the "example text" field,
  // we must set MaskSave to False and SpaceChar to #32, otherwise the result
  // won't be the same as in Delphi
  S := MaskPart+';0; ';
  try
    Result := FormatMaskText(S, Value, EnableSets);
  except
    Result := Value;
  end;
end;

function TMaskEditorForm.ParseMaskLineDelphi(Line: String; EnableSets: Boolean; out aCaption, aExample, aFormattedExample, aMask: String): Boolean;
var
  P1, P2: SizeInt;
begin
   {
   in delphi .dem files every line is made up like this:

   Maskname
   zero or more spaces
   a pipe symbol (|)
   one space
   Example Text
   one space
   a pipe symbol (|)
   one space
   Editmask
   zero or more spaces

   Trailing spaces from Maskname will be stripped
   The first and last character from the Example Text field will be stripped away (hence they should be spaces)
   The first character from the Editmask field will be stripped away, as well as trailing spaces
   So notice that except for 2 places, whitespace is significant.
   }

  Result := False;
  //we need at least 2 | symbols
  P1 := Pos('|', Line);
  if (P1 = 0) then
    Exit;
  P2 := Pos('|', Line, P1 + 1);
  if (P2 = 0) then
    Exit;
  aCaption := TrimRight(Copy(Line, 1, P1-1));
  if (aCaption = '') then
    Exit;
  aExample := (Copy(Line, P1+2, P2-P1-3));
  if (aExample = '') then
    Exit;
  aMask := TrimRight(Copy(Line,P2+2,MaxInt));
  if (aMask = '') then
    Exit;
  aFormattedExample := MaskDoFormatText(aMask, aExample, EnableSets);
  Result := True;
end;

function TMaskEditorForm.ParseMaskLineLazarus(Line: String; EnableSets: Boolean; out aCaption, aExample, aFormattedExample, aMask: String): Boolean;
var
  P1, P2: SizeInt;
begin
   {
   in Lazarus .lem files every line is made up like this:

   Maskname
   zero or more spaces
   a pipe symbol (|)
   zero or more spaces
   Example Text
   zero or more spaces
   a pipe symbol (|)
   zero or more spaces
   Editmask
   zero or more spaces

   Trailing spaces from Maskname will be stripped
   Leading and trailing spaces from Example Text and EditMask will be stripped.

   Lines starting with '//' will be ignored
   }

  Result := False;
  if (TrimLeft(Line).StartsWith('//')) then
    Exit;
  //we need at least 2 | symbols
  P1 := Pos('|', Line);
  if (P1 = 0) then
    Exit;
  P2 := Pos('|', Line, P1 + 1);
  if (P2 = 0) then
    Exit;
  aCaption := TrimRight(Copy(Line, 1, P1-1));
  if (aCaption = '') then
    Exit;
  aExample := Trim(Copy(Line, P1+1, P2-P1-1));
  if (aExample = '') then
    Exit;
  aMask := Trim(Copy(Line,P2+1,MaxInt));
  if (aMask = '') then
    Exit;
  aFormattedExample := MaskDoFormatText(aMask, aExample, EnableSets);
  Result := True;
end;


procedure TMaskEditorForm.LoadAndCleanSampleFile(Fn: String; EnableSets: Boolean; List: TStrings; out AParsedSamples: TParsedSamples);
type
  TParseFunc = function (Line: String; EnableSets: Boolean; out Caption, Example, FormattedExample, Mask: String): Boolean of Object;
var
  i, Index: Integer;
  S, aCaption, aExample, aFormattedExample, aMask: String;
  SL: TStringList;
  ParseFunc: TParseFunc;
begin
  FSampleEditMaskFilename := '';
  AParsedSamples := nil;
  if (CompareText(ExtractFileExt(Fn),'.dem') = 0) then
    ParseFunc := @ParseMaskLineDelphi
  else
    ParseFunc := @ParseMaskLineLazarus;
  List.BeginUpdate;
  List.Clear;
  if not FileExistsUtf8(Fn) then
    Exit;
  SL := TStringList.Create;
  try
    try
      SL.LoadFromFile(Fn, TEncoding.UTF8);
      SetLength(AParsedSamples, SL.Count);
      Index := 0;
      for i := 0 to SL.Count - 1 do
      begin
        S := SL[i];
        if ParseFunc(S, EnableSets, aCaption, aExample, aFormattedExample, aMask) then
        begin
          AParsedSamples[Index].Caption := aCaption;
          AParsedSamples[Index].Example := aExample;
          AParsedSamples[Index].FormattedExample := aFormattedExample;
          AParsedSamples[Index].Mask := aMask;
          List.Add(S);
          Inc(Index);
        end;
      end;
      SetLength(AParsedSamples, Index);
      FSampleEditMaskFilename := Fn;
    except
      on ESTreamError do
      begin
        List.Clear;
        AParsedSamples := nil;
        FSampleEditMaskFilename := '';
        MessageDlg(Format(oisErrorReadingSampleFile,[Fn]), mtError, [mbOk], 0);
      end;
    end;
  finally
    List.EndUpdate;
    SL.Free;
  end;
end;


procedure TMaskEditorForm.SetEditMask(AValue: string);

Var
  M : String;
  B : Char;
  S : Boolean;

begin
  SplitEditMask(AValue,M,S,B);
  InputMaskEdit.Text := AValue;
  SaveLiteralCheckBox.Checked := S;
  CharactersForBlanksEdit.Text := B;
  UpdateTestEditor;
end;

procedure TMaskEditorForm.SetEnableSets(AValue: Boolean);
var
  OldMask: String;
  WasMasked: Boolean;
  i: Integer;
begin
  FEnableSets := AValue;
  WasMasked := TestMaskEdit.IsMasked;
  if WasMasked then
  begin
    OldMask := TestMaskEdit.EditMask;
    TestMaskEdit.EditMask := '';
  end;
  TestMaskEdit.EnableSets := FEnableSets;
  if WasMasked then
  begin
    TestMaskEdit.EditMask := OldMask;
  end;
  //since this is not only called from clicking on the checkbox
  EnableSetsCheckBox.Checked := FEnableSets;
  for i := 0 to Length(ParsedSamples) - 1 do
  begin
    ParsedSamples[i].FormattedExample := MaskDoFormatText(ParsedSamples[i].Mask, ParsedSamples[i].Example, FEnableSets);
  end;
  SampleMasksListBox.Invalidate;
  //since this is not only called from clicking on the checkbox
  EnableSetsCheckBox.Checked := FEnableSets;
end;

procedure TMaskEditorForm.UpdateTestEditor;
begin
  TestMaskEdit.EditMask:=InputMaskEdit.Text;
end;

{ TEditMaskProperty }

function TEditMaskProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paDialog, paMultiSelect, paRevertable];
end;

procedure TEditMaskProperty.Edit;
var
  MaskEditorForm: TMaskEditorForm;
  C: TPersistent;
  AMaskEdit: TCustomMaskEdit;
begin
  MaskEditorForm:=TMaskEditorForm.Create(Application);
  try
    AMaskEdit := GetMaskEdit;
    MaskEditorForm.EnableSets := AMaskEdit.EnableSets;
    MaskEditorForm.EditMask:=GetValue;
    if MaskEditorForm.ShowModal = mrOk then
      SetValue(MaskEditorForm.EditMask);
  finally
    MaskEditorForm.Free;
  end;
end;


function TEditMaskProperty.GetMaskEdit: TCustomMaskEdit;
var
  i: Integer;
  C: TPersistent;
begin
  Result := nil;
  for i:=0 to PropCount-1 do
  begin
    C := GetComponent(i);
    if C is TCustomMaskEdit then
    begin
      Result := TCustomMaskEdit(C);
      Exit;
    end;
  end;
end;


{ TMaskEditEditor }

procedure TMaskEditEditor.ExecuteVerb(Index: Integer);
var
  MaskEditorForm: TMaskEditorForm;
begin
  if Index = 0 then
  begin
    MaskEditorForm := TMaskEditorForm.Create(Application);
    try
      MaskEditorForm.EnableSets := MaskEdit.EnableSets;
      MaskEditorForm.EditMask := MaskEdit.EditMask;
      if MaskEditorForm.ShowModal = mrOk then
        MaskEdit.EditMask := MaskEditorForm.EditMask;
    finally
      MaskEditorForm.Free;
    end;
  end;
end;

function TMaskEditEditor.GetVerb(Index: Integer): string;
begin
  Assert(Index = 0, 'TMaskEditEditor.GetVerb');
  Result := sccsMaskEditor;
end;

function TMaskEditEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TMaskEditEditor.MaskEdit: TCustomMaskEditAccess;
begin
  Result := TCustomMaskEditAccess(GetComponent)
end;

initialization
  RegisterPropertyEditor(TypeInfo(string), TCustomMaskEdit, 'EditMask',
                         TEditMaskProperty);
  RegisterComponentEditor(TMaskEdit, TMaskEditEditor);

end.

