unit keymapping;
{
  Author: Mattias Gaertner

  Abstract:
    Contains classes to store key-command relationships, can update
    TSynEditKeyStrokes and provides a dialog for editing a single
    commandkey.

  ToDo:
}

{$mode objfpc}

interface

uses
  LCLLinux,
  Forms, Classes, SysUtils, Buttons, LResources, StdCtrls, Controls,
  SynEdit, SynEditKeyCmds, XMLCfg;

const
  // editor commands constants. see syneditkeycmds.pp for more
  ecFind             = ecUserFirst+1;
  ecFindAgain        = ecUserFirst+2;
  ecReplace          = ecUserFirst+3;
  ecFindProcedureDefinition = ecUserFirst+4;
  ecFindProcedureMethod = ecUserFirst+5;
  ecGotoLineNumber   = ecUserFirst+6;

  ecNextEditor       = ecUserFirst+7;
  ecPrevEditor       = ecUserFirst+8;

  ecPeriod           = ecUserFirst+9;

  ecFirstParent      = ecUserFirst+1000;
  ecSave             = ecFirstParent+1;
  ecOpen             = ecFirstParent+2;
  ecClose            = ecFirstParent+3;

  ecJumpToEditor     = ecFirstParent+4;

  ecGotoEditor0      = ecUserFirst + 2000;
  ecGotoEditor1      = ecGotoEditor0 + 1;
  ecGotoEditor2      = ecGotoEditor1 + 1;
  ecGotoEditor3      = ecGotoEditor2 + 1;
  ecGotoEditor4      = ecGotoEditor3 + 1;
  ecGotoEditor5      = ecGotoEditor4 + 1;
  ecGotoEditor6      = ecGotoEditor5 + 1;
  ecGotoEditor7      = ecGotoEditor6 + 1;
  ecGotoEditor8      = ecGotoEditor7 + 1;
  ecGotoEditor9      = ecGotoEditor8 + 1;


type
  //---------------------------------------------------------------------------
  // class for storing the keys for a single command (key-command relationship)
  TKeyCommandRelation = class
  private
  public
    Name:ShortString;
    Command:TSynEditorCommand;  // see the ecXXX constants above
    Key1:word;
    Shift1:TShiftState;
    Key2:word;
    Shift2:TShiftState;
    constructor Create(AName:ShortString;ACommand:TSynEditorCommand;
      AKey1:Word;AShift1:TShiftState;AKey2:Word;AShift2:TShiftState);
  end;

  //---------------------------------------------------------------------------
  // class for a list of key - command relations
  TKeyCommandRelationList = class
  private
    FRelations:TList;
    function GetRelation(Index:integer):TKeyCommandRelation;
    function Add(Name:shortstring;Command:TSynEditorCommand;
       Key1:Word; Shift1:TShiftState; 
       Key2:Word; Shift2:TShiftState):integer;
    function ShiftStateToStr(Shift:TShiftState):AnsiString;
  public
    property Relations[Index:integer]:TKeyCommandRelation read GetRelation;
    function Count:integer;
    function Find(AKey:Word; AShiftState:TShiftState):TKeyCommandRelation;
    function LoadFromXMLConfig(XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
    function SaveToXMLConfig(XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
    procedure AssignTo(ASynEditKeyStrokes:TSynEditKeyStrokes);
    constructor Create;
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // form for editing one command - key relationship
  TKeyMappingEditForm = class(TForm)
    OkButton:TButton;
    CancelButton:TButton;
    CommandLabel:TLabel;
    Key1GroupBox:TGroupBox;
    Key1CtrlCheckBox:TCheckBox;
    Key1AltCheckBox:TCheckBox;
    Key1ShiftCheckBox:TCheckBox;
    Key1KeyComboBox:TComboBox;
    Key2GroupBox:TGroupBox;
    Key2CtrlCheckBox:TCheckBox;
    Key2AltCheckBox:TCheckBox;
    Key2ShiftCheckBox:TCheckBox;
    Key2KeyComboBox:TComboBox;
    procedure OkButtonClick(Sender:TObject);
    procedure CancelButtonClick(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    KeyCommandRelationList:TKeyCommandRelationList;
    KeyIndex:integer;
  end;

function KeyAndShiftStateToStr(Key:Word; ShiftState:TShiftState):AnsiString;
function ShowKeyMappingEditForm(Index:integer;
   AKeyCommandRelationList:TKeyCommandRelationList):TModalResult;

var KeyMappingEditForm:TKeyMappingEditForm;


implementation


function ShowKeyMappingEditForm(Index:integer;
   AKeyCommandRelationList:TKeyCommandRelationList):TModalResult;
begin
  Result:=mrCancel;
  if KeyMappingEditForm<>nil then exit;
  KeyMappingEditForm:=TKeyMappingEditForm.Create(Application);
  with KeyMappingEditForm do
    try
      KeyCommandRelationList:=AKeyCommandRelationList;
      KeyIndex:=Index;
      Caption:='Edit Keys';
      with KeyCommandRelationList.Relations[Index] do begin
        CommandLabel.Caption:='Command: '+Name;
        if Key1<>VK_UNKNOWN then begin
          Key1CtrlCheckBox.Checked:=ssCtrl in Shift1;
          Key1AltCheckBox.Checked:=ssAlt in Shift1;
          Key1ShiftCheckBox.Checked:=ssShift in Shift1;
          Key1KeyComboBox.ItemIndex:=Key1KeyComboBox.Items.IndexOf(
             KeyAndShiftStateToStr(Key1,[]));
        end;
        if Key2<>VK_UNKNOWN then begin
          Key2CtrlCheckBox.Checked:=ssCtrl in Shift2;
          Key2AltCheckBox.Checked:=ssAlt in Shift2;
          Key2ShiftCheckBox.Checked:=ssShift in Shift2;
          Key2KeyComboBox.ItemIndex:=Key1KeyComboBox.Items.IndexOf(
             KeyAndShiftStateToStr(Key2,[]));
        end;
      end;
      Result:=ShowModal;
    finally
      Free;
      KeyMappingEditForm:=nil;
    end;
end;

function KeyAndShiftStateToStr(Key:Word; ShiftState:TShiftState):AnsiString;
begin
  Result:='';
  if ssCtrl in ShiftState then Result:=Result+'+Ctrl';
  if ssAlt in ShiftState then Result:=Result+'+Alt';
  if ssShift in ShiftState then Result:=Result+'+Shift';
  if Result<>'' then
    Result:=copy(Result,2,length(Result)-1)+'+';
  case Key of
  VK_UNKNOWN    :Result:=Result+'Unknown';
  VK_LBUTTON    :Result:=Result+'Mouse Button Left';
  VK_RBUTTON    :Result:=Result+'Mouse Button Right';
  VK_CANCEL     :Result:=Result+'Cancel';
  VK_MBUTTON    :Result:=Result+'Mouse Button Middle';
  VK_BACK       :Result:=Result+'Backspace';
  VK_TAB        :Result:=Result+'Tab';
  VK_CLEAR      :Result:=Result+'Clear';
  VK_RETURN     :Result:=Result+'Return';
  VK_SHIFT      :Result:=Result+'Shift';
  VK_CONTROL    :Result:=Result+'Control';
  VK_MENU       :Result:=Result+'Menu';
  VK_PAUSE      :Result:=Result+'Pause';
  VK_CAPITAL    :Result:=Result+'Capital';
  VK_KANA       :Result:=Result+'Kana';
//  VK_HANGUL     :Result:=Result+'Hangul';
  VK_JUNJA      :Result:=Result+'Junja';
  VK_FINAL      :Result:=Result+'Final';
  VK_HANJA      :Result:=Result+'Hanja';
//  VK_KANJI      :Result:=Result+'Kanji';
  VK_ESCAPE     :Result:=Result+'Escape';
  VK_CONVERT    :Result:=Result+'Convert';
  VK_NONCONVERT :Result:=Result+'Nonconvert';
  VK_ACCEPT     :Result:=Result+'Accept';
  VK_MODECHANGE :Result:=Result+'Mode Change';
  VK_SPACE      :Result:=Result+'Space';
  VK_PRIOR      :Result:=Result+'Prior';
  VK_NEXT       :Result:=Result+'Next';
  VK_END        :Result:=Result+'End';
  VK_HOME       :Result:=Result+'Home';
  VK_LEFT       :Result:=Result+'Left';
  VK_UP         :Result:=Result+'Up';
  VK_RIGHT      :Result:=Result+'Right';
  VK_DOWN       :Result:=Result+'Down';
  VK_SELECT     :Result:=Result+'Select';
  VK_PRINT      :Result:=Result+'Print';
  VK_EXECUTE    :Result:=Result+'Execute';
  VK_SNAPSHOT   :Result:=Result+'Snapshot';
  VK_INSERT     :Result:=Result+'Insert';
  VK_DELETE     :Result:=Result+'Delete';
  VK_HELP       :Result:=Result+'Help';
  VK_0..VK_9    :Result:=Result+IntToStr(Key-VK_0);
  VK_A..VK_Z    :Result:=Result+chr(ord('A')+Key-VK_A);
  VK_LWIN       :Result:=Result+'left windows key';
  VK_RWIN       :Result:=Result+'right windows key';
  VK_APPS       :Result:=Result+'application key';
  VK_NUMPAD0..VK_NUMPAD9:Result:=Result+'Numpad '+IntToStr(Key-VK_NUMPAD0);
  VK_MULTIPLY   :Result:=Result+'*';
  VK_ADD        :Result:=Result+'+';
  VK_SEPARATOR  :Result:=Result+'|';
  VK_SUBTRACT   :Result:=Result+'-';
  VK_DECIMAL    :Result:=Result+'.';
  VK_DIVIDE     :Result:=Result+'/';
  VK_F1..VK_F24 :Result:=Result+'F'+IntToStr(Key-VK_F1+1);
  VK_NUMLOCK    :Result:=Result+'Numlock';
  VK_SCROLL     :Result:=Result+'Scroll';
  else
    Result:=Result+'Word('''+IntToStr(Key)+''')';
  end;
end;

{ TKeyMappingEditForm }

constructor TKeyMappingEditForm.Create(AOwner:TComponent);
var a:integer;
  s:AnsiString;
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:='Edit keys for command';
    Width:=220;
    Height:=250;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Caption:='Ok';
      Left:=15;
      Top:=Self.ClientHeight-Height-15;
      Width:=80;
      OnClick:=@OkButtonClick;
      Show;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Caption:='Cancel';
      Left:=125;
      Top:=OkButton.Top;
      Width:=OkButton.Width;
      OnClick:=@CancelButtonClick;
      Show;
    end;

    CommandLabel:=TLabel.Create(Self);
    with CommandLabel do begin
      Name:='CommandLabel';
      Parent:=Self;
      Caption:='Command';
      Left:=10;
      Top:=5;
      Width:=Self.ClientWidth-Left-Left-4;
      Height:=20;
      Show;
    end;

    Key1GroupBox:=TGroupBox.Create(Self);
    with Key1GroupBox do begin
      Name:='Key1GroupBox';
      Parent:=Self;
      Caption:='Key 1';
      Left:=5;
      Top:=CommandLabel.Top+CommandLabel.Height+8;
      Width:=Self.ClientWidth-4-Left-Left;
      Height:=80;
      Show;
    end;

    Key1CtrlCheckBox:=TCheckBox.Create(Self);
    with Key1CtrlCheckBox do begin
      Name:='Key1CtrlCheckBox';
      Parent:=Key1GroupBox;
      Caption:='Ctrl';
      Left:=5;
      Top:=2;
      Width:=55;
      Height:=20;
      Show;
    end;

    Key1AltCheckBox:=TCheckBox.Create(Self);
    with Key1AltCheckBox do begin
      Name:='Key1AltCheckBox';
      Parent:=Key1GroupBox;
      Caption:='Alt';
      Left:=Key1CtrlCheckBox.Left+Key1CtrlCheckBox.Width+10;
      Top:=Key1CtrlCheckBox.Top;
      Height:=20;
      Width:=Key1CtrlCheckBox.Width;
      Show;
    end;

    Key1ShiftCheckBox:=TCheckBox.Create(Self);
    with Key1ShiftCheckBox do begin
      Name:='Key1ShiftCheckBox';
      Parent:=Key1GroupBox;
      Caption:='Shift';
      Left:=Key1AltCheckBox.Left+Key1AltCheckBox.Width+10;
      Top:=Key1CtrlCheckBox.Top;
      Height:=20;
      Width:=Key1CtrlCheckBox.Width;
      Show;
    end;

    Key1KeyComboBox:=TComboBox.Create(Self);
    with Key1KeyComboBox do begin
      Name:='Key1KeyComboBox';
      Parent:=Key1GroupBox;
      Left:=10;
      Top:=Key1CtrlCheckBox.Top+Key1CtrlCheckBox.Height+5;
      Width:=180;
      Items.BeginUpdate;
      Items.Add('none');
      for a:=1 to 145 do begin
        s:=KeyAndShiftStateToStr(a,[]);
        if lowercase(copy(s,1,5))<>'word(' then
          Items.Add(s);
      end;
      Items.EndUpdate;
      ItemIndex:=0;
      Show;
    end;

    Key2GroupBox:=TGroupBox.Create(Self);
    with Key2GroupBox do begin
      Name:='Key2GroupBox';
      Parent:=Self;
      Caption:='Key 2';
      Left:=5;
      Top:=Key1GroupBox.Top+Key1GroupBox.Height+8;
      Width:=Key1GroupBox.Width;
      Height:=80;
      Show;
    end;

    Key2CtrlCheckBox:=TCheckBox.Create(Self);
    with Key2CtrlCheckBox do begin
      Name:='Key2CtrlCheckBox';
      Parent:=Key2GroupBox;
      Caption:='Ctrl';
      Left:=5;
      Top:=2;
      Width:=55;
      Height:=20;
      Show;
    end;

    Key2AltCheckBox:=TCheckBox.Create(Self);
    with Key2AltCheckBox do begin
      Name:='Key2AltCheckBox';
      Parent:=Key2GroupBox;
      Caption:='Alt';
      Left:=Key2CtrlCheckBox.Left+Key2CtrlCheckBox.Width+10;
      Top:=Key2CtrlCheckBox.Top;
      Height:=20;
      Width:=Key2CtrlCheckBox.Width;
      Show;
    end;

    Key2ShiftCheckBox:=TCheckBox.Create(Self);
    with Key2ShiftCheckBox do begin
      Name:='Key2ShiftCheckBox';
      Parent:=Key2GroupBox;
      Caption:='Shift';
      Left:=Key2AltCheckBox.Left+Key2AltCheckBox.Width+10;
      Top:=Key2CtrlCheckBox.Top;
      Height:=20;
      Width:=Key2CtrlCheckBox.Width;
      Show;
    end;

    Key2KeyComboBox:=TComboBox.Create(Self);
    with Key2KeyComboBox do begin
      Name:='Key2KeyComboBox';
      Parent:=Key2GroupBox;
      Left:=10;
      Top:=Key2CtrlCheckBox.Top+Key2CtrlCheckBox.Height+5;
      Width:=180;
      Items.BeginUpdate;
      Items.Add('none');
      for a:=1 to 145 do begin
        s:=KeyAndShiftStateToStr(a,[]);
        if lowercase(copy(s,1,5))<>'word(' then
          Items.Add(s);
      end;
      Items.EndUpdate;
      ItemIndex:=0;
      Show;
    end;
    
  end;
end;

procedure TKeyMappingEditForm.OkButtonClick(Sender:TObject);
var NewKey1,NewKey2,a:integer;
  NewShiftState1,NewShiftState2:TShiftState;
  s,ACaption,AText:AnsiString;
  DummyRelation:TKeyCommandRelation;
begin
  NewKey1:=VK_UNKNOWN;
  NewShiftState1:=[];
  NewKey2:=VK_UNKNOWN;
  NewShiftState2:=[];
  s:=Key1KeyComboBox.Text;
  if s<>'none' then
    for a:=1 to 145 do
      if KeyAndShiftStateToStr(a,[])=s then
        NewKey1:=a;
  if NewKey1<>VK_UNKNOWN then begin
    if Key1CtrlCheckBox.Checked then include(NewShiftState1,ssCtrl);
    if Key1AltCheckBox.Checked then include(NewShiftState1,ssAlt);
    if Key1ShiftCheckBox.Checked then include(NewShiftState1,ssShift);
  end;
  DummyRelation:=KeyCommandRelationList.Find(NewKey1,NewShiftState1);
  if (DummyRelation<>nil) 
  and (DummyRelation<>KeyCommandRelationList.Relations[KeyIndex]) then begin
    ACaption:='No No No';
    AText:=' The key "'+KeyAndShiftStateToStr(NewKey1,NewShiftState1)+'"'
            +' is already connected to "'+DummyRelation.Name+'".';
    Application.MessageBox(PChar(AText),PChar(ACaption),0);
    exit;
  end;
  s:=Key2KeyComboBox.Text;
  if s<>'none' then
    for a:=1 to 145 do
      if KeyAndShiftStateToStr(a,[])=s then
        NewKey2:=a;
  if (NewKey1=NewKey2) and (NewShiftState1=NewShiftState2) then
    NewKey2:=VK_UNKNOWN;
  if NewKey2<>VK_UNKNOWN then begin
    if Key2CtrlCheckBox.Checked then include(NewShiftState2,ssCtrl);
    if Key2AltCheckBox.Checked then include(NewShiftState2,ssAlt);
    if Key2ShiftCheckBox.Checked then include(NewShiftState2,ssShift);
  end;
  DummyRelation:=KeyCommandRelationList.Find(NewKey2,NewShiftState2);
  if (DummyRelation<>nil) 
  and (DummyRelation<>KeyCommandRelationList.Relations[KeyIndex]) then begin
    ACaption:='No No No';
    AText:=' The key "'+KeyAndShiftStateToStr(NewKey2,NewShiftState2)+'"'
            +' is already connected to "'+DummyRelation.Name+'".';
    Application.MessageBox(PChar(AText),PChar(ACaption),0);
    exit;
  end;
  if NewKey1=VK_UNKNOWN then begin
    NewKey1:=NewKey2;
    NewShiftState1:=NewShiftState2;
    NewKey2:=VK_UNKNOWN;
  end;
  with KeyCommandRelationList.Relations[KeyIndex] do begin
    Key1:=NewKey1;
    Shift1:=NewShiftState1;
    Key2:=NewKey2;
    Shift2:=NewShiftState2;
  end;
  ModalResult:=mrOk;
end;

procedure TKeyMappingEditForm.CancelButtonClick(Sender:TObject);
begin
  ModalResult:=mrCancel;
end;


{ TKeyCommandRelation }

constructor TKeyCommandRelation.Create(AName:ShortString;
  ACommand:TSynEditorCommand;
  AKey1:Word;AShift1:TShiftState;AKey2:Word;AShift2:TShiftState);
begin
  Name:=AName;
  Command:=ACommand;
  Key1:=AKey1;
  Shift1:=AShift1;
  Key2:=AKey2;
  Shift2:=AShift2;
end;

{ TKeyCommandRelationList }

constructor TKeyCommandRelationList.Create;
begin
  inherited Create;
  FRelations:=TList.Create;

  // normal synedit commands
  Add('Select All',ecSelectAll,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add('Copy selection to clipboard',ecCopy,VK_C,[ssCtrl],VK_UNKNOWN,[]);
  Add('Cut selection to clipboard',ecCut,VK_X,[ssCtrl],VK_UNKNOWN,[]);
  Add('Paste clipboard to current position',ecPaste,VK_V,[ssCtrl],VK_UNKNOWN,[]);
  Add('Undo',ecUndo,VK_Z,[ssCtrl],VK_UNKNOWN,[]);
  Add('Redo',ecRedo,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add('Normal selection mode',ecNormalSelect,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add('Column selection mode',ecColumnSelect,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add('Line selection mode',ecLineSelect,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add('Go to matching bracket',ecMatchBracket,VK_UNKNOWN,[],VK_UNKNOWN,[]);
  Add('Indent block',ecBlockIndent,VK_I,[ssCtrl],VK_UNKNOWN,[]);
  Add('Unindent block',ecBlockUnindent,VK_U,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 0',ecGotoMarker0,VK_0,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 1',ecGotoMarker1,VK_1,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 2',ecGotoMarker2,VK_2,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 3',ecGotoMarker3,VK_3,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 4',ecGotoMarker4,VK_4,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 5',ecGotoMarker5,VK_5,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 6',ecGotoMarker6,VK_6,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 7',ecGotoMarker7,VK_7,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 8',ecGotoMarker8,VK_8,[ssCtrl],VK_UNKNOWN,[]);
  Add('Go to marker 9',ecGotoMarker9,VK_9,[ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 0',ecSetMarker0,VK_0,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 1',ecSetMarker1,VK_1,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 2',ecSetMarker2,VK_2,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 3',ecSetMarker3,VK_3,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 4',ecSetMarker4,VK_4,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 5',ecSetMarker5,VK_5,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 6',ecSetMarker6,VK_6,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 7',ecSetMarker7,VK_7,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 8',ecSetMarker8,VK_8,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Set marker 9',ecSetMarker9,VK_9,[ssShift,ssCtrl],VK_UNKNOWN,[]);
  Add('Code template completion',ecAutoCompletion,VK_J,[ssCtrl],VK_UNKNOWN,[]);

  // user defined commands
  Add('Find text',ecFind,VK_F,[SSCtrl],VK_UNKNOWN,[]);
  Add('Find text again',ecFindAgain,VK_F3,[],VK_UNKNOWN,[]);
  Add('Replace text',ecReplace,VK_R,[SSCtrl],VK_UNKNOWN,[]);
  Add('Find procedure definiton',ecFindProcedureDefinition,
                                 VK_UP,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  Add('Find procedure method',ecFindProcedureMethod,
                                 VK_DOWN,[ssShift,SSCtrl],VK_UNKNOWN,[]);
  Add('Go to line number',ecGotoLineNumber,VK_G,[ssCtrl],VK_UNKNOWN,[]);

  Add('Go to next editor',ecNextEditor, VK_S, [ssShift,ssCtrl], VK_UNKNOWN, []);
  Add('Go to prior editor',ecPrevEditor, VK_A, [ssShift,ssCtrl], VK_UNKNOWN, []);

  Add('Save',ecSave,VK_S,[ssCtrl],VK_UNKNOWN,[]);
  Add('Open',ecOpen,VK_O,[ssCtrl],VK_UNKNOWN,[]);
  Add('Close',ecClose,VK_F4,[ssCtrl],VK_UNKNOWN,[]);

  Add('Go to source editor 0',ecGotoEditor0,VK_0,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 1',ecGotoEditor0,VK_1,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 2',ecGotoEditor0,VK_2,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 3',ecGotoEditor0,VK_3,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 4',ecGotoEditor0,VK_4,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 5',ecGotoEditor0,VK_5,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 6',ecGotoEditor0,VK_6,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 7',ecGotoEditor0,VK_7,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 8',ecGotoEditor0,VK_8,[ssAlt],VK_UNKNOWN,[]);
  Add('Go to source editor 9',ecGotoEditor0,VK_9,[ssAlt],VK_UNKNOWN,[]);
end;

destructor TKeyCommandRelationList.Destroy;
var a:integer;
begin
  for a:=0 to FRelations.Count-1 do
    Relations[a].Free;
  FRelations.Free;
  inherited Destroy;
end;

function TKeyCommandRelationList.GetRelation(
  Index:integer):TKeyCommandRelation;
begin
  if (Index<0) or (Index>=Count) then begin
    writeln('[TKeyCommandRelationList.GetRelation] Index out of bounds '
      ,Index,' Count=',Count);
    Halt;
  end;
  Result:= TKeyCommandRelation(FRelations[Index]);
end;

function TKeyCommandRelationList.Count:integer;
begin
  Result:=FRelations.Count;
end;

function TKeyCommandRelationList.Add(Name:shortstring; 
  Command:TSynEditorCommand;
  Key1:Word; Shift1:TShiftState; Key2:Word; Shift2:TShiftState):integer;
begin
  Result:=FRelations.Add(TKeyCommandRelation.Create(Name,Command
      ,Key1,Shift1,Key2,Shift2));
end;

function TKeyCommandRelationList.LoadFromXMLConfig(
  XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
var a,b,p:integer;
  Name:ShortString;
  Default,NewValue:AnsiString;

  function ReadNextInt:integer;
  begin
    Result:=0;
    while (p<=length(NewValue)) and (not (NewValue[p] in ['0'..'9']))
      do inc(p);
    while (p<=length(NewValue)) and (NewValue[p] in ['0'..'9']) 
    and (Result<$10000)do begin
      Result:=Result*10+ord(NewValue[p])-ord('0');
      inc(p);
    end;
  end;

  function IntToShiftState(i:integer):TShiftState;
  begin
    Result:=[];
    if (i and 1)>0 then Include(Result,ssCtrl);
    if (i and 2)>0 then Include(Result,ssShift);
    if (i and 4)>0 then Include(Result,ssAlt);
  end;

// LoadFromXMLConfig
begin
  for a:=0 to FRelations.Count-1 do begin
    Name:=lowercase(Relations[a].Name);
    for b:=1 to length(Name) do
      if Name[b]=' ' then Name[b]:='_';
    with Relations[a] do 
      Default:=IntToStr(Key1)+','+ShiftStateToStr(Shift1)
              +','+IntToStr(Key2)+','+ShiftStateToStr(Shift2);
    NewValue:=XMLConfig.GetValue(Prefix+Name,Default);
    p:=1;
    with Relations[a] do begin
      Key1:=ReadNextInt;
      Shift1:=IntToShiftState(ReadNextInt);
      Key2:=ReadNextInt;
      Shift2:=IntToShiftState(ReadNextInt);
    end;
  end;
  Result:=true;
end;

function TKeyCommandRelationList.SaveToXMLConfig(
  XMLConfig:TXMLConfig; Prefix:AnsiString):boolean;
var a,b:integer;
  Name:ShortString;
  s:AnsiString;
begin
  for a:=0 to FRelations.Count-1 do begin
    Name:=lowercase(Relations[a].Name);
    for b:=1 to length(Name) do
      if Name[b]=' ' then Name[b]:='_';
    with Relations[a] do 
      s:=IntToStr(Key1)+','+ShiftStateToStr(Shift1)
        +','+IntToStr(Key2)+','+ShiftStateToStr(Shift2);
    XMLConfig.SetValue(Prefix+Name,s);
  end;
  Result:=true;
end;

function TKeyCommandRelationList.ShiftStateToStr(Shift:TShiftState):AnsiString;
var i:integer;
begin
  i:=0;
  if ssCtrl in Shift then inc(i,1);
  if ssShift in Shift then inc(i,2);
  if ssAlt in Shift then inc(i,4);
  Result:=IntToStr(i);
end;

function TKeyCommandRelationList.Find(AKey:Word; AShiftState:TShiftState
  ):TKeyCommandRelation;
var a:integer;
begin
  Result:=nil;
  if AKey=VK_UNKNOWN then exit;
  for a:=0 to FRelations.Count-1 do with Relations[a] do
    if ((Key1=AKey) and (Shift1=AShiftState)) 
    or ((Key2=AKey) and (Shift2=AShiftState)) then begin
      Result:=Relations[a];
      exit;
    end;
end;

procedure TKeyCommandRelationList.AssignTo(
  ASynEditKeyStrokes:TSynEditKeyStrokes);
var a,b,MaxKeyCnt,KeyCnt:integer;
  Key:TSynEditKeyStroke;
begin
  for a:=0 to FRelations.Count-1 do begin
    if Relations[a].Key1=VK_UNKNOWN then MaxKeyCnt:=0
    else if Relations[a].Key2=VK_UNKNOWN then MaxKeyCnt:=1
    else MaxKeyCnt:=2;
    KeyCnt:=1;
    b:=0;
    while b<ASynEditKeyStrokes.Count do begin
      if ASynEditKeyStrokes[b].Command=Relations[a].Command then begin
        if KeyCnt>MaxKeyCnt then begin
          ASynEditKeyStrokes[b].Free;
        end else if KeyCnt=1 then begin
          ASynEditKeyStrokes[b].Key:=Relations[a].Key1;
          ASynEditKeyStrokes[b].Shift:=Relations[a].Shift1;
          ASynEditKeyStrokes[b].Key2:=VK_UNKNOWN;
          ASynEditKeyStrokes[b].Shift2:=[];
          inc(b);
        end else if KeyCnt=2 then begin
          ASynEditKeyStrokes[b].Key:=Relations[a].Key2;
          ASynEditKeyStrokes[b].Shift:=Relations[a].Shift2;
          ASynEditKeyStrokes[b].Key2:=VK_UNKNOWN;
          ASynEditKeyStrokes[b].Shift2:=[];
          inc(b);
        end;
        inc(KeyCnt);
      end else inc(b);
    end;
    while KeyCnt<=MaxKeyCnt do begin
      Key:=ASynEditKeyStrokes.Add;
      Key.Command:=Relations[a].Command;
      Key.Key:=Relations[a].Key1;
      Key.Shift:=Relations[a].Shift1;
      Key.Key2:=VK_UNKNOWN;
      Key.Shift2:=[];
      inc(KeyCnt);
    end;
  end;
end;

initialization
  KeyMappingEditForm:=nil;

end.
