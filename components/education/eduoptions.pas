{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the EducationLaz package                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Options.
}
unit EduOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LazConfigStorage, Controls, Forms, BaseIDEIntf,
  FileUtil, LazIDEIntf, IDEOptionsIntf, ProjectIntf;

resourcestring
  EduRSEducation = 'Education';
  ersShowAll = 'Show all';
  ersHideAll = 'Hide all';
  ersShowExtended = 'Show Extended';
  ersShowMinimal = 'Show Minimal';
  ersVisibleComponents = 'Visible components';
  ersShowAllChilds = 'Show all children';
  ersIDEMenuItems = 'IDE menu items';
  ersNewSingleFileProgram = 'New single file program';
  ersNewSingleFileEducationProgram = 'New single file education program';
  ersNewProgram = 'New program';
  ersAddIcon = 'Add icon';
  ersAddASpeedButtonToTheIDEToolbarToCreateANewProgram = 'Add a speed button '
    +'to the IDE toolbar to create a new program';
  ersAddMenuItem = 'Add menu item';
  ersAddAMenuItemTheIDEToolbarToCreateANewProgram = 'Add a menu item the IDE '
    +'toolbar to create a new program';
  ersAddToNewDialog = 'Add to %sNew ...%s dialog';
  ersAddAnEntryToTheNewDialogToCreateANewProgram = 'Add an entry to the %'
    +'sNew ...%s dialog to create a new program';
  ersSource = 'Source';
  ersSingleFileProgram = 'Single file program';
  ersASimpleProgramOnlyOneFileIsCreatedAndAddedToTheCur = 'A simple program. '
    +'Only one file is created and added to the current project.';
  ersLoadDefaultCode = 'Load default code?';
  ersReplaceCurrentWithDefaultSourceCode = 'Replace current with default '
    +'source code?';
  ersReplaceCurrentSourceWithDefaultSourceCode = 'Replace current source with '
    +'default source code';
  ersLoadSourceFromFile = 'Load source from file';

  ersGrpBoxPropsMin = 'Properties: Minimal Configuration';
  ersGrpBoxPropsExt = 'Properties: Extended Configuration';
  ersGrpBoxPropsFull = 'Properties: Full Configuration';

  ersGrpBoxEventsMin = 'Events: Minimal Configuration';
  ersGrpBoxEventsExt = 'Events: Extended Configuration';
  ersGrpBoxEventsFull = 'Events: Full Configuration';

  ersStTextPropsMin = 'Name, Caption, Visible, Text, Checked, Items, Font, Color, Enabled, Height, Width, MaxLength, Picture, Columns';
  ersStTextPropsExt ='Align, Left, Top, Hint, ShowHint, ParentFont, TabOrder, ParentShowHint, WordWrap, FixedCols, FixedRows, DefaultColWidth, DefaultRowHeight, ColCount, RowCount, Borderstyle, Glyph, State, Interval, DataSource, DataField + DB-Properties';
  ersStTextPropsFull = 'All Properties available';

  ersStTextEventsMin = 'OnClick, OnChange, OnMouseMove';
  ersStTextEventsExt = 'OnClick, OnChange, OnMouseMove, OnDblClick, OnCreate, OnKeyPress, OnFormCreate';
  ersStTextEventsFull = 'All Events available';

  ersRdGrpPropsCaption = 'Properties';
  ersRdGrpEventsCaption = 'Events';

  ersEduEnvOptsFrameTitle = 'General';
  ersEduPropsEventsTitle = 'Properties & Events';
  ersEduCompPaletteTitle = 'Component palette';
  ersEduNewProgramTitle = 'New program';
  ersEduMenuTitle = 'Menus';
  ersEduOIPages = 'Object Inspector';

  ersRdBtnFull = 'Show All';
  ersEnableEduCheckBoxCaption = 'Enable education settings';
  ersShowOIPages = 'Show Object Inspector Pages';

  ersEduSBTitle = 'Speed Buttons';
  ersShowSelection = 'Show Selection';
  ersVisibleSpeedButtons = 'Visible SpeedButtons';




const
  DefaultEduOptionsFilename = 'education.xml';
var
  EduOptionID: integer = 2000;
    EduOptionGeneralID: integer       = 100;
    EduOptionCompPaletteID: integer   = 200;
    EduOptionMenuID: integer          = 300;
    EduOptionNewPrgID: integer        = 400;
    EduPropsEventsOptionsID: integer  = 500;
    EduOIPagesOptionsID: integer      = 600;
    EduSpeedButtonsOptionsID: integer = 700;

type

  { TEduOptionsNode }

  TEduOptionsNode = class(TPersistent)
  private
    FChilds: TFPList; // list of TEduOptionsNode
    FName: string;
    FNextSibling: TEduOptionsNode;
    FParent: TEduOptionsNode;
    FPrevSibling: TEduOptionsNode;
    function GetChildCount: integer;
    function GetChilds(Index: integer): TEduOptionsNode;
    procedure SetName(const AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: integer); virtual;
    procedure Remove(Index: integer); virtual;
    procedure Add(Node: TEduOptionsNode);
    procedure Insert(Index: integer; Node: TEduOptionsNode);
    procedure Unbind;
    function Load(Config: TConfigStorage): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    procedure Changed; virtual;
    procedure Apply(Enable: boolean); virtual;
  public
    property Name: string read FName write SetName;
    property Parent: TEduOptionsNode read FParent;
    property NextSibling: TEduOptionsNode read FNextSibling;
    property PrevSibling: TEduOptionsNode read FPrevSibling;
    property ChildCount: integer read GetChildCount;
    property Children[Index: integer]: TEduOptionsNode read GetChilds; default;
  end;

  { TEduOptsRootNode }

  TEduOptsRootNode = class(TEduOptionsNode)
  private
    FChangeStep: integer;
    procedure SetChangeStep(const AValue: integer);
  public
    procedure Changed; override;
    procedure IncreaseChangeStep;
    property ChangeStep: integer read FChangeStep write SetChangeStep;
  end;

  TEduOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FEnabled: boolean;
    FFilename: string;
    FNeedLoad: boolean;
    FRoot: TEduOptionsNode;
    FLastSavedChangeStep: integer;
    procedure SetEnabled(const AValue: boolean);
    procedure SetFilename(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    property Root: TEduOptionsNode read FRoot;
    function Load(Config: TConfigStorage): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    function LoadFromFile(Filename: string): TModalResult; virtual;
    function SaveToFile(Filename: string): TModalResult; virtual;
    function Load: TModalResult; virtual;
    function Save: TModalResult; virtual;
    procedure DoAfterWrite(Restore: boolean); override;
    procedure Apply; virtual;
    function GetFullFilename: string;
    function OnProjectOpened(Sender: TObject; AProject: TLazProject): TModalResult;
    property Filename: string read FFilename write SetFilename;
    property Enabled: boolean read FEnabled write SetEnabled;
    property NeedLoad: boolean read FNeedLoad write FNeedLoad;
  end;

type
  EducationIDEOptionsClass = TAbstractIDEEnvironmentOptions;

var
  EducationOptions: TEduOptions = nil;

implementation

{ TEduOptionsNode }

procedure TEduOptionsNode.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

function TEduOptionsNode.GetChilds(Index: integer): TEduOptionsNode;
begin
  Result:=TEduOptionsNode(fChilds[Index]);
end;

function TEduOptionsNode.GetChildCount: integer;
begin
  Result:=fChilds.Count;
end;

constructor TEduOptionsNode.Create;
begin
  fChilds:=TFPList.Create;
end;

destructor TEduOptionsNode.Destroy;
begin
  Clear;
  FreeAndNil(fChilds);
  inherited Destroy;
end;

procedure TEduOptionsNode.Clear;
begin
  while ChildCount>0 do Delete(ChildCount-1);
end;

procedure TEduOptionsNode.Delete(Index: integer);
var
  Child: TEduOptionsNode;
begin
  Child:=Children[Index];
  Remove(Index);
  Child.Free;
end;

procedure TEduOptionsNode.Remove(Index: integer);
var
  Child: TEduOptionsNode;
begin
  Child:=Children[Index];
  fChilds.Delete(Index);
  Child.FParent:=nil;
  Child.Unbind;
end;

procedure TEduOptionsNode.Add(Node: TEduOptionsNode);
begin
  Insert(ChildCount,Node);
end;

procedure TEduOptionsNode.Insert(Index: integer; Node: TEduOptionsNode);
begin
  Node.Unbind;
  FChilds.Insert(Index,Node);
  Node.FParent:=Self;
  if Index>0 then begin
    Node.FPrevSibling:=Children[Index-1];
    Node.FPrevSibling.FNextSibling:=Node;
  end;
  if Index+1<ChildCount then begin
    Node.FNextSibling:=Children[Index+1];
    Node.FNextSibling.FPrevSibling:=Node;
  end;
end;

procedure TEduOptionsNode.Unbind;
begin
  if FParent<>nil then
    FParent.fChilds.Remove(Self);
  FParent:=nil;
  if FPrevSibling<>nil then
    FPrevSibling.FNextSibling:=FNextSibling;
  if FNextSibling<>nil then
    FNextSibling.FPrevSibling:=FPrevSibling;
  FPrevSibling:=nil;
  FNextSibling:=nil;
end;

function TEduOptionsNode.Load(Config: TConfigStorage): TModalResult;
var
  i: Integer;
  Child: TEduOptionsNode;
begin
  for i:=0 to ChildCount-1 do begin
    Child:=Children[i];
    if (Child.Name='') or (not IsValidIdent(Child.Name)) then continue;
    Config.AppendBasePath(Child.Name);
    try
      Result:=Child.Load(Config);
      if Result<>mrOK then exit;
    finally
      Config.UndoAppendBasePath;
    end;
  end;
  Result:=mrOk;
end;

function TEduOptionsNode.Save(Config: TConfigStorage): TModalResult;
var
  i: Integer;
  Child: TEduOptionsNode;
begin
  for i:=0 to ChildCount-1 do begin
    Child:=Children[i];
    if (Child.Name='') or (not IsValidIdent(Child.Name)) then continue;
    Config.AppendBasePath(Child.Name);
    try
      Result:=Child.Save(Config);
      if Result<>mrOK then exit;
    finally
      Config.UndoAppendBasePath;
    end;
  end;
  Result:=mrOk;
end;

procedure TEduOptionsNode.Changed;
begin
  if FParent<>nil then FParent.Changed;
end;

procedure TEduOptionsNode.Apply(Enable: boolean);
var
  i: Integer;
begin
  for i:=0 to ChildCount-1 do
    Children[i].Apply(Enable);
end;

{ TEduOptions }

procedure TEduOptions.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

procedure TEduOptions.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  Root.Changed;
  Apply;
end;

constructor TEduOptions.Create;
begin
  FRoot:=TEduOptsRootNode.Create;
  FFilename:=DefaultEduOptionsFilename;
  FNeedLoad:=true;
end;

destructor TEduOptions.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

class function TEduOptions.GetGroupCaption: string;
begin
  Result:=EduRSEducation;
end;

class function TEduOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=EducationOptions;
end;

function TEduOptions.Load(Config: TConfigStorage): TModalResult;
begin
  FEnabled:=Config.GetValue('Enabled',false);
  Result:=FRoot.Load(Config);
end;

function TEduOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetDeleteValue('Enabled',Enabled,false);
  Result:=FRoot.Save(Config);
end;

function TEduOptions.LoadFromFile(Filename: string): TModalResult;
var
  Config: TConfigStorage;
begin
  Config:=GetIDEConfigStorage(Filename,true);
  try
    Result:=Load(Config);
  finally
    Config.Free;
  end;
end;

function TEduOptions.SaveToFile(Filename: string): TModalResult;
var
  Config: TConfigStorage;
begin
  //DebugLn(['TEduOptions.SaveToFile ',Filename]);
  Config:=GetIDEConfigStorage(Filename,false);
  try
    Result:=Save(Config);
  finally
    Config.Free;
  end;
end;

function TEduOptions.Load: TModalResult;
begin
  Result:=LoadFromFile(Filename);
  FLastSavedChangeStep:=TEduOptsRootNode(Root).ChangeStep;
end;

function TEduOptions.Save: TModalResult;
var
  FullFilename: String;
begin
  FullFilename:=GetFullFilename;
  if FileExistsUTF8(FullFilename)
  and (FLastSavedChangeStep=TEduOptsRootNode(Root).ChangeStep) then
    Result:=mrOK;
  Result:=SaveToFile(Filename);
  FLastSavedChangeStep:=TEduOptsRootNode(Root).ChangeStep;
end;

procedure TEduOptions.DoAfterWrite(Restore: boolean);
begin
  inherited DoAfterWrite(Restore);
  if not Restore then begin
    if EducationOptions.Save<>mrOk then
      DebugLn(['TEduOptions.DoAfterWrite Failed']);
    Apply;
  end;
end;

procedure TEduOptions.Apply;
begin
  //DebugLn(['TEduOptions.Apply ']);
  Root.Apply(Enabled);
end;

function TEduOptions.GetFullFilename: string;
begin
  Result:=Filename;
  if FilenameIsAbsolute(Result) then exit;
  Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+Result;
end;

function TEduOptions.OnProjectOpened(Sender: TObject; AProject: TLazProject
  ): TModalResult;
begin
  Result:=mrOk;
  if NeedLoad then
    Load;
  Apply;
end;

{ TEduOptsRootNode }

procedure TEduOptsRootNode.SetChangeStep(const AValue: integer);
begin
  if FChangeStep=AValue then exit;
  FChangeStep:=AValue;
end;

procedure TEduOptsRootNode.Changed;
begin
  inherited Changed;
  IncreaseChangeStep;
end;

procedure TEduOptsRootNode.IncreaseChangeStep;
begin
  if FChangeStep=High(FChangeStep) then
    FChangeStep:=low(FChangeStep)
  else
    inc(FChangeStep);
end;

initialization
  EducationOptions:=TEduOptions.Create;

finalization
  FreeAndNil(EducationOptions);

end.
