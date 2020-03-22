{
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
 ***************************************************************************
}
unit IDEOptEditorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Controls, Buttons, Forms, StdCtrls, Graphics, ComCtrls, Grids,
  // BuildIntf
  IDEOptionsIntf,
  // IdeIntf
  EditorSyntaxHighlighterDef;

type
  // forward
  TAbstractOptionsEditorDialog = class;

  // Original font styles, used by filter.

  TDefaultFont = class
    FStyles: TFontStyles;  //  FColor: TColor;
    constructor Create(AStyles: TFontStyles);
  end;
  TDefaultFontList = TStringList;

  TIDEEditorOptions = class(TAbstractIDEEnvironmentOptions)
  protected
    function GetTabPosition: TTabPosition; virtual; abstract;
  public
    // read-only access to options needed by external packages.
    // feel free to extend when needed
    function ExtensionToLazSyntaxHighlighter(Ext: String): TLazSyntaxHighlighter; virtual; abstract;
    property TabPosition: TTabPosition read GetTabPosition;
  end;

  { TAbstractIDEOptionsEditor base frame class for all options frames (global, project and packages) }

  PIDEOptionsEditorRec = ^TIDEOptionsEditorRec;
  PIDEOptionsGroupRec = ^TIDEOptionsGroupRec;

  TAbstractIDEOptionsEditor = class(TFrame)
  private
    FOnChange: TNotifyEvent;
    FOnLoadIDEOptions: TOnLoadIDEOptions;
    FOnSaveIDEOptions: TOnSaveIDEOptions;
    FRec: PIDEOptionsEditorRec;
    FGroupRec: PIDEOptionsGroupRec;
    FDefaultFonts: TDefaultFontList;
  protected
    procedure DoOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; virtual;
    function GetTitle: String; virtual; abstract;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); virtual; abstract;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); virtual; abstract; // options to gui
    procedure WriteSettings(AOptions: TAbstractIDEOptions); virtual; abstract; // gui to options
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); virtual;// user cancelled dialog -> restore any changes
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; virtual; abstract;
    class function DefaultCollapseChildNodes: Boolean; virtual;
    function FindOptionControl(AClass: TControlClass): TControl;
    procedure RememberDefaultStyles;
    function ContainsTextInCaption(AText: string): Boolean;

    property OnLoadIDEOptions: TOnLoadIDEOptions read FOnLoadIDEOptions write FOnLoadIDEOptions;
    property OnSaveIDEOptions: TOnSaveIDEOptions read FOnSaveIDEOptions write FOnSaveIDEOptions;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Rec: PIDEOptionsEditorRec read FRec write FRec;
    property GroupRec: PIDEOptionsGroupRec read FGroupRec write FGroupRec;
  end;
  TAbstractIDEOptionsEditorClass = class of TAbstractIDEOptionsEditor;

  TIDEOptionsEditorRec = record
    Index: Integer;
    Parent: Integer;
    EditorClass: TAbstractIDEOptionsEditorClass;
    Collapsed, DefaultCollapsed: Boolean;
  end;

  { TIDEOptionsEditorList }

  TIDEOptionsEditorList = class(TList)
  private
    function GetItem(AIndex: Integer): PIDEOptionsEditorRec;
    procedure SetItem(AIndex: Integer; const AValue: PIDEOptionsEditorRec);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetByIndex(AIndex: Integer): PIDEOptionsEditorRec;
  public
    procedure Resort;
    function Add(AEditorClass: TAbstractIDEOptionsEditorClass; AIndex, AParent: Integer): PIDEOptionsEditorRec; reintroduce;
    property Items[AIndex: Integer]: PIDEOptionsEditorRec read GetItem write SetItem; default;
  end;

  TIDEOptionsGroupRec = record
    Index: Integer;
    GroupClass: TAbstractIDEOptionsClass;
    Items: TIDEOptionsEditorList;
    Collapsed, DefaultCollapsed: Boolean;
  end;

  { TIDEOptionsGroupList }

  TIDEOptionsGroupList = class(TList)
  private
    FLastSelected: PIDEOptionsEditorRec;
    function GetItem(Position: Integer): PIDEOptionsGroupRec;
    procedure SetItem(Position: Integer; const AValue: PIDEOptionsGroupRec);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Resort;
    procedure DoAfterWrite(Restore: boolean);
    function GetByIndex(AIndex: Integer): PIDEOptionsGroupRec;
    function GetByGroupClass(AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec;
    function Add(AGroupIndex: Integer; AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec; reintroduce;
    property Items[Position: Integer]: PIDEOptionsGroupRec read GetItem write SetItem; default;
    property LastSelected: PIDEOptionsEditorRec read FLastSelected write FLastSelected;
  end;

  TAbstractOptionsEditorDialog = class(TForm)
  public
    function AddButton: TBitBtn; virtual; abstract;
    procedure AddButtonSeparator; virtual; abstract;
    function AddControl(AControlClass: TControlClass): TControl; virtual; abstract; reintroduce;
    function FindEditor(AEditor: TAbstractIDEOptionsEditorClass): TAbstractIDEOptionsEditor; virtual; abstract;
    function FindEditor(const IDEOptionsEditorClassName: string): TAbstractIDEOptionsEditor; virtual; abstract;
    function FindEditor(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditor; virtual; abstract;
    function FindEditorClass(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditorClass; virtual; abstract;
    procedure OpenEditor(AEditor: TAbstractIDEOptionsEditorClass); virtual; abstract;
    procedure OpenEditor(GroupIndex, AIndex: integer); virtual; abstract;
    function ResetFilter: Boolean; virtual; abstract;
    procedure UpdateBuildModeGUI; virtual; abstract;
  end;

function GetFreeIDEOptionsGroupIndex(AStartIndex: Integer): Integer;
function GetFreeIDEOptionsIndex(AGroupIndex: Integer; AStartIndex: Integer): Integer;
function RegisterIDEOptionsGroup(AGroupIndex: Integer;
           AGroupClass: TAbstractIDEOptionsClass;
           FindFreeIndex: boolean = true): PIDEOptionsGroupRec;
function RegisterIDEOptionsEditor(AGroupIndex: Integer;
           AEditorClass: TAbstractIDEOptionsEditorClass;
           AIndex: Integer; AParent: Integer = NoParent;
           AutoCreateGroup: boolean = false): PIDEOptionsEditorRec;

function IDEEditorGroups: TIDEOptionsGroupList;

var
  IDEEditorOptions: TIDEEditorOptions;

const
  // Font style used by filter
  MatchFontStyle: TFontStyles = [fsBold, fsItalic]; // Color = clFuchsia;


implementation

var
  FIDEEditorGroups: TIDEOptionsGroupList;

function IDEEditorGroups: TIDEOptionsGroupList;
begin
  if FIDEEditorGroups = nil then
    FIDEEditorGroups := TIDEOptionsGroupList.Create;
  Result := FIDEEditorGroups;
end;

function RegisterIDEOptionsGroup(AGroupIndex: Integer;
  AGroupClass: TAbstractIDEOptionsClass; FindFreeIndex: boolean): PIDEOptionsGroupRec;
begin
  if FindFreeIndex then
    AGroupIndex:=GetFreeIDEOptionsGroupIndex(AGroupIndex);
  Result:=IDEEditorGroups.Add(AGroupIndex, AGroupClass);
end;

function RegisterIDEOptionsEditor(AGroupIndex: Integer;
  AEditorClass: TAbstractIDEOptionsEditorClass; AIndex: Integer;
  AParent: Integer; AutoCreateGroup: boolean): PIDEOptionsEditorRec;
var
  Rec: PIDEOptionsGroupRec;
begin
  Rec := IDEEditorGroups.GetByIndex(AGroupIndex);
  if Rec = nil then
  begin
    if not AutoCreateGroup then
      raise Exception.Create('RegisterIDEOptionsEditor: missing Group');
    Rec := RegisterIDEOptionsGroup(AGroupIndex, nil);
  end;

  if Rec^.Items = nil then
    Rec^.Items := TIDEOptionsEditorList.Create;
  Result:=Rec^.Items.Add(AEditorClass, AIndex, AParent);
end;

function GetFreeIDEOptionsGroupIndex(AStartIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := AStartIndex to High(Integer) do
    if IDEEditorGroups.GetByIndex(I) = nil then
      Exit(I);
  Result := -1;
end;

function GetFreeIDEOptionsIndex(AGroupIndex: Integer; AStartIndex: Integer): Integer;
var
  Rec: PIDEOptionsGroupRec;
  I: Integer;
begin
  Result := -1; // -1 = error
  Rec := IDEEditorGroups.GetByIndex(AGroupIndex);
  if Rec = nil then
    Exit;

  for I := AStartIndex to High(Integer) do
    if Rec^.Items.GetByIndex(I) = nil then
      Exit(I);
end;

function GroupListCompare(Item1, Item2: Pointer): Integer;
var
  Rec1: PIDEOptionsGroupRec absolute Item1;
  Rec2: PIDEOptionsGroupRec absolute Item2;
begin
  if Rec1^.Index < Rec2^.Index then
    Result := -1
  else
  if Rec1^.Index > Rec2^.Index then
    Result := 1
  else
    Result := 0;
end;

function OptionsListCompare(Item1, Item2: Pointer): Integer;
var
  Rec1: PIDEOptionsEditorRec absolute Item1;
  Rec2: PIDEOptionsEditorRec absolute Item2;
begin
  if Rec1^.Index < Rec2^.Index then
    Result := -1
  else
  if Rec1^.Index > Rec2^.Index then
    Result := 1
  else
    Result := 0;
end;

{ TDefaultFont }

constructor TDefaultFont.Create(AStyles: TFontStyles);
begin
  FStyles:=AStyles;
end;

{ TAbstractIDEOptionsEditor }

constructor TAbstractIDEOptionsEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultFonts:=nil;
end;

destructor TAbstractIDEOptionsEditor.Destroy;
var
  i: Integer;
begin
  if Assigned(FDefaultFonts) then begin
    for i := 0 to FDefaultFonts.Count-1 do
      FDefaultFonts.Objects[i].Free;
    FDefaultFonts.Free;
  end;
  inherited Destroy;
end;

procedure TAbstractIDEOptionsEditor.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

function TAbstractIDEOptionsEditor.Check: Boolean;
begin
  Result := True;
end;

procedure TAbstractIDEOptionsEditor.RestoreSettings(AOptions: TAbstractIDEOptions);
begin

end;

class function TAbstractIDEOptionsEditor.DefaultCollapseChildNodes: Boolean;
begin
  Result := False;
end;

function TAbstractIDEOptionsEditor.FindOptionControl(AClass: TControlClass): TControl;

  function Search(AControl: TControl): TControl;
  var
    i: Integer;
    AWinControl: TWinControl;
  begin
    if AControl is AClass then
      exit(AControl);
    // Search child controls inside this one.
    if AControl is TWinControl then begin
      AWinControl:=TWinControl(AControl);
      for i:=0 to AWinControl.ControlCount-1 do begin
        Result:=Search(AWinControl.Controls[i]); // Recursive call.
        if Result<>nil then exit;
      end;
    end;
    Result:=nil;
  end;

begin
  Result:=Search(GetParentForm(Self));
end;

procedure TAbstractIDEOptionsEditor.RememberDefaultStyles;
// Store original font styles of controls to a map so the filter can restore them.

  procedure Search(AControl: TControl);
  var
    i: Integer;
    AWinControl: TWinControl;
  begin
    // Store only if there is any style defined
    if AControl.Font.Style <> [] then begin
      Assert(AControl.Font.Style<>MatchFontStyle, 'Do not use the same font style that filter uses.');
      FDefaultFonts.AddObject(AControl.Name, TDefaultFont.Create(AControl.Font.Style));
    end;
    // Search child controls inside this one.
    if AControl is TWinControl then begin
      AWinControl:=TWinControl(AControl);
      for i:=0 to AWinControl.ControlCount-1 do
        Search(AWinControl.Controls[i]);  // Recursive call.
    end;
  end;

begin
  if FDefaultFonts=nil then begin
    FDefaultFonts:=TDefaultFontList.Create;
    Search(Self);
    FDefaultFonts.Sorted:=True;
  end;
end;

function TAbstractIDEOptionsEditor.ContainsTextInCaption(AText: string): Boolean;
var
  LowerText: String;

  function SearchComboBox({%H-}AControl: TCustomComboBox): Boolean;
  begin
    Result:=False;  // ToDo...
  end;

  function SearchListBox(AControl: TCustomListBox): Boolean;
  var
    i: Integer;
  begin
    Result:=False;
    for i := 0 to AControl.Items.Count-1 do begin
      if Pos(LowerText, LowerCase(AControl.Items[i]))>0 then begin
        //if Length(LowerText)>2 then
        //  DebugLn('TAbstractIDEOptionsEditor.ContainsTextInCaption: Searching "',
        //      LowerText, '", Found "', AControl.Items[i], '", in ListBox ', AControl.Name);
        // ToDo: Indicate found item somehow.
        Result:=True;
      end;
    end;
  end;

  function SearchListView({%H-}AControl: TCustomListView): Boolean;
  begin
    Result:=False;  // ToDo...
  end;

  function SearchTreeView({%H-}AControl: TCustomTreeView): Boolean;
  begin
    Result:=False;  // ToDo...
  end;

  function SearchStringGrid({%H-}AControl: TCustomStringGrid): Boolean;
  begin
    Result:=False;  // ToDo...
  end;

  function SearchMemo({%H-}AControl: TCustomMemo): Boolean;
  begin
    Result:=False;  // Memo.Caption returns all the lines, skip.
  end;

  function Search(AControl: TControl): Boolean;
  var
    i: Integer;
    AWinControl: TWinControl;
    DefStyle: TFontStyles;
  begin
    Result:=False;
    if AControl.Visible then begin
      // *** First find matches in different controls ***
      // TSynEdit can't be used here in IdeOptionsIntf !
      //if AControl is TSynEdit then  Found:=SearchSynEdit(AControl)
      if AControl is TCustomComboBox then
        Result:=SearchComboBox(TCustomComboBox(AControl))
      else if AControl is TCustomListBox then
        Result:=SearchListBox(TCustomListBox(AControl))
      else if AControl is TCustomListView then
        Result:=SearchListView(TCustomListView(AControl))
      else if AControl is TCustomTreeView then
        Result:=SearchTreeView(TCustomTreeView(AControl))
      else if AControl is TCustomStringGrid then
        Result:=SearchStringGrid(TCustomStringGrid(AControl))
      else if AControl is TCustomMemo then
        Result:=SearchMemo(TCustomMemo(AControl))
      else begin
        Result:=Pos(LowerText, LowerCase(AControl.Caption))>0;
        // Indicate the match
        if Result then
          AControl.Font.Style:=MatchFontStyle
        // or, remove the indication.
        else if AControl.Font.Style=MatchFontStyle then begin
          DefStyle:=[];
          if FDefaultFonts.Find(AControl.Name, i) then
            DefStyle:=TDefaultFont(FDefaultFonts.Objects[i]).FStyles;
          AControl.Font.Style:=DefStyle;
        end;
      end;
    end;

    // Check child controls inside this one.
    if AControl is TWinControl then begin
      AWinControl:=TWinControl(AControl);
      for i:=0 to AWinControl.ControlCount-1 do
        if Search(AWinControl.Controls[i]) then      // Recursive call
          Result:=True;
    end;
  end;

begin
  LowerText:=LowerCase(AText);
  Result:=Search(Self);
end;

{ TIDEOptionsEditorList }

function TIDEOptionsEditorList.GetItem(AIndex: Integer): PIDEOptionsEditorRec;
begin
  Result := PIDEOptionsEditorRec(inherited Get(AIndex));
end;

procedure TIDEOptionsEditorList.SetItem(AIndex: Integer; const AValue: PIDEOptionsEditorRec);
begin
  inherited Put(AIndex, AValue);
end;

procedure TIDEOptionsEditorList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(PIDEOptionsEditorRec(Ptr));
  inherited Notify(Ptr, Action);
end;

function TIDEOptionsEditorList.GetByIndex(AIndex: Integer): PIDEOptionsEditorRec;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i]^.Index = AIndex then
    begin
      Result := Items[i];
      break;
    end;
end;

procedure TIDEOptionsEditorList.Resort;
begin
  Sort(@OptionsListCompare);
end;

function TIDEOptionsEditorList.Add(AEditorClass: TAbstractIDEOptionsEditorClass;
  AIndex, AParent: Integer): PIDEOptionsEditorRec;
begin
  repeat
    Result := GetByIndex(AIndex);
    if Result = nil then break;
    inc(AIndex);
  until false;
  New(Result);
  Result^.Index := AIndex;
  Result^.Parent := AParent;
  Result^.Collapsed := AEditorClass.DefaultCollapseChildNodes;
  Result^.DefaultCollapsed := AEditorClass.DefaultCollapseChildNodes;
  inherited Add(Result);

  Result^.EditorClass := AEditorClass;
end;

{ TIDEOptionsGroupList }

function TIDEOptionsGroupList.GetItem(Position: Integer): PIDEOptionsGroupRec;
begin
  Result := PIDEOptionsGroupRec(inherited Get(Position));
end;

procedure TIDEOptionsGroupList.SetItem(Position: Integer;
  const AValue: PIDEOptionsGroupRec);
begin
  inherited Put(Position, AValue);
end;

procedure TIDEOptionsGroupList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    PIDEOptionsGroupRec(Ptr)^.Items.Free;
    Dispose(PIDEOptionsGroupRec(Ptr));
  end;
  inherited Notify(Ptr, Action);
end;

function TIDEOptionsGroupList.GetByIndex(AIndex: Integer): PIDEOptionsGroupRec;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i]^.Index = AIndex then
    begin
      Result := Items[i];
      break;
    end;
end;

function TIDEOptionsGroupList.GetByGroupClass(
  AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
  begin
    Result:=Items[i];
    if Result^.GroupClass=AGroupClass then exit;
  end;
  Result:=nil;
end;

procedure TIDEOptionsGroupList.Resort;
var
  i: integer;
begin
  Sort(@GroupListCompare);
  for i := 0 to Count - 1 do
    if Items[i]^.Items <> nil then
      Items[i]^.Items.Resort;
end;

procedure TIDEOptionsGroupList.DoAfterWrite(Restore: boolean);
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
begin
  for i := 0 to Count - 1 do
  begin
    Rec := Items[i];
    if Rec^.Items <> nil then
    begin
      if Rec^.GroupClass <> nil then
      begin
        Instance := Rec^.GroupClass.GetInstance;
        if Instance <> nil then
          Instance.DoAfterWrite(Restore);
      end;
    end;
  end;
end;

function TIDEOptionsGroupList.Add(AGroupIndex: Integer;
  AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec;
begin
  Result := GetByIndex(AGroupIndex);
  if Result = nil then
  begin
    New(Result);
    Result^.Index := AGroupIndex;
    Result^.Items := nil;
    Result^.Collapsed := False;
    Result^.DefaultCollapsed := False;
    inherited Add(Result);
  end;

  Result^.GroupClass := AGroupClass;
end;


initialization
  FIDEEditorGroups := nil;

finalization
  FreeAndNil(FIDEEditorGroups);

end.

