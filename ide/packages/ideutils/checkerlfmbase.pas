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
unit CheckerLfmBase;

{$mode objfpc}{$H+}

interface

uses
  // FCL
  Classes, SysUtils, Math, Contnrs, System.UITypes,
  // LCL
  StdCtrls,
  // LazUtils
  LazLoggerBase, LazTracer,
  // CodeTools
  BasicCodeTools, CodeCache, LFMTrees,
  // BuildIntf
  ComponentReg,
  // SynEdit
  SynEdit;

type
  TLFMChangeEntry = class
  public
    StartPos, EndPos: integer;
    NewText: string;
  end;

  { TCheckerLFMBase }

  TCheckerLFMBase = class
  private
  protected
    fPascalBuffer: TCodeBuffer;
    fLFMBuffer: TCodeBuffer;
    fLFMTree: TLFMTree;
    fRootMustBeClassInUnit: boolean;
    fRootMustBeClassInIntf: boolean;
    fObjectsMustExist: boolean;
    // References to controls in UI:
    fLFMSynEdit: TSynEdit;
    fErrorsListBox: TListBox;
    procedure AddReplacement(LFMChangeList: TObjectList; StartPos, EndPos: integer;
                             const NewText: string);
    function ApplyReplacements(LFMChangeList: TList): boolean;
    procedure FillErrorsListBox;
    function FindAndFixMissingComponentClasses: TModalResult;
    procedure FindNiceNodeBounds(LFMNode: TLFMTreeNode;
                                 out StartPos, EndPos: integer);
    function FixMissingComponentClasses(aMissingTypes: TClassList): TModalResult; virtual; abstract;
    procedure LoadFormFile;
  public
    constructor Create(APascalBuffer, ALFMBuffer: TCodeBuffer);
    destructor Destroy; override;
    procedure JumpToError(LFMError: TLFMError);
    function FindListBoxError: TLFMError;
  public
    property PascalBuffer: TCodeBuffer read fPascalBuffer;
    property LFMBuffer: TCodeBuffer read fLFMBuffer;
    property LFMTree: TLFMTree read fLFMTree;
    property RootMustBeClassInUnit: boolean read fRootMustBeClassInUnit
                                           write fRootMustBeClassInUnit;
    property RootMustBeClassInIntf: boolean read fRootMustBeClassInIntf
                                           write fRootMustBeClassInIntf;
    property ObjectsMustExist: boolean read fObjectsMustExist
                                       write fObjectsMustExist;
  end;


implementation


{ TCheckerLFMBase }

constructor TCheckerLFMBase.Create(APascalBuffer, ALFMBuffer: TCodeBuffer);
begin
  fPascalBuffer:=APascalBuffer;
  fLFMBuffer:=ALFMBuffer;
  fRootMustBeClassInIntf:=false;
  fObjectsMustExist:=false;
end;

destructor TCheckerLFMBase.Destroy;
begin
  inherited Destroy;
end;

procedure TCheckerLFMBase.AddReplacement(LFMChangeList: TObjectList;
  StartPos, EndPos: integer; const NewText: string);
var
  Entry: TLFMChangeEntry;
  NewEntry: TLFMChangeEntry;
  i: Integer;
begin
  if StartPos>EndPos then
    RaiseGDBException('TCheckLFMDialog.AddReplaceMent StartPos>EndPos');
  // check for intersection
  for i:=0 to LFMChangeList.Count-1 do begin
    Entry:=TLFMChangeEntry(LFMChangeList[i]);
    if ((Entry.StartPos<EndPos) and (Entry.EndPos>StartPos)) then begin
      // New and Entry intersects
      if (Entry.NewText='') and (NewText='') then begin
        // both are deletes => combine
        StartPos:=Min(StartPos,Entry.StartPos);
        EndPos:=Max(EndPos,Entry.EndPos);
      end else begin
        // not allowed
        RaiseGDBException('TCheckLFMDialog.AddReplaceMent invalid Intersection');
      end;
    end;
  end;
  // combine deletions
  if NewText='' then begin
    for i:=LFMChangeList.Count-1 downto 0 do begin
      Entry:=TLFMChangeEntry(LFMChangeList[i]);
      if ((Entry.StartPos<EndPos) and (Entry.EndPos>StartPos)) then
        // New and Entry intersects -> remove Entry
        LFMChangeList.Delete(i);
    end;
  end;
  // insert new entry
  NewEntry:=TLFMChangeEntry.Create;
  NewEntry.NewText:=NewText;
  NewEntry.StartPos:=StartPos;
  NewEntry.EndPos:=EndPos;
  if LFMChangeList.Count=0 then begin
    LFMChangeList.Add(NewEntry);
  end else begin
    for i:=0 to LFMChangeList.Count-1 do begin
      Entry:=TLFMChangeEntry(LFMChangeList[i]);
      if EndPos<=Entry.StartPos then begin
        // insert in front
        LFMChangeList.Insert(i,NewEntry);
        break;
      end else if i=LFMChangeList.Count-1 then begin
        // insert behind
        LFMChangeList.Add(NewEntry);
        break;
      end;
    end;
  end;
end;

function TCheckerLFMBase.ApplyReplacements(LFMChangeList: TList): boolean;
var
  i: Integer;
  Entry: TLFMChangeEntry;
begin
  Result:=false;
  for i:=LfmChangeList.Count-1 downto 0 do begin
    Entry:=TLFMChangeEntry(LfmChangeList[i]);
    fLFMBuffer.Replace(Entry.StartPos,Entry.EndPos-Entry.StartPos,Entry.NewText);
  end;
  Result:=true;
end;

procedure TCheckerLFMBase.FillErrorsListBox;
var
  CurError: TLFMError;
  Filename: String;
  Msg: String;
begin
  fErrorsListBox.Items.BeginUpdate;
  fErrorsListBox.Items.Clear;
  if fLFMTree<>nil then begin
    Filename:=ExtractFileName(fLFMBuffer.Filename);
    CurError:=fLFMTree.FirstError;
    while CurError<>nil do begin
      Msg:=Filename
           +'('+IntToStr(CurError.Caret.Y)+','+IntToStr(CurError.Caret.X)+')'
           +' Error: '
           +CurError.ErrorMessage;
      fErrorsListBox.Items.Add(Msg);
      CurError:=CurError.NextError;
    end;
  end;
  fErrorsListBox.Items.EndUpdate;
end;

function TCheckerLFMBase.FindAndFixMissingComponentClasses: TModalResult;
// returns true, if after adding units to uses section all errors are fixed
var
  CurError: TLFMError;
  MissingObjectTypes: TClassList;
  RegComp: TRegisteredComponent;
  AClassName: String;
begin
  Result:=mrOK;
  MissingObjectTypes:=TClassList.Create;
  try
    // collect all missing object types
    CurError:=fLFMTree.FirstError;
    while CurError<>nil do begin
      if CurError.IsMissingObjectType then begin
        AClassName:=(CurError.Node as TLFMObjectNode).TypeName;
        RegComp:=IDEComponentPalette.FindRegComponent(AClassName);
        if Assigned(RegComp) and (RegComp.GetUnitName<>'')
        and (MissingObjectTypes.IndexOf(RegComp.ComponentClass)<0)
        then
          MissingObjectTypes.Add(RegComp.ComponentClass);
      end;
      CurError:=CurError.NextError;
    end;
    // Now the list contains only types that are found in IDE.
    if MissingObjectTypes.Count>0 then
      Result:=FixMissingComponentClasses(MissingObjectTypes); // Fix them.
  finally
    MissingObjectTypes.Free;
  end;
end;

function TCheckerLFMBase.FindListBoxError: TLFMError;
var
  i: Integer;
begin
  Result:=nil;
  i:=fErrorsListBox.ItemIndex;
  if (i<0) or (i>=fErrorsListBox.Items.Count) then exit;
  Result:=fLFMTree.FirstError;
  while Result<>nil do begin
    if i=0 then exit;
    Result:=Result.NextError;
    dec(i);
  end;
end;

procedure TCheckerLFMBase.FindNiceNodeBounds(LFMNode: TLFMTreeNode;
  out StartPos, EndPos: integer);
var
  Src: String;
begin
  Src:=fLFMBuffer.Source;
  StartPos:=FindLineEndOrCodeInFrontOfPosition(Src,LFMNode.StartPos,1,false,true);
  EndPos:=FindLineEndOrCodeInFrontOfPosition(Src,LFMNode.EndPos,1,false,true);
  EndPos:=FindLineEndOrCodeAfterPosition(Src,EndPos,length(Src),false);
end;

procedure TCheckerLFMBase.JumpToError(LFMError: TLFMError);
begin
  if LFMError=nil then exit;
  fLFMSynEdit.CaretXY:=LFMError.Caret;
end;

procedure TCheckerLFMBase.LoadFormFile;
begin
  fLFMSynEdit.Lines.Text:=fLFMBuffer.Source;
  FillErrorsListBox;
end;

end.
