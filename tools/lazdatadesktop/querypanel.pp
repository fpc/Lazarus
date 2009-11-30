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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit querypanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, fpDatadict, FileUtil, Controls, ExtCtrls, StdCtrls,
  ComCtrls, LResources, LCLType, Dialogs, ActnList, datapanel, SynEdit, SynMemo,
  SynHighlighterSQL, lazdatadeskstr;
  
Type

  { TQueryPanel }

  TQueryPanel = Class(TCustomPanel)
  private
    FEngine: TFPDDEngine;
    FPToolBar : TPanel;
    FToolBar : TToolBar;
    FIL : TImageList;
    FAL : TActionList;
    AExecute : TAction;
    ANextQuery : TAction;
    APreviousQuery : TAction;
    ACloseQuery : TAction;
    ALoadSQL : TAction;
    ASaveSQL : TAction;
    AExport : TAction;
    ACreateCode : TAction;
    FMSQL: TSynMemo; // later change to SQL highlighting Syn memo.
    FSplit: TSplitter;
    FData : TDataPanel;
    FQueryHistory : TStrings;
    FCurrentQuery : Integer;
    FBusy : Boolean;
    procedure BExecClick(Sender: TObject);
    procedure CloseQueryClick(Sender: TObject);
    Function GetDataset: TDataset;
    procedure HaveNextQuery(Sender: TObject);
    procedure HavePreviousQuery(Sender: TObject);
    procedure LoadQueryClick(Sender: TObject);
    procedure NextQueryClick(Sender: TObject);
    procedure OnMemoKey(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PreviousQueryClick(Sender: TObject);
    procedure SaveQueryClick(Sender: TObject);
    procedure SetEngine(const AValue: TFPDDEngine);
    procedure ExportDataClick(Sender: TObject);
    procedure CreateCodeClick(Sender: TObject);
  Protected
    Procedure CreateControls; virtual;
    procedure CreateActions; virtual;
    procedure CreateButtons; virtual;
    procedure CreateImageList; virtual;
    Procedure NotBusy(Sender: TObject);
    Procedure DataShowing(Sender: TObject);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    procedure ExecuteQuery(Qry: String);
    procedure SaveQuery(AFileName: String);
    procedure LoadQuery(AFileName: String);
    Function AddToHistory(Qry : String) : Integer;
    Function NextQuery : Integer;
    Function PreviousQuery : Integer;
    Procedure CloseDataset;
    Procedure FreeDataset;
    Procedure ExportData;
    Procedure CreateCode;
    Property Dataset : TDataset Read GetDataset;
    Property Engine : TFPDDEngine Read FEngine Write SetEngine;
    Property QueryHistory : TStrings Read FQueryHistory;
    Property CurrentQuery : Integer Read FCurrentQuery;
    Property Busy : Boolean Read FBusy;
  end;

implementation

uses strutils, fpdataexporter, fpcodegenerator;

{ TQueryPanel }

{ ---------------------------------------------------------------------
  Setup
  ---------------------------------------------------------------------}
  
constructor TQueryPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateControls;
  FQueryHistory:=TStringList.Create;
  FCurrentQuery:=-1;
end;

destructor TQueryPanel.Destroy;
begin
  FreeAndNil(FQueryHistory);
  inherited Destroy;
end;

procedure TQueryPanel.SetEngine(const AValue: TFPDDEngine);
begin
  if FEngine=AValue then exit;
  If Assigned(Dataset) then
    begin
    CloseDataset;
    FreeDataset;
    end;
  FEngine:=AValue;
end;

procedure TQueryPanel.ExportDataClick(Sender: TObject);
begin
  ExportData;
end;

procedure TQueryPanel.CreateCodeClick(Sender: TObject);
begin
  CreateCode;
end;

function TQueryPanel.GetDataset: TDataset;
begin
  Result:=FData.Dataset;
end;

procedure TQueryPanel.CreateControls;
begin
  // Images for actionlist/toolbar
  CreateImageList;
  // Actions;
  CreateActions;
  // Toolbar panel;
  FPToolBar:=TPanel.Create(Self);
  FPToolBar.Parent:=Self;
  FPToolBar.Align:=alTop;
  FPToolBar.height:=30;
  // Toolbar itself
  FToolBar:=TToolbar.Create(Self);
  FToolBar.Parent:=FPToolBar;
  FToolBar.Images:=FIL;
  FToolbar.Flat:=True;
  FToolBar.ShowHint:=True;
  // Toolbar buttons
  CreateButtons;
  // Data panel
  FData:=TDataPanel.Create(Self);
  FData.Parent:=Self;
  FData.Align:=alBottom;
  FData.Height:=200;
  FData.Visible:=False;
  FData.ShowExtraButtons:=False;
  // Splitter
  FSplit:=TSplitter.Create(Self);
  FSplit.Parent:=Self;
  FSplit.Align:=alBottom;
  // Syntax memo;
  FMSQL:=TSynMemo.Create(Self);
  FMSQL.Parent:=Self;
  FMSQL.Align:=AlClient;
  FMSQL.Highlighter:=TSynSQLSyn.Create(Self);
  FMSQL.Options:=[eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces, eoTrimTrailingSpaces, eoBracketHighlight];
  FMSQL.OnKeyDown:=@OnMemoKey;
  FMSQL.ExtraLineSpacing:=2;
end;

procedure TQueryPanel.CreateImageList;

begin
  FIL:=TImageList.Create(Self);
  FIL.AddLazarusResource('qrybtn_execute');
  FIL.AddLazarusResource('qrybtn_close');
  FIL.AddLazarusResource('qrybtn_previous');
  FIL.AddLazarusResource('qrybtn_next');
  FIL.AddLazarusResource('qrybtn_open');
  FIL.AddLazarusResource('qrybtn_save');
  FIL.AddLazarusResource('qrybtn_export');
  FIL.AddLazarusResource('qrybtn_code');
end;

procedure TQueryPanel.CreateActions;

  Function NewAction(ACaption,AHint : String; AImageIndex : Integer; AOnExecute,AOnUpdate : TNotifyEvent) : TAction;
  
  begin
    Result:=TAction.Create(Self);
    Result.Caption:=ACaption;
    Result.Hint:=AHint;
    Result.ImageIndex:=AImageIndex;
    Result.OnExecute:=AOnExecute;
    Result.OnUpdate:=AOnUpdate;
    Result.ActionList:=FAL;
  end;

begin
  FAL:=TActionList.Create(Self);
  FAL.Images:=FIL;
  AExecute:=NewAction(SExecute,SHintExecute,0,@BExecClick,@NotBusy);
  AExecute.ShortCut:=KeyToShortCut(VK_E,[ssCtrl]);
  ACloseQuery:=NewAction(SClose,SHintClose,1,@CloseQueryClick,@DataShowing);
  APreviousQuery:=NewAction(SPrevious,SHintPrevious,2,@PreviousQueryClick,@HavePreviousQuery);
  ANextQuery:=NewAction(SNext,SHintNext,3,@NextQueryClick,@HaveNextQuery);
  ALoadSQL:=NewAction(SLoad,SHintLoad,4,@LoadQueryClick,@NotBusy);
  ASaveSQL:=NewAction(SSave,SHintSave,5,@SaveQueryClick,@NotBusy);
  AExport:=NewAction(SExport,SHintExport,6,@ExportDataClick,@DataShowing);
  ACreateCode:=NewAction(SCreateCode,SHintCreateCode,7,@CreateCodeClick,@DataShowing);
end;


procedure TQueryPanel.CreateButtons;

  Function NewButton(AAction : TAction; Var L : Integer) : TToolButton;

  begin
    Result:=TToolbutton.Create(FToolBar);
    Result.Parent:=FToolBar;
    Result.Action:=AAction;
    Result.Left:=L;
    L:=L+FToolBar.ButtonWidth+1;
  end;
  
  procedure NewSeparator(Var L : Integer);
  var
    B : TToolButton;
  begin
    B:=NewButton(Nil,L);
    B.Style:=tbsSeparator;
    B.Width:=8;
    Dec(L,FToolBar.ButtonWidth-8);
  end;

Var
  L : integer;

begin
  L:=0;
  NewButton(AExecute,L);
  NewButton(ACloseQuery,L);
  NewSeparator(L);
  NewButton(APreviousQuery,L);
  NewButton(ANextQuery,L);
  NewSeparator(L);
  NewButton(ALoadSQL,L);
  NewButton(ASaveSQL,L);
  NewSeparator(L);
  NewButton(AExport,L);
  NewButton(ACreateCode,L);
end;

{ ---------------------------------------------------------------------
  Callbacks
  ---------------------------------------------------------------------}

procedure TQueryPanel.OnMemoKey(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  If (Key=VK_E) and (Shift=[ssCtrl]) then
    begin
    AExecute.Execute;
    Key:=0;
    end;
end;

procedure TQueryPanel.BExecClick(Sender : TObject);

begin
  ExecuteQuery(FMSQL.Lines.Text);
end;

procedure TQueryPanel.CloseQueryClick(Sender : TObject);

begin
  CloseDataset;
end;

procedure TQueryPanel.NotBusy(Sender : TObject);

begin
  (Sender as TAction).Enabled:=Not FBusy;
end;

procedure TQueryPanel.DataShowing(Sender : TObject);

Var
  DS : TDataset;

begin
  DS:=Dataset;
  (Sender as TAction).Enabled:=Assigned(DS) and DS.Active;
end;

procedure TQueryPanel.HaveNextQuery(Sender : TObject);

begin
  (Sender as TAction).Enabled:=(FCurrentQuery<FQueryHistory.Count-1);
end;

procedure TQueryPanel.HavePreviousQuery(Sender : TObject);

begin
  (Sender as TAction).Enabled:=(FCurrentQuery>0);
end;

procedure TQueryPanel.NextQueryClick(Sender : TObject);

begin
  NextQuery;
end;

procedure TQueryPanel.PreviousQueryClick(Sender : TObject);

begin
  PreviousQuery;
end;

procedure TQueryPanel.LoadQueryClick(Sender : TObject);

begin
  With TOpenDialog.Create(Self) do
    try
      Filter:=SSQLFilters;
      Options:=[ofFileMustExist];
      If Execute then
        LoadQuery(FileName);
    Finally
      Free;
    end;
end;

procedure TQueryPanel.SaveQueryClick(Sender : TObject);

begin
  With TSaveDialog.Create(Self) do
    try
      Filter:=SSQLFilters;
      Options:=[ofPathMustExist,ofOverwritePrompt];
      If Execute then
        SaveQuery(FileName);
    Finally
      Free;
    end;
end;

{ ---------------------------------------------------------------------
  Actual commands
  ---------------------------------------------------------------------}

procedure TQueryPanel.LoadQuery(AFileName: String);

begin
  FMSQL.Lines.LoadFromFile(UTF8ToSys(AFileName));
end;

function TQueryPanel.AddToHistory(Qry: String): Integer;

Var
  I : Integer;

begin
  I:=FQueryHistory.IndexOf(Qry);
  If (I=-1) then
    FCurrentQuery:=FQueryHistory.Add(Qry)
  else
    begin
    FQueryHistory.Move(I,FQueryHistory.Count-1);
    FCurrentQuery:=FQueryHistory.Count-1;
    end;
  Result:=FCurrentQuery;
end;

Function TQueryPanel.NextQuery : Integer;
begin
  If FCurrentQuery<FQueryHistory.Count-1 then
    begin
    Inc(FCurrentQuery);
    FMSQL.Lines.Text:=FQueryHistory[FCurrentQuery];
    end;
  Result:=FCurrentQuery;
end;

Function TQueryPanel.PreviousQuery : Integer;
begin
  If (FCurrentQuery>0) then
    begin
    Dec(FCurrentQuery);
    FMSQL.Lines.Text:=FQueryHistory[FCurrentQuery];
    end;
  Result:=FCurrentQuery;
end;


procedure TQueryPanel.SaveQuery(AFileName: String);

begin
  FMSQL.Lines.SaveToFile(UTF8ToSys(AFileName));
end;

procedure TQueryPanel.ExecuteQuery(Qry : String);

Var
  DS : TDataset;
  S : String;
  N : Integer;
  
begin
  FBusy:=True;
  Try
    If Not assigned(FEngine) then
      Raise Exception.Create(SErrNoEngine);
    DS:=Dataset;
    If Assigned(DS) then
      CloseDataset;
    S:=ExtractDelimited(1,Trim(Qry),[' ',#9,#13,#10]);
    If (CompareText(S,'SELECT')<>0) then
      begin
      N:=FEngine.RunQuery(Qry);
      If ecRowsAffected in FEngine.EngineCapabilities then
        ShowMessage(Format(SRowsAffected,[N]));
      end
    else
      begin
      If Assigned(DS) then
        FEngine.SetQueryStatement(Qry,DS)
      else
        begin
        DS:=FEngine.CreateQuery(Qry,Self);
        FData.Dataset:=DS;
        end;
      FData.Visible:=True;
      FSplit.Top:=FData.Top-10;
      DS.Open;
      end;
    AddToHistory(Qry);
    ACloseQuery.Update;
  Finally
    FBusy:=False;
  end;
end;

procedure TQueryPanel.CloseDataset;
begin
  FBusy:=True;
  Try
    FData.Dataset.Close;
    FData.Visible:=False;
    ACloseQuery.Update;
  Finally
    FBusy:=False;
  end;
end;

procedure TQueryPanel.FreeDataset;

Var
  D : TDataset;

begin
  D:=FData.Dataset;
  FData.Dataset:=Nil;
  D.Free;
end;



procedure TQueryPanel.ExportData;

begin
  With TFPDataExporter.Create(Dataset) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TQueryPanel.CreateCode;
begin
  With TFPCodeGenerator.Create(Dataset) do
    try
      SQL:=FMSQL.Lines;
      DataSet:=Self.Dataset;
      Execute;
    Finally
      Free;
    end;
end;



initialization
{$i querypanel.lrs}
end.

