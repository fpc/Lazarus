{  $Id$  }
{
 /***************************************************************************
                          ideoptionsdefs.pp  -  Toolbar
                          -----------------------------


 ***************************************************************************/

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
unit IDEOptionDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, Laz_XMLCfg, LCLProc, FileUtil,
  Forms, Controls, Buttons, BaseIDEIntf, LazConfigStorage,
  IDEWindowIntf, LazConf;

type
  { TXMLOptionsStorage }

  TXMLOptionsStorage = class(TConfigStorage)
  private
    FFreeXMLConfig: boolean;
    FXMLConfig: TXMLConfig;
  protected
    function  GetFullPathValue(const APath, ADefault: String): String; override;
    function  GetFullPathValue(const APath: String; ADefault: Integer): Integer; override;
    function  GetFullPathValue(const APath: String; ADefault: Boolean): Boolean; override;
    procedure SetFullPathValue(const APath, AValue: String); override;
    procedure SetDeleteFullPathValue(const APath, AValue, DefValue: String); override;
    procedure SetFullPathValue(const APath: String; AValue: Integer); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Integer); override;
    procedure SetFullPathValue(const APath: String; AValue: Boolean); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Boolean); override;
    procedure DeleteFullPath(const APath: string); override;
    procedure DeleteFullPathValue(const APath: string); override;
  public
    constructor Create(const Filename: string; LoadFromDisk: Boolean); override;
    constructor Create(TheXMLConfig: TXMLConfig);
    constructor Create(TheXMLConfig: TXMLConfig; const StartPath: string);
    destructor Destroy; override;
    procedure Clear; override;
    property XMLConfig: TXMLConfig read FXMLConfig;
    property FreeXMLConfig: boolean read FFreeXMLConfig write FFreeXMLConfig;
    procedure WriteToDisk; override;
    function GetFilename: string; override;
  end;


  { non modal IDE windows }
type
  TNonModalIDEWindow = (
    nmiwNone, // empty/none/undefined
    nmiwMainIDEName,
    nmiwSourceNoteBookName,
    nmiwMessagesViewName,
    nmiwUnitDependenciesName,
    nmiwCodeExplorerName,
    nmiwFPDocEditorName,
    nmiwClipbrdHistoryName,
    nmiwPkgGraphExplorer,
    nmiwProjectInspector,
    // debugger
    nmiwDbgOutput,
    nmiwDbgEvents,
    nmiwBreakPoints,
    nmiwWatches,
    nmiwLocals,
    nmiwCallStack,
    nmiwEvaluate,
    nmiwRegisters,
    nmiwAssembler,
    nmiwInspect,
    // extra
    nmiwSearchResultsViewName,
    nmiwAnchorEditor,
    nmiwCodeBrowser,
    nmiwIssueBrowser,
    nmiwJumpHistory
    );

const
  // This is the list of IDE windows, that will not be automatically reopened
  // on startup. These windows are opened automatically when needed.
  NonModalIDEWindowManualOpen = [
    nmiwNone,
    nmiwMainIDEName,
    nmiwSourceNoteBookName,
    nmiwDbgOutput,
    nmiwDbgEvents,
    nmiwSearchResultsViewName,
    nmiwAnchorEditor
    ];

  // form names for non modal IDE windows:
  NonModalIDEWindowNames: array[TNonModalIDEWindow] of string = (
    '?',
    'MainIDE',
    'SourceNotebook',
    'MessagesView',
    'UnitDependencies',
    'CodeExplorerView',
    'FPDocEditor',
    'ClipBrdHistory',
    'PkgGraphExplorer',
    'ProjectInspector',
    // debugger
    'DbgOutput',
    'DbgEvents',
    'BreakPoints',
    'Watches',
    'Locals',
    'CallStack',
    'EvaluateModify',
    'Registers',
    'Assembler',
    'Inspect',
    // extra
    'SearchResults',
    'AnchorEditor',
    'CodeBrowser',
    'IssueBrowser',
    'JumpHistory'
   );

function CreateNiceWindowPosition(Width, Height: integer): TRect;
function NonModalIDEFormIDToEnum(const FormID: string): TNonModalIDEWindow;

function GetLazIDEConfigStorage(const Filename: string; LoadFromDisk: Boolean
                                ): TConfigStorage; // load errors: raises exceptions

implementation


function CreateNiceWindowPosition(Width, Height: integer): TRect;

  function FindFormAt(x,y: integer): TCustomForm;
  var
    i: Integer;
  begin
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      Result := Screen.CustomForms[i];
      if Result.HandleAllocated and Result.Visible
      and (Result.Left >= x - 5) and (Result.Left <= x + 5)
      and (Result.Top >= y - 5) and (Result.Top <= y + 5)
      then
        exit;
    end;
    Result := nil;
  end;

var
  MinX: Integer;
  MinY: Integer;
  MaxX: Integer;
  MaxY: Integer;
  x: Integer;
  y: Integer;
  MidX: Integer;
  MidY: Integer;
  Step: Integer;
  ABounds: TRect;
begin
  if Screen.ActiveCustomForm <> nil then
    ABounds := Screen.ActiveCustomForm.Monitor.BoundsRect
  else
  if Application.MainForm <> nil then
    ABounds := Application.MainForm.Monitor.BoundsRect
  else
    ABounds := Screen.PrimaryMonitor.BoundsRect;

  MinX := ABounds.Left;
  MinY := ABounds.Top;
  MaxX := ABounds.Right - Width - 10;
  if MaxX < MinX + 10 then MaxX := MinX + 10;
  MaxY := ABounds.Bottom - Height - 100; // why -100?
  if MaxY < MinY + 10 then MaxY := MinY + 10;
  MidX := (MaxX + MinX) div 2;
  MidY := (MaxY + MinY) div 2;
  Step := 0;
  repeat
    x := MidX - Step * 20;
    y := MidY - Step * 20;
    if (x < MinX) or (x > MaxX) or (y < MinY) or (y > MaxY) then break;
    if (FindFormAt(x, y)=nil) or (Step > 1000) then break;
    inc(Step);
  until False;
  Result.Left := x;
  Result.Top := y;
  Result.Right := x + Width;
  Result.Bottom := y + Height;
end;

function NonModalIDEFormIDToEnum(const FormID: string): TNonModalIDEWindow;
begin
  for Result:=Low(TNonModalIDEWindow) to High(TNonModalIDEWindow) do
    if NonModalIDEWindowNames[Result]=FormID then
      exit;
  Result:=nmiwNone;
end;

function GetLazIDEConfigStorage(const Filename: string; LoadFromDisk: Boolean
  ): TConfigStorage;
var
  ConfigFilename: String;
begin
  if LoadFromDisk then begin
    // copy template config file to users config directory
    CopySecondaryConfigFile(Filename);
  end;
  // create storage
  ConfigFilename:=AppendPathDelim(GetPrimaryConfigPath)+Filename;
  Result:=TXMLOptionsStorage.Create(ConfigFilename,LoadFromDisk);
end;

{ TXMLOptionsStorage }

function TXMLOptionsStorage.GetFullPathValue(const APath, ADefault: String): String;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLOptionsStorage.GetFullPathValue(const APath: String;
  ADefault: Integer): Integer;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLOptionsStorage.GetFullPathValue(const APath: String;
  ADefault: Boolean): Boolean;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath, AValue: String);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath, AValue,
  DefValue: String);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath: String;
  AValue: Integer);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Integer);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath: String;
  AValue: Boolean);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Boolean);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.DeleteFullPath(const APath: string);
begin
  XMLConfig.DeletePath(APath);
end;

procedure TXMLOptionsStorage.DeleteFullPathValue(const APath: string);
begin
  XMLConfig.DeleteValue(APath);
end;

constructor TXMLOptionsStorage.Create(const Filename: string;
  LoadFromDisk: Boolean);
begin
  if LoadFromDisk then
    FXMLConfig:=TXMLConfig.Create(Filename)
  else
    FXMLConfig:=TXMLConfig.CreateClean(Filename);
  FFreeXMLConfig:=true;
end;

constructor TXMLOptionsStorage.Create(TheXMLConfig: TXMLConfig);
begin
  FXMLConfig:=TheXMLConfig;
  if FXMLConfig=nil then
    raise Exception.Create('');
end;

constructor TXMLOptionsStorage.Create(TheXMLConfig: TXMLConfig;
  const StartPath: string);
begin
  Create(TheXMLConfig);
  AppendBasePath(StartPath);
end;

destructor TXMLOptionsStorage.Destroy;
begin
  if FreeXMLConfig then FreeAndNil(FXMLConfig);
  inherited Destroy;
end;

procedure TXMLOptionsStorage.Clear;
begin
  FXMLConfig.Clear;
end;

procedure TXMLOptionsStorage.WriteToDisk;
begin
  FXMLConfig.Flush;
end;

function TXMLOptionsStorage.GetFilename: string;
begin
  Result:=FXMLConfig.Filename;
end;

initialization
  DefaultConfigClass:=TXMLOptionsStorage;
  GetIDEConfigStorage:=@GetLazIDEConfigStorage;

end.

