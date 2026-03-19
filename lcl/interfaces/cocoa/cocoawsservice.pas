unit CocoaWSService;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  Forms,
  MacOSAll, CocoaAll, CocoaConst, CocoaUtils, Cocoa_Extra;

type

  { TCocoaWidgetSetService }

  TCocoaWidgetSetService = class
  private
    _readyDropFiles: Boolean;
    _waitingDropFiles: NSMutableArray;
  public
    procedure setReadyDropFiles; inline;
    procedure dropWaitingFiles;
    procedure tryDropFiles( const filenames: NSArray );
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  CocoaWidgetSetService: TCocoaWidgetSetService;

implementation

{ TCocoaWidgetSetService }

procedure TCocoaWidgetSetService.setReadyDropFiles;
begin
  _readyDropFiles:= True;
end;

procedure TCocoaWidgetSetService.dropWaitingFiles;
var
  lFiles: array of string;
  lNSStr: NSString;
  i: Integer;
begin
  if _waitingDropFiles.count = 0 then
    exit;

  SetLength(lFiles, _waitingDropFiles.count);
  for i := 0 to _waitingDropFiles.count-1 do
  begin
    lNSStr := NSString(_waitingDropFiles.objectAtIndex(i));
    lFiles[i] := NSStringToString(lNSStr);
  end;
  Application.IntfDropFiles(lFiles);
  if Application.MainForm<>nil then
    Application.MainForm.IntfDropFiles(lFiles);

  _waitingDropFiles.removeAllObjects;
end;

// on MacOS, the system notifies the APP to open the files by calling
// TAppDelegate.application_openFiles(). for example, double-click
// the associated files in Finder to open the APP.
// at this time, the MainForm may not have been created, and the notifies
// information about the files will be lost.
// including Lazarus IDE itself will also be affected by this issue on startup.
// so save it in _waitingDropFiles first, DropWaitingFiles() will be called
// in TCocoaWidgetSet.AppRun().
procedure TCocoaWidgetSetService.tryDropFiles(const filenames: NSArray);
begin
  _waitingDropFiles.addObjectsFromArray(filenames);
  if _readyDropFiles then
    dropWaitingFiles;
end;

constructor TCocoaWidgetSetService.Create;
begin
  _waitingDropFiles:= NSMutableArray.new;
end;

destructor TCocoaWidgetSetService.Destroy;
begin
  _waitingDropFiles.release;
end;

initialization
  CocoaWidgetSetService:= TCocoaWidgetSetService.Create;

finalization
  FreeAndNil( CocoaWidgetSetService );

end.

