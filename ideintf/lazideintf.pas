{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Interface to the general IDE functions.
}
unit LazIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ProjectIntf;

type
  // open file flags
  TOpenFlag = (
    ofProjectLoading,// this open is part of opening a whole project
    ofOnlyIfExists,  // do not auto create non existing files
    ofRevert,        // reload file if already open
    ofQuiet,         // less messages
    ofAddToRecent,   // add file to recent files
    ofRegularFile,   // open as regular file (e.g. do not open projects)
    ofVirtualFile,   // open the virtual file
    ofConvertMacros, // replace macros in filename
    ofUseCache,      // do not update file from disk
    ofMultiOpen,     // set during loading multiple files
    ofDoNotLoadResource// do not open form, datamodule, ...
    );
  TOpenFlags = set of TOpenFlag;

  // new file flags
  TNewFlag = (
    nfIsPartOfProject, // force IsPartOfProject,
                       //   default is to use a heuristic
    nfIsNotPartOfProject,// forbid IsPartOfProject
    nfOpenInEditor,    // open in editor
    nfSave,            // save file instantly
    nfAddToRecent,     // add file to recent files
    nfQuiet,           // less messages
    nfConvertMacros,   // replace macros in filename
    nfBeautifySrc,     // beautify custom source
    nfCreateDefaultSrc // create initial source based on the type
    );
  TNewFlags = set of TNewFlag;


  { TLazIDEInterface }

  TLazIDEInterface = class(TComponent)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    function DoNewEditorFile(NewFileDescriptor: TProjectFileDescriptor;
        NewFilename: string; const NewSource: string;
        NewFlags: TNewFlags): TModalResult; virtual; abstract;
    function DoOpenEditorFile(AFileName:string; PageIndex: integer;
        Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoOpenFileAndJumpToIdentifier(const AFilename, AnIdentifier: string;
        PageIndex: integer; Flags: TOpenFlags): TModalResult; virtual; abstract;
  end;
  
var
  LazarusIDE: TLazIDEInterface; // will be set by the IDE

implementation

{ TLazIDEInterface }

constructor TLazIDEInterface.Create(TheOwner: TComponent);
begin
  LazarusIDE:=Self;
  inherited Create(TheOwner);
end;

destructor TLazIDEInterface.Destroy;
begin
  inherited Destroy;
  LazarusIDE:=nil;
end;

end.

