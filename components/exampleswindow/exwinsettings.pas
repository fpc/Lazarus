unit exwinsettings;
{
 **********************************************************************
  This file is part of a Lazarus Package, Examples Window.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

This unit makes a frame that is poked into Lazarus's Options Tree. At present
all it gets back is the user's preference as to where the Example Projects
working space is. Easily extended.  David Bannon, Feb 2022

}



{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn, IDEOptionsIntf,
    IDEOptEditorIntf;

{ TExWinSettings }

// -------- The Options Group ID, and, perhaps, a place in the Tree View -------

type
    TExWinSettings = class(TAbstractIDEEnvironmentOptions)          // needed by options group.

    private

    public
        constructor Create(const pbReadRegFile: boolean);
        destructor Destroy; override;
        class function GetGroupCaption: String; override;
        class function GetInstance: TAbstractIDEOptions; override;
        procedure DoAfterWrite({%H-}Restore: boolean); override;
    end;


// ------ This is the Frame displayed when user clicks the Tree View note ------
type
    { TExWinSettingsFrame }
    TExWinSettingsFrame = class(TAbstractIDEOptionsEditor)
        ButtonDefault: TButton;
        DirectoryEdit1: TDirectoryEdit;
        Label1: TLabel;
        procedure ButtonDefaultClick(Sender: TObject);

    private
        DefaultExamplesHome : string;

    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        function GetTitle: String; override;
        procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
        procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
        class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
        procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
        procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    end;


var
  ExWindowOptionsGroup : integer;
  ExWinOptionsFrameID : integer;

implementation

uses Dialogs, LazLogger, UConst, baseIDEIntf, LazConfigStorage, LazFileUtils,
    LazIDEIntf;

{$R *.lfm}

{ TExWinSettings }

constructor TExWinSettings.Create(const pbReadRegFile: boolean);
begin
    // inherited Create;
end;

destructor TExWinSettings.Destroy;
begin
    inherited Destroy;
end;

class function TExWinSettings.GetGroupCaption: String;
begin
    Result := rsExampleProjects;
end;

class function TExWinSettings.GetInstance: TAbstractIDEOptions;
begin
    //result := TAbstractIDEOptions(self);    // Nope, it does not like that !
    result := nil;
end;

procedure TExWinSettings.DoAfterWrite(Restore: boolean);
begin
    inherited DoAfterWrite(Restore);
end;

{ TExWinSettingsFrame }


procedure TExWinSettingsFrame.ButtonDefaultClick(Sender: TObject);
begin
    DirectoryEdit1.Text := DefaultExamplesHome;
end;

constructor TExWinSettingsFrame.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    DefaultExamplesHome := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)
                                                 + AppendPathDelim(cExamplesDir);
end;

destructor TExWinSettingsFrame.Destroy;
begin
    inherited Destroy;
end;

function TExWinSettingsFrame.GetTitle: String;
begin
    Result := rsExampleProjects;
end;

procedure TExWinSettingsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Config: TConfigStorage;
begin
    try
      Config := GetIDEConfigStorage(cConfigFileName, true);
      try
        DirectoryEdit1.Text := Config.GetValue('Examples/Directory', DefaultExamplesHome);

      finally
        Config.Free;
      end;
    except
      on E: Exception do begin
        DebugLn('TExWinSettingsFrame.ReadSettings Loading ' +  cConfigFileName + ' failed: ' + E.Message);
      end;
    end;

end;

// Maybe the initial settings before we have a config file ?  Labels and Captions.
procedure TExWinSettingsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
    Label1.Caption := rsDirWhereExamplesGo;
end;

class function TExWinSettingsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
    Result := nil;
end;

// Gets called whenever user opens Options tree.
procedure TExWinSettingsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Config: TConfigStorage;
begin
    try
       Config := GetIDEConfigStorage(cConfigFileName,false);
       try
         Config.SetDeleteValue('Examples/Directory',DirectoryEdit1.Text, DefaultExamplesHome);
       finally
         Config.Free;
       end;
     except
       on E: Exception do begin
         DebugLn('TExWinSettingsFrame.ReadSettings Saving ' +  cConfigFileName + ' failed: ' + E.Message);
       end;
     end;
end;

procedure TExWinSettingsFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
begin
    inherited RestoreSettings(AOptions);
end;


initialization
    ExWindowOptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
    RegisterIDEOptionsGroup(ExWindowOptionsGroup, TExWinSettings, False);   // F cos I get Index from above line. I think.


end.
