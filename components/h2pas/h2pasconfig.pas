{ Copyright (C) 2006 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

  Abstract: Options
}
unit H2PasConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage;
  
type
  TH2PasProject = class;

  TH2PasFile = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
  public
    property Project: TH2PasProject;
    property Filename: string;
    property Enabled: boolean read FEnabled write FEnabled;
    property Modified: boolean;
  end;

  TH2PasProject = class(TPersistent)
  private
    FCHeaderFiles: TFPList;// list of TH2PasFile
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
  public
    property CHeaderFileCount: integer read GetCHeaderFileCount;
    property CHeaderFiles[Index: integer]: TH2PasFile read GetCHeaderFiles;
    property Modified: boolean;
  end;
  
  TH2PasOptions = class(TPersistent)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
  public
    property ProjectHistory: TStrings;
    property CurrentProjectFilename: string read FCurrentProjectFilename
                                            write FCurrentProjectFilename;
    property WindowSize: TPoint;
    property AutoOpenLastProject: boolean read FAutoOpenLastProject
                                          write FAutoOpenLastProject;
    property Modified: boolean read FModified write FModified;
  end;

implementation

end.

