{ This file was automatically created by Lazarus. do not edit!
  This source is only used to compile and install the package.
 }

unit lazsvnpkg; 

interface

uses
  SVNLogForm, SVNUpdateForm, SVNDiffForm, SVNClasses, SVNStatusForm, 
  SVNCommitForm, LazSVNIntf, SVNAddProjectForm, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('LazSVNIntf', @LazSVNIntf.Register); 
end; 

initialization
  RegisterPackage('lazsvnpkg', @Register); 
end.
