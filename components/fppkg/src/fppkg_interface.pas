unit Fppkg_Interface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  fpmkunit,
  PackageIntf,
  FppkgIntf,
  Fppkg_EnvironmentOptions,
  fppkg_packagefileoptionsfrm,
  fppkg_packagevariant;

type

  { TFppkgInterfaceEx }

  TFppkgInterfaceEx = class(TFppkgInterface)
  private
    function GetComponentName(AName: string): String;
  protected
    function GetInstallFPMakeDependencies: Boolean; override;
    function GetUseFPMakeWhenPossible: Boolean; override;
  public
    function ConstructFpMakeImplementationSection(APackage: TIDEPackage): string; override;
    function ConstructFpMakeInterfaceSection(APackage: TIDEPackage): string; override;
    function ConstructFpMakeDependenciesFileSection(APackage: TIDEPackage): string; override;
  end;

implementation

{ TFppkgInterfaceEx }

function TFppkgInterfaceEx.GetComponentName(AName: string): String;
begin
  Result := StringsReplace(AName, ['/',' ', '\', ':', ';', '.', ',','(',')'], ['_','_', '_', '_', '_', '_', '_', '_', '_'], [rfReplaceAll]);
end;

function TFppkgInterfaceEx.GetInstallFPMakeDependencies: Boolean;
begin
  Result := TFppkgEnvironmentOptions(TFppkgEnvironmentOptions.GetInstance).InstallFPMakeDependencies;
end;

function TFppkgInterfaceEx.GetUseFPMakeWhenPossible: Boolean;
begin
  Result := TFppkgEnvironmentOptions(TFppkgEnvironmentOptions.GetInstance).UseFPMakeWhenPossible;
end;

  function TFppkgInterfaceEx.ConstructFpMakeInterfaceSection(APackage: TIDEPackage): string;
var
  VariantList: TFppkgPackageVariantList;
  Variant: TFppkgPackageVariant;
  i, j: Integer;
begin
  Result := '';
  VariantList := TFppkgPackageVariantList.Create(True);
  try
    APackage.CustomOptions.AppendBasePath('Fppkg/');
    APackage.CustomOptions.AppendBasePath('PackageVariants/');
    try
      VariantList.Load(APackage.CustomOptions);

      for i := 0 to VariantList.Count -1 do
        begin
        Variant := VariantList.Items[i];
        result := Result + '  ' + GetComponentName(Variant.Name) + 'Variant: TPackageVariants;' + LineEnding +'  ';
        if Variant.Items.Count > 0 then
          begin
          for j := 0 to Variant.Items.Count -1 do
            begin
            result := result + GetComponentName(Variant.Items[j].Name) + 'VariantItem, ';
            end;
          SetLength(Result, length(Result) -2);
          Result := Result + ': TPackageVariant;' + LineEnding;
          end;
        end;

      if Result <> '' then
        Result := 'var' + LineEnding + Result;
    finally
      APackage.CustomOptions.UndoAppendBasePath;
      APackage.CustomOptions.UndoAppendBasePath;
    end;
  finally
    VariantList.Free;
  end;
end;

function TFppkgInterfaceEx.ConstructFpMakeDependenciesFileSection(APackage: TIDEPackage): string;
var
  VariantList: TFppkgPackageVariantList;
  Variant: TFppkgPackageVariant;
  Found: Boolean;
  FilterStr: string;
  ResourceStr: string;

  function ProcessTargetInfo(AFile: TLazPackageFile; variable: string; out FilterStr: string): Boolean;
  var
    FppkgFileOptions: TFppkgPackageFileIDEOptions;
  begin
    FppkgFileOptions := TFppkgPackageFileIDEOptions(AFile.GetOptionsInstanceOf(TFppkgPackageFileIDEOptions));
    FppkgFileOptions.ParseOptions;

    FilterStr := '';

    Result := not (not FppkgFileOptions.AvailableOnAllTargetCPUs and (FppkgFileOptions.AvailableOnTargetCPUs=[])) and
              not (not FppkgFileOptions.AvailableOnAllTargetOSes and (FppkgFileOptions.AvailableOnTargetOSes=[]));
    if result then
    begin
      if (FppkgFileOptions.AvailableOnTargetCPUs <> []) then
      begin
        if FppkgFileOptions.AvailableOnAllTargetCPUs then
          FilterStr:='    '+variable+'.CPUs := AllCPUs - ['+CPUSToString(FppkgFileOptions.AvailableOnTargetCPUs)+'];'+LineEnding
        else
          FilterStr:='    '+variable+'.CPUs := ['+CPUSToString(FppkgFileOptions.AvailableOnTargetCPUs)+'];'+LineEnding
      end;
      if (FppkgFileOptions.AvailableOnTargetOSes <> []) then
      begin
        if FppkgFileOptions.AvailableOnAllTargetOSes then
          FilterStr:=FilterStr + '    '+variable+'.OSes := AllOSes - ['+OSesToString(FppkgFileOptions.AvailableOnTargetOSes)+'];'+LineEnding
        else
          FilterStr:=FilterStr + '    '+variable+'.OSes := ['+OSesToString(FppkgFileOptions.AvailableOnTargetOSes)+'];'+LineEnding
      end;
    end;
  end;

  function ProcessResourceFiles(AFile: TLazPackageFile): string;
  var
    PackageFile: TLazPackageFile;
    j: Integer;
    BaseName: String;
  begin
    Result := '';
    BaseName := ChangeFileExt(AFile.Filename, '');
    for j := 0 to APackage.FileCount -1 do
      begin
      PackageFile := APackage.Files[j];
      if (PackageFile.FileType = pftLFM) and SameText(BaseName, ChangeFileExt(PackageFile.Filename, '')) then
        Result := Result + '    T.ResourceFiles.Add('''+ ExtractFileName(PackageFile.Filename) +''');' + sLineBreak;
      end;
  end;

var
  i, j, k: Integer;
begin
  Result := '';
  VariantList := TFppkgPackageVariantList.Create(True);
  try
    APackage.CustomOptions.AppendBasePath('Fppkg/');
    APackage.CustomOptions.AppendBasePath('PackageVariants/');
    try
      VariantList.Load(APackage.CustomOptions);

      for i := 0 to APackage.FileCount -1 do
        if APackage.Files[i].FileType = pftUnit then
          begin
          Found := False;

          for j := 0 to VariantList.Count -1 do
            begin
            Variant := VariantList.Items[j];
            for k := 0 to Variant.Items.Count -1 do
              begin
              if Variant.Items[k].PackageFiles.IndexOf(APackage.Files[i].GetShortFilename(False)) > -1 then
                begin
                Found := True;
                //Result := Result + '    ' + GetComponentName(Variant.Items[k].Name) +'VariantItem.Targets.AddImplicitUnit('''+APackage.Files[i].GetShortFilename(False)+''');' + LineEnding;
                end;
              end;
            end;

          if not Found and ProcessTargetInfo(APackage.Files[i], 'D', FilterStr) then
            begin
            Result := Result + '    D := T.Dependencies.AddUnit('''+APackage.files[i].Unit_Name+''');' + LineEnding;
            Result := Result + FilterStr;
            end;
          end
        else if APackage.Files[i].FileType = pftInclude then
          if ProcessTargetInfo(APackage.Files[i], 'D', FilterStr) then
            begin
            Result := Result + '    D := T.Dependencies.AddInclude('''+APackage.files[i].GetShortFilename(False)+''');' + LineEnding;
            Result := Result + FilterStr;
            end;

      for i := 0 to APackage.FileCount-1 do
        if (APackage.Files[i].FileType=pftUnit) then
          begin
          Found := False;

          for j := 0 to VariantList.Count -1 do
            begin
            Variant := VariantList.Items[j];
            for k := 0 to Variant.Items.Count -1 do
              begin
              if Variant.Items[k].PackageFiles.IndexOf(APackage.Files[i].GetShortFilename(False)) > -1 then
                begin
                Found := True;
                if ProcessTargetInfo(APackage.Files[i], 'T', FilterStr) then
                  begin
                  Result := Result + '    T := ' + GetComponentName(Variant.Items[k].Name) +'VariantItem.Targets.AddImplicitUnit('''+APackage.Files[i].GetShortFilename(False)+''');' + LineEnding;
                  Result:=Result+FilterStr;
                  if ResourceStr <> '' then
                    Result := Result+ResourceStr;
                  end;
                end;
              end;
            end;

          if not found then
            begin
            if ProcessTargetInfo(APackage.Files[i], 'T', FilterStr) then
              begin
              Result:=Result+'    T := P.Targets.AddImplicitUnit('''+APackage.Files[i].GetShortFilename(False)+''');'+LineEnding;
              Result:=Result+FilterStr;
              ResourceStr := ProcessResourceFiles(APackage.Files[i]);
              if ResourceStr <> '' then
                Result := Result+ResourceStr;
              end;
            end;
          end;
    finally
      APackage.CustomOptions.UndoAppendBasePath;
      APackage.CustomOptions.UndoAppendBasePath;
    end;
  finally
    VariantList.Free;
  end;
end;

function TFppkgInterfaceEx.ConstructFpMakeImplementationSection(APackage: TIDEPackage): string;
var
  VariantList: TFppkgPackageVariantList;
  Variant: TFppkgPackageVariant;
  ItemName: string;
  i, j, k: Integer;
  CustomCode: TStrings;
begin
  Result := '';
  VariantList := TFppkgPackageVariantList.Create(True);
  try
    APackage.CustomOptions.AppendBasePath('Fppkg/');
    try
    APackage.CustomOptions.AppendBasePath('PackageVariants/');
      try
        VariantList.Load(APackage.CustomOptions);

        for i := 0 to VariantList.Count -1 do
          begin
          Variant := VariantList.Items[i];
          result := result + '    ' + GetComponentName(Variant.Name) + 'Variant := AddPackageVariant('''+Variant.Name+''',true);' + LineEnding;
          result := result + '    P.AddPackageVariant('+GetComponentName(Variant.Name)+'Variant);' + LineEnding;
          if Variant.Items.Count > 0 then
            begin
            for j := 0 to Variant.Items.Count -1 do
              begin
              ItemName := GetComponentName(Variant.Items[j].Name) + 'VariantItem';
              result := result + '    ' + ItemName + ' := ' + GetComponentName(Variant.Name) + 'Variant.add('''+Variant.Items[j].Name+''');' + LineEnding;
              for k := 0 to Variant.Items[j].CompilerOptions.Count -1 do
                begin
                Result := Result + '    ' + ItemName + '.Options.Add(''' + Variant.Items[j].CompilerOptions[k] + ''');' + LineEnding;
                end;
              end;
            end;
          end;
      finally
        APackage.CustomOptions.UndoAppendBasePath;
      end;
      CustomCode := TStringList.Create;
      try
        APackage.CustomOptions.GetValue('CustomCode', CustomCode);
        result := result + CustomCode.Text;
      finally
        CustomCode.Free;
      end;
    finally
      APackage.CustomOptions.UndoAppendBasePath;
    end;
  finally
    VariantList.Free;
  end;
end;

initialization
  FppkgInterface := TFppkgInterfaceEx.Create;
finalization
  FreeAndNil(FppkgInterface);
end.

