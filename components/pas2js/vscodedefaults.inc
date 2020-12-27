
Procedure GetDefaultGlueFile(S : TStrings);

begin
  With S do
    begin
    Add('const vscode = require(''vscode'');');
    Add('const pascalRuntime = require(''./%PACKAGEPROJECTNAME%.js'');');
    Add('');
    Add('var callbacks = {');
    Add('    onDeactivate: function (a) { }');
    Add('}');
    Add('');
    Add('// this method is called when your extension is activated');
    Add('function activate(context) {');
    Add('    pascalRuntime.rtl.run();');
    Add('    var vscodeEnv = {');
    Add('        vscodeGlobal: vscode,');
    Add('        extensionContext: context');
    Add('    }');
    Add('  pascalRuntime.pas.program.InitVSCode(vscodeEnv,callbacks);');
    Add('}');
    Add('');
    Add('// this method is called when your extension is deactivated');
    Add('function deactivate() {');
    Add('  if (callbacks.onDeactivate) {');
    Add('    callbacks.onDeactivate();  ');
    Add('  }	');
    Add('}');
    Add('');
    Add('// eslint-disable-next-line no-undef');
    Add('module.exports = {');
    Add('  activate,');
    Add('  deactivate');
    Add('}');
    end;
end;

Procedure GetDefaultProjectFile(S : TStrings);

begin
  With S do
    begin
    Add('program %PACKAGEPROJECTNAME%;');
    Add('');
    Add('{$mode objfpc}');
    Add('');
    Add('uses');
    Add('  JS, Types, Classes, SysUtils, libvscode, vscodeapp;');
    Add('');
    Add('');
    Add('Type');
    Add('  { %CLASSNAME% }');
    Add('');
    Add('  %CLASSNAME% = Class(TVSCodeApplication)');
    Add('  Protected');
    Add('    procedure DoActivate; override;');
    Add('    procedure DoDeactivate; override;');
    Add('  Public');
    Add('    // %PACKAGEHANDLERINTFS%');
    Add('  end;');
    Add('');
    Add('');
    Add('// Do not change the name of this procedure, the Javascript glue code depends on it.');
    Add('// If you do want to change it, change the glue code as well.');
    Add('Procedure InitVSCode(aVSCode : TVSCodeEnvironment; aCallBacks : TVSCodeExtensionCallBacks);');
    Add('');
    Add('begin');
    Add('  If Application=Nil then');
    Add('    Application:=%CLASSNAME%.Create(Nil);');
    Add('  Application.SaveVSCodeEnvironment(aVSCode,aCallBacks);');
    Add('end;');
    Add('');
    Add('{ %CLASSNAME% }');
    Add('');
    Add('procedure %CLASSNAME%.DoActivate;');
    Add('');
    Add('Var');
    Add('  disp : TVSDisposable;');
    Add('');
    Add('begin');
    Add('  inherited DoActivate;');
    Add('  // %PACKAGEHANDLERREGS%');
    Add('end;');
    Add('');
    Add('procedure %CLASSNAME%.DoDeactivate();');
    Add('begin');
    Add('  inherited DoDeactivate();');
    Add('end;');
    Add('');
    Add('  // %PACKAGEHANDLERIMPLS%');
    Add('');

    Add('// This code is needed to prevent the pas2js compiler from removing the InitAtom call.');
    Add('var');
    Add('  dummy : JSValue;');
    Add('');
    Add('begin');
    Add('  Application:=%CLASSNAME%.Create(Nil);');
    Add('  dummy:=@InitVSCode;');
    Add('end.');
    end;
end;


Procedure GetDefaultLaunchFile(S : TStrings);

begin
  With S do
    begin
    Add('{');
    Add('    "version": "0.2.0",');
    Add('    "configurations": [');
    Add('        {');
    Add('            "name": "Run Extension",');
    Add('            "type": "extensionHost",');
    Add('            "request": "launch",');
    Add('            "runtimeExecutable": "${execPath}",');
    Add('            "args": [');
    Add('                "--extensionDevelopmentPath=${workspaceFolder}"');
    Add('            ]');
    Add('        }');
    Add('    ]');
    Add('}');
    end;
end;

Procedure GetDefaultTasksFile(S : TStrings);

begin
  With S do
    begin
    Add('    {');
    Add('        "version": "2.0.0",');
    Add('        "tasks": [');
    Add('            {');
    Add('                "label": "Compile",');
    Add('                "type": "shell",');
    Add('                "command": "pas2js %PACKAGEPROJECTNAME%.lpr -ojs/%PACKAGEPROJECTNAME%.js -Tbrowser -Jivscodeimports.js -Jirtl.js -Javscodeexports.js -Jc ",');
    Add('                "group":{');
    Add('                    "kind": "build",');
    Add('                    "isDefault": true');
    Add('                },');
    Add('                "presentation": {');
    Add('                    "reveal": "always",');
    Add('                    "panel": "new"');
    Add('                }');
    Add('            }');
    Add('        ]');
    Add('    }');
    end;
end;

