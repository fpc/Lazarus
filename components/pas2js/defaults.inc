procedure getDefaultMenuFile(S : TStrings);
begin
  With S do
    begin
    Add('{');
    Add('  "context-menu": {');
    Add('    "atom-text-editor": [');
    Add('      {');
    Add('        "label": "Toggle %PACKAGENAME%",');
    Add('        "command": "%PACKAGENAME%:toggle"');
    Add('      }');
    Add('    ]');
    Add('  },');
    Add('  "menu": [');
    Add('    {');
    Add('      "label": "Packages",');
    Add('      "submenu": [');
    Add('        {');
    Add('          "label": "%PACKAGENAME%",');
    Add('          "submenu": [');
    Add('            {');
    Add('              "label": "Toggle",');
    Add('              "command": "%PACKAGENAME%:toggle"');
    Add('            }');
    Add('          ]');
    Add('        }');
    Add('      ]');
    Add('    }');
    Add('  ]');
    Add('}');
    end;

end;

procedure getDefaultCSSFile(S : TStrings);

begin
  With S do
    begin
    Add('// The ui-variables file is provided by base themes provided by Atom.');
    Add('//');
    Add('// See https://github.com/atom/atom-dark-ui/blob/master/styles/ui-variables.less');
    Add('// for a full listing of what''s available.');
    Add('@import "ui-variables";');
    Add('');
    Add('.%PACKAGENAME% {');
    Add('}');
    end;
end;


Procedure GetDefaultGlueFile(S : TStrings);

begin
  With S do
    begin
    Add('''use babel'';');
    Add('');
    Add('import { CompositeDisposable } from ''atom'';');
    Add('import { pas, rtl } from ''./%PACKAGEPROJECTNAME%.js'';');
    Add('');
    Add('export default {');
    Add('  activate(state) {');
    Add('    rtl.run();');
    Add('    this.subscriptions = new CompositeDisposable();');
    Add('    this.atomEnv = {');
    Add('      atomGlobal : atom,');
    Add('      subscriptions : this.subscriptions,');
    Add('      initialState : state');
    Add('    }');
    Add('    this.atomHandler = {');
    Add('      onDeactivate : function (a) {},');
    Add('      onSerialize : function (a,o) {}');
    Add('    }');
    Add('    pas.program.InitAtom(this.atomEnv,this.atomHandler);');
    Add('  },');
    Add('');
    Add('  deactivate() {');
    Add('    if (this.atomHandler.onDeactivate) {');
    Add('      this.atomHandler.onDeactivate(this.atomEnv)');
    Add('    }');
    Add('    this.subscriptions.dispose();');
    Add('  },');
    Add('');
    Add('  serialize() {');
    Add('    var obj = {};');
    Add('    if (this.atomHandler.onSerialize) {');
    Add('       this.atomHandler.onSerialize(this.atomEnv,obj)');
    Add('    }');
    Add('    return obj;');
    Add('  }');
    Add('};');
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
    Add('  JS, Classes, SysUtils, libAtom, atomapp, Web;');
    Add('');
    Add('');
    Add('Type');
    Add('  { %CLASSNAME% }');
    Add('');
    Add('  %CLASSNAME% = Class(TAtomApplication)');
    Add('  Private');
    Add('  Protected');
    Add('    procedure DoActivate(aState : TJSObject); override;');
    Add('    procedure DoDeactivate; override;');
    Add('    procedure DoSerialize(aState : TJSObject); override;');
    Add('  Public');
    Add('    // %PACKAGEHANDLERINTFS%');
    Add('  end;');
    Add('');
    Add('');
    Add('// Do not change the name of this procedure, the Javascript glue code depends on it.');
    Add('// If you do want to change it, change the glue code as well.');
    Add('Procedure InitAtom(aAtom : TAtomEnvironment; aCallBacks : TAtomPackageCallBacks);');
    Add('');
    Add('begin');
    Add('  If Application=Nil then');
    Add('    Application:=%CLASSNAME%.Create(Nil);');
    Add('  Application.SaveAtomEnvironment(aAtom,aCallBacks);');
    Add('end;');
    Add('');
    Add('{ %CLASSNAME% }');
    Add('');
    Add('procedure %CLASSNAME%.DoActivate(aState: TJSObject);');
    Add('');
    Add('Var');
    Add('  cmds : TJSObject;');
    Add('begin');
    Add('  inherited DoActivate(aState);');
    Add('  // %PACKAGEHANDLERREGS%');
    Add('end;');
    Add('');
    Add('procedure %CLASSNAME%.DoDeactivate();');
    Add('begin');
    Add('// Deactivation code here');
    Add('end;');
    Add('');
    Add('procedure %CLASSNAME%.DoSerialize(aState: TJSObject);');
    Add('begin');
    Add('  inherited DoSerialize(aState);');
    Add('end;');
    Add('');
    Add('// %PACKAGEHANDLERIMPLS%');

    Add('// This code is needed to prevent the pas2js compiler from removing the InitAtom call.');
    Add('var');
    Add('  dummy : JSValue;');
    Add('');
    Add('begin');
    Add('  Application:=%CLASSNAME%.Create(Nil);');
    Add('  dummy:=@InitAtom;');
    Add('end.');
    end;
end;

Procedure getdefaultKeyMapFile(Src : TStrings);

begin
  With Src do
    begin
    Add('{');
    Add('  "atom-workspace": {');
    Add('    "ctrl-alt-o": "%PACKAGENAME%:toggle"');
    Add('  }');
    Add('}');
    end;
end;
