unit PkgSysBasePkgs;

{$mode ObjFPC}{$H+}

interface

type
  // the base packages needed by the minimal IDE
  TLazarusIDEBasePkg = (
    libpFCL,
    libpLazUtils,
    libpFreeTypeLaz,
    libpLCLBase,
    libpLCL,
    libpSynEdit,
    libpBuildIntf,
    libpLazDebuggerIntf,
    libpIDEIntf,
    libpDebuggerIntf,
    libpCmdLineDebuggerBase,
    libpfpdebug,
    libpLazDebuggerGdbmi,
    libpLazDebuggerFp,
    libpLazDebuggerLldb,
    libpLazDebuggerFpLldb,
    libpCodeTools,
    libpLazControls,
    libpLazControlDsgn,
    libpIdeConfig,
    libpLCLExtensions_package,
    libpLazVirtualtreeview_package,
    libpIdeDebugger
    );
const
  LazarusIDEBasePkgNames: array[TLazarusIDEBasePkg] of string = (
    'FCL',
    'LazUtils',
    'freetypelaz',
    'LCLBase',
    'LCL',
    'SynEdit',
    'BuildIntf',
    'LazDebuggerIntf',
    'IDEIntf',
    'DebuggerIntf',
    'CmdLineDebuggerBase',
    'fpdebug',
    'LazDebuggerGdbmi',
    'LazDebuggerFp',
    'LazDebuggerLldb',
    'LazDebuggerFpLldb',
    'CodeTools',
    'LazControls',
    'LazControlDsgn',
    'IdeConfig',
    'lclextensions_package',
    'laz.virtualtreeview_package',
    'IdeDebugger'
    );

implementation

end.

