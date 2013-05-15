library Delfordll;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  DelforEngine in 'DelforEngine.pas',
  DelForInterf in 'DelForInterf.pas',
  OObjects in 'OObjects.pas',
  DelforTypes in 'DelforTypes.pas';

{$R *.RES}

exports
  Formatter_Destroy,
  Formatter_Create,
  Formatter_LoadFromFile,
  Formatter_LoadFromList,
  Formatter_Parse,
  Formatter_Clear,
  Formatter_WriteToFile,
  Formatter_GetTextStr,
  Formatter_SetTextStr,
  Formatter_SetOnProgress,
  Formatter_LoadCapFile,
  Formatter_SaveCapFile,
  Formatter_Version;
end.

