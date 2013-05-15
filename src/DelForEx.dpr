library DelForEx;

{$I DelForEx.inc}

uses
{$IFNDEF DELPHI9_UP}
  ShareMem,
{$ENDIF}
  DelForExpert in 'DelForExpert.pas',
  MyIDEStream in 'MyIDEStream.pas',
  Delfor1 in 'Delfor1.pas',
  DelforEng in 'DelforEng.pas',
  DelforTypes in 'DelforTypes.pas',
  DelForCommon in 'DelForCommon.pas',
{$IFDEF DELPHI9_UP}
  DelForOTAUtils in 'DelForOTAUtils.pas',
{$ENDIF}
  DelExpert in 'DelExpert.pas' {DelExpertDlg},
  OptDlg in 'OptDlg.pas' {OptionsDlg},
  EditFile in 'EditFile.pas' {FileEditDlg},
  progr in 'progr.pas' {ProgressDlg};

begin
end.

