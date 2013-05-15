unit DelforEng;

{$I DelForEx.inc}

{.$DEFINE NODLL}

interface

{dynamical loading of the dll for greater flexibility}
uses Classes, DelForTypes;

const
  DelForDll = 'DELFORDLL.DLL';

type
  TProc = procedure;
  TCharProc = procedure(P: PAnsiChar);
  TCharSettingProc = procedure(P: PAnsiChar; S: PSettings; I: Integer);
  TListProc = procedure(L: TStringList; S: PSettings; I: Integer);
  TParseFunc = function(S: PSettings; I: Integer): Boolean;
  TCharFunc = function: PAnsiChar;
  TIntegerFunc = function: Integer;
  TProgrProc = procedure(AOnProgress: TProgressEvent);

function LoadDll(APath: string): Boolean;
procedure FreeDll;

var
  IsLoadDLL: Boolean = false;
  Formatter_Create: TProc;
  Formatter_Destroy: TProc;
  Formatter_LoadFromFile: TCharSettingProc;
  Formatter_LoadFromList: TListProc;
  Formatter_Parse: TParseFunc;
  Formatter_clear: TProc;
  Formatter_writeToFile: TCharProc;
  Formatter_GetTextStr: TCharFunc;
  Formatter_SetTextStr: TCharProc;
  Formatter_SetOnProgress: TProgrProc;
  Formatter_LoadCapFile: TCharProc;
  Formatter_SaveCapFile: TCharProc;
  Formatter_Version: TIntegerFunc;

implementation

uses Windows, SysUtils
{$IFDEF NODLL}, DelForInterf{$ENDIF}
{$IFDEF DEBUG}, CnDebug{$ENDIF};
var
  Handle: Integer = 0;

function LoadDll(APath: string): Boolean;
{$IFNDEF NODLL}
var
  S: string;
  A: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
  Result := false;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('Dll Loading.');
{$ENDIF}
{$IFDEF NODLL}
  DelForInterf.Formatter_Create;
  @Formatter_Destroy := @DelForInterf.Formatter_Destroy;
  @Formatter_LoadFromFile := @DelForInterf.Formatter_LoadFromFile;
  @Formatter_LoadFromList := @DelForInterf.Formatter_LoadFromList;
  @Formatter_Parse := @DelForInterf.Formatter_Parse;
  @Formatter_clear := @DelForInterf.Formatter_clear;
  @Formatter_writeToFile := @DelForInterf.Formatter_writeToFile;
  @Formatter_GetTextStr := @DelForInterf.Formatter_GetTextStr;
  @Formatter_SetTextStr := @DelForInterf.Formatter_SetTextStr;
  @Formatter_SetOnProgress := @DelForInterf.Formatter_SetOnProgress;
  @Formatter_LoadCapFile := @DelForInterf.Formatter_LoadCapFile;
  @Formatter_SaveCapFile := @DelForInterf.Formatter_SaveCapFile;
  @Formatter_Version := @DelForInterf.Formatter_Version;
{$ELSE}
  if Handle = 0 then
  begin
    S := APath + DelForDll;
    if not FileExists(S) then
    begin
{$IFDEF Delphi12_UP}
      GetSystemDirectory(PChar(@A), Length(A));
{$ELSE}
      GetSystemDirectory(PChar(@A), Sizeof(A));
{$ENDIF}
      S := string(A) + '\' + DelForDll;
    end;
    Handle := LoadLibrary(PChar(S));
    if Handle <> 0 then
    begin
      @Formatter_Create := GetProcAddress(Handle, 'Formatter_Create');
      Formatter_Create;
      @Formatter_Destroy := GetProcAddress(Handle, 'Formatter_Destroy');
      @Formatter_LoadFromFile := GetProcAddress(Handle, 'Formatter_LoadFromFile');
      @Formatter_LoadFromList := GetProcAddress(Handle, 'Formatter_LoadFromList');
      @Formatter_Parse := GetProcAddress(Handle, 'Formatter_Parse');
      @Formatter_clear := GetProcAddress(Handle, 'Formatter_Clear');
      @Formatter_writeToFile := GetProcAddress(Handle, 'Formatter_WriteToFile');
      @Formatter_GetTextStr := GetProcAddress(Handle, 'Formatter_GetTextStr');
      @Formatter_SetTextStr := GetProcAddress(Handle, 'Formatter_SetTextStr');
      @Formatter_SetOnProgress := GetProcAddress(Handle, 'Formatter_SetOnProgress');
      @Formatter_LoadCapFile := GetProcAddress(Handle, 'Formatter_LoadCapFile');
      @Formatter_SaveCapFile := GetProcAddress(Handle, 'Formatter_SaveCapFile');
      @Formatter_Version := GetProcAddress(Handle, 'Formatter_Version');
      Result := True;
    end;
  end;
{$ENDIF}
  IsLoadDLL := Result;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('Dll Loaded.');
{$ENDIF}
end;

procedure FreeDll;
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('Dll Freeing.');
{$ENDIF}
  if Handle <> 0 then FreeLibrary(Handle);
  @Formatter_Create := nil;
  @Formatter_Destroy := nil;
  @Formatter_LoadFromFile := nil;
  @Formatter_LoadFromList := nil;
  @Formatter_Parse := nil;
  @Formatter_clear := nil;
  @Formatter_writeToFile := nil;
  @Formatter_GetTextStr := nil;
  @Formatter_SetTextStr := nil;
  @Formatter_SetOnProgress := nil;
  @Formatter_LoadCapFile := nil;
  @Formatter_SaveCapFile := nil;
  @Formatter_Version := nil;
  Handle := 0;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('Dll Freed.');
{$ENDIF}
end;

initialization
  FreeDll;
finalization
  FreeDll;
end.

