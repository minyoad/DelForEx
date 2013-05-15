unit DelForCommon;

{$I DelForEx.inc}

interface

uses
  Windows, SysUtils;

{$IFNDEF Delphi6_UP}
function GetModuleName(Module: HMODULE): string;
{$ENDIF}

//文件是否只读
function IsReadonlyFile(const FileName: string): Boolean;

//在指定位置插入字符
function StrInsert(Str1, Str2: PAnsiChar; I: Integer): PAnsiChar;

//备份文件
function MakeBakFile(Dest, FileName: PAnsiChar): Boolean;

{$IFDEF DELPHI9_UP}
// 文件时间转本地时间
function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;

// 文件时间转本地日期时间
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;

// 取文件长度
function GetFileSize(const FileName: string): Int64;

// 取文件Delphi格式日期时间
function GetFileDateTime(const FileName: string): TDateTime;
{$ENDIF}
implementation

{$IFNDEF Delphi6_UP}

function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of AnsiChar;
begin
  SetString(Result, ModName, GetModuleFileNameA(Module, ModName, SizeOf(ModName)));
end;
{$ENDIF}

//文件是否只读

function IsReadonlyFile(const FileName: string): Boolean;
var
  Attributes: Integer;
begin
  Attributes := FileGetAttr(FileName);
  Result := ((Attributes and faReadOnly) > 0);
end;

//在指定位置插入字符

function StrInsert(Str1, Str2: PAnsiChar; I: Integer): PAnsiChar;
var
  LenStr2: Integer;
begin
  if I < 0 then I := 0;
  LenStr2 := StrLen(Str2);
  StrMove(Str1 + I + LenStr2, Str1 + I, Integer(StrLen(Str1)) - I + 1);
  StrMove(Str1 + I, Str2, LenStr2);
  StrInsert := Str1;
end;

//备份文件

function MakeBakFile(Dest, FileName: PAnsiChar): Boolean;
var
  //F: File;
  P: PAnsiChar;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    StrCopy(Dest, FileName);
    P := StrRScan(Dest, '.');
    if P = nil then
      StrCat(Dest, '.~')
    else
    begin
      (StrEnd(P) - 1)^ := #0;
      StrInsert(P + 1, '~', 0);
    end;
    DeleteFile(string(Dest));
    Result := CopyFileA(FileName, Dest, False);
    (*AssignFile(F, StrPas(FileName));
    {try}
    Rename(F, PChar(String(AnsiString(Dest))));
    {except
      on EInOutError do ;
    end;} *)
  end;
end;

{$IFDEF DELPHI9_UP}
// 文件时间转本地时间

function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;
var
  STime: TSystemTime;
begin
  FileTimeToLocalFileTime(FTime, FTime);
  FileTimeToSystemTime(FTime, STime);
  Result := STime;
end;

// 文件时间转本地日期时间

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  SystemTime: TSystemTime;
begin
  SystemTime := FileTimeToLocalSystemTime(FileTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute,
      wSecond, wMilliseconds);
end;

// 取文件信息

function GetFileInfo(const FileName: string; var FileSize: Int64;
  var FileTime: TDateTime): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := False;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Int64Rec(FileSize).Lo := FindData.nFileSizeLow;
      Int64Rec(FileSize).Hi := FindData.nFileSizeHigh;
      FileTime := FileTimeToDateTime(FindData.ftLastWriteTime);
      Result := True;
    end;
  end;
end;

// 取文件长度

function GetFileSize(const FileName: string): Int64;
var
  FileTime: TDateTime;
begin
  Result := -1;
  GetFileInfo(FileName, Result, FileTime);
end;

// 取文件Delphi格式日期时间

function GetFileDateTime(const FileName: string): TDateTime;
var
  Size: Int64;
begin
  Result := 0;
  GetFileInfo(FileName, Size, Result);
end;
{$ENDIF}
end.

