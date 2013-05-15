unit MyIDEStream;

{$I DelForEx.inc}

interface

uses
  SysUtils, Classes,
{$IFDEF Delphi9_UP}
  ToolsAPI
{$ELSE}
  ToolIntf, EditIntf, ExptIntf
{$ENDIF};

type
  TIDEStream = class(TMemoryStream)
  private
    FLines: TStringList;
    FFileName: string;
{$IFNDEF Delphi9_UP}
    function GetAsPChar: PAnsiChar;
    procedure LoadLines;
    function GetStreamTextLen: Longint;
    procedure GetEditReader;
    procedure GetEditWriter;
    procedure FreeEditReader;
    procedure FreeEditWriter;
{$ENDIF}
    function GetLines: TStringList;
    procedure SetLines(NewValue: TStringList);
  protected
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
    procedure WriteText(Text: PAnsiChar);
    property Capacity;
{$IFNDEF Delphi9_UP}
    property AsPChar: PAnsiChar read GetAsPChar;
    function GetText: PAnsiChar;
    procedure SetMemPointer(Ptr: Pointer; Size: Longint);
    property StreamTextLen: Longint read GetStreamTextLen;
{$ENDIF}
    property Lines: TStringList read GetLines write SetLines;
    property FileName: string read FFileName;
  published
    procedure Clear;
  end;

{$IFNDEF Delphi9_UP}
var
  FToolServices: TIToolServices;
  FModuleInterface: TIModuleInterface;
  FEditorInterface: TIEditorInterface;
  ActualReader: TIEditReader;
  ActualWriter: TIEditWriter;
function GetProjectName: string;
function GetCurrentFile: string;
function IsReadonlyBuffer(FileName: string): Boolean;
function GetToolServieces: TIToolServices;
procedure SaveFile(AFileName: string);
{$ENDIF}
implementation

uses Forms, ComCtrls, Progr
{$IFDEF Delphi9_UP}
  , DelForOTAUtils
{$ENDIF};

{$IFNDEF Delphi9_UP}

procedure SaveFile(AFileName: string);
begin
  FToolServices.SaveFile(AFileName);
end;

function GetProjectName: string;
begin
  Result := FToolServices.GetProjectName;
end;

function GetCurrentFile: string;
begin
  Result := FToolServices.GetCurrentFile;
end;

function GetToolServieces: TIToolServices;
begin
  FToolServices := ExptIntf.ToolServices;
  Result := FToolServices;
end;

function IsReadonlyBuffer(FileName: string): Boolean;
{ This function can only check currently opened edit buffer
  build with packages  }
{$IFNDEF VER90} {Not in D3} {probably it works in Component style expert}
var
  I, J: Integer;
begin
  Result := False; {default value}
  if Application <> nil then
    with Application do
      for I := 0 to ComponentCount - 1 do
        if Components[I] is TForm then
          if CompareText(ExtractFileName(TForm(Components[I]).Caption),
            ExtractFileName(FileName)) = 0 then
            with Components[I] do
              for J := 0 to ComponentCount - 1 do
                if Components[J] is TStatusBar then
                  Result := TStatusBar(Components[J]).Panels[2].Text =
                    'Read only';
end;
{$ELSE}
begin
  Result := False;
end;
{$ENDIF}

procedure TIDEStream.GetEditReader;
begin
  GetToolServieces;
  FModuleInterface := FToolServices.GetModuleInterface(FFileName);
  FEditorInterface := FModuleInterface.GetEditorInterface;
  ActualReader := FEditorInterface.CreateReader;
end;

procedure TIDEStream.GetEditWriter;
begin
  GetToolServieces;
  FModuleInterface := FToolServices.GetModuleInterface(FFileName);
  FEditorInterface := FModuleInterface.GetEditorInterface;
  ActualWriter := FEditorInterface.CreateWriter;
end;

procedure TIDEStream.FreeEditReader;
begin
  ActualReader.Free;
  FEditorInterface.Free;
  FModuleInterface.Free;
end;

procedure TIDEStream.FreeEditWriter;
begin
  ActualWriter.Free;
  FEditorInterface.Free;
  FModuleInterface.Free;
end;
{$ENDIF}

constructor TIDEStream.Create(AFileName: string);
begin
  inherited Create;
  if AFileName = '' then
  begin
{$IFDEF Delphi9_UP}
    FFileName := OtaGetCurrentSourceFile;
{$ELSE}
    GetToolServieces;
    FFileName := GetCurrentFile;
{$ENDIF}
  end
  else
    FFileName := AFileName;
    if (LowerCase(ExtractFileExt(FFileName)) = '.dpr') or
      (LowerCase(ExtractFileExt(FFileName)) = '.pas') then
    else
    begin
      if Assigned(ProgressDlg) then ProgressDlg.Hide;
      raise Exception.Create('对不起, 只对 PAS 或 DPR 文件有效。');
    end;
    FLines := TStringList.Create;
end;

procedure TIDEStream.Clear;
begin
  FLines.Clear;
  inherited Clear;
end;

destructor TIDEStream.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

{$IFNDEF Delphi9_UP}

function TIDEStream.GetAsPChar: PAnsiChar;
const
  TheEnd: AnsiChar = #0;
begin
  Position := Size;
  Write(TheEnd, 1);
  SetPointer(Memory, Size - 1);
  Result := Memory;
end;

procedure TIDEStream.SetMemPointer(Ptr: Pointer; Size: Longint);
begin
  SetPointer(Ptr, Size);
end;

function TIDEStream.GetStreamTextLen: Longint;
const
  {BuffLen = 16383;}
  BuffLen = $FF;
var
  TextBuffer: PAnsiChar;
  Readed, BuffPos, TextLen: Longint;
begin
  TextLen := 0;
  GetMem(TextBuffer, BuffLen + 1);
  BuffPos := 0;
  GetEditReader;
  try
    repeat
      Readed := ActualReader.GetText(BuffPos, TextBuffer, BuffLen);
      TextLen := TextLen + Readed;
      inc(BuffPos, Readed);
    until Readed < BuffLen;
  finally
    FreeEditReader;
    FreeMem(TextBuffer, BuffLen + 1);
  end;
  Result := TextLen;
end;

function TIDEStream.GetText: PAnsiChar;
const
  BuffLen = $FF;
var
  TextBuffer: PAnsiChar;
  Readed, BuffPos: Longint;
begin
  Clear;
  GetMem(TextBuffer, BuffLen + 1);
  BuffPos := 0;
  GetEditReader;
  try
    repeat
      Readed := ActualReader.GetText(BuffPos, TextBuffer, BuffLen);
      Write(TextBuffer^, Readed);
      inc(BuffPos, Readed);
    until Readed < BuffLen;
  finally
    FreeEditReader;
    FreeMem(TextBuffer, BuffLen + 1);
  end;
  Result := AsPChar;
end;

procedure TIDEStream.WriteText(Text: PAnsiChar);
var
  TextLen: Longint;
begin
  if Size = 0 then
    TextLen := GetStreamTextLen
  else
    TextLen := Size;
  GetEditWriter;
  if (ActualWriter <> nil) and (Text <> nil) then
  try
    if TextLen > 0 then
    begin
      ActualWriter.CopyTo(0);
      ActualWriter.DeleteTo(TextLen - 2);
      {a bit tricky, in Delphi 3.00 2 char remain in
       editor, while in Delphi 2.01 the editor is clean,
       The following might cause problems in other versions}
{$IFDEF VER100}
      ActualWriter.DeleteTo(2);
{$ENDIF}
    end;
    ActualWriter.Insert(Text);
  finally
    FreeEditWriter;
  end;
end;

procedure TIDEStream.LoadLines;
var
  Run, LineStart, LineEnd: PAnsiChar;
begin
  LineStart := GetText;
  Run := LineStart;
  while Run^ <> #0 do
  begin
    Run := LineStart;
    while not (Run^ in [#0, #10, #13]) do
      inc(Run);
    LineEnd := Run;
    if Run^ <> #0 then
    begin
      inc(Run);
      if Run^ in [#10, #13] then inc(Run);
    end;
    LineEnd^ := #0;
    FLines.Add(LineStart);
    LineStart := Run;
  end;
end;
{$ELSE}

procedure TIDEStream.WriteText(Text: PAnsiChar);
begin
  OtaSettringAsActiveEditorText(StrPas(Text));
end;
{$ENDIF}

function TIDEStream.GetLines: TStringList;
begin
  if FLines.Count = 0 then
{$IFDEF Delphi9_UP}
    OtaGetActiveEditorTextAsString(FLines, False);
{$ELSE}
    LoadLines;
{$ENDIF}
  Result := FLines;
end;

procedure TIDEStream.SetLines(NewValue: TStringList);
begin
  FLines.Assign(NewValue);
end;

end.
