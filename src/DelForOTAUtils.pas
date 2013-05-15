unit DelForOTAUtils;

{$I DelForEx.inc}

interface

uses ToolsAPI, Classes, SysUtils, Windows;

type
  TUnitInfo = class(TObject)
  private
    FFormName: string;
    FUnitName: string;
    FFileName: string;
  public
    property UnitName: string read FUnitName write FUnitName;
    property FormName: string read FFormName write FFormName;
    property FileName: string read FFileName write FFileName;
  end;

function OtaGetCurrentModule: IOTAModule;

function OtaGetCurrentProjectName: string;

function OtaGetCurrentProject: IOTAProject;

function OtaGetProjectGroup: IOTAProjectGroup;

function OtaGetActiveEditorTextAsString(Lines: TStringList; UseSelection: Boolean): Boolean;

function OtaGetActiveEditorText(Stream: TMemoryStream; UseSelection, FromCurrPos: Boolean; CheckUtf8: Boolean = True): Boolean;

function OtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;

// 取模块编辑器
function OtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;

function OtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView;

function OtaGetCurrentSourceEditor: IOTASourceEditor;

// 保存EditReader内容到流中
procedure OtaSaveReaderToStream(EditReader: IOTAEditReader; Stream:
  TMemoryStream; StartPos: Integer = 0; EndPos: Integer = 0;
  PreSize: Integer = 0; CheckUtf8: Boolean = True);

// 在指定位置处插入文本，如果 SourceEditor 为空使用当前值。
procedure OtaInsertTextIntoEditorAtPos(const Text: String; Position: Longint;
  SourceEditor: IOTASourceEditor);

function OtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;

procedure OtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);

function OtaGetTopMostEditBuffer: IOTAEditBuffer;

procedure OtaFillUnitInfoListForCurrentProject(const List: TList);

procedure ClearUnitInfoList(const List: TList);

function OtaOpenFile(const FileName: string): Boolean;

function OtaIsFileOpen(const AFileName: string): Boolean;

function OtaGetCurrentSourceFile: string;

procedure OtaDeleteTextFromPos(StartPos, Count: Longint; SourceEditor: IOTASourceEditor = nil);

 // 返回 SourceEditor 当前光标位置的线性地址
function OtaGetCurrPos(SourceEditor: IOTASourceEditor): Integer;

// 插入文本到当前 IOTASourceEditor，允许多行文本。
procedure OtaSettringAsActiveEditorText(Text: String);

// 在当前光标下插入文本到当前 IOTASourceEditor，允许多行文本。
procedure OtaInsertTextIntoEditor(const Text: String);

//function GetInstallPath: string;

//function DeleteBlankLines(ACodes: TStringList): string;

implementation

uses
 Math, DelForCommon
 {$IFDEF DEBUG}, CnDebug{$ENDIF};
{function DeleteBlankLines(ACodes: TStringList): string;
var
  i: Integer;
begin
  i := ACodes.Count - 1;
  while i > 0 do
  begin
    if (ACodes[i] = EmptyStr) and (ACodes[i-1] = EmptyStr) then
      ACodes.Delete(i)
    else
      Inc(i, -1);
  end;
  Result := ACodes.Text;
end;}

{function GetInstallPath: string;
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CURRENT_USER;
    OpenKey('Software\CnPack\CodeFormatter', True);
    if ValueExists('InstallPath') then
      Result := ReadString('InstallPath')
    else
      Result := EmptyStr;
    CloseKey;
    Free;
  end;
end;}

// 转换字符串为编辑器使用的字符串
function ConvertTextToEditorText(const Text: string): AnsiString;
begin
{$IFDEF DELPHI9_UP}
  Result := AnsiToUtf8(Text);
{$ELSE}
  Result := Text;
{$ENDIF}
end;

// 转换编辑器使用的字符串为普通字符串
function ConvertEditorTextToText(const Text: AnsiString): string;
begin
{$IFDEF DELPHI9_UP}
  Result := Utf8ToAnsi(Text);
{$ELSE}
  Result := Text;
{$ENDIF}
end;

procedure ClearUnitInfoList(const List: TList);
var
  i: Integer;
begin
  if Assigned(List) then
    begin
      for i := 0 to List.Count - 1 do
        TObject(List[i]).Free;
      List.Clear;
    end;
end;

function OtaGetCurrentSourceFile: string;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
begin
  Result := '';
  Module := OtaGetCurrentModule;
  if Module <> nil then
    begin
      Editor := Module.GetCurrentEditor;
      if Editor <> nil then
        Result := Editor.FileName
      else // C++Builder 6 returns nil for some old-style modules without DFMs
        Result := Module.FileName;
    end;
end;

function OtaIsFileOpen(const AFileName: string): Boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  i: Integer;
  FileName: string;
begin
  Result := False;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  FileName := AFileName;

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
    begin
      for i := 0 to Module.GetModuleFileCount - 1 do
        begin
          FileEditor := OtaGetFileEditorForModule(Module, i);
          Assert(Assigned(FileEditor));

          Result := SameFileName(FileName, FileEditor.FileName);
          if Result then
            Exit;
        end;
    end;
end;

function OtaOpenFile(const FileName: string): Boolean;
var
  ActionServices: IOTAActionServices;
begin
  ActionServices := BorlandIDEServices as IOTAActionServices;
  Assert(Assigned(ActionServices));

  Result := ActionServices.OpenFile(FileName);
end;

procedure OtaFillUnitInfoListForCurrentProject(const List: TList);
var
  i: Integer;
  CurrentProject: IOTAProject;
  ModuleInfo: IOTAModuleInfo;
  UnitInfo: TUnitInfo;
begin
  Assert(Assigned(List));
  ClearUnitInfoList(List);

  CurrentProject := OtaGetCurrentProject;
  if not Assigned(CurrentProject) then
    Exit;

  for i := 0 to CurrentProject.GetModuleCount - 1 do
    begin
      ModuleInfo := CurrentProject.GetModule(i);
      Assert(Assigned(ModuleInfo));

      if ModuleInfo.FileName <> '' then
        begin
          UnitInfo := TUnitInfo.Create;
          List.Add(UnitInfo);

          UnitInfo.FileName := ModuleInfo.FileName;
          UnitInfo.FormName := ModuleInfo.FormName;
          UnitInfo.UnitName := ModuleInfo.Name;
        end;
    end;
end;

procedure OtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);
begin
  Assert(Assigned(SourceEditor));
  if Supports(SourceEditor, IOTAEditBuffer) then
    if (SourceEditor as IOTAEditBuffer).IsReadOnly then
      raise Exception.CreateFmt('%s is read only', [ExtractFileName(SourceEditor.FileName)]);
end;

function OtaGetEditWriterForSourceEditor(SourceEditor: IOTASourceEditor = nil): IOTAEditWriter;
resourcestring
  SEditWriterNotAvail = 'Edit writer not available';
begin
  if not Assigned(SourceEditor) then
    SourceEditor := OtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) then
    begin
      OtaAssertSourceEditorNotReadOnly(SourceEditor);
      Result := SourceEditor.CreateUndoableWriter;
    end;
  Assert(Assigned(Result), SEditWriterNotAvail);
end;
// 插入文本到当前 IOTASourceEditor，允许多行文本。
procedure OtaSettringAsActiveEditorText(Text: String);
var
  EditView: IOTAEditView;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
begin
  EditView := OtaGetTopMostEditView(OtaGetCurrentSourceEditor);
  Assert(Assigned(EditView), 'No edit view found');
  EditPos := EditView.CursorPos;
  EditView.ConvertPos(True, EditPos, CharPos);
  OtaDeleteTextFromPos(0, MaxInt, OtaGetCurrentSourceEditor);
  OtaInsertTextIntoEditorAtPos(Text, 0, OtaGetCurrentSourceEditor);
  EditView.CursorPos := EditPos;
  EditView.MoveViewToCursor;
  EditView.Paint;
end;

// 在当前光标下插入文本到当前 IOTASourceEditor，允许多行文本。
procedure OtaInsertTextIntoEditor(const Text: String);
var
  EditView: IOTAEditView;
  Position: Longint;
  CharPos: TOTACharPos;
  EditPos: TOTAEditPos;
begin
  EditView := OtaGetTopMostEditView(OtaGetCurrentSourceEditor);
  Assert(Assigned(EditView), 'No edit view found');
  EditPos := EditView.CursorPos;
  EditView.ConvertPos(True, EditPos, CharPos);
  Position := EditView.CharPosToPos(CharPos);
  OtaInsertTextIntoEditorAtPos(Text, Position, OtaGetCurrentSourceEditor);
  EditView.MoveViewToCursor;
  EditView.Paint;
end;

 // 返回 SourceEditor 当前光标位置的线性地址
function OtaGetCurrPos(SourceEditor: IOTASourceEditor): Integer;
var
  CharPos: TOTACharPos;
  IEditView: IOTAEditView;
  EditPos: TOTAEditPos;
begin
  if not Assigned(SourceEditor) then
    SourceEditor := OtaGetCurrentSourceEditor;
  if SourceEditor.EditViewCount > 0 then
  begin
    IEditView := OtaGetTopMostEditView(SourceEditor);
    Assert(IEditView <> nil);
    EditPos := IEditView.CursorPos;
    IEditView.ConvertPos(True, EditPos, CharPos);
    Result := IEditView.CharPosToPos(CharPos);
    if Result < 0 then Result := 0;
  end
  else
    Result := 0;
end;

procedure OtaDeleteTextFromPos(StartPos, Count: Longint; SourceEditor: IOTASourceEditor = nil);
var
  EditWriter: IOTAEditWriter;
begin
  EditWriter := OtaGetEditWriterForSourceEditor(SourceEditor);
  EditWriter.CopyTo(StartPos);
  EditWriter.DeleteTo(StartPos + Count);
end;

// 在指定位置处插入文本，如果 SourceEditor 为空使用当前值。
procedure OtaInsertTextIntoEditorAtPos(const Text: String; Position: Longint;
  SourceEditor: IOTASourceEditor);
var
  EditWriter: IOTAEditWriter;
begin
  if Text = '' then
    Exit;
  EditWriter := OtaGetEditWriterForSourceEditor(SourceEditor);
  try
  EditWriter.CopyTo(Position);
{$IFDEF DELPHI12_UP}
    EditWriter.Insert(PAnsiChar(ConvertTextToEditorText(Text)));
{$ELSE}
    EditWriter.Insert(PChar(ConvertTextToEditorText(Text)));
{$ENDIF}
  finally
    EditWriter := nil;
  end;
end;

function OtaGetTopMostEditView(SourceEditor: IOTASourceEditor): IOTAEditView;
begin
  if SourceEditor = nil then
    SourceEditor := OtaGetCurrentSourceEditor;
  if Assigned(SourceEditor) and (SourceEditor.EditViewCount > 0) then
    Result := SourceEditor.EditViews[0]
  else
    Result := nil;
end;

function OtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
begin
  Assert(Assigned(Module));
  Result := Module.GetModuleFileEditor(Index);
end;

function OtaGetSourceEditorFromModule(Module: IOTAModule; const FileName: string = ''): IOTASourceEditor;
var
  i: Integer;
  IEditor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
begin
  Result := nil;
  if not Assigned(Module) then
    Exit;

  for i := 0 to Module.GetModuleFileCount - 1 do
    begin
      IEditor := OtaGetFileEditorForModule(Module, i);

      if Supports(IEditor, IOTASourceEditor, ISourceEditor) then
        if Assigned(ISourceEditor) then
          if (FileName = '') or SameFileName(ISourceEditor.FileName, FileName) then
          begin
            Result := ISourceEditor;
            Break;
          end;
    end;
end;

function OtaGetTopMostEditBuffer: IOTAEditBuffer;
var
  EditorServices: IOTAEditorServices;
begin
  EditorServices := (BorlandIDEServices as IOTAEditorServices);
  Assert(Assigned(EditorServices));
  Result := EditorServices.TopBuffer;
end;

function OtaGetCurrentSourceEditor: IOTASourceEditor;
var
  EditBuffer: IOTAEditBuffer;
begin
  Result := nil;
  EditBuffer := OtaGetTopMostEditBuffer;
  if Assigned(EditBuffer) and (EditBuffer.FileName <> '') then
    Result := OtaGetSourceEditorFromModule(OtaGetCurrentModule, EditBuffer.FileName);
  if Result = nil then
    Result := OtaGetSourceEditorFromModule(OtaGetCurrentModule);
end;

// 保存EditReader内容到流中
procedure OtaSaveReaderToStream(EditReader: IOTAEditReader; Stream:
  TMemoryStream; StartPos: Integer = 0; EndPos: Integer = 0;
  PreSize: Integer = 0; CheckUtf8: Boolean = True);
const
  // Leave typed constant as is - needed for streaming code.
  TerminatingNulChar: Char = #0;
  BufferSize = 1024 * 24;
var
  Buffer: PAnsiChar;
  EditReaderPos: Integer;
  DataLen: Integer;
  ReadDataSize: Integer;
{$IFDEF DELPHI9_UP}
  Text: AnsiString;
{$ENDIF}
begin
  Assert(EditReader <> nil);
  Assert(Stream <> nil);

{$IFDEF DEBUG}
  CnDebugger.LogFmt('OtaSaveReaderToStream. StartPos %d, EndPos %d, PreSize %d.',
    [StartPos, EndPos, PreSize]);
{$ENDIF}

  if EndPos > 0 then
  begin
    DataLen := EndPos - StartPos;
    Stream.Size := DataLen + 1;
  end
  else
  begin
    // 分配预计的内存以提高性能
    DataLen := MaxInt;
    Stream.Size := PreSize;
  end;
  Stream.Position := 0;
  GetMem(Buffer, BufferSize);
  try
    EditReaderPos := StartPos;
    ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, Min(BufferSize, DataLen));
    Inc(EditReaderPos, ReadDataSize);
    Dec(DataLen, ReadDataSize);
    while (ReadDataSize = BufferSize) and (DataLen > 0) do
    begin
      Stream.Write(Buffer^, ReadDataSize);
      ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, Min(BufferSize, DataLen));
      Inc(EditReaderPos, ReadDataSize);
      Dec(DataLen, ReadDataSize);
    end;
    Stream.Write(Buffer^, ReadDataSize);
    Stream.Write(TerminatingNulChar, SizeOf(TerminatingNulChar));
    if Stream.Size > Stream.Position then
      Stream.Size := Stream.Position;
  finally
    FreeMem(Buffer);
  end;

{$IFDEF DELPHI9_UP}
  if CheckUtf8 then
  begin
{$IFDEF DELPHI12_UP}
    Text := UTF8ToUnicodeString(PAnsiChar(Stream.Memory));
{$ELSE}
    Text := Utf8ToAnsi(PAnsiChar(Stream.Memory));
{$ENDIF}
    Stream.Size := Length(Text) + 1;
    Stream.Position := 0;
    Stream.Write(PAnsiChar(Text)^, Length(Text) + 1);
  end;
{$ENDIF}

  Stream.Position := 0;
end;

function OtaGetActiveEditorText(Stream: TMemoryStream; UseSelection, FromCurrPos: Boolean; CheckUtf8: Boolean = True): Boolean;
var
  ISourceEditor: IOTASourceEditor;
  IEditView: IOTAEditView;
  IEditReader: IOTAEditReader;
  BlockSelText: string;

  IPos: Integer;
  PreSize: Integer;
begin
  Assert(Stream <> nil);

  Result := False;

  ISourceEditor := OtaGetCurrentSourceEditor;
  if ISourceEditor = nil then
    Exit;

  if ISourceEditor.EditViewCount > 0 then
    begin
      IEditView := OtaGetTopMostEditView(ISourceEditor);
      Assert(IEditView <> nil);

      if (IEditView.Block.Size > 0) and UseSelection then
        begin
          BlockSelText := IEditView.Block.Text;
          Stream.WriteBuffer(PChar(BlockSelText)^, Length(BlockSelText) + SizeOf(#0));
          Result := True;
        end
      else
        begin
          if FromCurrPos then
            IPos := OtaGetCurrPos(ISourceEditor)
          else
            IPos := 0;
          // 如果此文件未保存，则会出现 FileSize 与其不一致的情况，
          // 可能导致 PreSize 为负从而出现问题
          if FileExists(ISourceEditor.FileName) then
            PreSize := Round(GetFileSize(ISourceEditor.FileName) * 1.5) - IPos
          else
            PreSize := 0;

          // 修补上述问题
          if PreSize < 0 then
            PreSize := 0;

          IEditReader := ISourceEditor.CreateReader;
          try
            OtaSaveReaderToStream(IEditReader, Stream, IPos, 0, PreSize, CheckUtf8);
            Result := True;
          finally
            IEditReader := nil;
          end;

        end;
    end;
end;

function OtaGetActiveEditorTextAsString(Lines: TStringList; UseSelection: Boolean): Boolean;
var
  Strm: TMemoryStream;
begin
  Strm := TMemoryStream.Create;
  try
    Result := OtaGetActiveEditorText(Strm, UseSelection, False, True);
    if Result then
      Lines.LoadFromStream(Strm);
  finally
    FreeAndNil(Strm);
  end;
end;

function OtaGetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(ModuleServices));
  Result := ModuleServices.CurrentModule;
end;

function OtaGetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Assert(Assigned(BorlandIDEServices));

  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(IModuleServices));

  Result := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[i];
      if Supports(IModule, IOTAProjectGroup, Result) then
        Break;
    end;
end;

function OtaGetCurrentProject: IOTAProject;
var
  IProjectGroup: IOTAProjectGroup;
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Result := nil;

  IProjectGroup := OtaGetProjectGroup;
  if not Assigned(IProjectGroup) then
    begin
      Assert(Assigned(BorlandIDEServices));
      IModuleServices := BorlandIDEServices as IOTAModuleServices;
      Assert(Assigned(IModuleServices));

      for i := 0 to IModuleServices.ModuleCount - 1 do
        begin
          IModule := IModuleServices.Modules[i];
          if Supports(IModule, IOTAProject, Result) then
            Break;
        end;
    end;

  try
    if Assigned(IProjectGroup) and (not Assigned(Result)) then
      Result := IProjectGroup.ActiveProject;
  except
    Result := nil;
  end;
end;

function OtaGetCurrentProjectName: string;
var
  IProject: IOTAProject;
begin
  Result := '';
  IProject := OtaGetCurrentProject;
  if Assigned(IProject) then
    begin
      Result := ExtractFileName(IProject.FileName);
      Result := ChangeFileExt(Result, '');
    end;
end;

end.
