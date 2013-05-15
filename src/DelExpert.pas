unit DelExpert;

{$I DelForEx.inc}

interface

uses Windows, SysUtils, Classes, Forms, Controls, StdCtrls,
  Delfor1
{$IFDEF DELPHI6_UP}
    , ToolsAPI
{$ENDIF};

type
{$IFDEF DELPHI6_UP}
  TBookMarkRed = record
    Exists: Boolean;
    BookMarkNo: Integer;
    FPos: TOTAEditPos;
  end;

  TBookmarks = array [0 .. 19] of TBookMarkRed;
{$ENDIF}
{$IFDEF Delphi9_UP}

  TBreakPoint = record
    StackFramesToLog: Integer;
    DoHandleExceptions: Boolean;
    DoIgnoreExceptions: Boolean;
    GroupName: string;
    DoBreak: Boolean;
    LogMessage: string;
    EvalExpression: string;
    LogResult: Boolean;
    EnableGroup: string;
    DisableGroup: string;
    Enabled: Boolean;
    Expression: string;
    FileName: string;
    LineNumber: Integer;
    PassCount: Integer;
  end;

  TBreakPoints = array of TBreakPoint;
{$ENDIF}

  TDelExpertDlg = class(TForm)
    OptionsButton: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    GroupBox1: TGroupBox;
    CurrentBtn: TButton;
    AllButton: TButton;
    WholeProjBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure CurrentBtnClick(Sender: TObject);
    procedure AllButtonClick(Sender: TObject);
    procedure WholeProjBtnClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
{$IFDEF Delphi9_UP}
    BreakPoints: TBreakPoints;
{$ENDIF}
{$IFDEF DELPHI6_UP}
    ISourceEditor: IOTASourceEditor;
    FCurPos: TOTAEditPos;
    Bookmarks: TBookmarks;
    procedure BeforeFormat(AFileName: string);
    procedure AfterFormat;
{$ENDIF}
    procedure FormatterProgress(Sender: TObject; Progress: Integer);
    procedure CapFileNameEditChange(Sender: TObject);
  public
    HelpFile: string;
    CfgFile: string;
    procedure DoFormatFile(AFileName: string);
    { Public declarations }
  end;

var
  DelExpertDlg: TDelExpertDlg = nil;
  Formatter: TPascalParser = nil;

function ShowErrMsgBox(const S: string): Integer;

implementation

uses DelForCommon, DelForTypes, MyIDEStream, Progr, OptDlg
{$IFDEF Delphi9_UP}, DelForOTAUtils{$ENDIF}
{$IFDEF DEBUG}, CnDebug{$ENDIF};

{$R *.DFM}

function ShowErrMsgBox(const S: string): Integer;
begin
  if Assigned(ProgressDlg) then
    ProgressDlg.Hide;
  Result := Application.MessageBox(PChar(S), '错误', MB_ICONHAND);
end;

function FormatFile(Param: Pointer; const FileName, UnitName, FormName: string)
  : Boolean stdcall;
var
  BakFile: array [0 .. MAX_PATH] of AnsiChar;
  ExtName: string;
begin
  Result := True;
  ExtName := LowerCase(ExtractFileExt(FileName));
  if ((ExtName = '.pas') or (ExtName = '.dpr')) and (Trim(FileName) <> '') then
  begin
{$IFDEF Delphi9_UP}
    if OtaIsFileOpen(FileName) then
{$ELSE}
    if FToolServices.IsFileOpen(FileName) then
{$ENDIF}
      DelExpertDlg.DoFormatFile(FileName)
    else if (Param = nil) then
    begin
      if FileExists(FileName) and not IsReadonlyFile(FileName) then
      begin
        { IF PARAM<>NIL then only open files }
        if ProgressDlg.Visible then
        begin
          { if file not open then load from file }
          ProgressDlg.SetFileName(FileName);
          Application.ProcessMessages;
          Formatter.Clear;
          with Formatter do
          begin
            try
              Clear;
              try
                if MakeBakFile(BakFile, PAnsiChar(AnsiString(FileName))) then
                begin
                  LoadFromFile(BakFile);
                  if Parse then
                    WriteToFile(PAnsiChar(AnsiString(FileName)));
                  DeleteFileA(BakFile);
                end;
              except
                on E: EInOutError do
                  ShowErrMsgBox('I/O 错误 [' + E.Message + '] , "' +
                    FileName + '"');
              end;
            finally
              Formatter.Clear;
            end;
          end;
        end;
      end
      else
        ShowErrMsgBox('没找到 "' + FileName + '" 文件。');
    end;
  end;
end;

procedure TDelExpertDlg.CapFileNameEditChange(Sender: TObject);
begin
{$IFNDEF Delphi9_UP}
  if (OptionsDlg <> nil) then
    with OptionsDlg do
      SaveFile(CapFileNameEdit.Text);
{$ENDIF}
end;

procedure TDelExpertDlg.FormatterProgress(Sender: TObject; Progress: Integer);
begin
  ProgressDlg.ProgressBar.Position := Progress;
end;

procedure TDelExpertDlg.DoFormatFile(AFileName: string);
{$DEFINE tmpFile }
{$IFDEF tmpFile}
const
  TmpFile = 'DELFOR.~$$';
var
  P1: PAnsiChar;
  Tmp: file;
  LenP: Integer;
{$ENDIF}
var
  IDEStream: TIDEStream;
{$IFDEF Delphi9_UP}
  bReadOnly: Boolean;
{$ENDIF}
begin
  if (ProgressDlg.Visible) then
  begin

    IDEStream := TIDEStream.Create(AFileName);
    try
{$IFDEF Delphi6_UP}
      BeforeFormat(AFileName);
{$ENDIF}
{$IFDEF Delphi9_UP}
      try
        OtaAssertSourceEditorNotReadOnly(OtaGetCurrentSourceEditor);
        bReadOnly := False;
      except
        bReadOnly := True;
      end;
      if (not IsReadonlyFile(IDEStream.FileName)) and (not bReadOnly) then
{$ELSE}
      if (not IsReadonlyFile(IDEStream.FileName)) and
        (not IsReadonlyBuffer(IDEStream.FileName)) then
{$ENDIF}
      begin
        Formatter.Clear;
        ProgressDlg.SetFileName(IDEStream.FileName);

        Application.ProcessMessages;
{$IFNDEF tmpFile}
{$IFDEF Delphi12_UP}
        // //Application.MessageBox(pwidechar(inttostr(IDEStream.Lines.Count)), '');
        Formatter.Text := PAnsiChar(AnsiString(IDEStream.Lines.Text));
        // // application.MessageBox(pwidechar(formatter.Text),'');
{$ELSE}
        Formatter.LoadFromList(IDEStream.Lines);
{$ENDIF}
        IDEStream.Lines.Clear;
{$ELSE}
{$IFDEF Delphi12_UP}
        P1 := PAnsiChar(AnsiString(IDEStream.Lines.Text));
{$ELSE}
        P1 := IDEStream.GetText;
{$ENDIF}
        if P1 <> nil then
        begin
          LenP := StrLen(P1);
          if LenP > $8FFF then
          begin
            AssignFile(Tmp, TmpFile);
            Rewrite(Tmp, 1);
            BlockWrite(Tmp, P1^, LenP);
            CloseFile(Tmp);
            Formatter.LoadFromFile(TmpFile);
            Erase(Tmp);
          end
          else
            Formatter.Text := P1; { GetTextStr is very slow for large strings }
        end;
{$ENDIF}
        // Application.MessageBox('before parse', '');
        if Formatter.Parse then
          IDEStream.WriteText(Formatter.Text);
      end
      else
        ShowErrMsgBox(ExtractFileName(IDEStream.FileName) + ': 文件只读或未保存！');
    finally
      Formatter.Clear;
      IDEStream.Free;
{$IFDEF Delphi6_UP}
      AfterFormat;
{$ENDIF}
    end;
  end;
end;

procedure TDelExpertDlg.FormCreate(Sender: TObject);
var
  DelForName: string;
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('DelExpertDlg Createing.');
{$ENDIF}
{$IFNDEF Delphi9_UP}
  GetToolServieces;
{$ENDIF}
  DelForName := GetModuleName(HInstance); // GetDelForName;
  if DelForName <> '' then
  begin
    if Formatter = nil then
      Formatter := TPascalParser.Create(ExtractFilePath(DelForName));
    Formatter.OnProgress := FormatterProgress;
    HelpFile := Formatter.RootDir + 'DelFor.hlp';
    CfgFile := Formatter.RootDir + ExtractFileName(DelForName);
    ChangeFileExt(CfgFile, '.cfg');
  end
  else
  begin
    if Formatter = nil then
      Formatter := TPascalParser.Create('');
    HelpFile := 'DelFor.hlp';
    CfgFile := '';
  end;
  Formatter.CfgFile := CfgFile;
  Formatter.Config(True);
  if ProgressDlg = nil then
    ProgressDlg := TProgressDlg.Create(Application);
  ActiveControl := CurrentBtn;
  Formatter.Deactivate;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('DelExpertDlg Created.');
{$ENDIF}
end;

procedure TDelExpertDlg.FormShow(Sender: TObject);
var
  ProjectOpened: Boolean;
  // S: Array[0..MAX_PATH] Of AnsiChar;
begin
  Formatter.Activate;
  with Formatter do
    if CapFileName <> nil then
    begin
{$IFNDEF Delphi9_UP}
      SaveFile(CapFileName);
{$ENDIF}
      // CapFileName := StrCopy(S, CapFileName);
    end;
  { if file is changed reread }
{$IFDEF Delphi9_UP} // default.htm
  CurrentBtn.Enabled := OtaGetCurrentSourceFile <> '';
  ProjectOpened := OtaGetCurrentProjectName <> '';
{$ELSE}
  CurrentBtn.Enabled := GetCurrentFile <> '';
  ProjectOpened := GetProjectName <> '';
{$ENDIF}
  AllButton.Enabled := ProjectOpened;
  WholeProjBtn.Enabled := ProjectOpened;
end;

procedure TDelExpertDlg.FormClose(Sender: TObject; var Action: TCloseAction);
{$IFDEF Delphi9_UP}
var
  I: Integer;
{$ENDIF}
begin
  ProgressDlg.Hide;
  with Formatter do
  begin
    Clear;
    if (FillNewWords = fmAddNewWord) then
      SaveCapFile(CapFileName);
    Deactivate;
  end;
{$IFDEF Delphi9_UP}
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TButton then
      with (Controls[I] as TButton) do
        Default := Focused;
{$ENDIF}
end;

procedure TDelExpertDlg.FormDestroy(Sender: TObject);
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('DelExpertDlg Destroying.');
{$ENDIF}
  Formatter.Config(False);
  Formatter.Free;
  Formatter := nil;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('DelExpertDlg Destroyed.');
{$ENDIF}
end;

procedure TDelExpertDlg.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    if Self.ActiveControl is TButton then
      TButton(Self.ActiveControl).Click;
  end;
end;

procedure TDelExpertDlg.CurrentBtnClick(Sender: TObject);
begin
  ProgressDlg.Show;
  DoFormatFile('');
  ProgressDlg.Hide;
  Close;
end;

procedure TDelExpertDlg.AllButtonClick(Sender: TObject);
var
  I: Integer;
{$IFDEF Delphi9_UP}
  Dir: string;
  List: TList;
{$ENDIF}
begin
  ProgressDlg.Show;
{$IFDEF Delphi9_UP}
  Dir := ExtractFileDir(ExpandFileName(OtaGetCurrentProjectName));
  if Dir <> '' then
    ChDir(Dir);

  List := TList.Create;
  try
    OtaFillUnitInfoListForCurrentProject(List);
    for I := 0 to List.Count - 1 do
      FormatFile(@I, TUnitInfo(List.Items[I]).FileName, TUnitInfo(List.Items[I])
        .UnitName, TUnitInfo(List.Items[I]).FormName);
  finally
    List.Free;
    ProgressDlg.Hide;
  end;
{$ELSE}
  FToolServices.EnumProjectUnits(FormatFile, @I);
  ProgressDlg.Hide;
{$ENDIF}
  Close;
end;

procedure TDelExpertDlg.WholeProjBtnClick(Sender: TObject);
var
  Dir, S: string;
{$IFDEF Delphi9_UP}
  List: TList;
  I: Integer;
{$ENDIF}
begin
  ProgressDlg.Show;
{$IFDEF Delphi9_UP}
  S := OtaGetCurrentProjectName;
{$ELSE}
  S := GetProjectName;
{$ENDIF}
  Dir := ExtractFileDir(ExpandFileName(S));
  if Dir <> '' then
    ChDir(Dir);
{$IFDEF DElphi9_UP}
  List := TList.Create;
  try
    OtaFillUnitInfoListForCurrentProject(List);
    for I := 0 to List.Count - 1 do
      FormatFile(nil, TUnitInfo(List.Items[I]).FileName,
        TUnitInfo(List.Items[I]).UnitName, TUnitInfo(List.Items[I]).FormName);
  finally
    List.Free;
    ProgressDlg.Hide;
  end;
{$ELSE}
  FToolServices.EnumProjectUnits(FormatFile, nil);
  ProgressDlg.Hide;
{$ENDIF}
  Close;
end;

procedure TDelExpertDlg.OptionsButtonClick(Sender: TObject);
begin
  if OptionsDlg = nil then
  begin
    OptionsDlg := TOptionsDlg.Create(Application);
    OptionsDlg.HelpFile := HelpFile;
    OptionsDlg.CapFileNameEdit.OnChange := CapFileNameEditChange;
  end;
  OptionsDlg.Formatter := Formatter;
  OptionsDlg.ShowModal;
end;

procedure TDelExpertDlg.HelpBtnClick(Sender: TObject);
begin
  WinHelp(0, PChar(HelpFile), Help_Contents, 0);
end;

// ================================================================================

{$IFDEF Delphi6_UP}

procedure TDelExpertDlg.AfterFormat;
{$IFDEF DELPHI9_UP}
  procedure AddBreakPoint;
  var
    DebugSvs: IOTADebuggerServices;
    Tmp: TBreakPoint;
    NewBreakPoint: IOTABreakpoint;
    j: Integer;
  begin
    if Assigned(BorlandIDEServices) and Supports(BorlandIDEServices,
      IOTADebuggerServices, DebugSvs) then
    begin
      for j := 0 to Length(BreakPoints) - 1 do
      begin
        Tmp := BreakPoints[j];
        NewBreakPoint := DebugSvs.NewSourceBreakpoint(Tmp.FileName,
          Tmp.LineNumber, DebugSvs.CurrentProcess);
        NewBreakPoint.StackFramesToLog := Tmp.StackFramesToLog;
        NewBreakPoint.DoHandleExceptions := Tmp.DoHandleExceptions;
        NewBreakPoint.DoIgnoreExceptions := Tmp.DoIgnoreExceptions;
        NewBreakPoint.GroupName := Tmp.GroupName;
        NewBreakPoint.DoBreak := Tmp.DoBreak;
        NewBreakPoint.LogMessage := Tmp.LogMessage;
        NewBreakPoint.EvalExpression := Tmp.EvalExpression;
        NewBreakPoint.LogResult := Tmp.LogResult;
        NewBreakPoint.EnableGroup := Tmp.EnableGroup;
        NewBreakPoint.DisableGroup := Tmp.DisableGroup;
        NewBreakPoint.Enabled := Tmp.Enabled;
        NewBreakPoint.Expression := Tmp.Expression;
        NewBreakPoint.PassCount := Tmp.PassCount;
      end;
    end;
    BreakPoints := nil;

  end;
{$ENDIF}

var
  I: Integer;
begin
  if Assigned(ISourceEditor) and (ISourceEditor.EditViewCount > 0) then
  begin
    for I := 0 to 19 do
    begin
      if Bookmarks[I].Exists then
      begin
        ISourceEditor.EditViews[0].CursorPos := Bookmarks[I].FPos;
        ISourceEditor.EditViews[0].BookmarkRecord(Bookmarks[I].BookMarkNo);
      end;
    end;
    ISourceEditor.EditViews[0].CursorPos := FCurPos;
    ISourceEditor.EditViews[0].MoveViewToCursor;
    ISourceEditor.EditViews[0].Paint;
    ISourceEditor := nil;
  end;
{$IFDEF DELPHI9_UP}
  AddBreakPoint;
{$ENDIF}
end;

procedure TDelExpertDlg.BeforeFormat(AFileName: string);

  function GetTheModule: IOTAModule;
  var
    SVC: IOTAModuleServices;
  begin
    Result := nil;
    if Assigned(BorlandIDEServices) and Supports(BorlandIDEServices,
      IOTAModuleServices, SVC) then
    begin
      if AFileName = '' then
        Result := SVC.CurrentModule
      else
        Result := SVC.FindModule(AFileName);
    end;
  end;

{$IFDEF DELPHI9_UP}
  procedure SaveBreakPoint;
  var
    DebugSvs: IOTADebuggerServices;
    I: Integer;
  begin
    if Assigned(BorlandIDEServices) and Supports(BorlandIDEServices,
      IOTADebuggerServices, DebugSvs) then
    begin
      SetLength(BreakPoints, DebugSvs.SourceBkptCount);
      for I := 0 to DebugSvs.SourceBkptCount - 1 do
      begin
        BreakPoints[I].StackFramesToLog := DebugSvs.SourceBkpts[I]
          .StackFramesToLog;
        BreakPoints[I].DoHandleExceptions := DebugSvs.SourceBkpts[I]
          .DoHandleExceptions;
        BreakPoints[I].DoIgnoreExceptions := DebugSvs.SourceBkpts[I]
          .DoIgnoreExceptions;
        BreakPoints[I].GroupName := DebugSvs.SourceBkpts[I].GroupName;
        BreakPoints[I].DoBreak := DebugSvs.SourceBkpts[I].DoBreak;
        BreakPoints[I].LogMessage := DebugSvs.SourceBkpts[I].LogMessage;
        BreakPoints[I].EvalExpression := DebugSvs.SourceBkpts[I].EvalExpression;
        BreakPoints[I].LogResult := DebugSvs.SourceBkpts[I].LogResult;
        BreakPoints[I].EnableGroup := DebugSvs.SourceBkpts[I].EnableGroup;
        BreakPoints[I].DisableGroup := DebugSvs.SourceBkpts[I].DisableGroup;
        BreakPoints[I].Enabled := DebugSvs.SourceBkpts[I].Enabled;
        BreakPoints[I].Expression := DebugSvs.SourceBkpts[I].Expression;
        BreakPoints[I].FileName := DebugSvs.SourceBkpts[I].FileName;
        BreakPoints[I].LineNumber := DebugSvs.SourceBkpts[I].LineNumber;
        BreakPoints[I].PassCount := DebugSvs.SourceBkpts[I].PassCount;
      end;
    end;
  end;
{$ENDIF}

var
  MyDDD: IOTAModule;
  FormEditor: IOTAFormEditor;
  I: Integer;
begin
  ISourceEditor := nil;
  MyDDD := GetTheModule;
  if Assigned(MyDDD) then
  begin
    if not Supports(MyDDD.CurrentEditor, IOTAFormEditor, FormEditor) then
      ISourceEditor := MyDDD.CurrentEditor as IOTASourceEditor;
  end;
  if Assigned(ISourceEditor) and (ISourceEditor.EditViewCount > 0) then
  begin
    FCurPos := ISourceEditor.EditViews[0].CursorPos;
    for I := 0 to 19 do
    begin
      Bookmarks[I].Exists := False;
      if ISourceEditor.EditViews[0].BookmarkGoto(I) then
      begin
        Bookmarks[I].BookMarkNo := I;
        Bookmarks[I].Exists := True;
        Bookmarks[I].FPos := ISourceEditor.EditViews[0].CursorPos;
      end;
    end;
  end;
{$IFDEF DELPHI9_UP}
  SaveBreakPoint;
{$ENDIF}
end;
{$ENDIF}

end.
