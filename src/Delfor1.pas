unit Delfor1;

interface

uses
  SysUtils, Classes, ExtCtrls, Menus, DelForTypes, DelforEng;

type
  TPascalParser = class(TObject)
  private
    Timer: TTimer;
    FSettings: TSettings;
    FCapFileName: PAnsiChar;
    CapFileTime: Integer;
    FOnProgress: TProgressEvent;
    FCfgFile: string;
    FRootDir: string;
    procedure SetFillNewWords(AFillNewWords: TFillMode);
    procedure SetCapFileName(AFileName: PAnsiChar);
    procedure TimerTimer(Sender: TObject);
    function GetNewCapFileTime: Integer;
    procedure SetTextStr(AText: PAnsiChar);
    function GetShortCut: TShortCut;
    procedure SetShortCut(AShortCut: TShortCut);
    procedure SetShortCutText(AShortCut: string);
    function GetShortCutText: string;
    function GetTextStr: PAnsiChar;
    function GetRootDir: string;
    procedure SetRootdir(const Value: string);
    procedure SetOnProgress(const Value: TProgressEvent);
  public
    procedure Activate;
    procedure Deactivate;
    procedure SaveCapFile(AFileName: PAnsiChar);
    procedure LoadCapFile(AFileName: PAnsiChar);
    constructor Create(ARootdir: string);
    procedure Clear;
    procedure SetDefault;
    procedure SetBorland;
    procedure LoadFromFile(AFileName: PAnsiChar);
    procedure LoadFromList(AList: TStringList);
    function Parse: Boolean;
    procedure Config(DoRead: Boolean);
    procedure WriteToFile(AFileName: PAnsiChar);
    destructor Destroy; override;
    property SpacePerIndent: Integer read FSettings.SpacePerIndent write
      FSettings.SpacePerIndent;
    property SpaceOperators: TSpaceBefore read FSettings.SpaceOperators write
      FSettings.SpaceOperators;
    property SpaceEqualOper: TSpaceBefore read FSettings.SpaceEqualOper write
      FSettings.SpaceEqualOper;
    property SpaceColon: TSpaceBefore read FSettings.SpaceColon write
      FSettings.SpaceColon;
    property SpaceComma: TSpaceBefore read FSettings.SpaceComma write
      FSettings.SpaceComma;
    property SpaceSemiColon: TSpaceBefore read FSettings.SpaceSemiColon write
      FSettings.SpaceSemiColon;
    property SpaceLeftBr: TSpaceBefore read FSettings.SpaceLeftBr write
      FSettings.SpaceLeftBr;
    property SpaceRightBr: TSpaceBefore read FSettings.SpaceRightBr write
      FSettings.SpaceRightBr;
    property SpaceLeftHook: TSpaceBefore read FSettings.SpaceLeftHook write
      FSettings.SpaceLeftHook;
    property SpaceRightHook: TSpaceBefore read FSettings.SpaceRightHook write
      FSettings.SpaceRightHook;
    property UpperCompDirectives: Boolean read FSettings.UpperCompDirectives
      write FSettings.UpperCompDirectives;
    property UpperNumbers: Boolean read FSettings.UpperNumbers write
      FSettings.UpperNumbers;
    property ReservedCase: TCase read FSettings.ReservedCase write
      FSettings.ReservedCase;
    property StandDirectivesCase: TCase read FSettings.StandDirectivesCase write
      FSettings.StandDirectivesCase;
    property ChangeIndent: Boolean read FSettings.ChangeIndent write
      FSettings.ChangeIndent;
    property indentBegin: Boolean read FSettings.indentBegin write
      FSettings.indentBegin;
    property NoIndentElseIf: Boolean read FSettings.NoIndentElseIf write
      FSettings.NoIndentElseIf;
    property IndentComments: Boolean read FSettings.IndentComments write
      FSettings.IndentComments;
    property IndentCompDirectives: Boolean read FSettings.IndentCompDirectives
      write FSettings.IndentCompDirectives;
    property IndentTry: Boolean read FSettings.IndentTry write
      FSettings.IndentTry;
    property IndentTryElse: Boolean read FSettings.IndentTryElse write
      FSettings.IndentTryElse;
    property IndentCaseElse: Boolean read FSettings.IndentCaseElse write
      FSettings.IndentCaseElse;
    property BlankProc: Boolean read FSettings.BlankProc write
      FSettings.BlankProc;
    property RemoveDoubleBlank: Boolean read FSettings.RemoveDoubleBlank write
      FSettings.RemoveDoubleBlank;
    property FeedRoundBegin: TFeedBegin read FSettings.FeedRoundBegin write
      FSettings.FeedRoundBegin;
    property FeedAfterThen: Boolean read FSettings.FeedAfterThen write
      FSettings.FeedAfterThen;
    property ExceptSingle: Boolean read FSettings.ExceptSingle write
      FSettings.ExceptSingle;
    property NoFeedBeforeThen: Boolean read FSettings.NoFeedBeforeThen write
      FSettings.NoFeedBeforeThen;
    property FeedElseIf: Boolean read FSettings.FeedElseIf write
      FSettings.FeedElseIf;
    property FeedEachUnit: Boolean read FSettings.FeedEachUnit write
      FSettings.FeedEachUnit;
    property FeedAfterVar: Boolean read FSettings.FeedAfterVar write
      FSettings.FeedAfterVar;
    property WrapLines: Boolean read FSettings.WrapLines write
      FSettings.WrapLines;
    property WrapPosition: Byte read FSettings.WrapPosition write
      FSettings.WrapPosition;
    property AlignCommentPos: Byte read FSettings.AlignCommentPos write
      FSettings.AlignCommentPos;
    property AlignComments: Boolean read FSettings.AlignComments write
      FSettings.AlignComments;
    property AlignVarPos: Byte read FSettings.AlignVarPos write
      FSettings.AlignVarPos;
    property AlignVar: Boolean read FSettings.AlignVar write
      FSettings.AlignVar;
    property FeedBeforeEnd: Boolean read FSettings.FeedBeforeEnd write
      FSettings.FeedBeforeEnd;
    property FillNewWords: TFillMode read FSettings.FillNewWords write
      SetFillNewWords;
    property FeedAfterSemiColon: Boolean read FSettings.FeedAfterSemiColon write
      FSettings.FeedAfterSemiColon;
    property BlankSubProc: Boolean read FSettings.BlankSubProc write
      FSettings.BlankSubProc;
    property CommentFunction: Boolean read FSettings.CommentFunction write
      FSettings.CommentFunction;
    property CommentUnit: Boolean read FSettings.CommentUnit write
      FSettings.CommentUnit;
    property StartCommentOut: TCommentArray read FSettings.StartCommentOut write
      FSettings.StartCommentOut;
    property EndCommentOut: TCommentArray read FSettings.EndCommentOut write
      FSettings.EndCommentOut;
    property CapFileName: PAnsiChar read FCapFileName write SetCapFileName;
    property Text: PAnsiChar read GetTextStr write SetTextStr;
    property OnProgress: TProgressEvent read FOnProgress write SetOnProgress;
    property ShortCutText: string read GetShortCutText write SetShortCutText;
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
    property CfgFile: string read FCfgFile write FCfgFile;
    property RootDir: string read GetRootDir write SetRootdir;
    property Settings: TSettings read FSettings write FSettings;
  end;

var
  Formatter: TPascalParser;

implementation

uses FileCtrl
{$IFDEF DLL}, DelForExpert{$ENDIF}
{$IFDEF DEBUG}, CnDebug{$ENDIF};

constructor TPascalParser.Create(ARootdir: string);
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('PascalParser Createing.');
{$ENDIF}
  Formatter := Self;
  CapFileTime := -1;
  SetDefault;
  Timer := TTimer.Create(nil);
  Timer.Interval := 1000;
  Timer.OnTimer := TimerTimer;
  RootDir := ARootdir;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('PascalParser Created.');
{$ENDIF}
  Activate;
end;

procedure TPascalParser.Activate;
begin
  LoadDll(RootDir);
  if not IsLoadDLL then Exception.Create('º”‘ÿ ' + DelForDll + '  ß∞‹°£');
end;

procedure TPascalParser.Deactivate;
begin
  FreeDll;
end;

procedure TPascalParser.SetDefault;
begin
  CapFileName := nil; //PAnsiChar(AnsiString(RootDir) + 'CodeFormat.txt');
  WrapLines := False;
  WrapPosition := 81;
  AlignCommentPos := 40;
  AlignComments := False;
  AlignVarPos := 20;
  AlignVar := False;
  SpaceEqualOper := spBoth;
  SpaceOperators := spBoth;
  SpaceColon := spAfter;
  SpaceComma := spAfter;
  SpaceSemiColon := spAfter;
  SpaceLeftBr := spNone;
  SpaceRightBr := spNone;
  SpaceLeftHook := spNone;
  SpaceRightHook := spNone;
  ReservedCase := rfLowerCase;
  StandDirectivesCase := rfLowerCase;
  ChangeIndent := True;
  indentBegin := False;
  IndentComments := False;
  IndentCompDirectives := False;
  IndentTryElse := False;
  IndentCaseElse := False;
  FeedAfterThen := False;
  ExceptSingle := False;
  FeedElseIf := False;
  FeedEachUnit := False;
  NoFeedBeforeThen := False;
  NoIndentElseIf := False;
  FeedAfterVar := False;
  FeedBeforeEnd := False;
  FeedRoundBegin := UnChanged;
  FeedAfterSemiColon := False;
  FillNewWords := fmUnchanged;
  IndentTry := False;
  UpperCompDirectives := True;
  UpperNumbers := True;
  SpacePerIndent := 2;
  BlankProc := True;
  RemoveDoubleBlank := False;
  BlankSubProc := False;
  CommentFunction := False;
  CommentUnit := False;
  StrCopy(StartCommentOut, '{(*}');
  StrCopy(EndCommentOut, '{*)}');
  ShortCut := Menus.ShortCut(Word('D'), [ssCtrl]);
end;

procedure TPascalParser.SetBorland;
begin
  SetDefault;
  WrapLines := True;
  IndentComments := True;
  FeedAfterThen := True;
  ExceptSingle := False;
  NoFeedBeforeThen := True;
  FeedAfterVar := True;
  FeedBeforeEnd := True;
  FeedRoundBegin := NewLine;
  FeedAfterSemiColon := True;
  RemoveDoubleBlank := True;
end;

function TPascalParser.GetNewCapFileTime: Integer;
var
  H: Integer;
begin
  H := FileOpen(FCapFileName, fmOpenRead);
  try
    Result := FileGetDate(H);
  finally
    FileClose(H);
  end;
end;

procedure TPascalParser.SetCapFileName(AFileName: PAnsiChar);
var
  P: PAnsiChar;
  S: array[0..260] of AnsiChar;
  TimeStamp: Integer;
begin
  if AFileName <> FCapFileName then
  begin
    StrDispose(FCapFileName);
    if AFileName <> nil then
    begin
      while AFileName^ = ' ' do
        inc(AFileName);
      P := StrEnd(StrCopy(S, AFileName)) - 1;
      while P^ = ' ' do
        Dec(P);
      (P + 1)^ := #0;
      FCapFileName := StrNew(S);
    end
    else
      FCapFileName := nil;

    if (FCapFileName <> nil) and FileExists(FCapFileName) then
    begin
      TimeStamp := GetNewCapFileTime;
      if (TimeStamp <> CapFileTime) then
        LoadCapFile(FCapFileName);
    end
    else
    begin
      if @Formatter_LoadCapFile <> nil then
        Formatter_LoadCapFile(nil);
      CapFileTime := -1;
    end;
  end;
end;

procedure TPascalParser.SetShortCutText(AShortCut: string);
begin
  ShortCut := TextToShortCut(AShortCut);
end;

function TPascalParser.GetShortCutText: string;
begin
  Result := ShortCutToText(ShortCut);
end;

procedure TPascalParser.SaveCapFile(AFileName: PAnsiChar);
var
  Path: string;
begin
  if @Formatter_SaveCapFile = nil then Exit;
  Path := ExtractFilePath(ExpandFileName(AFileName));
  if Path = '' then Exit;
  if not DirectoryExists(Path) then
    ForceDirectories(Path);
  Formatter_SaveCapFile(AFileName);
end;

procedure TPascalParser.Clear;
begin
  if @Formatter_Version = nil then Exit;
  if CurrentDllVersion <> Formatter_Version then
    Exception.Create('Œ¥÷™ DELFORDLL ∞Ê±æ');
  Formatter_clear;
end;

procedure TPascalParser.LoadFromFile(AFileName: PAnsiChar);
begin
  if @Formatter_LoadFromFile = nil then Exit;
  Formatter_LoadFromFile(AFileName, @FSettings, SizeOf(TSettings));
end;

procedure TPascalParser.LoadFromList(AList: TStringList);
begin
  if @Formatter_LoadFromList = nil then Exit;
  Formatter_LoadFromList(AList, @FSettings, SizeOf(TSettings));
end;

function TPascalParser.Parse: Boolean;
begin
  Result := False;
  if @Formatter_Parse = nil then Exit;
  Result := Formatter_Parse(@FSettings, SizeOf(TSettings));
end;

procedure TPascalParser.Config(DoRead: Boolean);
{$IFDEF ver100}
type
  Cardinal = Integer;
{$ENDIF}
const
  Sign: Cardinal = $623DFE51;
var
  Signature: Cardinal;
  Err, Size: Integer;
  Cfg: file;
  S: array[0..260] of AnsiChar;
begin
  if CfgFile = '' then
    CfgFile := 'DelForExp.cfg';
  CfgFile := ChangeFileExt(CfgFile, '.cfg');
  if DoRead then
  begin
    if not FileExists(CfgFile) then
      CapFileName := nil
    else
    begin
      if FillNewWords in [fmAddNewWord, fmAddUse, fmAddUseExcept] then
        SaveCapFile(CapFileName);
      AssignFile(Cfg, CfgFile);
      try
        Reset(Cfg, 1);
        BlockRead(Cfg, Signature, SizeOf(Signature), Err);
        if (Err = SizeOf(Signature)) and (Signature = Sign) then
        begin
          BlockRead(Cfg, FSettings, SizeOf(Settings), Err);
          BlockRead(Cfg, Size, SizeOf(Integer));
          if Size > 0 then
          begin
            BlockRead(Cfg, S, Size);
            CapFileName := S;
          end
          else
            CapFileName := nil;
          ShortCut := Settings.ShortCut;
        end;
      finally
        CloseFile(Cfg);
      end;
    end;
  end
  else
  begin
    AssignFile(Cfg, CfgFile);
    try
      Rewrite(Cfg, 1);
      Signature := Sign;
      BlockWrite(Cfg, Signature, SizeOf(Signature));
      BlockWrite(Cfg, Settings, SizeOf(Settings));
      if CapFileName = nil then
      begin
        Size := 0;
        BlockWrite(Cfg, Size, SizeOf(Integer));
      end
      else
      begin
        Size := StrLen(CapFileName) + 1;
        BlockWrite(Cfg, Size, SizeOf(Integer));
        StrCopy(S, CapFileName);
        BlockWrite(Cfg, S, Size);
      end;
    finally
      CloseFile(Cfg);
    end;
  end;
end;

procedure TPascalParser.WriteToFile(AFileName: PAnsiChar);
begin
  if @Formatter_writeToFile = nil then Exit;
  Formatter_writeToFile(AFileName);
end;

procedure TPascalParser.SetTextStr(AText: PAnsiChar);
begin
  if @Formatter_SetTextStr = nil then Exit;
  Formatter_SetTextStr(AText);
end;

function TPascalParser.GetTextStr: PAnsiChar;
begin
  Result := '';
  if @Formatter_GetTextStr = nil then Exit;
  Result := Formatter_GetTextStr;
end;

destructor TPascalParser.Destroy;
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('PascalParser Destroying.');
{$ENDIF}
  if @Formatter_Destroy <> nil then
  begin
    if FillNewWords in [fmAddNewWord, fmAddUse, fmAddUseExcept] then
      SaveCapFile(CapFileName);
    CapFileName := nil;
    Formatter_Destroy;
  end;
  inherited Destroy;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('PascalParser Destroyed.');
{$ENDIF}
end;

procedure TPascalParser.SetShortCut(AShortCut: TShortCut);
begin
  FSettings.ShortCut := AShortCut;
{$IFDEF DLL}
  if IDETextExpert <> nil then
    IDETextExpert.MenuShortCut := AShortCut;
{$ENDIF}
end;

procedure TPascalParser.TimerTimer(Sender: TObject);
{$IFDEF DLL}
var
  SC: TShortCut;
{$ENDIF}
begin
{$IFDEF DLL}
  SC := Settings.ShortCut;
  if HasShortCut(SC) then
    ShortCut := SC;
{$ENDIF}
  ShortCut := Settings.ShortCut;
  Timer.Free;
  Timer := nil;
end;

procedure TPascalParser.SetFillNewWords(AFillNewWords: TFillMode);
begin
  if Settings.FillNewWords in [fmAddNewWord, fmAddUse, fmAddUseExcept] then
    SaveCapFile(CapFileName);
  FSettings.FillNewWords := AFillNewWords;
end;

function TPascalParser.GetRootDir: string;
begin
  if FRootDir = '' then
    FRootDir := ExtractFilePath(ParamStr(0));
  Result := FRootDir;
end;

procedure TPascalParser.SetRootdir(const Value: string);
begin
  FRootDir := Value;
  if FRootDir[Length(FRootDir)] <> '\' then
    FRootDir := FRootDir + '\';
end;

function TPascalParser.GetShortCut: TShortCut;
begin
  Result := TShortCut(Settings.ShortCut);
end;

procedure TPascalParser.LoadCapFile(AFileName: PAnsiChar);
begin
  if @Formatter_LoadCapFile = nil then Exit;
  Formatter_LoadCapFile(AFileName);
end;

procedure TPascalParser.SetOnProgress(const Value: TProgressEvent);
begin
  FOnProgress := Value;
  if @Formatter_SetOnProgress = nil then Exit;
  Formatter_SetOnProgress(Value);
end;

end.
