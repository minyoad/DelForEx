unit OptDlg;

interface

uses Windows, SysUtils, Classes, Forms, Controls, StdCtrls,
  ComCtrls, Delfor1, Dialogs, Grids, DelForTypes;

type
  TOptionsDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    PageControl1: TPageControl;
    Indent: TTabSheet;
    Spaces: TTabSheet;
    Blanklines: TTabSheet;
    SpacePerIndentEdit: TEdit;
    SpacePerIndentUpDown: TUpDown;
    Label1: TLabel;
    Capitals: TTabSheet;
    IndentBeginCheck: TCheckBox;
    UpperCompDirectivesCheck: TCheckBox;
    BlankProcCheck: TCheckBox;
    BlankSubProcCheck: TCheckBox;
    Label2: TLabel;
    IndentTryCheck: TCheckBox;
    Label3: TLabel;
    FeedAfterSemiColonCheck: TCheckBox;
    FeedRoundBeginCombo: TComboBox;
    Label4: TLabel;
    FeedAfterThenCheck: TCheckBox;
    FeedBeforeEndCheck: TCheckBox;
    UpperNumbersCheck: TCheckBox;
    IndentTryElseCheck: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    DefaultBtn: TButton;
    TabSheet1: TTabSheet;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    StartCommentOutEdit: TEdit;
    EndCommentOutEdit: TEdit;
    Label11: TLabel;
    CapFileNameButton: TButton;
    CapFileNameEdit: TEdit;
    OpenDialog: TOpenDialog;
    WrapLinesCheck: TCheckBox;
    Label13: TLabel;
    WrapPositionEdit: TEdit;
    WrapPositionUpDown: TUpDown;
    HelpButton: TButton;
    FeedAfterVarCheck: TCheckBox;
    Label14: TLabel;
    ReservedCaseCombo: TComboBox;
    Label15: TLabel;
    StandDirectivesCombo: TComboBox;
    IndentCommentsCheck: TCheckBox;
    IndentCompDirectivesCheck: TCheckBox;
    TabSheet2: TTabSheet;
    AlignCommentsCheck: TCheckBox;
    Label16: TLabel;
    AlignCommentPosEdit: TEdit;
    AlignCommentPosUpDown: TUpDown;
    AlignVarCheck: TCheckBox;
    Label17: TLabel;
    AlignVarPosEdit: TEdit;
    AlignVarPosUpDown: TUpDown;
    FillNewWordsCombo: TComboBox;
    FeedElseIfCheck: TCheckBox;
    NoFeedBeforeThenCheck: TCheckBox;
    NoIndentElseIfCheck: TCheckBox;
    IndentCaseElseCheck: TCheckBox;
    Label12: TLabel;
    ShortCutEdit: TEdit;
    RemoveDoubleBlankCheck: TCheckBox;
    EditButton: TButton;
    TabSheet3: TTabSheet;
    BeforeMemo: TMemo;
    Label18: TLabel;
    AfterMemo: TMemo;
    Label19: TLabel;
    SpaceGrid: TStringGrid;
    SpacingCombo: TComboBox;
    FeedEachUnitCheck: TCheckBox;
    ExceptSingleCheck: TCheckBox;
    BorlandButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DefaultBtnClick(Sender: TObject);
    procedure CapFileNameButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ShortCutEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditButtonClick(Sender: TObject);
    procedure CapFileNameEditChange(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
    procedure BeforeMemoClick(Sender: TObject);
    procedure BeforeMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BeforeMemoKeyPress(Sender: TObject; var Key: Char);
    procedure BeforeMemoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpacingComboExit(Sender: TObject);
    procedure SpaceGridClick(Sender: TObject);
    procedure SpaceGridTopLeftChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BorlandButtonClick(Sender: TObject);
    procedure FeedAfterThenCheckClick(Sender: TObject);
    procedure SpacesShow(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateButtons;
    procedure FillPreview;
    procedure UpdateFormatter;
    procedure SetFormatterSettings(OldSettings: TSettings; OldCapFileName: PAnsiChar);
    function GetFormatterSettings(OldCapFileName: PAnsiChar): TSettings;
    procedure FillSpaceCombo(ACombo: TComboBox);
    procedure AddSpaceRow(RowNo: Integer; StrCol1, StrCol2: string;
      Space: TSpaceBefore);
    function GetSpaceItem(I: Integer): TSpaceBefore;
    procedure SetDefault(BorlandStyle: Boolean);
  public
    HelpFile: string;
    Formatter: TPascalParser;
    { Public declarations }
  end;

var
  OptionsDlg: TOptionsDlg = nil;

implementation

uses Menus, EditFile, Messages;

{$R *.DFM}

const
  str_None = '没有';
  str_Before = '之前';
  str_after = '之后';
  str_BeforeAfter = '前后都加';

function StrTrim(Dest: PAnsiChar): PAnsiChar;
var
  P: PAnsiChar;
begin
  if Dest^ <> #0 then
  begin
    P := StrEnd(Dest) - 1;
    while (P > Dest) and (P^ = ' ') do
      Dec(P);
    if P^ <> ' ' then inc(P);
    P^ := #0;
  end;
  StrTrim := Dest;
end;

function StrLTrim(Dest: PAnsiChar): PAnsiChar;
var
  P: PAnsiChar;
begin
  P := Dest;
  while (P^ = ' ') and (P^ <> #0) do
    inc(P);
  StrLTrim := StrCopy(Dest, P);
end;

procedure TOptionsDlg.FillSpaceCombo(ACombo: TComboBox);
begin
  with ACombo.Items do
  begin
    Clear;
    Add(str_None);
    Add(str_Before);
    Add(str_after);
    Add(str_BeforeAfter);
  end;
end;

function TOptionsDlg.GetSpaceItem(I: Integer): TSpaceBefore;
var
  S: string;
begin
  S := SpaceGrid.Cells[2, I];
  Result := spNone;
  if S = str_Before then
    Result := spBefore
  else if S = str_after then
    Result := spAfter
  else if S = str_BeforeAfter then
    Result := spBoth;
end;

procedure TOptionsDlg.UpdateFormatter;
begin
  with Formatter do
    if Formatter <> nil then
    begin
      SpacePerIndent := SpacePerIndentUpDown.Position;
      CapFileName := PAnsiChar(AnsiString(CapFileNameEdit.Text));
      ShortCutText := ShortCutEdit.Text;
      indentBegin := IndentBeginCheck.Checked;
      IndentComments := IndentCommentsCheck.Checked;
      IndentCompDirectives := IndentCompDirectivesCheck.Checked;
      IndentTry := IndentTryCheck.Checked;
      IndentTryElse := IndentTryElseCheck.Checked;
      IndentCaseElse := IndentCaseElseCheck.Checked;
      UpperCompDirectives := UpperCompDirectivesCheck.Checked;
      UpperNumbers := UpperNumbersCheck.Checked;
      ReservedCase := TCase(ReservedCaseCombo.ItemIndex);
      StandDirectivesCase := TCase(StandDirectivesCombo.ItemIndex);
      BlankProc := BlankProcCheck.Checked;
      BlankSubProc := BlankSubProcCheck.Checked;
      RemoveDoubleBlank := RemoveDoubleBlankCheck.Checked;
      WrapLines := WrapLinesCheck.Checked;
      WrapPosition := WrapPositionUpDown.Position;
      AlignComments := AlignCommentsCheck.Checked;
      AlignCommentPos := AlignCommentPosUpDown.Position;
      AlignVar := AlignVarCheck.Checked;
      AlignVarPos := AlignVarPosUpDown.Position;
      SpaceEqualOper := GetSpaceItem(1);
      SpaceOperators := GetSpaceItem(2);
      SpaceColon := GetSpaceItem(3);
      SpaceSemiColon := GetSpaceItem(4);
      SpaceComma := GetSpaceItem(5);
      SpaceLeftBr := GetSpaceItem(6);
      SpaceRightBr := GetSpaceItem(7);
      SpaceLeftHook := GetSpaceItem(8);
      SpaceRightHook := GetSpaceItem(9);
      FeedAfterThen := FeedAfterThenCheck.Checked;
      ExceptSingle := ExceptSingleCheck.Checked;
      FeedEachUnit := FeedEachUnitCheck.Checked;
      NoFeedBeforeThen := NoFeedBeforeThenCheck.Checked;
      FeedAfterVar := FeedAfterVarCheck.Checked;
      FeedElseIf := FeedElseIfCheck.Checked;
      NoIndentElseIf := NoIndentElseIfCheck.Checked;
      FeedBeforeEnd := FeedBeforeEndCheck.Checked;
      FeedAfterSemiColon := FeedAfterSemiColonCheck.Checked;
      FillNewWords := TFillMode(FillNewWordsCombo.ItemIndex);
      StrLCopy(StartCommentOut, PAnsiChar(AnsiString(StartCommentOutEdit.Text)), 20);
      StrLTrim(StrTrim(StartCommentOut));
      StrLCopy(EndCommentOut, PAnsiChar(AnsiString(EndCommentOutEdit.Text)), 20);
      StrLTrim(StrTrim(EndCommentOut));
      FeedRoundBegin := TFeedBegin(FeedRoundBeginCombo.ItemIndex);
    end;
end;

procedure TOptionsDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult <> mrCancel then
  begin
    UpdateFormatter;
    Formatter.Config(False);
  end;
end;

procedure TOptionsDlg.AddSpaceRow(RowNo: Integer; StrCol1, StrCol2: string;
  Space: TSpaceBefore);
begin
  with SpaceGrid do
  begin
    Cells[0, RowNo] := StrCol1;
    Cells[1, RowNo] := StrCol2;
    case Space of
      spNone: Cells[2, RowNo] := str_None;
      spBefore: Cells[2, RowNo] := str_Before;
      spAfter: Cells[2, RowNo] := str_after;
      spBoth: Cells[2, RowNo] := str_BeforeAfter;
    end;
  end;
end;

procedure TOptionsDlg.FillPreview;
var
  S: string;
begin
  S := Formatter.RootDir + 'preview.pas';
  if FileExists(S) then
    with BeforeMemo do
    begin
      Clear;
      Lines.LoadFromFile(S);
    end
end;

procedure TOptionsDlg.UpdateButtons;
begin
  with Formatter do
    if Formatter <> nil then
    begin
      CapFileNameEdit.Text := string(CapFileName);
      ShortCutEdit.Text := ShortCutText;
      SpacePerIndentUpDown.Position := SpacePerIndent;
      IndentBeginCheck.Checked := indentBegin;
      IndentCommentsCheck.Checked := IndentComments;
      IndentCompDirectivesCheck.Checked := IndentCompDirectives;
      IndentTryCheck.Checked := IndentTry;
      IndentTryElseCheck.Checked := IndentTryElse;
      IndentCaseElseCheck.Checked := IndentCaseElse;
      UpperCompDirectivesCheck.Checked := UpperCompDirectives;
      UpperNumbersCheck.Checked := UpperNumbers;
      ReservedCaseCombo.ItemIndex := Byte(ReservedCase);
      StandDirectivesCombo.ItemIndex := Byte(StandDirectivesCase);
      BlankProcCheck.Checked := BlankProc;
      BlankSubProcCheck.Checked := BlankSubProc;
      RemoveDoubleBlankCheck.Checked := RemoveDoubleBlank;
      with SpaceGrid do
      begin
        RowCount := 10;
        Cells[0, 0] := '描述';
        Cells[1, 0] := '操作符';
        Cells[2, 0] := '空格';
        AddSpaceRow(1, '赋值符', ':=', SpaceEqualOper);
        AddSpaceRow(2, '运算符', '< > = + - / * etc.', SpaceOperators);
        AddSpaceRow(3, '冒号', ':', SpaceColon);
        AddSpaceRow(4, '分号', ';', SpaceSemiColon);
        AddSpaceRow(5, '豆号', ',', SpaceComma);
        AddSpaceRow(6, '左括号', '(', SpaceLeftBr);
        AddSpaceRow(7, '右括号', ')', SpaceRightBr);
        AddSpaceRow(8, '左中括号', '[', SpaceLeftHook);
        AddSpaceRow(9, '右中括号', ']', SpaceRightHook);
      end;
      FeedAfterSemiColonCheck.Checked := FeedAfterSemiColon;
      FeedEachUnitCheck.Checked := FeedEachUnit;
      FeedAfterThenCheck.Checked := FeedAfterThen;
      ExceptSingleCheck.Checked := ExceptSingle;
      NoFeedBeforeThenCheck.Checked := NoFeedBeforeThen;
      FeedAfterVarCheck.Checked := FeedAfterVar;
      FeedElseIfCheck.Checked := FeedElseIf;
      NoIndentElseIfCheck.Checked := NoIndentElseIf;
      FeedBeforeEndCheck.Checked := FeedBeforeEnd;
      WrapLinesCheck.Checked := WrapLines;
      WrapPositionUpDown.Position := WrapPosition;
      AlignCommentsCheck.Checked := AlignComments;
      AlignCommentPosUpDown.Position := AlignCommentPos;
      AlignVarCheck.Checked := AlignVar;
      AlignVarPosUpDown.Position := AlignVarPos;
      FillNewWordsCombo.ItemIndex := Byte(FillNewWords);
      StartCommentOutEdit.Text := string(StartCommentOut);
      EndCommentOutEdit.Text := string(EndCommentOut);
      FeedRoundBeginCombo.ItemIndex := Integer(FeedRoundBegin);
      SpacePerIndentUpDown.Associate := SpacePerIndentEdit;
      WrapPositionUpDown.Associate := WrapPositionEdit;
      AlignCommentPosUpDown.Associate := AlignCommentPosEdit;
      AlignVarPosUpDown.Associate := AlignVarPosEdit;
      EditButton.Enabled := CapFileNameEdit.Text <> '';
      FeedAfterThenCheckClick(nil)
    end;
end;

function TOptionsDlg.GetFormatterSettings(OldCapFileName: PAnsiChar): TSettings;
begin
  with Formatter do
  begin
    Result := Formatter.Settings;
    if CapFileName <> nil then
      StrCopy(OldCapFileName, Formatter.CapFileName)
    else
      OldCapFileName^ := #0;
  end;
end;

procedure TOptionsDlg.SetFormatterSettings(OldSettings: TSettings; OldCapFileName: PAnsiChar);
begin
  with Formatter do
  begin
    Settings := OldSettings;
    if OldCapFileName^ <> #0 then
      CapFileName := OldCapFileName
    else
      CapFileName := nil;
    CapFileNameEdit.Text := string(OldCapFileName);
  end;
end;

procedure TOptionsDlg.SetDefault(BorlandStyle: Boolean);
var
  OldSettings: TSettings;
  OldCapFileName: array[0..MAX_PATH] of AnsiChar;
begin
  with Formatter do
    if Formatter <> nil then
    begin
      SpacingCombo.Hide;
      OldSettings := GetFormatterSettings(OldCapFileName);
      if BorlandStyle then
        Formatter.SetBorland
      else
        Formatter.SetDefault;
      UpdateButtons;
      SetFormatterSettings(OldSettings, OldCapFileName);
      if PageControl1.ActivePage = TabSheet3 then
        TabSheet3Show(nil);
    end;
end;

procedure TOptionsDlg.DefaultBtnClick(Sender: TObject);
begin
  SetDefault(False);
end;

procedure TOptionsDlg.CapFileNameButtonClick(Sender: TObject);
begin
  OpenDialog.FileName := CapFileNameEdit.Text;
  if OpenDialog.Execute then
    CapFileNameEdit.Text := OpenDialog.FileName;
  CapFileNameEditChange(Sender);
end;

procedure TOptionsDlg.HelpButtonClick(Sender: TObject);
begin
  if FileExists(HelpFile) then
  begin
    WinHelp(0, PChar(HelpFile), HELP_KEY,
      Integer(PageControl1.ActivePage.Caption));
  end;
end;

procedure TOptionsDlg.ShortCutEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  AShortCut: TShortCut;
  S: string;
begin
  if (Shift = [ssAlt, ssCtrl]) or (Shift = [ssCtrl])
    or (Shift = [ssAlt]) or (Key in [VK_F1..vk_f12]) then
  begin
    AShortCut := Menus.ShortCut(Key, Shift);
    S := ShortCutToText(AShortCut);
    if Length(S) > 1 then
      ShortCutEdit.Text := S;
  end;
end;

procedure TOptionsDlg.EditButtonClick(Sender: TObject);
var
  FileEditDlg: TFileEditDlg;
  Cur: TCursor;
begin
  Cur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if Formatter <> nil then
    with Formatter do
      if FillNewWords in [fmAddNewWord, fmAddUse, fmAddUseExcept] then
        SaveCapFile(PAnsiChar(AnsiString(CapFileNameEdit.Text)));
  FileEditDlg := TFileEditDlg.Create(Self);
  try
    FileEditDlg.LoadFile(CapFileNameEdit.Text);
    Screen.Cursor := Cur;
    FileEditDlg.ShowModal;
    if FileEditDlg.IsChanged then Formatter.LoadCapFile(PAnsiChar(AnsiString(CapFileNameEdit.Text)));
  finally
    Screen.Cursor := Cur;
    FileEditDlg.Free;
  end;
end;

procedure TOptionsDlg.CapFileNameEditChange(Sender: TObject);
begin
  EditButton.Enabled := CapFileNameEdit.Text <> '';
end;

procedure TOptionsDlg.TabSheet3Show(Sender: TObject);
var
  OldSettings: TSettings;
  OldCapFileName: array[0..MAX_PATH] of AnsiChar;
begin
  with Formatter do
    if Formatter <> nil then
    begin
      OldSettings := GetFormatterSettings(OldCapFileName);
      UpdateFormatter;
      Formatter.Clear;
      Formatter.Text := PAnsiChar(AnsiString(BeforeMemo.Text));
      if Formatter.Parse then
      begin
        AfterMemo.Lines.BeginUpdate;
        AfterMemo.Lines.Clear;
        AfterMemo.Text := Formatter.Text;
        AfterMemo.Lines.EndUpdate;
        BeforeMemoClick(nil);
      end;
      SetFormatterSettings(OldSettings, OldCapFileName);
    end;
end;

procedure TOptionsDlg.BeforeMemoClick(Sender: TObject);
var
  CurLine2, CurLine: Integer;
begin
  CurLine := SendMessage(BeforeMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  CurLine2 := SendMessage(AfterMemo.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  SendMessage(AfterMemo.Handle, EM_LINESCROLL, 0, CurLine - CurLine2);
  AfterMemo.SelStart := BeforeMemo.SelStart;
end;

procedure TOptionsDlg.BeforeMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  BeforeMemoClick(nil);
end;

procedure TOptionsDlg.BeforeMemoKeyPress(Sender: TObject; var Key: Char);
begin
  BeforeMemoClick(nil);
end;

procedure TOptionsDlg.BeforeMemoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BeforeMemoClick(nil);
end;

procedure TOptionsDlg.SpacingComboExit(Sender: TObject);
begin
  with SpaceGrid do
    Cells[Col, Row] := SpacingCombo.Items[SpacingCombo.ItemIndex];
end;

procedure TOptionsDlg.SpaceGridClick(Sender: TObject);
var
  Rect: TRect;
  Pnt: TPoint;
begin
  with SpaceGrid do
    if Col = 2 then
    begin
      with SpacingCombo do
        ItemIndex := Items.IndexOf(Cells[2, Row]);
      Rect := CellRect(2, Row);
      with Rect do
      begin
        Pnt := SpacingCombo.Parent.ScreenToClient((SpaceGrid.ClientToScreen(Point(Left,
          Top))));
        SpacingCombo.SetBounds(Pnt.X, Pnt.Y, Right - Left, Bottom - Top);
        SpacingCombo.Show;
      end;
    end else SpacingCombo.Hide;
end;

procedure TOptionsDlg.SpaceGridTopLeftChanged(Sender: TObject);
begin
  SpacingCombo.Hide;
end;

procedure TOptionsDlg.FormShow(Sender: TObject);
begin
  FillSpaceCombo(SpacingCombo);
  UpdateButtons;
  FillPreview;
  PageControl1.ActivePage := Indent;
  CapFileNameButton.Height := CapFileNameEdit.Height;
  CapFileNameButton.Left := CapFileNameEdit.Left + CapFileNameEdit.Width + 3;
  CapFileNameButton.Top := CapFileNameEdit.Top;
end;

procedure TOptionsDlg.BorlandButtonClick(Sender: TObject);
begin
  SetDefault(True);
end;

procedure TOptionsDlg.FeedAfterThenCheckClick(Sender: TObject);
begin
  ExceptSingleCheck.Enabled := FeedAfterThenCheck.Checked;
end;

procedure TOptionsDlg.SpacesShow(Sender: TObject);
begin
  SpacingCombo.Hide;
end;

end.

