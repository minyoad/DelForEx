unit EditFile;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons;

type
  TAction = (acUpperCase, acLowerCase, acFirstUp, acFirstLow, acCommentOut);

  TFileEditDlg = class(TForm)
    ItemsList: TListBox;
    SearchEdit: TEdit;
    Label1: TLabel;
    ChangeEdit: TEdit;
    FileNameLabel: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    HelpBtn: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure ItemsListClick(Sender: TObject);
    procedure ChangeEditChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    FisChanged: Boolean;
    procedure ChangeSelected(AnAction: TAction);
  public
    procedure LoadFile(AFileName: string);
    property IsChanged: Boolean read FisChanged write FisChanged;
  end;

implementation

uses OptDlg;

{$R *.DFM}

procedure TFileEditDlg.FormShow(Sender: TObject);
begin
  IsChanged := False;
end;

procedure TFileEditDlg.LoadFile(AFileName: string);
var
  TheFile: TextFile;
  S: string;
begin
  FileNameLabel.Caption := AFileName;
  if FileExists(AFileName) then
  begin
    AssignFile(TheFile, AFileName);
    Reset(TheFile);
    try
      SearchEdit.Text := '';
      ChangeEdit.Text := '';
      ItemsList.Clear;
      ItemsList.Sorted := False;
      while not Eof(TheFile) do
      begin
        Readln(TheFile, S);
        if S <> '' then ItemsList.Items.Add(S);
      end;
    finally
      CloseFile(TheFile);
    end;
    ItemsList.Sorted := True;
  end;
end;

procedure TFileEditDlg.ItemsListClick(Sender: TObject);
begin
  with ItemsList do
    if SelCount <= 1 then
    begin
      SearchEdit.Enabled := True;
      ChangeEdit.Enabled := True;
      SearchEdit.OnChange := nil;
      ChangeEdit.OnChange := nil;
      ChangeEdit.Text := Items[ItemIndex];
      SearchEdit.Text := ChangeEdit.Text;
      SearchEdit.OnChange := SearchEditChange;
      ChangeEdit.OnChange := ChangeEditChange;
    end
    else
    begin
      SearchEdit.Enabled := False;
      ChangeEdit.Enabled := False;
    end

end;

procedure TFileEditDlg.ChangeEditChange(Sender: TObject);
begin
  with ItemsList do
  begin
    if ItemIndex < 0 then
    begin
      Items.Add(ChangeEdit.Text);
      IsChanged := True;
    end
    else if StrIComp(PAnsiChar(AnsiString(Items[ItemIndex])),
      PAnsiChar(AnsiString(ChangeEdit.Text))) <> 0 then
    begin
      IsChanged := True;
      Items.Delete(ItemIndex);
      Items.Add(ChangeEdit.Text);
    end
    else if Items[ItemIndex] <> ChangeEdit.Text then
    begin
      Items[ItemIndex] := ChangeEdit.Text;
      IsChanged := True;
    end;
    ItemIndex := Items.IndexOf(ChangeEdit.Text);
  end;
end;

procedure TFileEditDlg.ChangeSelected(AnAction: TAction);
var
  I: Integer;

  function Change(S: string): string;
  var
    Ch: Char;
  begin
    case AnAction of
      acUpperCase: Result := UpperCase(S);
      acLowerCase: Result := LowerCase(S);
      acFirstUp:
        begin
          Result := S;
          Result[1] := UpCase(Result[1]);
        end;
      acFirstLow:
        begin
          Result := S;
          Ch := Result[1];
          if (Ch >= 'A') and (Ch <= 'Z') then
            inc(Result[1], 32);
        end;
      acCommentOut:
        if S[1] = '*' then
          Result := Copy(S, 2, Length(S))
        else
          Result := '*' + S;
    end;
    IsChanged := IsChanged or (S <> Result);
  end;
begin
  if ChangeEdit.Enabled = True then
    with ChangeEdit do
    begin
      if SelLength = 0 then
        ChangeEdit.Text := Change(ChangeEdit.Text)
      else
        ChangeEdit.SelText := Change(ChangeEdit.SelText);
    end
  else
  begin
    with ItemsList, Items do
      for I := 0 to Count - 1 do
      begin
        if Selected[I] then
        begin
          Items[I] := Change(Items[I]);
          Selected[I] := True;
        end;
      end;
    if (AnAction = acCommentOut) then
    begin
      ItemsList.Sorted := False;
      ItemsList.Sorted := True; //Combination forces reorder
    end;
  end;
end;

procedure TFileEditDlg.Button1Click(Sender: TObject);
begin
  ChangeSelected(acUpperCase);
end;

procedure TFileEditDlg.Button2Click(Sender: TObject);
begin
  ChangeSelected(acLowerCase);
end;

procedure TFileEditDlg.Button3Click(Sender: TObject);
begin
  ChangeSelected(acFirstUp);
end;

procedure TFileEditDlg.SearchEditChange(Sender: TObject);
var
  SearchEditText: string;
  I, J, Lasti: Integer;
begin
  SearchEditText := SearchEdit.Text;
  with ItemsList, Items do
  begin
    J := 1;
    Lasti := 0;
    for I := 0 to Count - 1 do
    begin
      if (StrLIComp(PAnsiChar(AnsiString(SearchEditText)), PAnsiChar(AnsiString(Items[I])), J) = 0) then
      begin
        inc(J);
        Lasti := I;
      end;
      ItemsList.Selected[I] := False;
    end;
    ItemsList.OnClick := nil;
    ItemsList.Selected[Lasti] := True;
    TopIndex := Lasti - 1;
    ItemsList.OnClick := ItemsListClick;
  end;
end;

procedure TFileEditDlg.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  TheFile: TextFile;
  I: Integer;
begin
  case ModalResult of
    mrCancel:
      if IsChanged and (MessageBox(Handle, '关闭而不保存改变吗？', '自定义', MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = ID_No) then
        Action := caNone;
    mrOk:
      begin
        if IsChanged and (MessageBox(Handle, '确认保存设置吗？', '自定义', MB_ICONQUESTION or MB_YESNO) = ID_YES) then
        begin
          AssignFile(TheFile, FileNameLabel.Caption);
          Rewrite(TheFile);
          try
            for I := 0 to ItemsList.Items.Count - 1 do
              Writeln(TheFile, ItemsList.Items[I]);
          finally
            CloseFile(TheFile);
          end;
        end;
      end;
  end;
end;

procedure TFileEditDlg.Button4Click(Sender: TObject);
begin
  ChangeSelected(acFirstLow);
end;

procedure TFileEditDlg.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with ItemsList do
    case Key of
      VK_UP:
        begin
          Selected[ItemIndex] := False;
          ItemIndex := ItemIndex - 1;
          Selected[ItemIndex] := True;
          SetFocus;
        end;
      VK_DOWN:
        begin
          Selected[ItemIndex] := False;
          ItemIndex := ItemIndex + 1;
          Selected[ItemIndex] := True;
          SetFocus;
        end;
    end;
end;

procedure TFileEditDlg.Button5Click(Sender: TObject);
var
  I: Integer;
begin
  with ItemsList, Items do
  begin
    IsChanged := True;
    Add('<New Identifier>');
    ItemIndex := IndexOf('<New Identifier>');
    for I := 0 to Count - 1 do
      Selected[I] := False;
    Selected[ItemIndex] := True;
    ItemsListClick(nil);
  end;
end;

procedure TFileEditDlg.Button7Click(Sender: TObject);
begin
  ChangeSelected(acCommentOut);
end;

procedure TFileEditDlg.Button6Click(Sender: TObject);
var
  I: Integer;
begin
  with ItemsList, Items do
    for I := Count - 1 downto 0 do
      if Selected[I] then Delete(I);
end;

procedure TFileEditDlg.HelpBtnClick(Sender: TObject);
var
  S: string;
begin
  with TOptionsDlg(Owner) do
    if FileExists(HelpFile) then
    begin
      S := 'Edit file dialog';
      WinHelp(0, PChar(HelpFile), HELP_KEY,
        Integer(S));
    end;
end;

end.

