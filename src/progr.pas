unit Progr;

interface

uses Windows, SysUtils, Classes, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls;

type
  TProgressDlg = class(TForm)
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    FileLabel: TLabel;
    ProgressBar: TProgressBar;
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure SetFileName(AFileName: string);
    { Public declarations }
  end;

var
  ProgressDlg: TProgressDlg;

implementation

{$R *.DFM}

function StrShortenFileName(Dest, Source, CurPath: PAnsiChar; MaxLen: Integer):
  PAnsiChar;
var
  Buff: array[0..MAX_PATH] of AnsiChar;
  LenCurPath: Integer;
  P, P2: PAnsiChar;
begin
  Result := Dest;
  if CurPath = nil then CurPath := StrCopy(Buff, PAnsiChar(AnsiString(GetCurrentDir)));
  LenCurPath := StrLen(CurPath);
  if (StrLIComp(Source, CurPath, LenCurPath) = 0) and (StrScan(Source +
    LenCurPath + 1, '\') = nil) then
    StrCopy(Dest, Source + LenCurPath + 1)
  else
    StrCopy(Dest, Source);
  if Integer(StrLen(Dest)) > MaxLen then
  begin
    P := StrScan(Dest, '\');
    P2 := StrRScan(Dest, '\');
    if (P <> nil) and (P2 <> nil) and (P < P2) then
      StrCat(StrCopy(P + 1, '...'), P2);
  end;
  if Integer(StrLen(Dest)) > MaxLen then StrCopy(PAnsiChar(@Dest[MaxLen]), '...');
end;

procedure TProgressDlg.SetFileName(AFileName: string);
var
  Buff: array[0..255] of AnsiChar;
begin
  FileLabel.Caption := string(StrShortenFileName(Buff,
    PAnsiChar(AnsiString(AFileName)), nil, 40));
  Application.ProcessMessages;
end;

procedure TProgressDlg.CancelBtnClick(Sender: TObject);
begin
  Hide;
end;

end.
