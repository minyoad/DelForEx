unit Setup1;

interface

procedure Run;
function GetDelForName: string;

implementation

uses Windows, SysUtils, Registry;

var
  Reg: TRegistry;

const
  DelphiRoot = '\Software\';
  Delphi20 = 'Borland\Delphi\2.0';
  Delphi30 = 'Borland\Delphi\3.0';
  Delphi40 = 'Borland\Delphi\4.0';
  Delphi50 = 'Borland\Delphi\5.0';
  Delphi60 = 'Borland\Delphi\6.0';
  Delphi70 = 'Borland\Delphi\7.0';
  Delphi90 = 'Borland\BDS\3.0';
  Delphi100 = 'Borland\BDS\4.0';
  Delphi110 = 'Borland\BDS\5.0';
  Delphi120 = 'CodeGear\BDS\6.0';
  Delphi140 = 'CodeGear\BDS\7.0';
  Delphi150 = 'Embarcadero\BDS\8.0';
  Delphi160 = 'Embarcadero\BDS\9.0';
  Delphi170 = 'Embarcadero\BDS\10.0';

  Experts = '\Experts';

  OldTheExpert30 = 'DelForExp'; //Old name
  OldTheExpert20 = 'DelForEx'; //Old name

  TheExpert20 = 'DelForEx2';
  TheExpert30 = 'DelForEx3';
  TheExpert40 = 'DelForEx4';
  TheExpert50 = 'DelForEx5';
  TheExpert60 = 'DelForEx6'; //future
  TheExpert70 = 'DelForEx7'; //future
  TheExpert90 = 'DelForEx9';
  TheExpert100 = 'DelForEx10';
  TheExpert110 = 'DelForEx11';
  TheExpert120 = 'DelForEx12';
  TheExpert140 = 'DelForEx14';
  TheExpert150 = 'DelForEx15';
  TheExpert160 = 'DelForEx16';
  TheExpert170 = 'DelForEx17';

  Dll = '.dll';

const
{$IFDEF Ver240}DelphiVersion: Byte = 17;
{$ELSE}
{$IFDEF Ver230}DelphiVersion: Byte = 16;
{$ELSE}
{$IFDEF Ver220}DelphiVersion: Byte = 15;
{$ELSE}
{$IFDEF Ver210}DelphiVersion: Byte = 14;
{$ELSE}
{$IFDEF Ver200}DelphiVersion: Byte = 12;
{$ELSE}
{$IFDEF Ver190}DelphiVersion: Byte = 11;
{$ELSE}
{$IFDEF Ver185}DelphiVersion: Byte = 11;
{$ELSE}
{$IFDEF Ver180}DelphiVersion: Byte = 10;
{$ELSE}
{$IFDEF Ver170}DelphiVersion: Byte = 9;
{$ELSE}
{$IFDEF Ver150}DelphiVersion: Byte = 7;
{$ELSE}
{$IFDEF Ver140}DelphiVersion: Byte = 6;
{$ELSE}
{$IFDEF Ver130}DelphiVersion: Byte = 5;
{$ELSE}
{$IFDEF Ver120}DelphiVersion: Byte = 4;
{$ELSE}
{$IFDEF Ver100}DelphiVersion: Byte = 3;
{$ELSE}
{$IFDEF Ver90}DelphiVersion: Byte = 2;
{$ELSE}
  DelphiVersion: Byte = 0;
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

var
  Path: string;

procedure ShowMessage(const S: string);
const
  Cap: PChar = '安装 Delphi 源码格式化工具';
begin
  MessageBox(0, PChar(S), Cap, mb_Ok);
end;


procedure ShowOKMessage(ExpName, Delphi: string);
begin
  ShowMessage(ExpName + ' 已成功地安装到 ' + Delphi + '，' + #13#10 +
    '可在 ' + Delphi + ' 的 "Tools" 菜单下可找到运行行菜单，' + #13#10 +
    '再次运行 SetupEx.exe 可以反安装这个程序。');
end;

procedure Run;
var
  S, ExpName, PathName: string;
  HasDelphi2, HasDelphi3, HasDelphi4, HasDelphi5,
    HasDelphi6, HasDelphi7, HasDelphi9, HasDelphi10, HasDelphi11, HasDelphi12,

  HasDelphi14, HasDelphi15,HasDelphi16,HasDelphi17, HasPrevious, InstalledSomeThing: Boolean;

  procedure CheckVersion(const Dx: string; const HasDelphix: Boolean;
    const Delphixx, TheExpertxx: string);
  begin
    ExpName := TheExpertxx;
    PathName := Path + TheExpertxx + Dll;
    if HasDelphix and FileExists(PathName) then
      with Reg do
      begin
        OpenKey(DelphiRoot + Delphixx + Experts, True);
        if HasPrevious then
        begin
          S := ReadString(ExpName);
          if (S <> '') then
          begin
            if not InstalledSomeThing then
            begin
              DeleteValue(ExpName);
              ShowMessage(ExpName + ' 已从 ' + Dx + ' 中移除。');
            end
          end
          else
          begin
            if InstalledSomeThing then
            begin
              WriteString(ExpName, PathName);
              ShowOKMessage(ExpName, Dx);
            end;
          end;
        end
        else
        begin
          S := ReadString(ExpName);
          if S <> '' then
          begin
            DeleteValue(ExpName);
            ShowMessage(ExpName + ' 已从 ' + Dx + ' 中移除。');
          end
          else
          begin
            InstalledSomeThing := True;
            WriteString(ExpName, PathName);
            ShowOKMessage(ExpName, Dx);
          end;
        end;
        HasPrevious := True;
      end;
  end;
begin
  Reg := TRegistry.Create;
  try
    Path := ExtractFilePath(ParamStr(0));
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      HasDelphi2 := OpenKey(DelphiRoot + Delphi20, False);
      HasDelphi3 := OpenKey(DelphiRoot + Delphi30, False);
      HasDelphi4 := OpenKey(DelphiRoot + Delphi40, False);
      HasDelphi5 := OpenKey(DelphiRoot + Delphi50, False);
      HasDelphi6 := OpenKey(DelphiRoot + Delphi60, False);
      HasDelphi7 := OpenKey(DelphiRoot + Delphi70, False);
      HasDelphi9 := OpenKey(DelphiRoot + Delphi90, False);
      HasDelphi10 := OpenKey(DelphiRoot + Delphi100, False);
      HasDelphi11 := OpenKey(DelphiRoot + Delphi110, False);
      HasDelphi12 := OpenKey(DelphiRoot + Delphi120, False);
      HasDelphi14 := OpenKey(DelphiRoot + Delphi140, False);
      HasDelphi15 := OpenKey(DelphiRoot + Delphi150, False);
      HasDelphi16 := OpenKey(DelphiRoot + Delphi160, False);
      HasDelphi17 := OpenKey(DelphiRoot + Delphi170, False);
      if not (HasDelphi2 or HasDelphi3 or HasDelphi4 or HasDelphi5
        or HasDelphi6 or HasDelphi7 or HasDelphi9 or HasDelphi10
        or HasDelphi11 or HasDelphi12 or HasDelphi14 or HasDelphi15
        or HasDelphi16 or HasDelphi17) then
        raise ERegistryException.Create('Delphi 2, 3, 4, 5, 6, 7, 9, 10, 11 ,12,14或 15 未安装。')   
      else
      try
        InstalledSomeThing := False;
        HasPrevious := False;
//        CheckVersion('Delphi 17(XE3)', HasDelphi17, Delphi170, TheExpert170);
        CheckVersion('Delphi 16(XE2)', HasDelphi16, Delphi160, TheExpert160);

        CheckVersion('Delphi 15(XE)', HasDelphi15, Delphi150, TheExpert150);
        CheckVersion('Delphi 14(2010)', HasDelphi14, Delphi140, TheExpert140);
        CheckVersion('Delphi 12(2009)', HasDelphi12, Delphi120, TheExpert120);
        CheckVersion('Delphi 11(2007)', HasDelphi11, Delphi110, TheExpert110);
        CheckVersion('Delphi 10(2006)', HasDelphi10, Delphi100, TheExpert100);
        CheckVersion('Delphi 9(2005)', HasDelphi9, Delphi90, TheExpert90);
        CheckVersion('Delphi 7', HasDelphi7, Delphi70, TheExpert70);
        CheckVersion('Delphi 6', HasDelphi6, Delphi60, TheExpert60);
        CheckVersion('Delphi 5', HasDelphi5, Delphi50, TheExpert50);
        CheckVersion('Delphi 4', HasDelphi4, Delphi40, TheExpert40);
        CheckVersion('Delphi 3', HasDelphi3, Delphi30, OldTheExpert30);
        CheckVersion('Delphi 3', HasDelphi3, Delphi30, TheExpert30);
        CheckVersion('Delphi 2', HasDelphi2, Delphi20, OldTheExpert20);
        CheckVersion('Delphi 2', HasDelphi2, Delphi20, TheExpert20);
      finally
        CloseKey;
      end;
    end
  finally
    Reg.Free;
  end;
end;

function GetDelForName: string;
begin
  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_CURRENT_USER;
      Result := '';
      case DelphiVersion of
        15: if OpenKey(DelphiRoot + Delphi150 + Experts, False) then
            Result := ReadString(TheExpert150);
        14: if OpenKey(DelphiRoot + Delphi140 + Experts, False) then
            Result := ReadString(TheExpert140);
        12: if OpenKey(DelphiRoot + Delphi120 + Experts, False) then
            Result := ReadString(TheExpert120);
        11: if OpenKey(DelphiRoot + Delphi110 + Experts, False) then
            Result := ReadString(TheExpert110);
        10: if OpenKey(DelphiRoot + Delphi100 + Experts, False) then
            Result := ReadString(TheExpert100);
        9: if OpenKey(DelphiRoot + Delphi90 + Experts, False) then
            Result := ReadString(TheExpert90);
        7: if OpenKey(DelphiRoot + Delphi70 + Experts, False) then
            Result := ReadString(TheExpert70);
        6: if OpenKey(DelphiRoot + Delphi60 + Experts, False) then
            Result := ReadString(TheExpert60);
        5: if OpenKey(DelphiRoot + Delphi50 + Experts, False) then
            Result := ReadString(TheExpert50);
        4: if OpenKey(DelphiRoot + Delphi40 + Experts, False) then
            Result := ReadString(TheExpert40);
        3: if OpenKey(DelphiRoot + Delphi30 + Experts, False) then
            Result := ReadString(TheExpert30);
        2: if OpenKey(DelphiRoot + Delphi20 + Experts, False) then
            Result := ReadString(TheExpert20);
      end;
      CloseKey;
    end
  finally
    Reg.Free;
  end;
end;

initialization
end.

