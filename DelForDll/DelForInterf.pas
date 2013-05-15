unit DelForInterf;

interface
uses Classes, DelForTypes;

procedure Formatter_Destroy;
procedure Formatter_Create;
procedure Formatter_LoadFromFile(AFileName: PChar; ASettings: PSettings;
SizeOfSettings: Integer);
procedure Formatter_LoadFromList(AList: TStringList; ASettings: PSettings;
SizeOfSettings: Integer);
function Formatter_Parse(ASettings: PSettings; SizeOfSettings: Integer): Boolean;
procedure Formatter_Clear;
procedure Formatter_WriteToFile(AFileName: PChar);
function Formatter_GetTextStr: PChar;
procedure Formatter_SetTextStr(AText: PChar);
procedure Formatter_SetOnProgress(AOnProgress: TProgressEvent);
procedure Formatter_LoadCapFile(ACapFile: PChar);
procedure Formatter_SaveCapFile(ACapFile: PChar);
function Formatter_Version: Integer;

implementation
uses DelforEngine;

procedure Formatter_Destroy;
begin
  DelforParser.Free;
end;

procedure Formatter_Create;
begin
  DelforParser := TDelforParser.Create;
end;

procedure CopySettings(ASettings: PSettings;SizeOfSettings: Integer);
var
  TheSettings: TSettings;
begin
  {Some measures to improve future compatibility}
  if SizeOfSettings < SizeOf(TSettings) then
  begin
    Move(ASettings^, TheSettings, SizeOfSettings);
    DelforParser.Settings := TheSettings;
  end
  else
    DelforParser.Settings := ASettings^;
end;

procedure Formatter_LoadFromFile(AFileName: PChar; ASettings: PSettings;
SizeOfSettings: Integer);
begin
  CopySettings(ASettings,SizeOFSettings);
  DelforParser.LoadFromFile(AFileName);
end;

procedure Formatter_LoadFromList(AList: TStringList;ASettings: PSettings;
SizeOfSettings: Integer);
begin
  CopySettings(ASettings,SizeOFSettings);
  DelforParser.LoadFromList(AList);
end;


function Formatter_Parse(ASettings: PSettings; SizeOfSettings: Integer): Boolean;
begin
  CopySettings(ASettings,SizeOFSettings);
  Result := DelforParser.Parse;
end;

procedure Formatter_Clear;
begin
  DelforParser.Clear;
end;

procedure Formatter_WriteToFile(AFileName: PChar);
begin
  DelforParser.WriteToFile(AFileName);
end;

function Formatter_GetTextStr: PChar;
begin
  Result := DelforParser.Text;
end;

procedure Formatter_SetTextStr(AText: PChar);
begin
  DelforParser.Text := AText;
end;

procedure Formatter_SetOnProgress(AOnProgress: TProgressEvent);
begin
  DelforParser.OnProgress := AOnProgress;
end;

procedure Formatter_LoadCapFile(ACapFile: PChar);
begin
  DelforParser.LoadCapFile(ACapFile)
end;

procedure Formatter_SaveCapFile(ACapFile: PChar);
begin
  DelforParser.SaveCapFile(ACapFile)
end;

function Formatter_Version: Integer;
begin
  Result := CurrentDllVersion;
  {Change this version number only if the new version
  is not backwards compatible}
end;
end.

 