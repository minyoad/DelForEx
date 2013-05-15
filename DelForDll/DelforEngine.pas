unit DelforEngine;
interface
uses SysUtils, OObjects, Classes, DelForTypes;

(*
WISHLIST:
- suppress read-only file message
- read capitalization from var const type blocks
- sorting methods
- Is it possible to insert a "user customisable" line or group of lines before each
function/procedure, to allow the developer to comment it. Ex :

{------------Name of the proc------------------------}  (Simple)

{***************************
 * ...Comment ...
 * ...Comment ...
 ***************************/ (A few lines)}

 *)

type

  TPascalWord = class
  private
    procedure SetCase(ACase: TCase); virtual;
    function GetCase: TCase; virtual;
    procedure SetExpression(AExpression: PChar); virtual;
    function GetExpression: PChar; virtual;
  public
    constructor Create;
    function WordType: TWordType; virtual;
    procedure GetLength(var Length: Integer); virtual;
    function Space(SpaceBefore: TSpaceBefore): Boolean; virtual;
    function ReservedType: TReservedType; virtual;
    procedure SetSpace(SpaceBefore: TSpaceBefore; State: Boolean); virtual;
    procedure SetReservedType(AReservedType: TReservedType); virtual;
    function GetEString(Dest: PChar): PChar; virtual; abstract;
    function ChangeComment(commchar: char): boolean;
    property Expression: PChar read GetExpression write SetExpression;
    property ExpressionCase: TCase read GetCase write SetCase;
  end;

  TLineFeed = class(TPascalWord)
  public
    nSpaces: Integer;
    oldnSpaces: Integer;
    wrapped: Boolean;
    constructor Create(AOldnSpaces: Integer);
    procedure SetIndent(n: Integer);
    procedure IncIndent(n: Integer);
    procedure GetLength(var Length: Integer); override;
    function ReservedType: TReservedType; override;
    function GetEString(Dest: PChar): PChar; override;
  end;

  TExpression = class(TPascalWord)
  private
    procedure SetCase(ACase: TCase); override;
    function GetCase: TCase; override;
    procedure SetExpression(AExpression: PChar); override;
  public
    FExpression: PChar;
    FWordType: TWordType;
    FFormatType: Byte;
    FReservedType: TReservedType;
    constructor Create(AType: TWordType; AExpression: PChar);
    procedure CheckReserved;
    procedure SetSpace(SpaceBefore: TSpaceBefore; State: Boolean); override;
    procedure SetReservedType(AReservedType: TReservedType); override;
    function Space(SpaceBefore: TSpaceBefore): Boolean; override;
    function GetEString(Dest: PChar): PChar; override;
    procedure GetLength(var Length: Integer); override;
    function GetExpression: PChar; override;
    function WordType: TWordType; override;
    function ReservedType: TReservedType; override;
    destructor Destroy; override;
  end;

  TAlignExpression = class(TExpression)
  public
    AlignPos: Byte;
    nSpaces: Byte;
    constructor Create(Like: TExpression; Pos: Byte);
    procedure GetLength(var Length: Integer); override;
    function GetEString(Dest: PChar): PChar; override;
  end;
type

  TDelforParser = class(TObject)
  private
    FSettings: TSettings;
    FileText: TOCollection;
    FCurrentText: PChar;
    FCapNames: TKeywordColl;
    nIndent: Integer;
    ProcLevel: Integer;
    ReadingAsm: Boolean;
    AsmComment: TWordType;
    prev: TPascalWord;
    PrevLine: TLineFeed;
    prevType: TWordType;
    FOnProgress: TProgressEvent;
    HasAligned: Boolean;
    LeftPointBracket: Integer;
    procedure SetFillNewWords(AFillNewWords: TFillMode);
    function AlignExpression(I: Integer; aPos: Integer): TPascalWord;
    procedure checkPrintable(P: PChar);
    procedure ReadAsm(var Buff: PChar);
    function ReadHalfComment(Dest: PChar; var Source: PChar): TWordType;
    function ReadWord(Dest: PChar; var Source: PChar): TWordType;
    procedure SetTextStr(AText: PChar);
    function GetTextStr: PChar;
    procedure CheckWrapping;
    function GetString(Dest: PChar; var I: Integer): PChar;
  public
    constructor Create;
    procedure LoadFromFile(AFileName: PChar);
    procedure LoadFromList(AList: TStringList);
    procedure LoadCapFile(ACapFile: PChar);
    procedure SaveCapFile(ACapFile: PChar);
    function Parse: Boolean;
    procedure Clear;
    procedure WriteToFile(AFileName: PChar);
    procedure Add(Buff: PChar);
    destructor Destroy; override;
    property Text: PChar read GetTextStr write SetTextStr;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property CapNames: TKeywordColl read FCapNames write FCapNames;
    property Settings: TSettings read FSettings write FSettings;
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
    property ExceptSingle: Boolean read FSettings.FeedAfterThen write
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
    property StartCommentOut: TCommentArray read FSettings.StartCommentOut
      write FSettings.StartCommentOut;
    property EndCommentOut: TCommentArray read FSettings.EndCommentOut write
      FSettings.EndCommentOut;

  end;
var
  DelforParser: TDelforParser;

implementation

constructor TDelforParser.Create;
begin
  DelforParser := Self;
  CapNames := TKeywordColl.Create(10);
  Clear;
end;

function TDelforParser.AlignExpression(I: Integer; aPos: Integer): TPascalWord;
var
  OldExpr: TExpression;
begin
  HasAligned := True;
  with FileText do
  begin
    OldExpr := TExpression(Items[I]);
    Result := TAlignExpression.Create(OldExpr, aPos);
    Items[I] := Result;
    OldExpr.Free;
  end;
end;

procedure TDelforParser.Clear;
begin
  HasAligned := False;
  LeftPointBracket := 0;
  nIndent := 0;
  ReadingAsm := False;
  PrevLine := nil;
  prev := nil;
  prevType := wtNothing;
  if FileText = nil then
    FileText := TOCollection.Create(500)
  else
    FileText.FreeAll;
end;

procedure TDelforParser.Add(Buff: PChar);
var
  AWord: array[0..Maxline] of Char;
begin
  PrevLine := TLineFeed.Create(0); {New(TLineFeed, Create(-1));}
  FileText.Add(PrevLine);
  if ReadingAsm then
    ReadAsm(Buff);
  while (Buff^ <> #0) do
  begin
    case prevType of
      wtHalfComment, wtHalfStarComment,
        wtHalfOutComment: prevType := ReadHalfComment(AWord, Buff);
    else
      prevType := ReadWord(AWord, Buff);
    end;
    if not (prevType = wtSpaces) then
    begin
      FileText.Add(TExpression.Create(prevType, AWord));
      if ReadingAsm and (Buff^ <> #0) then
        ReadAsm(Buff);
    end
    else if (PrevLine <> nil) and (PrevLine.nSpaces = 0) then
    begin
      PrevLine.nSpaces := StrLen(AWord);
      PrevLine.oldnSpaces := StrLen(AWord);
    end;
  end;
end;

procedure TDelforParser.LoadFromFile(AFileName: PChar);
var
  InFile: TextFile;
  Buff: array[0..Maxline] of Char;
begin
  if Assigned(OnProgress) then
    OnProgress(Self, 0);
  PrevLine := nil;
  ReadingAsm := False;
  AssignFile(InFile, AFileName);
  try
    Reset(InFile);
    try
      while not Eof(InFile) and (FileText.Count < MaxCollectionSize - 100) do
      begin
        Readln(InFile, Buff);
        Add(Buff);
      end;
      if FileText.Count >= MaxCollectionSize - 100 then
        raise Exception.Create('File to large to reformat')
    finally
      CloseFile(InFile);
    end;
  finally
  end;
  if Assigned(OnProgress) then
    OnProgress(Self, 33);
end;

procedure TDelforParser.LoadFromList(AList: TStringList);
var
  Buff: array[0..Maxline] of Char;
  I, k: Integer;
begin
  if Assigned(OnProgress) then
    OnProgress(Self, 0);
  PrevLine := nil;
  k := 0;
  ReadingAsm := False;
  with AList do
    if Count = 0 then
      Self.Add(StrCopy(Buff, ''))
    else
      for I := 0 to Count - 1 do
      begin
        StrCopy(Buff, PChar(Strings[I]));
        Self.Add(Buff);
        if Assigned(OnProgress) then
        begin
          inc(k);
          if k = 20 then
          begin
            k := 0;
            OnProgress(Self, Round(I / Count * 34));
          end;
        end;
      end;
end;

procedure TDelforParser.ReadAsm(var Buff: PChar);
var
  P, P1: PChar;
begin
  P := Buff;
  P1 := Buff;
  while P1^ = ' ' do
    inc(P1);
  repeat
    checkPrintable(P);
    case AsmComment of
      wtHalfComment:
        begin
          if P^ = '}' then
            AsmComment := wtWord;
          inc(P);
        end;
      wtHalfStarComment:
        begin
          if strLComp(P, '*)', 2) = 0 then
          begin
            AsmComment := wtWord;
            inc(P);
          end;
          inc(P);
        end;
    else
      if (StrLIComp(P, 'end', 3) = 0) and ((P = Buff) or ((P - 1)^ in [' ',
        Tab])
          and ((P + 3)^ in [#0, ';', ' ', Tab])) then
      begin
        ReadingAsm := False;
        if P1 <> P then
        begin
          Dec(P);
          P^ := #0;
          FileText.Add(TExpression.Create(wtAsm, P1));
          P^ := ' ';
          inc(P);
        end;
        Buff := P;
        Exit;
      end
      else if P^ = '{' then
      begin
        while (P^ <> '}') and (P^ <> #0) do
        begin
          inc(P);
          checkPrintable(P);
        end;
        if (P^ <> '}') then
          AsmComment := wtHalfComment;
      end
      else if strLComp(P, '(*', 2) = 0 then
      begin
        while (strLComp(P, '*)', 2) <> 0) and (P^ <> #0) do
        begin
          inc(P);
          checkPrintable(P);
        end;
        if strLComp(P, '*)', 2) <> 0 then
          AsmComment := wtHalfStarComment;
      end
      else if strLComp(P, '//', 2) = 0 then
        while P^ <> #0 do
        begin
          checkPrintable(P);
          inc(P);
        end
      else
        inc(P);
    end;
  until (P^ = #0);
  FileText.Add(TExpression.Create(wtAsm, Buff));
  Buff := P;
end;

procedure TDelforParser.checkPrintable(P: PChar);
begin
  if (P <> nil) and (P^ in NotPrintable) then
  begin
    while (P^ in NotPrintable) and not (strLComp(P, #13#10, 2) = 0) do
    begin
      P^ := ' ';
      inc(P);
    end;
  end;
end;

function TDelforParser.ReadWord(Dest: PChar; var Source: PChar): TWordType;
const
  operators = '+-*/=<>[].,():;{}@^';
  AllOper = operators + ' {}'''#9;
var
  P: PChar;
  Len: Integer;

  procedure ReadString;

    procedure readQuotes;
    begin
      while (P^ = '''') and ((P + 1)^ = '''') do
        inc(P, 2);
    end;
  begin
    repeat
      inc(P);
      checkPrintable(P);
      if (P^ = '''') then
      begin
        readQuotes;
        if ((P + 1)^ = '#') then
        begin
          inc(P);
          while P^ in ['0'..'9', 'A'..'F', 'a'..'f', '$', '#', '^'] do
            inc(P);
          if P^ = '''' then
            inc(P)
          else
          begin
            Dec(P);
            Exit;
          end;
          readQuotes;
        end
        else if ((P + 1)^ = '^') then
        begin
          inc(P);
          while P^ in ['0'..'9', 'A'..'Z', 'a'..'z', '$', '#', '^'] do
            inc(P);
          if P^ = '''' then
            inc(P)
          else
          begin
            Dec(P);
            Exit;
          end;
          readQuotes;
        end
          {else
            readQuotes;}
      end;
    until (P^ = '''') or (P^ = #0);
  end;

  procedure ReadIdentifier;
  begin
    Result := wtWord;
    while (StrScan(AllOper, P^) = nil) and (P^ <> #0) do
      inc(P);
    Dec(P);
  end;
begin
  P := Source;
  checkPrintable(P);
  if P^ in [Tab, ' '] then
  begin
    Result := wtSpaces;
    while (P^ in [Tab, ' ']) do
      inc(P);
    Dec(P);
  end
  else if (Settings.StartCommentOut[0] <> #0) and (Settings.EndCommentOut[0] <>
    #0) and
    (StrLIComp(P, Settings.StartCommentOut, StrLen(Settings.StartCommentOut)) =
    0) then
  begin
    Result := wtHalfOutComment;
    inc(P, StrLen(Settings.StartCommentOut));
    Len := StrLen(Settings.EndCommentOut);
    while (StrLIComp(P, Settings.EndCommentOut, Len) <> 0) and (P^ <> #0) do
    begin
      inc(P);
      checkPrintable(P);
    end;
    if StrLIComp(P, Settings.EndCommentOut, Len) = 0 then
    begin
      inc(P, Len - 1);
      Result := wtFullOutComment;
    end;
  end
  else if P^ = '{' then
  begin
    Result := wtHalfComment;
    while (P^ <> '}') and (P^ <> #0) do
    begin
      inc(P);
      checkPrintable(P);
    end;
    if (P^ = '}') then
    begin
      Result := wtFullComment;
      if (Source + 1)^ = '$' then
        Result := wtCompDirective;
    end;
  end
  else if strLComp(P, '(*', 2) = 0 then
  begin
    Result := wtHalfStarComment;
    while (strLComp(P, '*)', 2) <> 0) and (P^ <> #0) do
    begin
      inc(P);
      checkPrintable(P);
    end;
    if strLComp(P, '*)', 2) = 0 then
    begin
      inc(P);
      Result := wtFullComment;
    end;
  end
  else if strLComp(P, '//', 2) = 0 then
  begin
    Result := wtFullComment;
    while P^ <> #0 do
    begin
      checkPrintable(P);
      inc(P);
    end
  end
  else if P^ = '''' then
  begin
    Result := wtString;
    ReadString;
    if (P^ = #0) then
      Result := wtErrorString;
  end
  else if P^ = '^' then {string starting with ^A or so}
  begin
    if ((P + 1)^ in ['a'..'z', 'A'..'Z']) and ((P + 2)^ in ['''', '^', '#'])
      then
    begin
      Result := wtString;
      while P^ in ['0'..'9', 'A'..'Z', 'a'..'z', '$', '#', '^'] do
        inc(P);
      if P^ = '''' then
        ReadString;
    end
    else
      Result := wtOperator;
  end
  else if StrScan(operators, P^) <> nil then
  begin
    Result := wtOperator;
    if strLComp(P, '<=', 2) = 0 then
      inc(P);
    if strLComp(P, '>=', 2) = 0 then
      inc(P);
    if strLComp(P, '<>', 2) = 0 then
      inc(P);
    if strLComp(P, ':=', 2) = 0 then
      inc(P);
    if strLComp(P, '..', 2) = 0 then
      inc(P);
    if strLComp(P, '(.', 2) = 0 then
    begin
      inc(LeftPointBracket);
      inc(P);
    end;
    if strLComp(P, '.)', 2) = 0 then
    begin
      Dec(LeftPointBracket);
      inc(P);
    end;
  end
  else if P^ = '$' then
  begin
    Result := wtHexNumber;
    inc(P);
    while UpCase(P^) in ['0'..'9', 'A'..'F'] do
      inc(P);
    Dec(P);
  end
  else if P^ = '#' then
  begin
    Result := wtNumber;
    while P^ in ['0'..'9', 'A'..'F', 'a'..'f', '$', '#', '^'] do
      inc(P);
    if P^ = '''' then
    begin
      Result := wtString;
      ReadString;
    end
    else
      Dec(P);
  end
  else if P^ in ['0'..'9'] then
  begin
    Result := wtNumber;
    while (P^ in ['0'..'9', '.']) and not (strLComp(P, '..', 2) = 0)
      and not ((LeftPointBracket > 0) and (strLComp(P, '.)', 2) = 0)) do
      inc(P);
    if UpCase(P^) = 'E' then
      if (P + 1)^ in ['0'..'9', '-', '+'] then
      begin
        inc(P, 2);
        while (P^ in ['0'..'9']) do
          inc(P);
      end;
    Dec(P);
  end
  else
    ReadIdentifier;
  StrLCopy(Dest, Source, P - Source + 1);
  if (StrIComp(Dest, 'asm') = 0) then
  begin
    ReadingAsm := True;
    AsmComment := wtWord;
  end;
  if (P^ = #0) then
    Source := P
  else
  begin
    if ((P + 1)^ in [Tab, ' ']) then
      inc(P);
    Source := P + 1;
  end;
  {  Readword := Result;}
end;

function TDelforParser.ReadHalfComment(Dest: PChar; var Source: PChar):
  TWordType;
var
  Len: Integer;
  P: PChar;
begin
  P := Source;
  while P^ in [Tab, ' '] do
    inc(P);
  if (PrevLine <> nil) and (PrevLine.nSpaces = 0) then
  begin
    PrevLine.nSpaces := P - Source;
    PrevLine.oldnSpaces := P - Source;
    P := StrCopy(Source, P);
  end;
  ReadHalfComment := prevType;
  if prevType = wtHalfComment then
  begin
    while (P^ <> '}') and (P^ <> #0) do
      inc(P);
    if (P^ = '}') then
    begin
      ReadHalfComment := wtFullComment;
      inc(P);
    end;
  end
  else if prevType = wtHalfStarComment then
  begin
    while (strLComp(P, '*)', 2) <> 0) and (P^ <> #0) do
      inc(P);
    if strLComp(P, '*)', 2) = 0 then
    begin
      ReadHalfComment := wtFullComment;
      inc(P, 2);
    end;
  end
  else
  begin
    Len := StrLen(Settings.EndCommentOut);
    while (StrLIComp(P, Settings.EndCommentOut, Len) <> 0) and (P^ <> #0) do
      inc(P);
    if StrLIComp(P, Settings.EndCommentOut, Len) = 0 then
    begin
      ReadHalfComment := wtFullOutComment;
      inc(P, Len);
    end;
  end;
  StrLCopy(Dest, Source, P - Source);
  if P^ = #0 then
    Source := P
  else
  begin
    if (P^ in [Tab, ' ']) then
      inc(P);
    Source := P;
  end;
end;

function TDelforParser.Parse: Boolean;
type
  TRec = record
    RT: TReservedType;
    nInd: Integer;
  end;
const
  MaxStack = 150;
type
  TStackArray = array[0..MaxStack] of TRec;
  TStackStackRec = record
    stackptr: Integer;
    ProcLevel: Integer;
    stack: TStackArray;
    nIndent: Integer;
  end;

var
  Prev1: TPascalWord;
  OldWrapIndent: Boolean;
  PrevPrevLine: TLineFeed;
  stack: TStackArray;
  stackptr: Integer;
  StackStack: array[0..2] of TStackStackRec;
  StackStackPtr: Integer;
  WrapIndent: Boolean;
  interfacePart: Boolean;
  NTmp: Integer;
  PrevOldNspaces: Integer;
  I, J: Integer;

  function GetStackTop: TReservedType;
  begin
    if stackptr >= 0 then
      GetStackTop := stack[stackptr].RT
    else
      GetStackTop := rtNothing;
  end;

  procedure SetSpacing(PascalWord, prev: TPascalWord; I: Integer);
  var
    Prev2: TPascalWord;
    rtype: TReservedType;
    wType: TWordType;
    k: Integer;
    S: array[0..Maxline] of Char;
    Found: Boolean;
  begin
    if PascalWord <> nil then
    begin
      rtype := PascalWord.ReservedType;
      wType := PascalWord.WordType;
      {if (rType = rtPrivate) and (prev <> nil) and
        (prev.ReservedType <> rtLineFeed) then
      begin
        PascalWord.SetReservedType(rtNothing);
        rType := rtNothing;
      end;}
      if not (rtype in NoReservedTypes) then
        PascalWord.ExpressionCase := ReservedCase
      else if rtype in StandardDirectives then
        PascalWord.ExpressionCase := StandDirectivesCase
      else
      begin
        PascalWord.ExpressionCase := rfUnchanged;
        if (wType = wtWord) and (PascalWord.Expression <> nil) then
        begin
          Found := False;
          if (FillNewWords in [fmAddNewWord, fmAddUse, fmAddUseExcept])
            and (rtype in
            (NoReservedTypes - StandardDirectives)) then
          begin
            Found := CapNames.Search(PascalWord.Expression, I);
            if not Found then
            begin
              StrCopy(S, '*');
              StrCat(S, PascalWord.Expression); {comment out}
              if not CapNames.Search(@S, J) then
                CapNames.Insert(I, StrNew(PascalWord.Expression));
            end;
          end;
          if (FillNewWords in [fmUse, fmAddUse])
            or ((FillNewWords in [fmExceptDirect, fmAddUseExcept]) and
            not (rtype in StandardDirectives)) then
          begin
            if not Found then
              Found := CapNames.Search(PascalWord.Expression, I);
            if Found then
            begin
              PascalWord.Expression := CapNames.Items[I];
              PascalWord.ExpressionCase := rfUnchanged;
            end;
          end;
        end;
        (*       else
                 if (FillNewWords = fmAddNewWord) and (rType in
                   (NoReservedTypes - StandardDirectives)) then
                 begin
                   StrCopy(S, '*');
                   StrCat(S, PascalWord.Expression); {comment out}
                   if not CapNames.Search(@S, I) then
                     CapNames.Insert(I, (StrNew(PascalWord.Expression));
                 end; *)
      end;

      case rtype of
        rtThen, rtOf, rtElse, rtDo, rtAsm: PascalWord.SetSpace(spBoth, True);
        rtEnd, rtFuncDirective: PascalWord.SetSpace(spBefore, True);
        rtIf, rtUntil, rtWhile, rtCase, rtRecord:
          PascalWord.SetSpace(spAfter, True);
        rtOper, rtMathOper, rtPlus, rtMinus, rtLogOper, rtEquals:
          PascalWord.SetSpace(SpaceOperators, True);
        rtEqualOper: PascalWord.SetSpace(SpaceEqualOper, True);
        rtColon: PascalWord.SetSpace(SpaceColon, True);
        rtSemiColon: PascalWord.SetSpace(SpaceSemiColon, True);
        rtComma: PascalWord.SetSpace(SpaceComma, True);
        rtLeftBr:
          begin
            PascalWord.SetSpace(SpaceLeftBr, True);
            if prev.ReservedType = rtLeftBr then
              PascalWord.SetSpace(spBefore, False);
          end;
        rtLeftHook:
          begin
            PascalWord.SetSpace(SpaceLeftHook, True);
            if prev.ReservedType = rtLeftHook then
              PascalWord.SetSpace(spBefore, False);
          end;
        rtRightBr:
          PascalWord.SetSpace(SpaceRightBr, True);
        rtRightHook:
          PascalWord.SetSpace(SpaceRightHook, True);
      end;
      {append space after : , ;}
      if (wType in [wtNumber, wtHexNumber]) and UpperNumbers then
        PascalWord.SetCase(rfUpperCase);
      {delimiter between 2 words (necesary)}
      if (prev <> nil) then
      begin
        if (SpaceOperators in [spBoth, spBefore, spAfter]) and
          (wType in [wtString, wtFullComment,
          wtHalfComment, wtHalfStarComment]) and
          not (prev.ReservedType in [rtDotDot, rtLineFeed]) then
          PascalWord.SetSpace(spBefore, True);
        if (rtype in [rtMinus, rtPlus]) then
        begin
          Prev2 := prev;
          k := 0;
          while (Prev2 <> nil) and (Prev2.ReservedType in [rtComment,
            rtLineFeed]) do
          begin
            inc(k);
            if k > I then
              Prev2 := nil
            else
              Prev2 := FileText.Items[I - k];
          end;
          if (Prev2 <> nil) and (Prev2.ReservedType in [rtOper,
            rtMathOper, rtPlus, rtMinus, rtSemiColon, rtOf,
              rtMinus, rtLogOper, rtEquals, rtEqualOper, rtLeftBr,
              rtLeftHook, rtComma, rtDefault]) then
            PascalWord.SetSpace(spAfter, False); {sign operator}
        end;
        if (rtype = rtLeftHook) then
        begin
          if not (prev.ReservedType in [rtReserved, rtNothing, rtRightBr,
            rtRightHook]) then
            //           PascalWord.SetSpace(spBefore, False) {array}
            //         else
            PascalWord.SetSpace(spBefore, True);
        end;
        if PascalWord.Space(spBefore) and
          (prev.ReservedType in [rtLeftBr, rtLeftHook,
          rtLineFeed]) then
          PascalWord.SetSpace(spBefore, False);
        if (prev.WordType in [wtWord, wtNumber, wtHexNumber, wtString]) and
          (wType in [wtWord, wtNumber, wtHexNumber]) then
          PascalWord.SetSpace(spBefore, True);
        if (prev.ReservedType = rtComment) and
          (wType in [wtWord, wtNumber, wtHexNumber]) then
         PascalWord.SetSpace(spBefore, True);
        if PascalWord.Space(spBefore) and prev.Space(spAfter) then
          prev.SetSpace(spAfter, False); {avoid double spaces}
      end;
    end;
  end;

  procedure SetPrevIndent;
  begin
    if PrevLine <> nil then
      PrevLine.SetIndent(nIndent + NTmp + ProcLevel);
  end;

  procedure Push(R: TReservedType; n, ninc: Integer);
  begin
    inc(stackptr);
    if stackptr > MaxStack then
      raise EFormatException.Create('Stack overflow');
    with stack[stackptr] do
    begin
      RT := R;
      nInd := n;
      nIndent := n + ninc;
    end;
  end;

  function HasType(AType: TReservedType): Boolean;
  var
    I: Integer;
  begin
    HasType := False;
    for I := 0 to stackptr do
      if stack[I].RT = AType then
      begin
        HasType := True;
        Exit;
      end;
  end;

  function Pop: TReservedType;
  begin
    if stackptr >= 0 then
    begin
      nIndent := stack[stackptr].nInd;
      if (stack[stackptr].RT = rtProcedure) and (ProcLevel > 0) then
        Dec(ProcLevel);
      Pop := stack[stackptr].RT;
      Dec(stackptr);
    end
    else
    begin
      nIndent := 0;
      ProcLevel := 0;
      Pop := rtNothing;
    end;
  end;

  function GetWord(I: Integer): TPascalWord;
  begin
    with FileText do
      if (I < Count) and (I >= 0) then
        GetWord := TPascalWord(Items[I])
      else
        GetWord := nil;
  end;

  function GetNextNoComment(var I, k: Integer): TPascalWord;
  begin
    k := 0;
    repeat
      inc(k);
      Result := GetWord(I + k);
    until (Result = nil) or (Result.ReservedType <> rtComment);
  end;

  function InsertBlankLines(atIndex, NLines: Integer): TLineFeed;
  var
    J: Integer;
    AfterWord: TPascalWord;
  begin
    Result := PrevLine;
    for J := 0 to NLines - 1 do
    begin
      Result := TLineFeed.Create(0);
      Result.SetIndent(nIndent);
      AfterWord := TPascalWord(FileText.Items[atIndex]);
      if AfterWord.Space(spBefore) then
        AfterWord.SetSpace(spBefore, False);
      FileText.Insert(atIndex, Result);
      SetSpacing(AfterWord, Result, atIndex);
    end;
    if atIndex <= I then
      inc(I, NLines);
  end;

  procedure CheckBlankProc;
  var
    k: Integer;
    Prev2: TPascalWord;
  begin
    if (prev <> nil) then
    begin
      k := 1;
      if prev.ReservedType = rtClass then
      begin
        k := 2;
        Prev2 := GetWord(I - 2);
      end
      else
        Prev2 := prev;
      if (Prev2 <> nil) and (Prev2.ReservedType <> rtLineFeed) then
      begin
        PrevLine := InsertBlankLines(I - k, 2);
        prev := PrevLine;
      end
      else
      begin
        inc(k);
        Prev2 := GetWord(I - k);
        if (Prev2 <> nil) and (Prev2.ReservedType <> rtLineFeed) then
        begin
          PrevLine := InsertBlankLines(I - k + 1, 1);
          prev := PrevLine;
        end;
      end;
    end;
  end;

  procedure PutCommentBefore(aComment: PChar);
  var
    J: Integer;
    P: TPascalWord;
  begin
    J := I - 2;
    P := GetWord(J);
    if P.ReservedType = rtComment then
      P.Expression := aComment
    else
    begin
      P := TExpression.Create(wtWord, aComment);
      P.SetReservedType(rtComment);
      FileText.Insert(I, P);
      inc(I);
      P := TLineFeed.Create(0);
      TLineFeed(P).SetIndent(nIndent);
      FileText.Insert(I, P);
      inc(I);
    end;
  end;

  function MakeLineFeed(k, J: Integer): TLineFeed;
  var
    next: TPascalWord;
  begin
    next := GetNextNoComment(k, J);
    if (next <> nil) and (next.ReservedType <> rtLineFeed) then
      Result := InsertBlankLines(k + J, 1)
    else
      Result := PrevLine;
  end;

  procedure SetTopIndent;
  begin
    if stackptr >= 0 then
    begin
      nIndent := stack[stackptr].nInd;
      SetPrevIndent;
    end;
  end;

  procedure DecPrevIndent;
  begin
    if PrevLine <> nil then
      PrevLine.IncIndent(-1);
  end;

  procedure CheckIndent(PascalWord: TPascalWord);
  var
    next: TPascalWord;
    lastPop: TReservedType;
    k: Integer;
    P, P2: PChar;
    Top: TReservedType;
    functdeclare, NoBlankLine: Boolean;
    rtype: TReservedType;
    wType: TWordType;
    procedure CheckSlashComment;
    var
      PasWord: TPascalWord;
      PrevPasWord: TPascalWord;
      Buff: array[0..200] of Char;
    begin
      prev := GetWord(I - 1);
      if (prev <> nil) and (prev.ReservedType = rtComment)
        and (prev.Expression^ = '/') then {fix for situation with a // comment
        on prev line: begin becomes part of the comment}
        if not prev.ChangeComment('{') then
        begin
          {FileText.Delete(I - 1);}
          k := 0;
          PasWord := nil;
          repeat
            PrevPasWord := PasWord;
            PasWord := GetWord(I + k);
            inc(k);
          until (PasWord = nil) or (PasWord.ReservedType = rtLineFeed);
          Dec(k);
          if ((PrevPasWord.ReservedType = rtComment) and (PrevPasWord.Expression^
            =
            '/')) then
          begin
            prev.Expression := StrCat(StrCat(StrCopy(Buff, '{'), prev.Expression
              +
              2), '}');
            Exit;
          end
          else
            FileText.Delete(I - 1);
          FileText.Insert(I + k, prev);
          prev := GetWord(I - 1);
          SetSpacing(prev, GetWord(I - 2), I - 1);
          PascalWord := GetWord(I);
        end;
      PrevLine := PrevPrevLine;
    end;

    function NoBeginTryIndent: Boolean;
    begin
      Result := not ((indentBegin and (rtype = rtBegin)) or
        (IndentTry and (rtype = rtTry))) and
        (GetStackTop in [rtDo, rtThen, rtIfElse]);
    end;

    procedure CheckShortLine;
    begin
      k := 1;
      next := GetWord(I + k);
      if (next <> nil) and (next.ReservedType = rtLineFeed) then
      begin
        while not ((next = nil) or (next.ReservedType in
          [rtSemiColon, rtBegin, rtElse, rtDo, rtWhile, rtOn, rtThen, rtCase])
          or ((k > 1) and (next.ReservedType = rtLineFeed))) do
        begin
          inc(k);
          next := GetWord(I + k);
        end;
        if (next <> nil) and (next.ReservedType = rtSemiColon) then
        begin
          FileText.AtFree(I + 1);
        end;
      end;
    end;

    procedure ComplexIfElse;
    begin
      while (stackptr >= 0) and (lastPop <> rtThen) do
      begin
        lastPop := Pop;
        if lastPop = rtIfElse then
          ComplexIfElse;
      end;
      SetPrevIndent;
    end;
  begin
    if PascalWord <> nil then
    begin
      rtype := PascalWord.ReservedType;
      wType := PascalWord.WordType;
      if (rtype in [rtWhile, rtEnd, rtRepeat, rtBegin, rtUses, rtTry,
        rtProgram, rtType, rtvar,
          rtIf, rtThen, rtElse] + standardDirectives) and (prev <> nil) and
        (prev.ReservedType = rtDot) then
      begin
        PascalWord.SetReservedType(rtNothing);
        rtype := rtNothing;
      end;
      {SetSpacing;}
      if rtype <> rtNothing then
      begin
        case rtype of
          rtIf:
            begin
              if FeedAfterThen and not FeedElseIf and (GetStackTop =
                rtIfElse) and (prev = PrevLine) then
              begin
                FileText.AtFree(I - 1);
                Dec(I);
                CheckSlashComment;
              end
              else
              begin
                if FeedElseIf and (prev <> PrevLine) then
                begin
                  PrevLine := MakeLineFeed(I - 1, 0);
                  prev := PrevLine;
                end;
              end;
              if ((prev <> nil) and (prev.ReservedType = rtElse)) or
                (NoIndentElseIf and (GetStackTop = rtIfElse)) then
              begin
                Pop;
                if GetStackTop = rtThen then
                  Pop;
                WrapIndent := True;
                Push(rtIfElse, nIndent, 0);
              end
              else
                Push(rtype, nIndent, 0);
            end;
          rtThen:
            if GetStackTop in [rtIf, rtIfElse] then
            begin
              WrapIndent := False;
              lastPop := Pop;
              if NoFeedBeforeThen and ((prev = PrevLine) and
                (TPascalWord(FileText.Items[I - 1]).ReservedType <>
                rtComment)) then
              begin
                FileText.AtFree(I - 1);
                Dec(I);
                CheckSlashComment;
              end;
              if FeedAfterThen then
              begin
                if MakeLineFeed(I, 1) <> PrevLine then
                begin
                  if (lastPop = rtIf) and Settings.ExceptSingle then
                    CheckShortLine;
                end;
              end;
              Push(rtype, nIndent, 1);
            end;
          rtColon:
            if GetStackTop = rtOf then
            begin
              Push(rtype, nIndent, 1);
              if FeedAfterThen then
              begin
                if (GetNextNoComment(I, k).ReservedType = rtBegin) and
                  (MakeLineFeed(I, 1) <> PrevLine) then
                  CheckShortLine;
              end;
              WrapIndent := False;
            end
            else if GetStackTop = rtClassDecl then
            begin
              Pop;
              Push(rtClass, nIndent, 1);
            end
            else if AlignVar and (GetStackTop = rtVar) then
              PascalWord := AlignExpression(I, AlignVarPos)
            else if not (GetStackTop in [rtProcedure, rtProcDeclare]) then
              //label????
              WrapIndent := False;
          rtElse:
            begin
              lastPop := rtNothing;
              while (stackptr >= 0) and not (GetStackTop in [rtThen,
                rtOf, rtTry]) do
                lastPop := Pop;
              if lastPop = rtIfElse then
                ComplexIfElse;
              if FeedAfterThen then
              begin
                if (prev <> nil) and (GetWord(I - 1).ReservedType <>
                  rtLineFeed) then
                begin
                  PrevLine := MakeLineFeed(I - 1, 0);
                  prev := PrevLine;
                end;
                if GetNextNoComment(I, k).ReservedType <> rtIf then
                  MakeLineFeed(I, 1);
              end;
              if stackptr >= 0 then
                nIndent := stack[stackptr].nInd;
              if prev = PrevLine then
                SetPrevIndent;
              if IndentTryElse and (GetStackTop = rtTry) then
              begin
                inc(nIndent);
                SetPrevIndent;
              end
              else if IndentCaseElse and (GetStackTop = rtOf) then
              begin
                inc(nIndent);
                SetPrevIndent;
              end;
              if GetStackTop = rtThen then
                Push(rtIfElse, nIndent, 1)
              else
                Push(rtype, nIndent, 1);
              WrapIndent := False;
            end;
          rtRepeat, rtRecord:
            begin
              Push(rtype, nIndent, 1);
              WrapIndent := False;
            end;
          rtClass:
            begin
              next := GetNextNoComment(I, k);
              if not ((next <> nil) and (next.ReservedType in [rtProcedure,
                rtProcDeclare, rtOf])) then
                {not a "class function" or "class of" declaration}
              begin
                WrapIndent := False;
                Push(rtClassDecl, nIndent, 1);
                {first assume that it is a class declaration
                 the first procedure replaces it with rtClass}
              end
              else
                PascalWord.SetSpace(spAfter, True);
            end;
          rtUntil:
            begin
              repeat
                lastPop := Pop;
              until (lastPop = rtRepeat) or (stackptr < 0);
              SetPrevIndent;
            end;
          rtLeftBr:
            if (GetStackTop = rtLeftBr) then
              Push(rtype, nIndent, 0)
            else
            begin
              OldWrapIndent := WrapIndent;
              if (ProcLevel <= 0) or (GetStackTop <> rtProcedure) then
                {niet erg netjes}
                Push(rtype, nIndent, 1)
              else
              begin
                k := 1;
                next := GetWord(I - k);
                while (I > k) and ((next <> nil) and (next.ReservedType
                  in
                  [rtDot, rtNothing])) do
                begin
                  inc(k);
                  next := GetWord(I - k);
                end;
                if (next <> nil) and (next.ReservedType = rtProcedure) then
                  Push(rtype, nIndent, 0)
                else
                  Push(rtype, nIndent, 1);
              end;
              WrapIndent := False;
            end;
          rtLeftHook, rtWhile, rtOn:
            Push(rtype, nIndent, 0);
          rtRightBr:
            begin
              repeat
                lastPop := Pop;
              until (lastPop = rtLeftBr) or (stackptr < 0);
              if not (GetStackTop = rtLeftBr) then
                WrapIndent := OldWrapIndent;
            end;
          rtRightHook:
            begin
              repeat
                lastPop := Pop;
              until (lastPop = rtLeftHook) or (stackptr < 0);
              if GetStackTop = rtClassDecl {Interface} then
                WrapIndent := False;
            end;
          rtExcept:
            begin
              while (stackptr >= 0) and (GetStackTop <> rtTry) do
                Pop;
              SetTopIndent;
              inc(nIndent);
              WrapIndent := False;
            end;
          rtPrivate:
            if not (GetStackTop in [rtClass, rtClassDecl]) then
              PascalWord.SetReservedType(rtNothing)
            else if prev.ReservedType = rtLineFeed then
            begin
              DecPrevIndent;
              WrapIndent := False;
            end;
          rtOf:
            begin
              case GetStackTop of
                rtCase:
                  begin
                    Push(rtype, nIndent, 1);
                    if FeedAfterThen then
                      MakeLineFeed(I, 1);
                    WrapIndent := False;
                  end;
                rtRecord: WrapIndent := False;
              end;
            end;
          rtLineFeed:
            begin
              if stackptr = -1 then
                WrapIndent := False;
              if RemoveDoubleBlank and (I >= 2) and (prev <> nil)
                and (prev = PrevLine) and
                (FileText.Items[I - 2] = PrevPrevLine) then
              begin
                FileText.AtFree(I - 2);
                Dec(I);
              end;
              next := GetNextNoComment(I, k);
              if (next <> nil) then
              begin
                if (next.ReservedType in [rtElse, rtIfElse, rtBegin,
                  rtEnd, rtUntil, rtExcept]) then
                  WrapIndent := False;
                {TLineFeed(PascalWord).Wrapped:=WrapIndent;}
                if WrapIndent then
                  NTmp := 1
                else
                  NTmp := 0;
                Top := GetStackTop;
                WrapIndent := True;
                if (next.ReservedType in [rtLineFeed])
                  or (Top in [rtUses, rtLeftBr]) then
                  WrapIndent := False;
              end;
              PrevPrevLine := PrevLine;
              PrevLine := TLineFeed(PascalWord);
              SetPrevIndent;
            end;
          rtAsm:
            begin
              while GetStackTop in [rtVar, rtType] do
                Pop;
              if GetStackTop = rtProcedure then
              begin
                Pop;
                DecPrevIndent;
              end;
              Push(rtype, nIndent, 0);
              with FileText do
              begin
                PascalWord := TPascalWord(Items[I]);
                while (I < Count - 1) and (PascalWord.ReservedType <>
                  rtEnd) do
                begin
                  if PascalWord.ReservedType = rtLineFeed then
                  begin
                    PrevLine := TLineFeed(PascalWord);
                    with PrevLine do
                      nSpaces := oldnSpaces;
                  end;
                  SetSpacing(PascalWord, prev, I);
                  inc(I);
                  prev := PascalWord;
                  PascalWord := TPascalWord(Items[I]);
                end;
                if I < Count then
                  SetPrevIndent;
                Dec(I);
                Exit;
              end;
            end;
          rtComma:
            if FeedEachUnit and (GetStackTop = rtUses) then
            begin
              next := GetNextNoComment(I, k);
              if next.ReservedType <> rtLineFeed then
              begin
                MakeLineFeed(I, 0);

              end;
            end;
          rtProgram, rtUses, rtInitialization:
            if GetStackTop <> rtLeftBr then
            begin
              next := GetNextNoComment(I, k);
              if (rtype =rtUses) and (GetStackTop in [rtProcedure, rtProcDeclare, rtClass])  then
                PascalWord.SetReservedType(rtNothing)
              else
              begin
                DecPrevIndent;
                stackptr := -1;
                nIndent := 0;
                Push(rtype, 0, 1);
                WrapIndent := False;
              end;
              {nIndent := 1;}
            end;
          rtAbsolute:
            if not (GetStackTop in [rtVar, rtType]) then
              PascalWord.SetReservedType(rtNothing)
            else
            begin
              next := GetNextNoComment(I, k);
              if next.ReservedType = rtColon then
              begin
                DecPrevIndent;
                PascalWord.SetReservedType(rtNothing);
              end;
            end;
          rtFuncDirective, rtDefault:
            begin
              next := GetNextNoComment(I, k);
              if (next.ReservedType = rtColon) or
                not (GetStackTop in [rtProcedure, rtProcDeclare, rtClass])
                or (prev.ReservedType in [rtProcedure, rtProcDeclare, rtDot])
                  then
                PascalWord.SetReservedType(rtNothing);
            end;
          rtForward:
            begin
              if GetStackTop in [rtProcedure, rtProcDeclare] then
                Pop
              else
                PascalWord.SetReservedType(rtNothing);
            end;
          rtProcedure:
            begin
              if GetStackTop = rtClassDecl then
              begin
                Pop;
                Push(rtClass, nIndent, 1);
              end;
              Prev1 := prev;
              J := I;
              if Prev1 <> nil then
              begin
                while (J > 0) and (Prev1.ReservedType in [rtComment,
                  rtLineFeed]) do
                begin
                  Dec(J);
                  Prev1 := FileText.Items[J];
                end;
                functdeclare := (Prev1 <> nil) and (Prev1.ReservedType in
                  [rtEquals, rtColon]);
              end
              else
                functdeclare := False;
              NoBlankLine := False;
              if not functdeclare then
              begin
                k := 0;
                repeat
                  inc(k);
                  next := GetWord(I + k);
                  if (next <> nil) and (next.ReservedType = rtLeftBr) then
                    repeat
                      inc(k);
                      next := GetWord(I + k);
                    until (next = nil) or (next.ReservedType = rtRightBr);
                until (next = nil) or (next.ReservedType = rtSemiColon);
                if next <> nil then
                begin
                  repeat
                    inc(k);
                    next := GetWord(I + k);
                  until (next = nil) or not (next.ReservedType in
                    [rtLineFeed,                     rtComment]);
                  if (next <> nil) and (next.ReservedType = rtForward) then
                    NoBlankLine := True;
                end;
              end;
              if not (functdeclare or interfacePart or
                (GetStackTop = rtClass)) then
              begin
                if not HasType(rtProcedure) then
                begin
                  if (nIndent > 0) then
                  begin
                    nIndent := 0;
                    SetPrevIndent;
                  end;
                  ProcLevel := 0;
                  if BlankProc and not NoBlankLine then
                    CheckBlankProc;
                  if CommentFunction then
                    PutCommentBefore('{ procedure }');
                end
                else
                begin
                  if BlankSubProc and not NoBlankLine then
                    CheckBlankProc;
                  inc(ProcLevel);
                  if nIndent = 0 then
                  begin
                    SetPrevIndent;
                    inc(nIndent);
                  end;
                end;
                {if indentProcedure then  inc(nIndent);}
                Push(rtProcedure, nIndent, 0);
              end
              else
              begin
                if (not functdeclare) and (not (GetStackTop = rtClass)) then
                begin
                  nIndent := 0;
                  SetPrevIndent;
                end;
                Push(rtProcDeclare, nIndent, 0);
              end;
            end;
          rtInterface:
            begin
              if (prev.ReservedType = rtEquals) then
              begin
                {declaration of a OLE object
                   IClass = interface
                     [' dfgsgdf']}
                Push(rtClassDecl, nIndent, 1);
              end
              else
              begin
                interfacePart := True;
                DecPrevIndent;
              end;
              WrapIndent := False;
            end;
          rtImplementation:
            begin
              stackptr := -1;
              nIndent := 0;
              interfacePart := False;
              WrapIndent := False;
              {DecPrevIndent;}
              nIndent := 0;
              SetPrevIndent;
            end;
          rtBegin, rtTry:
            begin
              if GetStackTop in [rtVar, rtType] then
                Pop;
              if GetStackTop in [rtProcedure, rtProgram] then
                Pop;
              if stackptr = -1 then
                nIndent := 0;
              if NoBeginTryIndent then
                Dec(nIndent);
              case FeedRoundBegin of
                Hanging:
                  begin
                    if (GetStackTop in [rtDo, rtThen, rtIfElse, rtElse,
                      rtColon])
                      and (prev <> nil) and (GetWord(I - 1) = PrevLine) then
                    begin
                      FileText.AtFree(I - 1);
                      Dec(I);
                      CheckSlashComment;
                    end;
                    MakeLineFeed(I, 1);
                  end;
                NewLine:
                  begin
                    if (prev <> nil) and (GetWord(I - 1).ReservedType <>
                      rtLineFeed) then
                    begin
                      PrevLine := MakeLineFeed(I - 1, 0);
                      prev := PrevLine;
                    end;
                    MakeLineFeed(I, 1);
                  end;
              end;
              Push(rtype, nIndent, 1);
              if prev = PrevLine then
              begin
                SetPrevIndent;
                DecPrevIndent;
              end;
              WrapIndent := False;
            end;
          rtEquals:
            if AlignVar and (GetStackTop = rtVar) then
              PascalWord := AlignExpression(I, AlignVarPos);
          rtVar, rtType:
            if not (GetStackTop in [rtLeftBr, rtLeftHook]) then
            begin
              WrapIndent := False;
              if nIndent < 1 then
                nIndent := 1;
              {in classes.pas I found
               t =  type string}
              if (GetStackTop in [rtVar, rtType]) then
                Pop;
              Push(rtype, nIndent, 0);
              if (not ((prev <> nil) and
                (prev.ReservedType = rtEquals))) then
              begin
                DecPrevIndent;
                if FeedAfterVar then
                  MakeLineFeed(I, 1);
              end;
            end;
          rtCase:
            if not (GetStackTop in [rtRecord, rtLeftBr]) then
              Push(rtype, nIndent, 0)
            else
            begin
              WrapIndent := False;
              Push(rtRecCase, nIndent, 1);
            end;
          rtDo:
            if GetStackTop in [rtWhile, rtOn] then
            begin
              lastPop := GetStackTop;
              Push(rtype, nIndent, 1);
              WrapIndent := False;
              if NoFeedBeforeThen and (prev = PrevLine) then
              begin
                FileText.AtFree(I - 1);
                Dec(I);
                CheckSlashComment;
              end;
              if FeedAfterThen then
              begin
                if MakeLineFeed(I, 1) <> PrevLine then
                begin
                  if (lastPop = rtOn) and Settings.ExceptSingle then
                    CheckShortLine;
                end;
              end;
            end;
          rtEnd:
            begin
              WrapIndent := False;
              repeat
                lastPop := Pop;
              until (stackptr < 0) or (lastPop in [rtClass, rtClassDecl,
                rtRecord, rtTry, rtCase, rtBegin, rtAsm]);
              if stackptr = -1 then
                nIndent := 0;
              if FeedBeforeEnd and (prev <> nil) and
                (GetWord(I - 1).ReservedType <> rtLineFeed) then
              begin
                PrevLine := MakeLineFeed(I - 1, 0);
                prev := PrevLine;
              end;
              if (prev = PrevLine) then
                SetPrevIndent;
              if NoBeginTryIndent then
                inc(nIndent);
            end;
          rtComment:
            begin
              if IndentComments then
                WrapIndent := False;
              if (stackptr = -1) and (nIndent > 0) then
              begin
                nIndent := 0;
                SetPrevIndent;
              end;
              SetSpacing(GetWord(I + 1), PascalWord, I + 1);
              if ((PrevLine <> nil) and (PrevLine = prev)) then
              begin
                if not IndentComments or
                  (PascalWord.WordType in [wtFullOutComment,
                  wtHalfOutComment]) then
                  PrevLine.nSpaces := PrevLine.oldnSpaces
                else
                begin
                  if PrevOldNspaces >= 0 then
                    PrevLine.nSpaces := PrevLine.nSpaces +
                      (PrevLine.oldnSpaces - PrevOldNspaces)
                  else
                    PrevOldNspaces := PrevLine.oldnSpaces;
                end;
              end
              else if AlignComments and (PascalWord.WordType = wtFullComment)
                then
              begin
                next := GetWord(I + 1);
                if (next <> nil) and (next.ReservedType = rtLineFeed) then
                  PascalWord := AlignExpression(I, AlignCommentPos);
              end;
            end;
          rtSemiColon:
            if not (GetStackTop in [rtLeftBr, rtLeftHook]) then
            begin
              while (stackptr >= 0) and (GetStackTop in [rtDo, rtWhile,
                rtProcDeclare, rtThen,
                  rtProgram, rtUses, rtColon, rtClassDecl]) or (GetStackTop
                = rtIfElse) do
                Pop;
              WrapIndent := False;
              k := 0;
              repeat
                inc(k);
                next := GetWord(I + k);
              until (next = nil) or (not (next.ReservedType in [{rtComment,}
                rtLineFeed]));
              if (next <> nil) then
              begin
                if (next.ReservedType = rtAbsolute)
                  or ((GetStackTop in [rtProcedure, rtProcDeclare,
                  rtClass]) and (next.ReservedType in [rtFuncDirective,
                    rtForward])
                    and (ProcLevel = 0)) then
                  WrapIndent := True
                else if FeedAfterSemiColon and not (next.ReservedType in
                  [rtForward, rtFuncDirective, rtDefault]) then
                  MakeLineFeed(I, 0);
              end;
            end;
          rtCompIf:
            begin
              inc(StackStackPtr);
              if (StackStackPtr <= 2) then
              begin
                StackStack[StackStackPtr].stack := stack;
                StackStack[StackStackPtr].stackptr := stackptr;
                StackStack[StackStackPtr].ProcLevel := ProcLevel;
                StackStack[StackStackPtr].nIndent := nIndent;
              end;
            end;
          rtCompElse:
            begin
              if (StackStackPtr >= 0) and (StackStackPtr <= 2) then
              begin
                stack := StackStack[StackStackPtr].stack;
                stackptr := StackStack[StackStackPtr].stackptr;
                ProcLevel := StackStack[StackStackPtr].ProcLevel;
                nIndent := StackStack[StackStackPtr].nIndent;
              end;
            end;
          rtCompEndif:
            begin
              if StackStackPtr >= 0 then
                Dec(StackStackPtr);
            end;
        end;
      end;
      SetSpacing(PascalWord, prev, I);
      if not (rtype in [rtLineFeed, rtComment]) then
        PrevOldNspaces := -1;
      if wType = wtCompDirective then
      begin
        WrapIndent := False;
        if not IndentCompDirectives and (prev <> nil) and
          (prev.ReservedType = rtLineFeed) then
        begin
          NTmp := -nIndent;
          SetPrevIndent;
        end;
        if UpperCompDirectives then
        begin
          P := PascalWord.Expression;
          P2 := P + 1;
          while not (P2^ in [' ', Tab, #0]) do
            inc(P2);
          while P < P2 do
          begin
            inc(P);
            P^ := UpCase(P^);
          end;
        end;
      end;
      if not (PascalWord.ReservedType = rtComment) then
        prev := PascalWord;
    end;
  end;
begin {procedure TPascalParser.Parse;}
  Result := True;
  try
    if Assigned(OnProgress) then
      OnProgress(Self, 33);
    if ChangeIndent then
    begin
      {LastPop := rtNothing;}
      PrevLine := nil;
      PrevPrevLine := nil;
      prev := nil;
      WrapIndent := True;
      interfacePart := False;
      nIndent := 0;
      ProcLevel := 0;
      NTmp := 0;
      PrevOldNspaces := -1;
      stackptr := -1;
      StackStackPtr := -1;
      I := 0;
      with FileText do
        while I < Count do
        begin
          CheckIndent(TPascalWord(Items[I]));
          inc(I);
          if (I mod 1024 * 4 = 0) and Assigned(OnProgress) then
            OnProgress(Self, Round(33 + I * 33 / FileText.Count));
        end;
    end;
    with FileText do
    begin
      I := Count - 1;
      while (TPascalWord(Items[I]).ReservedType = rtLineFeed) and (I > 0) do
      begin
        AtFree(I);
        Dec(I);
      end;
      if WrapLines or HasAligned then
        CheckWrapping;
    end;
    if Assigned(OnProgress) then
      OnProgress(Self, 66);
  except
    on E: Exception do
    begin
      Clear;
      Result := False;
      {ShowMessage('Error occurred, cannot format');}
    end;
  end;
end;

procedure TDelforParser.CheckWrapping;
var
  I, J, k: Integer;
  K2, lineLen: Integer;
  PrevPrevLine: TLineFeed;
  PascalWord: TPascalWord;
  Found: Boolean;

  procedure InsertLine(k: Integer);
  begin
    PrevPrevLine := PrevLine;
    PrevLine := TLineFeed.Create(PrevPrevLine.oldnSpaces);
    PrevLine.nSpaces := PrevPrevLine.nSpaces;
    PrevLine.wrapped := True;
    TPascalWord(FileText.Items[k]).SetSpace(spBefore, False);
    FileText.Insert(k, PrevLine);
    Found := True;
  end;

begin
  with FileText do
  begin
    lineLen := 0;
    PrevLine := nil;
    J := 0;
    I := 0;
    while I < Count do
    begin
      PascalWord := TPascalWord(Items[I]);
      PascalWord.GetLength(lineLen);
      if WrapLines and
        (PascalWord is TAlignExpression) and (lineLen > WrapPosition) then
        with TAlignExpression(PascalWord) do
        begin
          k := nSpaces - lineLen - WrapPosition;
          if k < 1 then
            nSpaces := 1
          else
            nSpaces := k;
          lineLen := WrapPosition;
        end;
      if PascalWord.ReservedType = rtLineFeed then
      begin
        PrevLine := TLineFeed(PascalWord);
        if (lineLen > WrapPosition) then
          lineLen := 0;
        J := I;
      end;
      if WrapLines and (lineLen > WrapPosition) and (I > J + 3) then
      begin
        k := I - 1;
        K2 := 0;
        Found := False;
        while (k >= J) and not Found do
        begin
          if (TPascalWord(Items[k]).ReservedType in [rtThen, rtDo]) or
            ((TPascalWord(Items[k]).ReservedType = rtElse) and
            (TPascalWord(Items[k + 1]).ReservedType <> rtIf)) then
          begin
            InsertLine(k + 1);
            I := J;
          end;
          if (K2 = 0) and (TPascalWord(Items[k]).Space(spAfter) or
            TPascalWord(Items[k + 1]).Space(spBefore)) then
            K2 := k + 1;
          Dec(k);
        end;
        if not Found and (K2 <> 0) and (K2 > J) then
        begin
          InsertLine(K2);
          I := J;
        end;
        lineLen := 0;
      end;
      inc(I);
    end;
  end;
end;

function TDelforParser.GetString(Dest: PChar; var I: Integer): PChar;
var
  PasWord: TPascalWord;
  P: PChar;
begin
  GetString := Dest;
  Dest^ := #0;
  P := Dest;
  with FileText do
    if (I < Count) and (I >= 0) then
    begin
      PasWord := TPascalWord(Items[I]);
      repeat
        P := PasWord.GetEString(P);
        inc(I);
        if I < Count then
          PasWord := TPascalWord(Items[I]);
      until (I >= Count) or (PasWord.ReservedType = rtLineFeed);
    end;
  P := StrEnd(Dest);
  if P - Dest > 1 then
    Dec(P);
  while (P >= Dest) and (P^ in [' ', Tab]) do
  begin
    P^ := #0;
    Dec(P);
  end;
end;

procedure TDelforParser.WriteToFile(AFileName: PChar);
var
  outFile: TextFile;
  I: Integer;
  A: array[0..Maxline] of Char;
begin
  Assign(outFile, AFileName);
  try
    Rewrite(outFile);
    I := 0;
    with FileText do
      while I < Count do
        Writeln(outFile, GetString(A, I));
  finally
    Close(outFile);
  end;
end;

procedure TDelforParser.SetTextStr(AText: PChar);
var
  P1, P2: PChar;
  Total, Curr, I, k: Integer;
begin
  P1 := AText;
  Total := StrLen(AText);
  Curr := 0;
  P2 := StrScan(P1, #13);
  k := 0;
  while P2 <> nil do
  begin
    inc(k);
    I := P2 - P1;
    P2^ := #0;
    Add(P1);
    P2^ := #13;
    {strLCopy(buff, p1, i);
    add(buff);             }
    P1 := P2 + 2;
    P2 := StrScan(P1, #13);
    Curr := Curr + I + 2;
    if (k mod 50 = 0) and Assigned(OnProgress) then
      OnProgress(Self, (Curr * 34) div Total);
  end;
  Add(P1);
end;

function TDelforParser.GetTextStr: PChar;
const
  Increment = $FFFF;
var
  CurSize, CurCap: Integer;
  Buff: array[0..Maxline] of Char;
  P, P2, Pres: PChar;
  I: Integer;
begin
  CurCap := Increment;
  StrDispose(FCurrentText);
  Pres := StrAlloc(CurCap + 10);
  P := Pres;
  P^ := #0;
  I := 0;
  CurSize := 0;
  with FileText do
    while I < Count do
    begin
      GetString(Buff, I);
      CurSize := CurSize + Integer(StrLen(Buff)) + 2;
      while CurSize > CurCap do
      begin
        inc(CurCap, Increment);
        P2 := Pres;
        Pres := StrAlloc(CurCap + 10);
        P := strECopy(Pres, P2);
        StrDispose(P2);
      end;
      P := strECopy(P, Buff);
      P := strECopy(P, CRLF);
      if (I mod 50 = 0) and Assigned(OnProgress) then
        OnProgress(Self, 66 + I * 34 div Count);
    end;
  Result := Pres;
  FCurrentText := Pres;
end;

destructor TDelforParser.Destroy;
begin
  inherited Destroy;
  StrDispose(FCurrentText);
  FileText.Free;
end;

constructor TPascalWord.Create;
begin
  inherited Create;
end;

function TPascalWord.GetExpression: PChar;
begin
  GetExpression := nil;
end;

procedure TPascalWord.GetLength(var Length: Integer);
begin
end;

function TPascalWord.WordType: TWordType;
begin
  WordType := wtLineFeed;
end;

function TPascalWord.Space(SpaceBefore: TSpaceBefore): Boolean;
begin
  Space := False;
end;

procedure TPascalWord.SetExpression(AExpression: PChar);
begin
end;

procedure TPascalWord.SetCase(ACase: TCase);
begin
end;

function TPascalWord.GetCase: TCase;
begin
  Result := rfUnchanged;
end;

procedure TPascalWord.SetSpace(SpaceBefore: TSpaceBefore; State: Boolean);
begin
end;

function TPascalWord.ReservedType: TReservedType;
begin
  ReservedType := rtNothing;
end;

procedure TPascalWord.SetReservedType(AReservedType: TReservedType);
begin
end;

constructor TExpression.Create(AType: TWordType; AExpression: PChar);
begin
  FWordType := AType;
  FFormatType := ftNothing;
  FReservedType := rtNothing;
  FExpression := nil;
  SetExpression(AExpression);
  SetSpace(spBoth, False);
  CheckReserved;
end;

procedure TExpression.CheckReserved;
var
  L, H, C, I: Integer;
  P: PChar;
  Buf: array[0..Maxline] of Char;
begin
  SetReservedType(rtNothing);
  case WordType of
    wtCompDirective:
      begin
        P := StrLower(StrCopy(Buf, Expression));
        if P <> nil then
        begin
          if StrLIComp(P, '{$ifdef', 7) = 0 then
            SetReservedType(rtCompIf)
          else if StrLIComp(P, '{$ifndef', 8) = 0 then
            SetReservedType(rtCompIf)
          else if StrLIComp(P, '{$else', 6) = 0 then
            SetReservedType(rtCompElse)
          else if StrLIComp(P, '{$endif', 7) = 0 then
            SetReservedType(rtCompEndif);
        end;
      end;
    wtFullComment, wtFullOutComment, wtHalfStarComment,
      wtHalfOutComment, wtHalfComment: SetReservedType(rtComment);
    wtWord:
      begin
        P := StrLower(StrCopy(Buf, Expression));
        if P <> nil then
        begin
          L := 0;
          H := NReservedWords - 1; {fast search method}
          while L <= H do
          begin
            I := (L + H) shr 1;
            C := StrComp(ReservedArray[I].Words, P);
            if C < 0 then
              L := I + 1
            else
            begin
              H := I - 1;
              if C = 0 then
                with ReservedArray[I] do
                begin
                  SetReservedType(ReservedType);
                  Exit;
                end;
            end;
          end;
        end;
      end;
    wtOperator:
      begin
        P := Expression;
        if StrLen(P) = 1 then
        begin
          case P^ of
            ':': SetReservedType(rtColon);
            '.': SetReservedType(rtDot);
            ';': SetReservedType(rtSemiColon);
            ',': SetReservedType(rtComma);
            ')': SetReservedType(rtRightBr);
            '(': SetReservedType(rtLeftBr);
            ']': SetReservedType(rtRightHook);
            '[': SetReservedType(rtLeftHook);
            '-': SetReservedType(rtMinus);
            '+': SetReservedType(rtPlus);
            '=': SetReservedType(rtEquals);
            '/', '*': SetReservedType(rtMathOper);
            '<', '>': SetReservedType(rtLogOper);
          end;
        end
        else if StrLen(P) = 2 then
        begin
          if (StrComp(P, '.)') = 0) then
            SetReservedType(rtRightHook)
          else if (StrComp(P, '(.') = 0) then
            SetReservedType(rtLeftHook)
          else if (StrComp(P, '..') = 0) then
            SetReservedType(rtDotDot)
          else if (StrComp(P, ':=') = 0) then
            SetReservedType(rtEqualOper)
          else if (P^ = '<') or (P^ = '>') then
            {if p in > < <> >=  <= =}
            SetReservedType(rtLogOper);
        end;
      end;

  end;
end;

procedure TExpression.SetExpression(AExpression: PChar);
begin
  StrDispose(FExpression);
  FExpression := StrNew(AExpression);
end;

function TExpression.WordType: TWordType;
begin
  WordType := FWordType;
end;

function TExpression.GetExpression: PChar;
begin
  Result := FExpression;
end;

function TExpression.Space(SpaceBefore: TSpaceBefore): Boolean;
begin
  case SpaceBefore of
    spBefore: Space := FFormatType and ftSpaceBefore > 0;
    spAfter: Space := FFormatType and ftSpaceAfter > 0;
    spBoth: Space := FFormatType and (ftSpaceAfter or ftSpaceBefore) > 0;
  else
    Space := False;
  end;
end;

function TExpression.ReservedType: TReservedType;
begin
  ReservedType := FReservedType;
end;

destructor TExpression.Destroy;
begin
  SetExpression(nil);
  inherited Destroy;
end;

procedure TExpression.SetSpace(SpaceBefore: TSpaceBefore; State: Boolean);
var
  B: Byte;
begin
  case SpaceBefore of
    spBefore: B := ftSpaceBefore;
    spAfter: B := ftSpaceAfter;
    spBoth: B := ftSpaceAfter or ftSpaceBefore;
  else
    B := 0;
  end;
  if State then
    FFormatType := FFormatType or B
  else
    FFormatType := FFormatType and not B;
end;

procedure TExpression.SetCase(ACase: TCase);
begin
  FFormatType := FFormatType and not (ftLowerCase + ftUpperCase);
  case ACase of
    rfUpperCase: FFormatType := FFormatType or ftUpperCase;
    rfLowerCase: FFormatType := FFormatType or ftLowerCase;
    rfFirstUp: FFormatType := FFormatType or ftFirstUp;
  end;
end;

function TExpression.GetCase: TCase;
begin
  if (FFormatType and ftUpperCase) > 0 then
    Result := rfUpperCase
  else if (FFormatType and ftLowerCase) > 0 then
    Result := rfLowerCase
  else if (FFormatType and ftFirstUp) > 0 then
    Result := rfFirstUp
  else
    Result := rfUnchanged;
end;

procedure TExpression.SetReservedType(AReservedType: TReservedType);
begin
  FReservedType := AReservedType;
end;

function TExpression.GetEString(Dest: PChar): PChar;
begin
  if Space(spBefore) then
    Result := strECopy(Dest, ' ')
  else
    Result := Dest;
  Result := strECopy(Result, Expression);
  StrCase(Dest, ExpressionCase);
  if Space(spAfter) then
    Result := strECopy(Result, ' ');
end;

procedure TExpression.GetLength(var Length: Integer);
begin
  if Space(spBefore) then
    inc(Length);
  if Space(spAfter) then
    inc(Length);
  if Expression <> nil then
    inc(Length, StrLen(Expression));
end;

function strSpace(Dest: PChar; n: Integer): PChar;
var
  I: Integer;
begin
  strSpace := Dest;
  for I := 0 to n - 1 do
  begin
    Dest^ := ' ';
    inc(Dest);
  end;
  Dest^ := #0;
end;

function TLineFeed.GetEString(Dest: PChar): PChar;
var
  Len: Integer;
begin
  if wrapped and (DelforParser <> nil) then
    Len := nSpaces + DelforParser.SpacePerIndent
  else
    Len := nSpaces;
  if (Len > 0) then
    GetEString := StrEnd(strSpace(Dest, Len))
  else
    GetEString := Dest;
end;

procedure TLineFeed.GetLength(var Length: Integer);
begin
  if nSpaces > 0 then
    Length := nSpaces
  else
    Length := 0;
end;

function TLineFeed.ReservedType: TReservedType;
begin
  ReservedType := rtLineFeed;
end;

constructor TLineFeed.Create(AOldnSpaces: Integer);
begin
  inherited Create;
  oldnSpaces := AOldnSpaces;
  wrapped := False;
  nSpaces := AOldnSpaces; {default not changed indent}
end;

procedure TLineFeed.IncIndent(n: Integer);
begin
  if DelforParser <> nil then
    inc(nSpaces, n * DelforParser.SpacePerIndent);
end;

procedure TLineFeed.SetIndent(n: Integer);
begin
  if DelforParser <> nil then
    nSpaces := n * DelforParser.SpacePerIndent;
end;
{ TAlignExpression }

constructor TAlignExpression.Create(Like: TExpression; Pos: Byte);
begin
  AlignPos := Pos;
  nSpaces := 0;
  FExpression := StrNew(Like.FExpression);
  FWordType := Like.FWordType;
  FFormatType := Like.FFormatType;
  FReservedType := Like.FReservedType;
end;

procedure TAlignExpression.GetLength(var Length: Integer);
begin
  if Length < AlignPos then
  begin
    nSpaces := AlignPos - Length;
    Length := AlignPos;
    SetSpace(spBefore, False);
  end
  else
    nSpaces := 0;
  inherited GetLength(Length);
end;

function TAlignExpression.GetEString(Dest: PChar): PChar;
begin
  if (nSpaces > 0) then
    Result := StrEnd(strSpace(Dest, nSpaces))
  else
    Result := Dest;
  Result := inherited GetEString(Result);
end;

procedure TDelforParser.SetFillNewWords(AFillNewWords: TFillMode);
begin
  FSettings.FillNewWords := AFillNewWords;
end;

procedure TDelforParser.LoadCapFile(ACapFile: PChar);
var
  InFile: TextFile;
  S: array[0..400] of Char;
  I: Integer;
begin
  if CapNames <> nil then
  begin
    CapNames.FreeAll;
    if (ACapFile <> nil) and FileExists(ACapFile) then
    begin
      AssignFile(InFile, ACapFile);
      try
        Reset(InFile);
        while not Eof(InFile) do
        begin
          Readln(InFile, S);
          if not CapNames.Search(@S, I) then
            CapNames.Insert(I, StrNew(S));
        end;
      finally
        CloseFile(InFile);
      end;
    end
  end;
end;

procedure TDelforParser.SaveCapFile(ACapFile: PChar);
var
  outFile: TextFile;
  I: Integer;
begin
  if (ACapFile <> nil) and (ACapFile^ <> #0) then
  begin
    Assign(outFile, ACapFile);
    try
      Rewrite(outFile);
      for I := 0 to CapNames.Count - 1 do
      begin
        Write(outFile, PChar(CapNames.Items[I]));
        Writeln(outFile);
      end;
    finally
      Close(outFile);
    end;
  end;
end;

function TPascalWord.ChangeComment(commchar: char): boolean;
var
  Buff, buff1: array[0..Maxline] of Char;
begin
  Result := False;
  if ReservedType = rtComment then
  begin
    strCopy(buff, expression);
    strCopy(buff1, expression);
    if strScan(buff, commChar) = nil then
      case commchar of
        '{': if strScan(buff, '}') = nil then
          begin
            StrCopy(buff, '{');
            strCat(buff, buff1);
            strCat(buff, '}');
            Result := True;
          end;
        '(': if strPos(buff, '*)') = nil then
          begin
            StrCopy(buff, '(*');
            strCat(buff, buff1);
            strCat(buff, '*)');
            Result := True;
          end;
        '/': if StrPos(buff, '//') <> @buff[0] then
          begin
            strCat(StrCopy(buff, '//'), buff1);
            Result := True;
          end;
      end;
    expression := buff;
  end;
end;

end.

