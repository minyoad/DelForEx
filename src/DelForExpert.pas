unit DelForExpert;

{$I DelForEx.inc}

interface

uses
  Classes, Forms, Menus
{$IFDEF DELPHI6_UP}
  , ToolsAPI
{$ENDIF}
{$IFNDEF Delphi9_UP}
  , WinProcs, ExptIntf, ToolIntf, VirtIntf
{$ENDIF};

type

  TIDETextExpert = class({$IFDEF Delphi9_UP}TNotifierObject, IOTANotifier, IOTAWizard{$ELSE}TIExpert{$ENDIF})
  private
{$IFNDEF Delphi9_UP}
    MainMenu: TIMainMenuIntf;
    MenuItem, MenuItem1: TIMenuItemIntf;
{$ELSE}
    MainMenu: TMainMenu;
    MenuItem, MenuItem1: TMenuItem;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnMenuItem1Click(Sender: TObject);
{$ENDIF}

    function GetMenuShortCut: TShortCut;
    procedure SetMenuShortCut(AShortCut: TShortCut);
    function GetMenu1ShortCut: TShortCut;
    procedure SetMenu1ShortCut(AShortCut: TShortCut);
  protected
{$IFNDEF Delphi9_UP}
    procedure OnClick(Sender: TIMenuItemIntf); virtual;
    procedure OnClick1(Sender: TIMenuItemIntf); virtual;
{$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
{$IFDEF Delphi9_UP}
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
{$ELSE}
    function GetName: string; override;
    function GetStyle: TExpertStyle; override;
    function GetIDString: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetState: TExpertState; override;
    function GetMenuText: string; override;
    procedure Execute; override;
{$ENDIF}
    property MenuShortCut: TShortCut read GetMenuShortCut write SetMenuShortCut;
  end;

{$IFDEF Delphi9_UP}
function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;

exports
  InitWizard Name WizardEntryPoint;
{$ELSE}
function InitExpert(ToolServices: TIToolServices; RegisterProc:
  TExpertRegisterProc;
  var Terminate: TExpertTerminateProc): Boolean; stdcall;

{$IFDEF DELPHI6_UP}
function WzdEntryPoint(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean stdcall;
{$ENDIF}
exports
  InitExpert Name ExpertEntryPoint{$IFDEF DELPHI6_UP}, WzdEntryPoint Name WizardEntryPoint{$ENDIF};
{$ENDIF}

var
  IDETextExpert: TIDETextExpert = nil;
function HasShortCut(var Value: TShortCut): Boolean;
implementation

uses DelExpert
{$IFDEF DEBUG}, CnDebug{$ENDIF};

function HasShortCut(var Value: TShortCut): Boolean;
label
  _Top;
var
  I, ICount: Integer;
  C, D: Word;
begin
  Result := False;
  if IDETextExpert = nil then Exit;
  if IDETextExpert.MainMenu = nil then Exit;
  D := Word(Value);
  _Top:
{$IFNDEF DELPHI9_UP}
  ICount := IDETextExpert.MainMenu.GetMenuItems.GetItemCount;
{$ELSE}
  ICount := IDETextExpert.MainMenu.Items.Count;
{$ENDIF}
  for I := 0 to ICount - 1 do
  begin
{$IFNDEF DELPHI9_UP}
    C := Word(IDETextExpert.MainMenu.GetMenuItems.GetShortCut);
{$ELSE}
    C := Word(IDETextExpert.MainMenu.Items.ShortCut);
{$ENDIF}
    if C = D then
    begin
      Inc(D);
      goto _Top;
    end;
  end;
  if D <> Word(Value) then
  begin
    Value := TShortCut(D);
    Result := True;
  end;
end;

{ TIDETextExpert code }

function TIDETextExpert.GetName: string;
begin
  Result := 'DelForExpert'
end;

{$IFNDEF Delphi9_UP}

function TIDETextExpert.GetAuthor: string;
begin
  Result := 'Egbert_van_Nes';
end;

function TIDETextExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn;
end;

function TIDETextExpert.GetMenuText: string;
begin
  Result := '源码格式化(&S)...';
end;

function TIDETextExpert.GetComment: string;
begin
  Result := 'Delphi 源码格式化工具';
end;

function TIDETextExpert.GetPage: string;
begin
end;

function TIDETextExpert.GetGlyph: HICON;
begin
  Result := 0;
end;

function TIDETextExpert.GetState: TExpertState;
begin
  Result := [];
end;
{$ELSE}

function TIDETextExpert.GetState: TWizardState;
begin
  Result := [];
end;
{$ENDIF}

function TIDETextExpert.GetIDString: string;
begin
  Result := 'e_van_nes.DelForExpert';
end;

constructor TIDETextExpert.Create;
var
{$IFDEF DElphi9_UP}
  Menu: TMenuItem;
  I: Integer;
{$ELSE}
  ReferenceMenuItem: TIMenuItemIntf;
  Menu: TIMenuItemIntf;
{$ENDIF}
begin
  inherited Create;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('TIDETextExpert Createing.');
{$ENDIF}
{$IFDEF Delphi9_UP}
  Application.Handle := (BorlandIDEServices as IOTAServices).GetParentHandle;
  Menu := nil;
  MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  for I := 0 to MainMenu.Items.Count - 1 do
  begin
    if AnsiSameCaption(MainMenu.Items[I].Name, 'ToolsMenu') then
    begin
      Menu := MainMenu.Items[I];
      Break;
    end;
  end;

  if Assigned(Menu) then
  begin
    MenuItem := TMenuItem.Create(Menu);
    MenuItem.Caption := '源码格式化(&S)...';
    MenuItem.OnClick := OnMenuItemClick;
    MenuItem.ShortCut := ShortCut(Word('D'), [ssCtrl,ssShift]);

    MenuItem1 := TMenuItem.Create(Menu);
    MenuItem1.Caption := '格式化当前文件(&F)...';
    MenuItem1.OnClick := OnMenuItem1Click;
    MenuItem1.ShortCut := ShortCut(Word('D'), [ssCtrl]);

    // find first separator
    for I := 0 to Menu.Count - 1 do
    begin
      if Menu.Items[I].IsLine then
      begin
        Menu.Insert(I, MenuItem);
        Menu.Insert(I, MenuItem1);
        Break;
      end;
    end;
  end;
{$ELSE}
  MenuItem := nil;
  MenuItem1 := nil;
  if ToolServices <> nil then
  begin
    MainMenu := ToolServices.GetMainMenu;
    if MainMenu <> nil then
    begin
      try
        ReferenceMenuItem := MainMenu.FindMenuItem('ToolsOptionsItem');
        if ReferenceMenuItem <> nil then
        try
          Menu := ReferenceMenuItem.GetParent;
          if Menu <> nil then
          try
            MenuItem := Menu.InsertItem(ReferenceMenuItem.GetIndex + 1,
              GetMenuText,
              'FormatExpertItem', '',
              0, 0, 0,
              [mfEnabled, mfVisible], OnClick);
            MenuItem.SetShortCut(ShortCut(Word('D'), [ssCtrl]));

            MenuItem1 := Menu.InsertItem(ReferenceMenuItem.GetIndex + 1,
              GetMenu1Text,
              'FormatCurrExpertItem', '',
              0, 0, 0,
              [mfEnabled, mfVisible], OnClick1);
            MenuItem1.SetShortCut(ShortCut($9, [ssCtrl, ssAlt]));
          finally
            Menu.DestroyMenuItem;
          end;
        finally
          ReferenceMenuItem.DestroyMenuItem;
        end;
      finally
        //MainMenu.Free;
      end;
    end;
  end;
{$ENDIF}

  DelExpertDlg := TDelExpertDlg.Create(Application);

{$IFDEF DEBUG}
  CnDebugger.LogMsg('TIDETextExpert Created.');
{$ENDIF}
end;

procedure TIDETextExpert.SetMenu1ShortCut(AShortCut: TShortCut);
begin
{$IFDEF Delphi9_UP}
  MenuItem1.ShortCut := AShortCut;
{$ELSE}
  MenuItem1.SetShortCut(AShortCut);
{$ENDIF}
end;

procedure TIDETextExpert.SetMenuShortCut(AShortCut: TShortCut);
begin
{$IFDEF Delphi9_UP}
  MenuItem.ShortCut := AShortCut;
{$ELSE}
  MenuItem.SetShortCut(AShortCut);
{$ENDIF}
end;

function TIDETextExpert.GetMenu1ShortCut: TShortCut;
begin
  if MenuItem1 <> nil then
{$IFDEF Delphi9_UP}
    Result := MenuItem1.ShortCut
{$ELSE}
    Result := MenuItem1.GetShortCut
{$ENDIF}
  else
    Result := ShortCut($9, [ssCtrl, ssAlt]);
end;

function TIDETextExpert.GetMenuShortCut: TShortCut;
begin
  if MenuItem <> nil then
{$IFDEF Delphi9_UP}
    Result := MenuItem.ShortCut
{$ELSE}
    Result := MenuItem.GetShortCut
{$ENDIF}
  else
    Result := ShortCut(Word('D'), [ssCtrl]);
end;

destructor TIDETextExpert.Destroy;
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('TIDETextExpert Destroying.');
{$ENDIF}
{$IFNDEF Delphi9_UP}
  if MenuItem <> nil then MenuItem.DestroyMenuItem;
  if MenuItem1 <> nil then MenuItem.DestroyMenuItem;
  if MainMenu <> nil then MainMenu.Free;
{$ENDIF}
  inherited Destroy;
{$IFDEF DEBUG}
  CnDebugger.LogMsg('TIDETextExpert Destroyed.');
{$ENDIF}
end; {Destroy}

{$IFNDEF Delphi9_UP}

procedure TIDETextExpert.OnClick(Sender: TIMenuItemIntf);
begin
  Execute;
end;

procedure TIDETextExpert.OnClick1(Sender: TIMenuItemIntf);
begin
  onMenuItem1Click(nil);
end;
{$ENDIF}

procedure TIDETextExpert.Execute;
begin
  if not Assigned(DelExpertDlg) then
    DelExpertDlg := TDelExpertDlg.Create(Application);
  //DelExpertDlg.ActiveControl := DelExpertDlg.CurrentBtn;
  DelExpertDlg.ShowModal;
end;

{$IFDEF Delphi9_UP}

procedure TIDETextExpert.OnMenuItem1Click(Sender: TObject);
begin
//  Execute;
  if not Assigned(DelExpertDlg) then
    DelExpertDlg := TDelExpertDlg.Create(Application);

  DelExpertDlg.Show;

  DelExpertDlg.CurrentBtn.Click;
end;

procedure TIDETextExpert.OnMenuItemClick(Sender: TObject);
begin
  Execute;
end;

const
  InvalidIndex = -1;

var
  FWizardIndex: Integer = InvalidIndex;

  // 专家DLL释放过程

procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  if FWizardIndex <> InvalidIndex then
  begin
    Assert(Assigned(BorlandIDEServices));
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));
    WizardServices.RemoveWizard(FWizardIndex);
    FWizardIndex := InvalidIndex;
  end;
end;

// 专家DLL初始化入口函数

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
{$IFDEF DEBUG}
  CnDebugger.LogMsg('Wizard dll entry');
{$ENDIF}

  Result := BorlandIDEServices <> nil;
  if Result then
  begin
    //Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);
    ToolsAPI.BorlandIDEServices := BorlandIDEServices;
    Terminate := FinalizeWizard;
    WizardServices := BorlandIDEServices as IOTAWizardServices;
    Assert(Assigned(WizardServices));
    IDETextExpert := TIDETextExpert.Create;
    FWizardIndex := WizardServices.AddWizard(IDETextExpert as IOTAWizard);
    Result := (FWizardIndex >= 0);

{$IFDEF DEBUG}
    CnDebugger.LogBoolean(Result, 'WizardMgr registered');
{$ENDIF}
  end;
end;

{$ELSE}

function InitExpert(ToolServices: TIToolServices; RegisterProc:
  TExpertRegisterProc;
  var Terminate: TExpertTerminateProc): Boolean;
begin
  Result := True;
  try
    ExptIntf.ToolServices := ToolServices;
    Application.Handle := ToolServices.GetParentHandle;
    IDETextExpert := TIDETextExpert.Create;
    RegisterProc(IDETextExpert);
  except
    ToolServices.RaiseException(ReleaseException);
  end;
end;

{$IFDEF DELPHI6_UP}

function WzdEntryPoint(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean;
begin
  ToolsAPI.BorlandIDEServices := BorlandIDEServices;
  Result := True;
end;
{$ENDIF}

{$ENDIF}
end.

