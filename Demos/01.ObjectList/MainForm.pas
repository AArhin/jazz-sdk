unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Menus, ExtCtrls, StdCtrls, Spin, ActnList,
  JazzPersisterIntf, JazzSessionIntf, ObjectLinkManager;

type
  TFormMain = class(TForm)
    ActionDelete: TAction;
    ActionInsert: TAction;
    ActionList1: TActionList;
    ActionSaveRestore: TAction;
    ButtonDelete: TButton;
    ButtonFile: TButton;
    ButtonInsert: TButton;
    GroupBox1: TGroupBox;
    GroupBoxDelete: TGroupBox;
    GroupBoxInsert: TGroupBox;
    ListView: TListView;
    MainMenu: TMainMenu;
    MenuItemClose: TMenuItem;
    MenuItemFile: TMenuItem;
    Panel3: TPanel;
    PanelToolBar: TPanel;
    RadioButtonAll: TRadioButton;
    RadioButtonSelected: TRadioButton;
    RadioRestore: TRadioButton;
    RadioSave: TRadioButton;
    SpinEditInsert: TSpinEdit;
    StatusBarPersonList: TStatusBar;
    procedure MenuItemCloseClick(Sender: TObject);
    procedure ActionInsertExecute(Sender: TObject);
    procedure ActionInsertUpdate(Sender: TObject);
    procedure ActionDeleteUpdate(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionSaveRestoreExecute(Sender: TObject);
    procedure ActionSaveRestoreUpdate(Sender: TObject);
  private
    { Private declarations }
    FFileName: string;
    FSession: ISession;
    FPersonManager: IObjectManager;
    function GetSession: ISession;
    function GetPersonManager: IObjectManager;
    procedure Log(const Time: Cardinal);
  public
    { Public declarations }
    property Session: ISession read GetSession;
    property PersonManager: IObjectManager read GetPersonManager;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses JazzPersister, JazzStreamMechanismIntf, JazzStreamMechanism, AppConsts,
  BOPerson, PersonManager;

procedure TFormMain.MenuItemCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionInsertExecute(Sender: TObject);
var
  LTime: Cardinal;
begin
  LTime:= GetTickCount;
  PersonManager.Populate(SpinEditInsert.Value);
  Log(GetTickCount - LTime);
end;

procedure TFormMain.ActionInsertUpdate(Sender: TObject);
begin
  // Update Screen
  TCustomAction(Sender).Enabled:= SpinEditInsert.Value > 0;
  ButtonInsert.Default:= (ActiveControl = SpinEditInsert);
end;

procedure TFormMain.ActionDeleteExecute(Sender: TObject);
begin
  if RadioButtonAll.Checked then
  begin
    if (MessageDlg(SDeleteAll, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      PersonManager.ObjectList.Clear;
  end
  else
  begin
    if (MessageDlg(Format(SDeleteSelection, [PersonManager.ListViewObserver.Selection.AsString]),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      PersonManager.ObjectList.RemoveObject(PersonManager.ListViewObserver.Selection);
  end;
  Log(0);
end;

procedure TFormMain.ActionDeleteUpdate(Sender: TObject);
begin
  // Update Screen
  TCustomAction(Sender).Enabled:= (PersonManager.ListViewObserver <> nil) and
   ((RadioButtonAll.Checked or
    (PersonManager.ListViewObserver.Selection <> nil)) and
    (PersonManager.ObjectList.Count > 0));
    
  ButtonDelete.Default:= (ActiveControl = RadioButtonSelected) or
    (ActiveControl = RadioButtonAll);
end;

procedure TFormMain.ActionSaveRestoreExecute(Sender: TObject);
var
  LTime: Cardinal;
begin
  LTime:= GetTickCount;

  if RadioSave.Checked then
    Session.Save(PersonManager.ObjectList)
  else
    Session.Load(PersonManager.ObjectList);

  Log(GetTickCount - LTime);
end;

procedure TFormMain.ActionSaveRestoreUpdate(Sender: TObject);
begin
  if RadioSave.Checked then
    TCustomAction(Sender).Enabled:= (PersonManager.ObjectList.Count > 0)
  else
    TCustomAction(Sender).Enabled:= FileExists(FFileName);
end;

function TFormMain.GetSession: ISession;
begin
  if FSession = nil then
  begin
    FSession:= NewSession(TStreamMechanism);
    with (FSession.Mechanism as IStreamMechanism) do
      FFileName:= GetFilePath(GetFileName(PersonManager.ObjectList));
  end;

  Result:= FSession;
end;

function TFormMain.GetPersonManager: IObjectManager;
begin
  if FPersonManager = nil then
  begin
    CreateDebugColumns:= False;
    FPersonManager:= TPersonManager.Create(Session, TPerson);

    // link ListView->ObjectList using ListViewObserver
    FPersonManager.LinkComponent(ListView);
    // link Status->ObjectList using StatusBarObserver
    FPersonManager.LinkComponent(StatusBarPersonList);
  end;

  Result:= FPersonManager;
end;

procedure TFormMain.Log(const Time: Cardinal);
begin
  if Time = 0 then
    StatusBarPersonList.Panels[1].Text:= ''
  else
    StatusBarPersonList.Panels[1].Text:= Format('Tempo: %dms', [Time]);
end;

end.

