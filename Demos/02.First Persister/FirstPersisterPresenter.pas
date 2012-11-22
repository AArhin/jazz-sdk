unit FirstPersisterPresenter;

interface

uses
  JazzDatabaseMechanismIntf,
  JazzInteractor,
  JazzInteractorIntf,
  JazzNotifyIntf,
  JazzVCLPresenter,
  JazzVCLPresenterIntf,
  JazzVCLView,
  JazzVCLViewIntf,
  JazzView,
  JazzViewIntf,
  JazzSessionIntf,
  FirstPersisterClassesIntf,
  FirstPersisterPresenterIntf,
  FirstPersisterViewIntf,
  DB,
  Forms;

type
  TFirstPersisterPresenter = class(TFormPresenter, IFirstPersisterPresenter)
  private
    FDBXSession: ISession;
    FADOSession: ISession;
    FStreamSession: ISession;
    FProject: IFirstPersisterType;
    FPersonListPresenter: IListViewPresenter;
    FAddressListPresenter: IListViewPresenter;
  protected
    function DBXSession: ISession;
    function ADOSession: ISession;
    function StreamSession: ISession;
    function Project: IFirstPersisterType;
    procedure CreateDetails;
    procedure InitializeSession;
    procedure SetDefaults;
  public
    constructor Create; reintroduce; virtual;
  end;

  TSelectActiveViewInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TPostInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TSessionInteractor = class(TInteractor)
  private
    procedure ExecuteStatement(const Mechanism: IDatabaseMechanism;
      const Command: string; const Params: TParams = nil);
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TLogInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TClearLogInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TSaveToFileInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

  TLoadFromFileInteractor = class(TInteractor)
  protected
    procedure Execute(Sender: TObject); override;
  end;

const
  DataViewIndex = 0;
  HistoryViewIndex = 1;
  HelpViewIndex = 2;

  ADOSessionIndex = 10;
  DBXSessionIndex = 11;
  ClearCacheIndex = 12;

var
  ApplicationPresenter: IFirstPersisterPresenter;  

implementation

uses
  JazzADOMechanism,
  JazzDBXFirebirdMechanism,
  JazzIntfUtils,
  JazzStreamMechanism,
  JazzStreamMechanismIntf,
  JazzMasterDetailIntf,
  JazzModel,
  JazzModelIntf, 
  JazzPersister,
  JazzPresenter, 
  JazzPresenterIntf,
  JazzSession,
  JazzValueTypeIntf,
  JazzVCLInteractor,
  JazzVCLUtils,
  SetupDBConnection,
  FirstPersisterClasses,
  FirstPersisterView,
  ComCtrls,
  Controls,
  MainForm,
  DatabaseSchemaPresenter,
  EditPersonPresenter,
  Menus,
  SysUtils,
  Dialogs;

{ TFirstPersisterPresenter }

constructor TFirstPersisterPresenter.Create();
begin
  FProject:= TFirstPersisterType.Create;
  Application.CreateForm(TFormMain, FormMain);
  SetDefaults;

  inherited Create(nil,
    TMemberModel.Create(FProject),
    TFormView.Create(FormMain)
  );
  ApplicationPresenter:= Self;

  InitializeSession;
  CreateDetails;
end;

procedure TFirstPersisterPresenter.CreateDetails;
var
  LStatusDetail: IPresenter;
begin
  FPersonListPresenter:= TListViewPresenter.Create(FProject.PersonListMaster,
    FormMain.ListViewPerson);

  Add(FPersonListPresenter);
  with FPersonListPresenter do
  begin
    Add(FormMain.ListViewPerson.Columns[0], 'ID');
    Add(FormMain.ListViewPerson.Columns[1], 'Name');
    Add(FormMain.ListViewPerson.Columns[2], 'Document');
  end;

  with FormMain, FPersonListPresenter.Interactor do
  begin
    // load data
    ToolButtonLoadData.OnClick:= Add(TObjectListLoadInteractor).OnExecute;
    MenuItemLoadData.OnClick:= Add(TObjectListLoadInteractor).OnExecute;
    // clear data
    MenuItemClearData.OnClick:= Add(TObjectListClearInteractor).OnExecute;

    // Edit
    ToolButtonInsert.OnClick:= Add(TNewPersonInteractor).OnExecute;
    ToolButtonEdit.OnClick:= Add(TEditPersonInteractor).OnExecute;

    // delete
    MenuItemDeleteSelected.OnClick:= Add(TObjectListDeleteInteractor).OnExecute;
    ToolButtonDelete.OnClick:= Add(TObjectListDeleteInteractor).OnExecute;
    MenuItemCancelDeleting.OnClick:= Add(TObjectListCancelDeleteInteractor).OnExecute;

    // Post
    FormMain.ToolButtonPost.OnClick:= Add(TPostInteractor).OnExecute;
  end;

  FAddressListPresenter:= TListViewPresenter.Create(FProject.AddressListDetail,
    FormMain.ListViewAddress);

  Add(FAddressListPresenter);
  with FAddressListPresenter do
  begin
    Add(FormMain.ListViewAddress.Columns[0], 'ID');
    Add(FormMain.ListViewAddress.Columns[1], 'Address1');
    Add(FormMain.ListViewAddress.Columns[2], 'Address2');
    Add(FormMain.ListViewAddress.Columns[3], 'City');
    Add(FormMain.ListViewAddress.Columns[4], 'IDOwner');
  end;

// link master-detail
  (FAddressListPresenter as IDetail).SetMaster(FPersonListPresenter, 'AddressList');

// StatusBar
  Add(TCustomObjectListPresenter.Create(Self, FPersonListPresenter.Model,
    TObjectListStatusView.Create(FormMain.StatusBar.Panels[1], 'Persons:')));

  LStatusDetail:= TCustomObjectListPresenter.Create(Self, FAddressListPresenter.Model,
    TObjectListStatusView.Create(FormMain.StatusBar.Panels[2], 'Address:'));
  Add(LStatusDetail);
  (LStatusDetail as IDetail).SetMaster(FPersonListPresenter, 'AddressList');

  Add(TSimpleControlPresenter.Create(Self,
    TMemoModel.Create(FProject.Member['History']),
    TEditMemoView.Create(FormMain.MemoHistory))
  );
  
  Add(TSimpleControlPresenter.Create(Self,
    TMemoModel.Create(FProject.Member['ActiveSession']),
    TStatusPanelView.Create(FormMain.StatusBar.Panels[3])));

  with FormMain do
  begin
    ToolButtonHome.OnClick:= Interactor.Add(TSelectActiveViewInteractor).OnExecute;
    ToolButtonHistory.OnClick:= Interactor.Add(TSelectActiveViewInteractor).OnExecute;
    ToolButtonHelp.OnClick:= Interactor.Add(TSelectActiveViewInteractor).OnExecute;

    MenuItemClose.OnClick:= Interactor.Add(TApplicationTerminateInteractor).OnExecute;

    MenuItemADOMechanism.OnClick:= Interactor.Add(TSessionInteractor).OnExecute;
    MenuItemDBXMechanism.OnClick:= Interactor.Get(TSessionInteractor).OnExecute;
    MenuItemClearCache.OnClick:= Interactor.Get(TSessionInteractor).OnExecute;
    Interactor.Get(TSessionInteractor).Execute(FormMain.MenuItemDBXMechanism);

    MenuItemLogHistory.OnClick:= Interactor.Add(TLogInteractor).OnExecute;
    MenuItemClearHistory.OnClick:= Interactor.Add(TClearLogInteractor).OnExecute;
    MenuItemGenerateSchema.OnClick:= Interactor.Add(TDatabaseSchemaInteractor).OnExecute;

    MenuItemOpenFile.OnClick:= Interactor.Add(TLoadFromFileInteractor).OnExecute;
    MenuItemSaveToFile.OnClick:= Interactor.Add(TSaveToFileInteractor).OnExecute;
  end;

end;

function TFirstPersisterPresenter.ADOSession: ISession;
begin
  Result:= FADOSession;
end;

function TFirstPersisterPresenter.DBXSession: ISession;
begin
  Result:= FDBXSession;
end;

function TFirstPersisterPresenter.Project: IFirstPersisterType;
begin
  Result:= FProject;
end;

function TFirstPersisterPresenter.StreamSession: ISession;
begin
  Result:= FStreamSession;
end;

procedure TFirstPersisterPresenter.InitializeSession;
const       
  mdbfile = '..\00.Common\DB\JazzDBDemos.mdb';
  ADOConnectionString = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;Persist Security Info=False';
begin
  // First Session is auto defined as "ActiveSession"
  FDBXSession:= NewSession(TDBXFirebirdMechanism);
  SetupDBXConnection((FDBXSession.Mechanism as IDatabaseMechanism).Connection);
  FDBXSession.Mechanism.Connected:= True;

  FADOSession:= NewSession(TADOMechanism);
  (FADOSession.Mechanism as IADOMechanism).SetConnectionString(
    Format(ADOConnectionString, [mdbfile]), EmptyStr, EmptyStr);
  FADOSession.Mechanism.Connected:= True;

  FStreamSession:= NewSession(TStreamMechanism, cmNoCache);
  FProject.FileName:= (FStreamSession.Mechanism as IStreamMechanism).GetFilePath(
    FProject.PersonListMaster);

  ActiveSession:= FDBXSession;
end;

procedure TFirstPersisterPresenter.SetDefaults;
begin
  with FormMain do
  begin
    KeyPreview:= True;
    PageControlHideTabs(PageControl);

    ToolButtonHome.Tag:= DataViewIndex;
    ToolButtonHistory.Tag:= HistoryViewIndex;
    ToolButtonHelp.Tag:= HelpViewIndex;

    MenuItemADOMechanism.Tag:= ADOSessionIndex;
    MenuItemDBXMechanism.Tag:= DBXSessionIndex;
    MenuItemClearCache.Tag:= ClearCacheIndex;
  end;
end;


{ TSelectActiveViewInteractor }

procedure TSelectActiveViewInteractor.Execute(Sender: TObject);
var
  LCaption: string;
  LIndex: Cardinal;
begin
  LIndex:= TToolButton(Sender).Tag;
   
  case LIndex of
    DataViewIndex: LCaption:= 'Home';
    HistoryViewIndex: LCaption:= 'History';
    HelpViewIndex: LCaption:= 'Help';
  end;

  with FormMain do
  begin
    PageControl.ActivePageIndex:= LIndex;
    ImageTitle.Picture:= nil;
    ImageListTitle.GetBitmap(LIndex, ImageTitle.Picture.Bitmap);
    ImageTitle.Refresh;
    LabelTitle.Caption:= LCaption;
  end;
end;

{ TSessionInteractor }

procedure TSessionInteractor.Execute(Sender: TObject);
begin
  inherited;
  case TMenuItem(Sender).Tag of
    ADOSessionIndex:
      begin
        ActiveSession:= ApplicationPresenter.ADOSession;
        (ActiveSession.Mechanism as IDatabaseMechanism).OnExecuteStatement:= ExecuteStatement;
        ApplicationPresenter.Project.ActiveSession:= 'ActiveSession: ADOSession';
      end;
    DBXSessionIndex:
      begin
        ActiveSession:= ApplicationPresenter.DBXSession;
        (ActiveSession.Mechanism as IDatabaseMechanism).OnExecuteStatement:= ExecuteStatement;
        ApplicationPresenter.Project.ActiveSession:= 'ActiveSession: DBXSession';
      end;
    ClearCacheIndex:
      begin
        if MessageDlg(Format('Remove %d objects from cache?',
          [ActiveSession.Cache.Count]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          ActiveSession.ClearCache;
      end;
  end;
end;

procedure TSessionInteractor.ExecuteStatement(
  const Mechanism: IDatabaseMechanism; const Command: string;
  const Params: TParams);
begin
  with ApplicationPresenter.Project do
  begin
  if LogHistory then
     History.Add(DateTimeToStr(Now) + ' - ' + Command);
  end;
end;

{ TLogInteractor }

procedure TLogInteractor.Execute(Sender: TObject);
begin
  ApplicationPresenter.Project.LogHistory:= TMenuItem(Sender).Checked;
end;

{ TClearLogInteractor }

procedure TClearLogInteractor.Execute(Sender: TObject);
begin
  ApplicationPresenter.Project.History.Clear;
end;

{ TSaveInteractor }

procedure TPostInteractor.Execute(Sender: TObject);
begin
  inherited;
  ActiveSession.Save(ApplicationPresenter.Project.PersonListMaster);
end;

{ TSaveToFileInteractor }

procedure TSaveToFileInteractor.Execute(Sender: TObject);
begin
  inherited;
  ApplicationPresenter.StreamSession.Save(ApplicationPresenter.Project.PersonListMaster);
end;

{ TLoadFromFileInteractor }

procedure TLoadFromFileInteractor.Execute(Sender: TObject);
begin
  inherited;
  ApplicationPresenter.StreamSession.Load(ApplicationPresenter.Project.PersonListMaster);
end;

end.




