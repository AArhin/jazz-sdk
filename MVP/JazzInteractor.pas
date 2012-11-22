unit JazzInteractor;

interface

uses
  Classes,
  JazzClasses,
  JazzCommandSetIntf,
  JazzInteractorIntf,
  JazzModelIntf,
  JazzNotify,
  JazzNotifyIntf,
  JazzObserverIntf,
  JazzSessionIntf,
  JazzViewIntf;

type
  TInteractorClass = class of TInteractor;
  TInteractor = class(TCustomObject, IInteractor)
  private
    FIsActive: Boolean;
    FIsDone: Boolean;
    FModel: Pointer;
    FOnExecute: TNotifyEvent;
    FView: Pointer;
  protected
    function GetOnExecute: TNotifyEvent;
    procedure SetOnExecute(const Value: TNotifyEvent);

    function GetModel: IModel;
    function GetView: IView;

    function IsActive: Boolean; virtual;
    function IsDone: Boolean; virtual;
    procedure SetDone(Value: Boolean); virtual;

    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure Execute(Sender: TObject); virtual;

    property Model: IModel read GetModel;
    property View: IView read GetView;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); reintroduce;
      virtual;
    property OnExecute: TNotifyEvent read GetOnExecute write SetOnExecute;
  end;

  IInteractorItemList = interface(INamedInterfaceList)
    ['{26D7BA6A-B8B1-4684-850E-F5D7EBA9C678}']
  end;

  TInteractorItemList = class(TNamedInterfaceList, IInteractorItemList);

  TInteractorList = class(TCustomObject, IInteractorList, IObserver)
  private
    FItems: IInteractorItemList;
    FModel: Pointer;
    FView: Pointer;
  protected
    // IObserver
    procedure Update(const Notification: IObjectEvent); virtual;

    // IInteractorList
    function GetModel: IModel;
    function GetView: IView;
    function GetItems(const Index: Integer): IInteractor;

    function Add(const Interactor: IInteractor): Integer; overload;
    function Add(const InteractorClass: TClass): IInteractor; overload;
    function Count: Integer;
    function Get(const InteractorClass: TClass): IInteractor;
    procedure Clear;
    procedure Delete(const Index: Integer);

    property Items[const Index: Integer]: IInteractor read GetItems; default;
    property Model: IModel read GetModel;
    property View: IView read GetView;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); reintroduce;
      virtual;
    destructor Destroy; override;
  end;

  TAddSelectionInteractor = class(TInteractor)
  private
    procedure HandleAddSelection(const Sender: IInterface = nil);
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TRemoveSelectionInteractor = class(TInteractor)
  private
    procedure HandleRemoveSelection(const Sender: IInterface = nil);
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TClearSelectionInteractor = class(TInteractor)
  private
    procedure HandleClearSelection(const Sender: IInterface = nil);
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TObjectListLoadInteractor = class(TInteractor)
  private
    FLoadCommand: ICommand;
  protected
    procedure Execute(Sender: TObject); override;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
  end;

  TObjectListClearInteractor = class(TInteractor)
  private
    FClearCommand: ICommand;
    procedure HandleObjectListClear(const Sender: IInterface = nil);
  protected
    procedure Execute(Sender: TObject); override;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TObjectListPostInteractor = class(TInteractor)
  private
    FPostCommand: ICommand;
    procedure HandleObjectListPost(const Sender: IInterface = nil);
  protected
    procedure Execute(Sender: TObject); override;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TObjectListAddInteractor = class(TInteractor)
  private
    FAddCommand: ICommand;
    procedure HandleObjectListAdd(const Sender: IInterface = nil);
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TObjectListInsertInteractor = class(TInteractor)
  private
    FInsertCommand: ICommand;
    procedure HandleObjectListInsert(const Sender: IInterface = nil);
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TObjectListDeleteInteractor = class(TInteractor)
  private
    FDeleteCommand: ICommand;
    procedure HandleObjectListDelete(const Sender: IInterface = nil);
  protected
    procedure Execute(Sender: TObject); override;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TObjectListCancelDeleteInteractor = class(TInteractor)
  private
    FCancelDeleteCommand: ICommand;
  protected
    procedure Execute(Sender: TObject); override;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
  end;

  TBooleanInteractor = class(TInteractor)
  protected
    procedure HandleStateChange(const Sender: IInterface = nil); virtual;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

implementation

uses
  JazzIntfUtils,
  JazzMasterDetailIntf,
  JazzSubjectIntf,
  JazzValueTypeIntf,
  SysUtils;

{ TInteractor }

function TInteractorList.Add(const Interactor: IInteractor): Integer;
begin
  Result := FItems.Add(InterfaceToObject(Interactor).ClassName, Interactor);
end;

function TInteractorList.Add(const InteractorClass: TClass): IInteractor;
begin
  Result := Get(InteractorClass);
  if Result = nil then
  begin
    Result := TInteractorClass(InteractorClass).Create(Owner, Model, View);
    Add(Result);
  end;
end;

procedure TInteractorList.Clear;
begin
  FItems.Clear;
end;

function TInteractorList.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TInteractorList.Create(const AOwner: IInterface; const Model: IModel; const View: IView);
var
  LSubject: ISubject;
begin
  inherited Create(AOwner);
  FItems := TInteractorItemList.Create;
  FModel := Pointer(Model);
  FView := Pointer(View);

  if Supports(View, ISubject, LSubject) then LSubject.Attach(Self);
end;

procedure TInteractorList.Delete(const Index: Integer);
begin
  FItems.Delete(Index);
end;

destructor TInteractorList.Destroy;
var
  LSubject: ISubject;
begin
  if Supports(View, ISubject, LSubject) then LSubject.Detach(Self);
  Clear;
  inherited;
end;

function TInteractorList.Get(const InteractorClass: TClass): IInteractor;
begin
  Result := FItems.Item[InteractorClass.ClassName] as IInteractor;
end;

function TInteractorList.GetItems(const Index: Integer): IInteractor;
begin
  Result := FItems[Index] as IInteractor;
end;

constructor TBooleanInteractor.Create(const AOwner: IInterface; const Model: IModel; const View:
  IView);
var
  LView: IBooleanTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IBooleanTypeView, LView) then
    LView.OnChanged := HandleStateChange;
end;

destructor TBooleanInteractor.Destroy;
var
  LView: IBooleanTypeView;
begin
  if Supports(View, IBooleanTypeView, LView) then LView.OnChanged := nil;
  inherited Destroy;
end;

procedure TBooleanInteractor.HandleStateChange(const Sender: IInterface = nil);
var
  LBoolean: IBooleanType;
begin
  if Supports(Sender, IBooleanType, LBoolean) then
    (Model.ObjectValue as IBooleanType).Value := LBoolean.Value
end;

{ TInteractor }

procedure TInteractor.Activate;
begin
  FIsActive := True;
end;

constructor TInteractor.Create(const AOwner: IInterface; const Model: IModel; const View: IView);
begin
  inherited Create(AOwner);
  FModel := Pointer(Model);
  FView := Pointer(View);
  OnExecute := Execute;
end;

procedure TInteractor.Deactivate;
begin
  FIsActive := False;
end;

procedure TInteractor.Execute(Sender: TObject);
begin
  //TODO: catch notifications
end;

function TInteractor.GetModel: IModel;
begin
  Result := IModel(FModel);
end;

function TInteractor.GetOnExecute: TNotifyEvent;
begin
  Result := FOnExecute;
end;

function TInteractor.GetView: IView;
begin
  Result := IView(FView);
end;

function TInteractor.IsActive: Boolean;
begin
  Result := FIsActive;
end;

function TInteractor.IsDone: Boolean;
begin
  Result := FIsDone;
end;

procedure TInteractor.SetDone(Value: Boolean);
begin
  FIsDone := Value;
end;

function TInteractorList.GetModel: IModel;
begin
  Result := IModel(FModel);
end;

function TInteractorList.GetView: IView;
begin
  Result := IView(FView);
end;

procedure TInteractor.SetOnExecute(const Value: TNotifyEvent);
begin
  FOnExecute := Value;
end;

{ TAddSelectionInteractor }

constructor TAddSelectionInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IValueTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IValueTypeView, LView) then
    LView.OnAddSelection := HandleAddSelection;
end;

destructor TAddSelectionInteractor.Destroy;
var
  LView: IValueTypeView;
begin
  if Supports(View, IValueTypeView, LView) then
    LView.OnAddSelection := nil;
  inherited;
end;

procedure TAddSelectionInteractor.HandleAddSelection(const Sender: IInterface);
begin
  Model.Selection.Clear;
  Model.Selection.Add((Sender as IObjectEvent).Param);
  (Owner as IMaster).ItemChanged((Sender as IObjectEvent).Param);
end;

{ TRemoveSelectionInteractor }

constructor TRemoveSelectionInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IValueTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IValueTypeView, LView) then
    LView.OnRemoveSelection := HandleRemoveSelection;
end;

destructor TRemoveSelectionInteractor.Destroy;
var
  LView: IValueTypeView;
begin
  if Supports(View, IValueTypeView, LView) then LView.OnRemoveSelection := nil;
  inherited;
end;

procedure TRemoveSelectionInteractor.HandleRemoveSelection(const Sender: IInterface);
begin
  Model.Selection.RemoveItem((Sender as IObjectEvent).Param);
end;

{ TClearSelectionInteractor }

constructor TClearSelectionInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IValueTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IValueTypeView, LView) then
    LView.OnClearSelection := HandleClearSelection;
end;

destructor TClearSelectionInteractor.Destroy;
var
  LView: IValueTypeView;
begin
  if Supports(View, IValueTypeView, LView) then LView.OnClearSelection := nil;
  inherited;
end;

procedure TClearSelectionInteractor.HandleClearSelection(const Sender: IInterface);
begin
  Model.Selection.Clear;
end;

{ TClearInteractor }

constructor TObjectListClearInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IObjectListTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IObjectListTypeView, LView) then
    LView.OnClear := HandleObjectListClear;
  FClearCommand := Model.CommandSet.Item['ObjectListClear']; // do not localize
end;

destructor TObjectListClearInteractor.Destroy;
var
  LView: IObjectListTypeView;
begin
  if Supports(View, IObjectListTypeView, LView) then LView.OnClear := nil;
  inherited;
end;

procedure TObjectListClearInteractor.Execute(Sender: TObject);
begin
  HandleObjectListClear(nil);
end;

procedure TObjectListClearInteractor.HandleObjectListClear(const Sender: IInterface);
var
  LObjectList: IObjectListType;
begin
  if Supports(Model.ObjectValue, IObjectListType, LObjectList) then
  begin
    (Owner as IMaster).ItemChanged(nil);
    Model.Selection.Clear;
  end;

  if FClearCommand <> nil then
    FClearCommand.Execute(Model.ObjectValue);
end;

{ TObjectListAddInteractor }

constructor TObjectListAddInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IObjectListTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IObjectListTypeView, LView) then
    LView.OnAppend := HandleObjectListAdd;

  FAddCommand := Model.CommandSet.Item['ObjectListAppend']; // do not localize
end;

destructor TObjectListAddInteractor.Destroy;
var
  LView: IObjectListTypeView;
begin
  FAddCommand := nil;
  if Supports(View, IObjectListTypeView, LView) then LView.OnAppend := nil;
  inherited;
end;

procedure TObjectListAddInteractor.HandleObjectListAdd(
  const Sender: IInterface);
begin
  if FAddCommand <> nil then
  begin
    (Sender as IObjectEvent).ClearParams;
    (Sender as IObjectEvent).AddParam(Model);
    FAddCommand.Execute(Sender);
  end;
end;

{ TObjectListInsertInteractor }

constructor TObjectListInsertInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IObjectListTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IObjectListTypeView, LView) then
    LView.OnInsert := HandleObjectListInsert;

  FInsertCommand := Model.CommandSet.Item['ObjectListInsert']; // do not localize
end;

destructor TObjectListInsertInteractor.Destroy;
var
  LView: IObjectListTypeView;
begin
  FInsertCommand := nil;
  if Supports(View, IObjectListTypeView, LView) then LView.OnInsert := nil;
  inherited;
end;

procedure TObjectListInsertInteractor.HandleObjectListInsert(
  const Sender: IInterface);
begin
  if FInsertCommand <> nil then
  begin
    (Sender as IObjectEvent).ClearParams;
    (Sender as IObjectEvent).AddParam(Model);
    FInsertCommand.Execute(Sender);
  end;
end;

{ TObjectListDeleteInteractor }

constructor TObjectListDeleteInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IObjectListTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IObjectListTypeView, LView) then
    LView.OnDelete := HandleObjectListDelete;
  FDeleteCommand := Model.CommandSet.Item['ObjectListDelete']; // do not localize
end;

destructor TObjectListDeleteInteractor.Destroy;
var
  LView: IObjectListTypeView;
begin
  FDeleteCommand := nil;
  if Supports(View, IObjectListTypeView, LView) then LView.OnDelete := nil;
  inherited;
end;

procedure TObjectListDeleteInteractor.Execute(Sender: TObject);
begin
  if FDeleteCommand <> nil then
    FDeleteCommand.Execute(NewEvent(TRemoveEvent, Self, [Model]));
end;

procedure TObjectListDeleteInteractor.HandleObjectListDelete(
  const Sender: IInterface);
begin
  if FDeleteCommand <> nil then
  begin
    (Sender as IObjectEvent).ClearParams;
    (Sender as IObjectEvent).AddParam(Model);
    FDeleteCommand.Execute(Sender);
  end;
end;

procedure TInteractorList.Update(const Notification: IObjectEvent);
begin
  // TODO: implement Interactor manipulation
end;

{ TObjectListLoadInteractor }

constructor TObjectListLoadInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
begin
  inherited Create(AOwner, Model, View);
  FLoadCommand := Model.CommandSet.Item['ObjectListLoad']; // do not localize
end;

procedure TObjectListLoadInteractor.Execute(Sender: TObject);
begin
  if FLoadCommand <> nil then FLoadCommand.Execute(Model.ObjectValue);
end;

{ TObjectListCancelDeleteInteractor }

constructor TObjectListCancelDeleteInteractor.Create(
  const AOwner: IInterface; const Model: IModel; const View: IView);
begin
  inherited Create(AOwner, Model, View);
  FCancelDeleteCommand := Model.CommandSet.Item['ObjectListCancelDelete']; // do not localize
end;

procedure TObjectListCancelDeleteInteractor.Execute(Sender: TObject);
begin
  if FCancelDeleteCommand <> nil then
    FCancelDeleteCommand.Execute(Model.ObjectValue);
end;

{ TObjectListPostInteractor }

constructor TObjectListPostInteractor.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
var
  LView: IObjectListTypeView;
begin
  inherited Create(AOwner, Model, View);
  if Supports(View, IObjectListTypeView, LView) then
    LView.OnPost := HandleObjectListPost;
  FPostCommand := Model.CommandSet.Item['ObjectListPost']; // do not localize
end;

destructor TObjectListPostInteractor.Destroy;
var
  LView: IObjectListTypeView;
begin
  if Supports(View, IObjectListTypeView, LView) then LView.OnPost := nil;
  inherited;
end;

procedure TObjectListPostInteractor.Execute(Sender: TObject);
begin
  HandleObjectListPost(NewEvent(TObjectListEvent, Model.ObjectValue));
end;

procedure TObjectListPostInteractor.HandleObjectListPost(
  const Sender: IInterface);
begin
  if FPostCommand <> nil then
  begin
    (Sender as IObjectEvent).ClearParams;
    (Sender as IObjectEvent).AddParam(Model);
    FPostCommand.Execute(Sender);
  end;
end;

end.

