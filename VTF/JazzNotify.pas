unit JazzNotify;

interface

uses
  Classes,
  JazzNotifyIntf;

type
  TObjectNotifyEvent = procedure(const Sender: IInterface = nil) of object;

  TObjectEventClass = class of TObjectEvent;
  TObjectEvent = class(TInterfacedObject, IObjectEvent)
  private
    FNotificationCount: Integer;
    FParams: IInterfaceList;
    FSender: IInterface;
  protected
    function GetNotifyType: TNotifyType; virtual;
    function GetParam: IInterface; // ony one or first params
    function GetParams(const Index: Integer): IInterface;
    function GetSender: IInterface;
    procedure AddParam(const Param: IInterface);
    procedure ClearParams;
    procedure RemoveParam(const Param: IInterface);

    procedure StartNotification;
    procedure EndNotification;

    property Param: IInterface read GetParam;
    property Params[const Index: Integer]: IInterface read GetParams; default;
    property NotifyType: TNotifyType read GetNotifyType;
    property Sender: IInterface read GetSender;
  public
    constructor Create(const Sender: IInterface); virtual;
    destructor Destroy; override;
  end;

  // observer and subject
  TAttachedEvent = class(TObjectEvent, IAttachedEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;
  
  TDetachedEvent = class(TObjectEvent, IDetachedEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  // ObjectList/Object/Member
  TClearEvent = class(TObjectEvent, IClearEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  //ObjectList
  TObjectListEvent = class(TObjectEvent, IObjectListEvent);

  TAddEvent = class(TObjectListEvent, IAddEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TInsertEvent = class(TObjectListEvent, IInsertEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TRemoveEvent = class(TObjectListEvent, IRemoveEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TBeginUpdateEvent = class(TObjectListEvent, IBeginUpdateEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TEndUpdateEvent = class(TObjectListEvent, IEndUpdateEvent)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TNotifyManager = class(TInterfacedObject, INotifyManager)
  private
    FItems: IInterfaceList;
  public
    procedure AddNotifier(const Item: IObjectEvent);
    procedure RemoveNotifier(const Item: IObjectEvent);
    procedure Clear;
  end;

  TEventParams = array of IInterface;

function NewEvent(NotifyClass: TObjectEventClass; const Sender: IInterface;
  const Params: array of IInterface): IObjectEvent; overload;
function NewEvent(NotifyClass: TObjectEventClass; const Sender: IInterface;
  const Param: IInterface = nil): IObjectEvent; overload;

function CheckNotify(const Notification: IObjectEvent; Events: array of TNotifyType): boolean;
function CheckAcceptNotify(const Notification: IObjectEvent): boolean;

implementation

var
  _NotifyManager: INotifyManager;

function NewEvent(NotifyClass: TObjectEventClass; const Sender: IInterface;
  const Params: array of IInterface): IObjectEvent;
var
  I: Integer;
begin
  if _NotifyManager = nil then _NotifyManager:= TNotifyManager.Create;

  Result:= NotifyClass.Create(Sender);
  for I:= Low(Params) to High(Params) do
    if Params[I] <> nil then Result.AddParam(Params[I]);

  _NotifyManager.AddNotifier(Result);
end;

function NewEvent(NotifyClass: TObjectEventClass; const Sender: IInterface;
  const Param: IInterface): IObjectEvent; overload;
begin
  Result:= NewEvent(NotifyClass, Sender, [Param]);
end;

function CheckNotify(const Notification: IObjectEvent; Events: array of TNotifyType): boolean;
var
  I: Integer;
begin
  Result:= False;
  for I:= Low(Events) to High(Events) do
  begin
    if Notification.NotifyType = Events[I] then
    begin
      Result:= True;
      Break;
    end;
  end;
end;

function CheckAcceptNotify(const Notification: IObjectEvent): boolean;
begin
  Result:= (Notification.NotifyType in [ntAttached, ntLoaded, ntModified,
    ntEndUpdate, ntAdd, ntInsert, ntRemove]);
end;

{ TNotify }

procedure TObjectEvent.AddParam(const Param: IInterface);
begin
  FParams.Add(Param);
end;

procedure TObjectEvent.ClearParams;
begin
  FParams.Clear;
end;

constructor TObjectEvent.Create(const Sender: IInterface);
begin
  inherited Create;
  FNotificationCount:= 0;
  FSender:= Sender;
  FParams:= TInterfaceList.Create;
end;

destructor TObjectEvent.Destroy;
begin
  if FParams <> nil then FParams.Clear;
  inherited;
end;

procedure TObjectEvent.EndNotification;
begin
  Dec(FNotificationCount);
  if FNotificationCount < 1 then _NotifyManager.RemoveNotifier(Self);
end;

function TObjectEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntMessage;
end;

function TObjectEvent.GetParam: IInterface;
begin
  if FParams.Count > 0 then Result:= GetParams(0) else Result:= nil;
end;

function TObjectEvent.GetParams(const Index: Integer): IInterface;
begin
  Result:= FParams[Index];
end;

function TObjectEvent.GetSender: IInterface;
begin
  Result:= FSender;
end;

{ TNotifyManager }

procedure TNotifyManager.AddNotifier(const Item: IObjectEvent);
begin
  if FItems = nil then FItems:= TInterfaceList.Create;
  FItems.Add(Item);
end;

procedure TNotifyManager.Clear;
begin
  if FItems <> nil then FItems.Clear;
end;

procedure TNotifyManager.RemoveNotifier(const Item: IObjectEvent);
begin
  if FItems <> nil then
  begin
    FItems.Remove(Item);
    if FItems.Count = 0 then FItems:= nil;
  end;
end;

procedure TObjectEvent.RemoveParam(const Param: IInterface);
begin
  FParams.Remove(Param);
end;

procedure TObjectEvent.StartNotification;
begin
  Inc(FNotificationCount)
end;

{ TAttachedEvent }

function TAttachedEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntAttached;
end;

{ TDetachedEvent }

function TDetachedEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntDetached;
end;

{ TClearEvent }

function TClearEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntClear;
end;

{ TAddEvent }

function TAddEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntAdd;
end;

{ TInsertEvent }

function TInsertEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntInsert;
end;

{ TRemoveEvent }

function TRemoveEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntRemove;
end;

{ TBeginUpdateEvent }

function TBeginUpdateEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntBeginUpdate;
end;

{ TEndUpdateEvent }

function TEndUpdateEvent.GetNotifyType: TNotifyType;
begin
  Result:= ntEndUpdate;
end;

initialization

finalization
  if _NotifyManager <> nil then
  begin
    _NotifyManager.Clear;
    _NotifyManager:= nil;
  end;

end.


