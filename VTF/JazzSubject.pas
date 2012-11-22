unit JazzSubject;

interface

uses
  Classes,
  JazzNotifyIntf,
  JazzObserverIntf,
  JazzSubjectIntf,
  JazzValueTypeIntf;

type
  TCustomSubject = class(TInterfacedObject, ISubject)
  private
    FController: Pointer;
    FNotifying: boolean;
    function GetController: IInterface;
    function GetNotifying: boolean;
  protected
    procedure Attach(const Observer: IObserver); virtual; abstract;
    procedure Detach(const Observer: IObserver); virtual; abstract;
    procedure DetachAll; virtual; abstract;
    procedure Notify(const Notification: IObjectEvent); virtual; abstract;
  public
    constructor Create(const Controller: IInterface);
    property Notifying: boolean read GetNotifying;
  end;

  TSubject = class(TCustomSubject)
  private
    FObservers: IInterfaceList;
  protected
    procedure Attach(const Observer: IObserver); override;
    procedure Detach(const Observer: IObserver); override;
    procedure DetachAll; override;
    procedure Notify(const Notification: IObjectEvent); override;
  end;

  TSimpleSubject = class(TCustomSubject)
  private
    FWeakReference: boolean;
    FWeakObserver: Pointer;
    FObserver: IObserver;
  protected
    procedure Attach(const Observer: IObserver); override;
    procedure Detach(const Observer: IObserver); override;
    procedure DetachAll; override;
    procedure Notify(const Notification: IObjectEvent); override;
  public
    constructor Create(const Controller: IInterface; const WeakReference: boolean = True);
  end;

implementation

uses
  SysUtils,
  JazzNotify,
  JazzUtils;

constructor TCustomSubject.Create(const Controller: IInterface);
begin
  inherited Create;
  FController:= Pointer(Controller);
end;

function TCustomSubject.GetNotifying: boolean;
begin
  Result:= FNotifying;
end;

function TCustomSubject.GetController: IInterface;
begin
  Result:= IInterface(FController);
end;

procedure TSubject.Attach(const Observer: IObserver);
begin
  if FObservers = nil then FObservers:= TInterfaceList.Create;
  if not InterfaceInList(Observer, FObservers) then
  begin
    FObservers.Add(Observer);
    Notify(NewEvent(TAttachedEvent, GetController));
  end;
end;

procedure TSubject.Detach(const Observer: IObserver);
begin
  if FObservers <> nil then
  begin
    FObservers.Lock;
    try
      if InterfaceInList(Observer, FObservers) then
      begin
        Observer.Update(NewEvent(TDetachedEvent, GetController));
        FObservers.Delete(FObservers.IndexOf(Observer));
      end;
    finally
      FObservers.Unlock;
      if FObservers.Count = 0 then FObservers:= nil;
    end;
  end;
end;

procedure TSubject.DetachAll;
var
  I: Integer;
  LObserver: IObserver;
begin
  if (FObservers <> nil) then
  begin
    FObservers.Lock;
    try
      for I:= 0 to FObservers.Count -1 do
      begin
        if Supports(FObservers[I], IObserver, LObserver) then
          Detach(LObserver);
      end;
    finally
      FObservers.Unlock;
    end;
  end;
end;

procedure TSubject.Notify(const Notification: IObjectEvent);
var
  I: Integer;
  LObserver: IObserver;
begin
  if (FObservers <> nil) then
  begin
    FNotifying:= True;
    FObservers.Lock;
    Notification.StartNotification;
    try
      for I:= 0 to FObservers.Count -1 do
      begin
        if Supports(FObservers[I], IObserver, LObserver) then
          LObserver.Update(Notification);
      end;
    finally
      Notification.EndNotification;
      FObservers.Unlock;
      FNotifying:= False;
    end;
  end;
end;

procedure TSimpleSubject.Attach(const Observer: IObserver);
begin
  if FWeakReference then
  begin
    if FWeakObserver <> nil then Detach(IObserver(FWeakObserver));
    FWeakObserver:= Pointer(Observer);
  end
  else
  begin
    if FObserver <> nil then Detach(FObserver);
    FObserver:= Observer;
  end;
  Notify(NewEvent(TAttachedEvent, Self));
end;

constructor TSimpleSubject.Create(const Controller: IInterface;
  const WeakReference: boolean);
begin
  inherited Create(Controller);
  FWeakReference:= WeakReference; 
end;

procedure TSimpleSubject.Detach(const Observer: IObserver);
begin
  if FWeakReference then
  begin
    if FWeakObserver <> nil then
    begin
      Notify(NewEvent(TDetachedEvent, Self));
      FWeakObserver:= nil;
    end;
  end
  else
  if FObserver <> nil then
  begin
    Notify(NewEvent(TDetachedEvent, Self));
    FObserver:= nil;
  end;
end;

procedure TSimpleSubject.DetachAll;
begin
  Detach(nil);
end;

procedure TSimpleSubject.Notify(const Notification: IObjectEvent);
begin
  FNotifying:= True;
  try
    if FWeakReference then
    begin
      if (FWeakObserver <> nil) then
        IObserver(FWeakObserver).Update(Notification);
    end
    else
    if (FObserver <> nil) then
      FObserver.Update(Notification);
  finally
    Notification.EndNotification;
    FNotifying:= False;
  end;
end;

end.


