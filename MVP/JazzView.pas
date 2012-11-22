unit JazzView;

interface

uses
  Classes,
  JazzViewIntf,
  JazzValueTypeIntf,
  JazzTypes,
  JazzObserverIntf,
  JazzSubject,
  JazzSubjectIntf,
  JazzNotifyIntf,
  JazzNotify;

type
  //TODO: change to TInterfacedObject 
  TView = class(TSimpleSubject, IView, IObserver)
  private
    FControl: TObject;
    FObjectValueSubject: Pointer;
    FOnAddSelection: TObjectNotifyEvent;
    FOnClearSelection: TObjectNotifyEvent;
    FOnRemoveSelection: TObjectNotifyEvent;
  protected
    function GetControl: TObject;
    procedure SetControl(const Value: TObject); virtual;

    procedure DoAddSelection(const AObject: IValueType = nil); virtual;
    procedure DoRemoveSelection(const AObject: IValueType = nil); virtual;
    procedure DoClearSelection; virtual;

    procedure Update(const Notification: IObjectEvent); virtual;

    function GetOnAddSelection: TObjectNotifyEvent;
    function GetOnClearSelection: TObjectNotifyEvent;
    function GetOnRemoveSelection: TObjectNotifyEvent;
    procedure SetOnAddSelection(const Value: TObjectNotifyEvent);
    procedure SetOnClearSelection(const Value: TObjectNotifyEvent);
    procedure SetOnRemoveSelection(const Value: TObjectNotifyEvent);

    property Control: TObject read GetControl write SetControl;
    property OnAddSelection: TObjectNotifyEvent read GetOnAddSelection write SetOnAddSelection;
    property OnClearSelection: TObjectNotifyEvent read GetOnClearSelection write SetOnClearSelection;
    property OnRemoveSelection: TObjectNotifyEvent read GetOnRemoveSelection write SetOnRemoveSelection;
  public
    constructor Create(const Control: TObject); virtual;
  end;

  TValueTypeViewClass = class of TValueTypeView;
  TValueTypeView = class(TView, IValueTypeView)
  protected
    // IValueTypeView
    function GetObjectValue: IValueType;
    property ObjectValue: IValueType read GetObjectValue;
  end;

  TMemberView = class(TValueTypeView, IMemberView)
  private
    FOnChanged: TObjectNotifyEvent;
  protected
    function GetOnChanged: TObjectNotifyEvent;
    procedure SetOnChanged(const Value: TObjectNotifyEvent);

    property OnChanged: TObjectNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  TObjectView = class(TValueTypeView)
  end;

  TObjectListView = class(TValueTypeView, IObjectListTypeView)
  private
    FOnAppend: TObjectNotifyEvent;
    FOnClear: TObjectNotifyEvent;
    FOnDelete: TObjectNotifyEvent;
    FOnInsert: TObjectNotifyEvent;
    FOnPost: TObjectNotifyEvent;
  protected
    function GetOnAppend: TObjectNotifyEvent;
    function GetOnClear: TObjectNotifyEvent;
    function GetOnDelete: TObjectNotifyEvent;
    function GetOnInsert: TObjectNotifyEvent;
    function GetOnPost: TObjectNotifyEvent;
    procedure SetOnAppend(const Value: TObjectNotifyEvent);
    procedure SetOnClear(const Value: TObjectNotifyEvent);
    procedure SetOnDelete(const Value: TObjectNotifyEvent);
    procedure SetOnInsert(const Value: TObjectNotifyEvent);
    procedure SetOnPost(const Value: TObjectNotifyEvent);

    function DoAppend: IObjectType;
    function DoInsert: IObjectType;
    procedure DoClear;
    procedure DoDelete;

    property OnAppend: TObjectNotifyEvent read GetOnAppend write SetOnAppend;
    property OnInsert: TObjectNotifyEvent read GetOnInsert write SetOnInsert;
    property OnClear: TObjectNotifyEvent read GetOnClear write SetOnClear;
    property OnDelete: TObjectNotifyEvent read GetOnDelete write SetOnDelete;
    property OnPost: TObjectNotifyEvent read GetOnPost write SetOnPost;
  end;

  TBlobView = class(TMemberView, IBlobTypeView)
  protected
    procedure DoChanged(const Value: TStream); virtual; 
  end;

  TBooleanView = class(TMemberView, IBooleanTypeView)
  protected
    procedure DoChanged(const Value: boolean); virtual;
  end;

  TCharView = class(TMemberView)
  protected
    procedure DoChanged(const Value: Char); virtual;
  end;

  TCurrencyView = class(TMemberView)
  protected
    procedure DoChanged(const Value: Currency); virtual;
  end;

  TDateView = class(TMemberView)
  protected
    procedure DoChanged(const Value: TDateTime); virtual; 
  end;

  TFloatView = class(TMemberView)
  protected
    procedure DoChanged(const Value: Double); virtual; 
  end;

  TIntegerView = class(TMemberView)
  protected
    procedure DoChanged(const Value: Integer); virtual; 
  end;

  TLongIntView = class(TMemberView)
  protected
    procedure DoChanged(const Value: LongInt); virtual; 
  end;

  TMemoView = class(TMemberView)
  protected
    procedure DoChanged(const Value: TStrings); virtual; 
  end;

  TSmallIntView = class(TMemberView)
  protected
    procedure DoChanged(const Value: SmallInt); virtual; 
  end;

  TStringView = class(TMemberView)
  protected
    procedure DoChanged(const Value: string); virtual; 
  end;

  TWideStringView = class(TMemberView)
  protected
    procedure DoChanged(const Value: WideString); virtual; 
  end;

function GuidToViewClass(const IID: TGuid): TValueTypeViewClass;

implementation

uses
  ComObj,
  SysUtils,
  JazzValueType,
  JazzMvpNotify;

function GuidToViewClass(const IID: TGuid): TValueTypeViewClass;
begin
  if IsEqualGUID(IID, IBlobType      ) then Result:= TBlobView       else
  if IsEqualGUID(IID, IBooleanType   ) then Result:= TBooleanView    else
  if IsEqualGUID(IID, ICharType      ) then Result:= TCharView       else
  if IsEqualGUID(IID, ICurrencyType  ) then Result:= TCurrencyView   else
  if IsEqualGUID(IID, IDateType      ) then Result:= TDateView       else
  if IsEqualGUID(IID, IFloatType     ) then Result:= TFloatView      else
  if IsEqualGUID(IID, IIntegerType   ) then Result:= TIntegerView    else
  if IsEqualGUID(IID, ILongIntType   ) then Result:= TLongIntView    else
  if IsEqualGUID(IID, IMemoType      ) then Result:= TMemoView       else
  if IsEqualGUID(IID, IObjectListType) then Result:= TObjectListView else
  if IsEqualGUID(IID, IObjectType    ) then Result:= TObjectView     else
  if IsEqualGUID(IID, ISmallIntType  ) then Result:= TSmallIntView   else
  if IsEqualGUID(IID, IWideStringType) then Result:= TWideStringView else
                                            Result:= TStringView;
end;

{ TView }

constructor TView.Create(const Control: TObject);
begin
  inherited Create(Self);
  SetControl(Control);  
end;

function TView.GetControl: TObject;
begin
  Result:= FControl;
end;

procedure TView.SetControl(const Value: TObject);
begin
  FControl:= Value;
end;

procedure TView.Update(const Notification: IObjectEvent);
begin
  case Notification.NotifyType of
    ntAttached: FObjectValueSubject:= Pointer(Notification.Sender);
    ntDetached: FObjectValueSubject:= nil;
  end;
end;

{ TValueTypeView }

procedure TView.DoAddSelection(const AObject: IValueType);
begin
  if Assigned(FOnAddSelection) then
    FOnAddSelection(NewEvent(TAddSelection, Self, [AObject]))
  else
    Notify(NewEvent(TAddSelection, Self, [AObject]));
end;

procedure TView.DoClearSelection;
begin
  if Assigned(FOnClearSelection) then
    FOnClearSelection(NewEvent(TClearSelection, Self))
  else
    Notify(NewEvent(TClearSelection, Self));
end;

procedure TView.DoRemoveSelection(const AObject: IValueType);
begin
  if Assigned(FOnRemoveSelection) then
    FOnRemoveSelection(NewEvent(TRemoveSelection, Self, [AObject]))
  else
    Notify(NewEvent(TRemoveSelection, Self, [AObject]));
end;

function TView.GetOnAddSelection: TObjectNotifyEvent;
begin
  Result:= FOnAddSelection;
end;

function TView.GetOnClearSelection: TObjectNotifyEvent;
begin
  Result:= FOnClearSelection;
end;

function TView.GetOnRemoveSelection: TObjectNotifyEvent;
begin
  Result:= FOnRemoveSelection;
end;

procedure TView.SetOnAddSelection(const Value: TObjectNotifyEvent);
begin
  FOnAddSelection:= Value;
end;

procedure TView.SetOnClearSelection(const Value: TObjectNotifyEvent);
begin
  FOnClearSelection:= Value;
end;

procedure TView.SetOnRemoveSelection(const Value: TObjectNotifyEvent);
begin
  FOnRemoveSelection:= Value;
end;

{ TBooleanTypeView }

procedure TBooleanView.DoChanged(const Value: boolean);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewBoolean(Value));
end;

{ TObjectListTypeView }

function TObjectListView.DoAppend: IObjectType;
var
  LEvent: IObjectEvent;
begin
  LEvent:= NewEvent(TAddEvent, Self);
  LEvent.StartNotification;
  try
    if Assigned(FOnAppend) then FOnAppend(LEvent) else Notify(LEvent);
    Supports(LEvent.Param, IObjectType, Result);
  finally
    LEvent.EndNotification;
  end;
end;

procedure TObjectListView.DoClear;
begin
  if Assigned(FOnClear) then
    FOnClear(NewEvent(TClearEvent, Self))
  else
    Notify(NewEvent(TClearEvent, Self));
end;

procedure TObjectListView.DoDelete;
begin
  if Assigned(FOnDelete) then
    FOnDelete(NewEvent(TDeleteEvent, Self))
  else
    Notify(NewEvent(TDeleteEvent, Self));
end;

function TObjectListView.DoInsert: IObjectType;
var
  LEvent: IObjectEvent;
begin
  LEvent:= NewEvent(TInsertEvent, Self);
  LEvent.StartNotification;
  try
    if Assigned(FOnInsert) then FOnInsert(LEvent) else Notify(LEvent);
    Supports(LEvent.Param, IObjectType, Result);
  finally
    LEvent.EndNotification;
  end;
end;

function TObjectListView.GetOnAppend: TObjectNotifyEvent;
begin
  Result:= FOnAppend;
end;

function TObjectListView.GetOnClear: TObjectNotifyEvent;
begin
  Result:= FOnClear;
end;

function TObjectListView.GetOnDelete: TObjectNotifyEvent;
begin
  Result:= FOnDelete;
end;

function TObjectListView.GetOnInsert: TObjectNotifyEvent;
begin
  Result:= FOnInsert;
end;

function TObjectListView.GetOnPost: TObjectNotifyEvent;
begin
  Result:= FOnPost; 
end;

procedure TObjectListView.SetOnAppend(const Value: TObjectNotifyEvent);
begin
  FOnAppend:= Value;
end;

procedure TObjectListView.SetOnClear(const Value: TObjectNotifyEvent);
begin
  FOnClear:= Value;
end;

procedure TObjectListView.SetOnDelete(const Value: TObjectNotifyEvent);
begin
  FOnDelete:= Value;
end;

procedure TObjectListView.SetOnInsert(const Value: TObjectNotifyEvent);
begin
  FOnInsert:= Value;
end;

function TMemberView.GetOnChanged: TObjectNotifyEvent;
begin
  Result:= FOnChanged;
end;

procedure TMemberView.SetOnChanged(const Value: TObjectNotifyEvent);
begin
  FOnChanged:= Value;
end;

function TValueTypeView.GetObjectValue: IValueType;
begin
  Result:= IValueType(FObjectValueSubject);
end;

{ TBlobTypeView }

procedure TBlobView.DoChanged(const Value: TStream);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewBlob(Value));
end;

{ TCharTypeView }

procedure TCharView.DoChanged(const Value: Char);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewChar(Value));
end;

{ TCurrencyTypeView }

procedure TCurrencyView.DoChanged(const Value: Currency);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewCurrency(Value));
end;

{ TDateTypeView }

procedure TDateView.DoChanged(const Value: TDateTime);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewDate(Value));
end;

{ TFloatTypeView }

procedure TFloatView.DoChanged(const Value: Double);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewFloat(Value));
end;

{ TIntegerTypeView }

procedure TIntegerView.DoChanged(const Value: Integer);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewInteger(Value));
end;

{ TLongIntTypeView }

procedure TLongIntView.DoChanged(const Value: Integer);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewLongInt(Value));
end;

{ TMemoTypeView }

procedure TMemoView.DoChanged(const Value: TStrings);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewMemo(Value));
end;

{ TSmallIntTypeView }

procedure TSmallIntView.DoChanged(const Value: SmallInt);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewSmallInt(Value));
end;

{ TStringTypeView }

procedure TStringView.DoChanged(const Value: string);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewString(Value));
end;

{ TWideStringTypeView }

procedure TWideStringView.DoChanged(const Value: WideString);
begin
  if Assigned(FOnChanged) then FOnChanged(TypeFactory.NewWideString(Value));
end;

procedure TObjectListView.SetOnPost(const Value: TObjectNotifyEvent);
begin
  FOnPost:= Value;
end;

end.



