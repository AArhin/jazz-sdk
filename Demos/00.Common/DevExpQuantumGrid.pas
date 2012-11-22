unit DevExpQuantumGrid;

interface

uses
  SysUtils, Dialogs, Classes, Contnrs, cxCustomData, cxGridCustomTableView,
  JazzValueTypeIntf;

type
  TcxPersistentListDataSourceGetValue = procedure(Sender: TcxCustomDataSource;
    AObject: IValueObject; AttributeName: string; var Value: Variant) of object;
  TObjectFocusEvent = procedure(Sender: TcxCustomDataSource; AObject: IValueObject)
    of object;
  TColumnFocusEvent = procedure(Sender: TcxCustomDataSource;
    AItem: TcxCustomGridTableItem; AColumnIndex: Integer) of object;

  IObjectListLink = interface(IInterface)
    ['{62468232-E93F-4CCD-8373-2CE48D08FDB4}']
    function GetDataSource: TcxCustomDataSource;
  end;

  TObjectListLink = class(TcxCustomDataSource, IObjectListLink)
  private
    FLock: boolean;
    FMasterAttribute: string;
    FMasterDataSource: TObjectListLink;
    FModified: Boolean;
    FObjectClass: TClass;
    FObjectList: IValueObjectList;
    FOnGetValue: TcxPersistentListDataSourceGetValue;
    FActiveObject: IValueObject;
    FActiveColumn: Integer;
    FOnColumnFocus: TColumnFocusEvent;
    FOnObjectFocus: TObjectFocusEvent;
    FOnGetObject: TNotifyEvent;
    procedure SetObjectClass(const Value: TClass);
    procedure SetObjectList(const Value: IValueObjectList);
    procedure SetActiveObject(const Value: IValueObject);
    procedure SetActiveColumn(const Value: Integer);
  protected
    FRefCount: Integer;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    function AppendRecord: TcxDataRecordHandle; override;
    function GetDataBinding(AItemIndex: Integer): TcxGridItemDataBinding;
    function GetDataSource: TcxCustomDataSource;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetObject(const Index: Integer): IValueObject; virtual;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    function InsertRecord(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    function NewRecord: IValueObject; virtual;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    procedure DoGetValue(AObject: IValueObject; AttributeName: string; var Value: Variant); virtual;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(MasterObjectListLink: TObjectListLink; MasterMember:
      string); overload;
    constructor Create(ObjectList: IValueObjectList; ObjectClass: TClass = nil);
      overload;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Modified: boolean read FModified;
    property ObjectClass: TClass read FObjectClass write SetObjectClass;
    property ObjectList: IValueObjectList read FObjectList write SetObjectList;
    property ActiveColumn: Integer read FActiveColumn write SetActiveColumn;
    property ActiveObject: IValueObject read FActiveObject write SetActiveObject;
  published
    property MasterAttribute: string read FMasterAttribute;
    property MasterDataSource: TObjectListLink read FMasterDataSource;
    property OnGetValue: TcxPersistentListDataSourceGetValue read FOnGetValue write FOnGetValue;
    property OnGetObject: TNotifyEvent read FOnGetObject write FOnGetObject;
    property OnObjectFocus: TObjectFocusEvent read FOnObjectFocus write FOnObjectFocus;
    property OnColumnFocus: TColumnFocusEvent read FOnColumnFocus write FOnColumnFocus;
  end;

implementation

uses
  ComObj, Variants;

const
  kernel = 'kernel32.dll';

function InterlockedIncrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedIncrement';
function InterlockedDecrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedDecrement';

constructor TObjectListLink.Create(MasterObjectListLink: TObjectListLink;
  MasterMember: string);
begin
  Create(MasterObjectListLink.ObjectList, MasterObjectListLink.ObjectClass);
  FMasterDatasource:= MasterObjectListLink;
  FMasterAttribute:= MasterMember;
end;

{ TcxPersistentCollectionDataSource }

constructor TObjectListLink.Create(ObjectList: IValueObjectList; ObjectClass:
  TClass = nil);
begin
  inherited Create;
  FModified:= False;
  Self.ObjectList:= ObjectList;

  if (ObjectClass = nil) then
    Self.ObjectClass:= (ObjectList as IValueObjectList).ItemClass
  else
    Self.ObjectClass:= ObjectClass;
end;

function TObjectListLink.AppendRecord: TcxDataRecordHandle;
begin
  ObjectList.Insert(ObjectList.Count, NewRecord);
  Result:= TcxDataRecordHandle(ObjectList.Count -1);
  if not Modified then FModified:= True;
end;

procedure TObjectListLink.BeforeDestruction;
begin
  inherited;
end;

procedure TObjectListLink.DeleteRecord(ARecordHandle: TcxDataRecordHandle);
var
  LIndex: Integer;
begin
  inherited;

  if (ARecordHandle = nil) then
  begin
    if (ObjectList.Count > 0) and (DataController.RecordCount > 0) then
       LIndex:= DataController.RecordCount -1
    else
       LIndex:= -1;
  end
  else
     LIndex:= Integer(ARecordHandle); 

  if (LIndex <> -1) then
  begin
    ObjectList.Delete(LIndex);
    if not Modified then FModified:= True;
  end;
end;

procedure TObjectListLink.DoGetValue(AObject: IValueObject; AttributeName: string; var Value: Variant);
begin
  if Assigned(FOnGetValue) then FOnGetValue(Self, AObject, AttributeName, Value);
end;

function TObjectListLink.GetDataBinding(AItemIndex: Integer): TcxGridItemDataBinding;
begin
  Result:= TcxCustomGridTableItem(DataController.GetItem(AItemIndex)).DataBinding;
end;

function TObjectListLink.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
begin
  Result:= TcxDataItemHandle(GetDataBinding(AItemIndex));
  ActiveColumn:= AItemIndex;
end;

function TObjectListLink.GetObject(const Index: Integer): IValueObject;
var
  LObjectList: IValueObjectList;
  LObject: IValueObject;
  LIndex: Integer;
begin
  Result:= nil;
  LObjectList:= ObjectList;

  if Assigned(FMasterDataSource) then
  begin
    LIndex:= DataController.GetMasterRecordIndex;
    LObject:= FMasterDataSource.GetObject(LIndex);
    LObject.Member[FMasterAttribute].QueryInterface(IValueObjectList, LObjectList);
  end;

  if Assigned(LObjectList) then
  begin
    if Index > (LObjectList.Count -1) then
      Result:= nil
    else
      Result:= LObjectList.Items[Index];
  end;

  if FLock then Exit;
  FLock:= True;
  try
    if Assigned(FOnGetObject) then FOnGetObject(Self);
  finally
    FLock:= False;
  end;
end;

function TObjectListLink.GetRecordCount: Integer;
var
  LObjectList: IValueObjectList;
  LObject: IValueObject;
  LIndex: Integer;
begin
  Result:= 0;
  LObjectList:= ObjectList;

  if Assigned(FMasterDataSource) then
  begin
    LIndex:= DataController.GetMasterRecordIndex;
    LObject:= FMasterDatasource.GetObject(LIndex);
    LObject.Member[FMasterAttribute].QueryInterface(IValueObjectList, LObjectList);
  end;
  if Assigned(LObjectList) then Result:= LObjectList.Count;
end;

function TObjectListLink.GetValue(ARecordHandle:
  TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
var
  ColumnName: string;
  LObject: IValueObject;
  LIndex: Integer;
begin
  Result:= Null;
  ColumnName:= PAnsiChar(TcxGridItemDataBinding(AItemHandle).Item.Name);
  LIndex:= Integer(ARecordHandle);
  LObject:= GetObject(LIndex);
  if (LObject <> nil) then
  begin
    Result:= LObject.Member[ColumnName].AsString;
    DoGetValue(LObject, ColumnName, Result);
  end;
end;

function TObjectListLink.InsertRecord(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
begin
  ObjectList.Insert(Integer(ARecordHandle), NewRecord);
  Result:= TcxDataRecordHandle(ARecordHandle);
  if not Modified then FModified:= True;
end;

function TObjectListLink.NewRecord: IValueObject;
begin
  if ObjectList <> nil then Result:= ObjectList.Add;
end;

procedure TObjectListLink.SetObjectClass(const Value: TClass);
begin
  if (FObjectClass <> Value) then FObjectClass:= Value;
end;

procedure TObjectListLink.SetObjectList(const Value: IValueObjectList);
begin
  FObjectList:= Value;
  DataChanged;
end;

procedure TObjectListLink.SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  ColumnName: string;
  LObject: IValueObject;
  LIndex: Integer;
begin
  if Assigned(FObjectList) then
  begin
    ColumnName:= PAnsiChar(TcxGridItemDataBinding(AItemHandle).Item.Name);
    LIndex:= Integer(ARecordHandle);
    LObject:= GetObject(LIndex);

    with LObject do
    begin
      if Supports(Member[ColumnName], IString) then
        (Member[ColumnName] as IString).Value:= AValue
      else
      if Supports(Member[ColumnName], IInteger) then
        (Member[ColumnName] as IInteger).Value:= AValue
      else
      if Supports(Member[ColumnName], Iboolean) then
        (Member[ColumnName] as Iboolean).Value:= AValue
      else
      if Supports(Member[ColumnName], ICurrency) then
        (Member[ColumnName] as ICurrency).Value:= AValue
      else
      if Supports(Member[ColumnName], IValueDate) then
        (Member[ColumnName] as IValueDate).Value:= AValue
      else
      if Supports(Member[ColumnName], IFloat) then
        (Member[ColumnName] as IFloat).Value:= AValue
      else
      if Supports(Member[ColumnName], ISmallInt) then
        (Member[ColumnName] as ISmallInt).Value:= AValue
      else
      {
      if Supports(Member[ColumnName], IChar) then
        (Member[ColumnName] as IChar).Value:= Char(AValue)
      else
      }
      if Supports(Member[ColumnName], IWideString) then
        (Member[ColumnName] as IWideString).Value:= AValue
      else
      if Supports(Member[ColumnName], IValueMemo) then
        (Member[ColumnName] as IValueMemo).Value.Text:= AValue
      else
      if Supports(Member[ColumnName], IBlob) then
        (Member[ColumnName] as IBlob).SetAsString(AValue)
        ;
    end;
    if not Modified then FModified:= True;
  end;
end;

procedure TObjectListLink.SetActiveObject(const Value: IValueObject);
begin
  if FActiveObject <> Value then
  begin
    FActiveObject:= Value;
    FActiveColumn:= -1;
    if Assigned(FOnObjectFocus) then FOnObjectFocus(Self, Value);
  end;
end;

procedure TObjectListLink.SetActiveColumn(const Value: Integer);
var
  lItem: TcxCustomGridTableItem;
begin
  if (FActiveColumn <> Value) then
  begin
     lItem:= TcxGridItemDataBinding(TcxDataItemHandle(GetDataBinding(Value))).Item;

     if lItem.Focused then
     begin
       FActiveColumn:= Value;
       if Assigned(FOnColumnFocus) then FOnColumnFocus(Self, lItem, FActiveColumn);
     end;
  end;
end;

procedure TObjectListLink.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
  inherited;
  FLock:= False;
end;

function TObjectListLink._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TObjectListLink._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then Destroy;
end;

function TObjectListLink.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TObjectListLink.GetDataSource: TcxCustomDataSource;
begin
  Result:= Self;
end;

end.
