unit JazzObjectTable;

interface

{
31mar2007
known issues
- Frozen while post record when in Filter mode: workaround
- bookmark: rewrite all bookmark implementation
- FCurrent is not working correctly
- Test with master-detail

missing features
- import fields design time support
}

uses
  Classes,
  DB,
  Dialogs,
  SysUtils,
  Variants,
  JazzBaseTable,
  JazzClasses,
  JazzCriteriaIntf,
  JazzMappingIntf,
  JazzSessionIntf,
  JazzValueTypeIntf,
  JazzValueType;

type
  TCustomObjectTable = class(TBaseTable)
  private
    FAfterApplyChanges: TNotifyEvent;
    FAfterCancelChanges: TNotifyEvent;
    FBeforeApplyChanges: TNotifyEvent;
    FBeforeCancelChanges: TNotifyEvent;
    FCriteria: ICriteria;
    FEditCache: INamedInterfaceList;
    FInsertItem: IObjectType;
    FObjectClassName: string;
    FObjectIndex: Integer;
    FObjectList: IObjectListType;
    FSession: ISession;
    FObjectClass: TObjectTypeClass;
    FFieldsAsProxy: Boolean;
    function FindMemberType(AObjectType: IObjectType; AColumnName: string): IMemberType;
    function GetActiveObject: IObjectType;
    function GetMemberMeta(ColumnName: string): IMemberMeta;
    function GetObjectMeta: IObjectMeta;
    procedure InternalCancelChanges;
    procedure SetObjectClass(const Value: TObjectTypeClass);
    procedure UpdateProxyFields;
  protected
    function DoOpen: Boolean; override;
    procedure DoBeforeEdit; override;
    procedure DoCancelRecord; override;
    procedure DoClose; override;
    procedure DoCreateFieldDefs; override;
    procedure DoDeleteRecord; override;
    procedure DoInsertRecord; override;
    procedure DoPostRecord; override;
    procedure DoAfterPost; override;
    procedure DoAfterCancel; override;
    procedure DoSetMasterField(Field: TField); override;

    function GetCanModify: Boolean; override;
    function GetRecordCount: Integer; override;
    procedure InternalRefresh; override;
    procedure GetBlobField(Field: TBlobField; Stream: TStream); override;
    procedure SetBlobField(Field: TBlobField; Stream: TStream); override;

    function GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; Value: Variant); override;
    function GetObjectClass: TObjectTypeClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyChanges;
    procedure CancelChanges;
    procedure ClearCriteria;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;

    property ActiveObject: IObjectType read GetActiveObject;
    property Criteria: ICriteria read FCriteria;
    property FieldsAsProxy: Boolean read FFieldsAsProxy write FFieldsAsProxy default True;
    property ObjectClass: TObjectTypeClass read GetObjectClass write SetObjectClass;
    property ObjectClassName: string read FObjectClassName write FObjectClassName;
    property Session: ISession read FSession write FSession;

    property AfterApplyChanges: TNotifyEvent read FAfterApplyChanges write FAfterApplyChanges;
    property AfterCancelChanges: TNotifyEvent read FAfterCancelChanges write FAfterCancelChanges;
    property BeforeApplyChanges: TNotifyEvent read FBeforeApplyChanges write FBeforeApplyChanges;
    property BeforeCancelChanges: TNotifyEvent read FBeforeCancelChanges write FBeforeCancelChanges;
  end;

  TObjectTable = class(TCustomObjectTable)
  published
    property AfterApplyChanges;
    property AfterCancel;
    property AfterCancelChanges;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;

    property BeforeApplyChanges;
    property BeforeCancel;
    property BeforeCancelChanges;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnFilterRecord;
    property OnNewRecord;

    property FieldsAsProxy;
    property Filtered;
    property ObjectClassName;
  end;

implementation

uses
  JazzConsts,
  JazzCriteria,
//  JazzDataSet,
  JazzDatabaseMechanismIntf,
  JazzObjectTableConsts,
  JazzSession,
  JazzUtils,
  JazzVarUtils;

{ TJazzCustomSnapDataSet }

procedure TCustomObjectTable.ApplyChanges;
begin
  CheckBrowseMode;
  if Assigned(FBeforeApplyChanges) then FBeforeApplyChanges(Self);
  FSession.Save(FObjectList);
  if Assigned(FAfterApplyChanges) then FAfterApplyChanges(Self);
  FEditCache.Clear;
  Refresh;
end;

procedure TCustomObjectTable.CancelChanges;
begin
  CheckBrowseMode;
  InternalCancelChanges;
  Refresh;
end;

procedure TCustomObjectTable.ClearCriteria;
begin
  FCriteria := nil;
  FCriteria := NewCriteria;
end;

constructor TCustomObjectTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditCache := TNamedInterfaceList.Create;
  ClearCriteria;
  FFieldsAsProxy := True;
end;

destructor TCustomObjectTable.Destroy;
begin
  FEditCache.Clear;
  FEditCache := nil;
  FCriteria := nil;
  inherited;
end;

procedure TCustomObjectTable.DoAfterCancel;
begin
  inherited;
  SetRecNo(FObjectIndex + 1);
end;

procedure TCustomObjectTable.DoAfterPost;
begin
  inherited;
  SetRecNo(FObjectIndex + 1);
end;

procedure TCustomObjectTable.DoBeforeEdit;
var
  LEditObjectType: IObjectType;
begin
  UpdateCursorPos;
  inherited DoBeforeEdit;

  if (FEditCache.IndexOf(IntToStr(Current)) = NotFound) then
  begin
    LEditObjectType := (FObjectList.Items[Current].Clone as IObjectType);
    FEditCache.Add(IntToStr(Current), LEditObjectType);
  end;
end;

procedure TCustomObjectTable.DoCancelRecord;
begin
  FInsertItem := nil;
  FObjectIndex := CurrentRecord;
end;

procedure TCustomObjectTable.DoClose;
begin
  inherited DoClose;
  FObjectList := nil;
end;

procedure TCustomObjectTable.DoCreateFieldDefs;
var
  LObjectMeta: IObjectMeta;
  LMemberMeta: IMemberMeta;
  LFieldDef: TFieldDef;
  I: Integer;
begin
  LObjectMeta := GetObjectMeta;
  if Assigned(LObjectMeta) and (LObjectMeta.Count > 0) then
  begin
    FieldDefs.Clear;
    for I := 0 to LObjectMeta.Count - 1 do
    begin
      LMemberMeta := (LObjectMeta.Items[I] as IMemberMeta);

      LFieldDef := FieldDefs.AddFieldDef;
      LFieldDef.DataType := LMemberMeta.FieldType;
      LFieldDef.Name := LMemberMeta.MemberName;
      LFieldDef.Precision := LMemberMeta.Precision;
      LFieldDef.Required := LMemberMeta.Required;
      if (LMemberMeta.FieldType in FieldWithSize) then
        LFieldDef.Size := LMemberMeta.Size;
    end;
  end
  else
    inherited DoCreateFieldDefs;
end;

procedure TCustomObjectTable.DoDeleteRecord;
begin
  FObjectList.Items[Current].Delete;
end;

procedure TCustomObjectTable.DoInsertRecord;
begin
  FInsertItem := FObjectList.New;
end;

function TCustomObjectTable.DoOpen: Boolean;
begin
  if FSession = nil then raise EJazzSessionNotDefined.Create;
  if ObjectClass = nil then raise EJazzObjectClassNotDefined.Create;
  FObjectList := ObjectClass.NewList;
  InternalRefresh;
  Result := inherited DoOpen;
end;

procedure TCustomObjectTable.DoPostRecord;
begin
  if FInsertItem <> nil then
  begin
    if Current <> 0 then
      FObjectIndex := FObjectList.Add(FInsertItem.Clone as IObjectType)
    else
    begin
      FObjectIndex := CurrentRecord;
      FObjectList.Insert(FObjectIndex, FInsertItem.Clone);
    end;

    UpdateCursorPos;
    FInsertItem := nil;
  end;
end;

procedure TCustomObjectTable.DoSetMasterField(Field: TField);
begin
  // TODO: DoSetMasterField nao implementado
end;

function TCustomObjectTable.FindMemberType(AObjectType: IObjectType;
  AColumnName: string): IMemberType;
begin
  Result := AObjectType.Member[GetMemberMeta(AColumnName).MemberName];
end;

procedure TCustomObjectTable.GetBlobField(Field: TBlobField; Stream: TStream);
var
  LObject: IObjectType;
  LMemberType: IMemberType;
begin
  if FInsertItem = nil then
    LObject := FObjectList.Items[Current]
  else
    LObject := FInsertItem;

  LMemberType := FindMemberType(LObject, Field.FieldName);
  MemberToStream(LMemberType, Stream);
end;

function TCustomObjectTable.GetCanModify: Boolean;
begin
  Result := True;
end;

function TCustomObjectTable.GetActiveObject: IObjectType;
begin
  Result := nil;
  CheckActive;

  if FInsertItem <> nil then
    Result := FInsertItem
  else
  begin
    if State <> dsEdit then
    begin
      CheckBrowseMode;
      UpdateCursorPos;
    end;

    Result := FObjectList.Items[Current];
  end;
end;

function TCustomObjectTable.GetFieldValue(Field: TField): Variant;
var
  LMember: IMemberType;
  LObject: IObjectType;
  LValue: Variant;
begin
  Result := Null;
  if FInsertItem <> nil then
    LObject := FInsertItem
  else if Current < 0 then
    LObject := nil
  else
    LObject := FObjectList.Items[Current];
  if LObject = nil then
    Result := Null
  else
  begin
    LMember := LObject.Member[GetMemberMeta(Field.FieldName).MemberName];
    MemberToVariant(LMember, LValue, Field.DataType = ftTimeStamp);
    Result := LValue;
  end;
end;

function TCustomObjectTable.GetMemberMeta(ColumnName: string): IMemberMeta;
begin
  Result := GetObjectMeta.FindMember(ColumnName, True, False);
end;

function TCustomObjectTable.GetObjectMeta: IObjectMeta;
begin
  Result := FSession.Mechanism.Mapping.Find(FObjectClassName, False);
end;

function TCustomObjectTable.GetRecordCount: Integer;
begin
  Result := 0;
  if FObjectList <> nil then Result := FObjectList.Count;
end;

procedure TCustomObjectTable.InternalCancelChanges;
var
  I: Integer;
  LObjectType: IObjectType;
  LEditObjectType: IObjectType;
begin
  if not (csDestroying in ComponentState) and Assigned(FBeforeCancelChanges) then
    FBeforeCancelChanges(Self);

  FObjectList.BeginUpdate;
  DisableControls;
  try
    for I := 0 to FObjectList.Count - 1 do
    begin
      LObjectType := FObjectList.Items[I];
      if (LObjectType as IObjectState).State.Modified then
        if (FEditCache.IndexOf(IntToStr(I)) <> NotFound) then
        begin
          LEditObjectType := (FEditCache.Item[IntToStr(I)] as IObjectType);
          LObjectType.Assign(LEditObjectType);
        end;
    end;
    FObjectList.CancelDeleting;
  finally
    FObjectList.EndUpdate;
    EnableControls;
  end;

  if not (csDestroying in ComponentState) and Assigned(FAfterCancelChanges) then
    FAfterCancelChanges(Self);
end;

procedure TCustomObjectTable.UpdateProxyFields;
var
  I: Integer;
begin
  FCriteria.Proxy.Clear;
  for I := 0 to Fields.Count - 1 do FCriteria.AddProxy(Fields[I].FieldName);
end;

procedure TCustomObjectTable.InternalRefresh;
begin
  FInsertItem := nil;
  InternalCancelChanges;
  FEditCache.Clear;
  if FieldsAsProxy then UpdateProxyFields;
  FSession.Load(FObjectList, FCriteria);
end;

function TCustomObjectTable.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  I: Integer;
  LFieldList: TList;
  LNames: array of string;
  LValues: array of string;
begin
  Result := False;
  CheckBrowseMode;
  UpdateCursorPos;
  if (KeyFields = EmptyStr) or IsVarNull(KeyValues) then Exit;
  LFieldList := TList.Create;
  try
    GetFieldList(LFieldList, KeyFields);
    SetLength(LNames, LFieldList.Count);
    SetLength(LValues, LFieldList.Count);
    for I := 0 to LFieldList.Count - 1 do
    begin
      LNames[I] := TField(LFieldList[I]).FieldName;
      if (LFieldList.Count = 1) and not VarIsArray(KeyValues) then
        LValues[I] := VarToStr(KeyValues)
      else
        LValues[I] := VarToStr(KeyValues[I]);
    end;
    I := FObjectList.FindObjectIndex(LNames, LValues);
    Result := I > NotFound;
    if Result then MoveBy(I - Current);
  finally
    FreeAndNil(LFieldList);
  end;
end;

procedure TCustomObjectTable.SetBlobField(Field: TBlobField; Stream: TStream);
var
  LMember: IMemberType;
  LObject: IObjectType;
begin
  if FInsertItem <> nil then
    LObject := FInsertItem
  else
    LObject := FObjectList.Items[Current];

  LMember := FindMemberType(LObject, Field.FieldName);
  StreamToMember(Stream, LMember);
end;

procedure TCustomObjectTable.SetFieldValue(Field: TField; Value: Variant);
var
  LMember: IMemberType;
  LObject: IObjectType;
begin
  if FInsertItem <> nil then
    LObject := FInsertItem
  else
    LObject := FObjectList.Items[Current];

  LMember := FindMemberType(LObject, Field.FieldName);
  VariantToMember(Value, LMember);
end;

procedure TCustomObjectTable.SetObjectClass(const Value: TObjectTypeClass);
begin
  FObjectClass:= Value;
  if FObjectClass <> nil then ObjectClassName:= Value.ClassName;
end;

function TCustomObjectTable.GetObjectClass: TObjectTypeClass;
begin
  if (FObjectClass = nil) and (ObjectClassName <> EmptyStr) then
  begin
    FObjectClass:= TObjectTypeClass(TypeRegister.GetTypeInfo(ObjectClassName,
      True).TypeClass);
  end;
  
  Result:= FObjectClass;
end;

end.

