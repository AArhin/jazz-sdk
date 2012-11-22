unit JazzModel;

interface

uses
  JazzModelIntf,
  JazzCommandSetIntf,
  JazzValueTypeIntf,
  JazzSelectionIntf;

type
  TModel = class(TInterfacedObject, IModel)
  private
    FCommandSet: ICommandSet;
    FObjectValue: IInterface;
    FSelection: ISelection;
  protected
    function GetCommandSet: ICommandSet;
    function GetObjectValue: IInterface;
    function GetSelection: ISelection;
    procedure SetCommandSet(const Value: ICommandSet);
    procedure SetObjectValue(const Value: IInterface);
    procedure SetSelection(const Value: ISelection);

    property CommandSet: ICommandSet read GetCommandSet write SetCommandSet;
    property ObjectValue: IInterface read GetObjectValue write SetObjectValue;
    property Selection: ISelection read GetSelection write SetSelection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TValueTypeModel = class(TModel, IValueTypeModel)
  public
    constructor Create(const ObjectValue: IValueType); reintroduce; virtual;
  end;

  TMemberModel = class(TValueTypeModel, IMemberModel);
  TBlobModel = class(TMemberModel, IBlobModel);
  TBooleanModel = class(TMemberModel, IBooleanModel);
  TCharModel = class(TMemberModel, ICharModel);
  TCurrencyModel = class(TMemberModel, ICurrencyModel);
  TDateModel = class(TMemberModel, IDateModel);
  TFloatModel = class(TMemberModel, IFloatModel);
  TIntegerModel = class(TMemberModel, IIntegerModel);
  TLongIntModel = class(TMemberModel, ILongIntModel);
  TMemoModel = class(TMemberModel, IMemoModel);
  TSmallIntModel = class(TMemberModel, ISmallIntModel);
  TStringModel = class(TMemberModel, IStringModel);
  TWideStringModel = class(TMemberModel, IWideStringModel);

  TObjectModel = class(TMemberModel, IObjectModel)
  public
    constructor Create(const ObjectValue: IValueType); override;
  end;

  TObjectListModel = class(TMemberModel, IObjectListModel)
  public
    constructor Create(const ObjectValue: IValueType); override;
  end;

implementation

uses JazzCommandSet, JazzSelection;

constructor TModel.Create;
begin
  inherited Create;
  FCommandSet:= TCommandSet.Create;
  FSelection:= TSelection.Create;
end;

destructor TModel.Destroy;
begin
  Selection.Clear;
  CommandSet.Clear;

  FCommandSet:= nil;
  FSelection:= nil;
  inherited;
end;

function TModel.GetCommandSet: ICommandSet;
begin
  Result:= FCommandSet;
end;

function TModel.GetObjectValue: IInterface;
begin
  Result:= FObjectValue;
end;

function TModel.GetSelection: ISelection;
begin
  Result:= FSelection;
end;

procedure TModel.SetCommandSet(const Value: ICommandSet);
begin
  FCommandSet:= Value;
end;

procedure TModel.SetObjectValue(const Value: IInterface);
begin
  FObjectValue:= Value;
end;

procedure TModel.SetSelection(const Value: ISelection);
begin
  FSelection:= Value;
end;

{ TModel }

constructor TValueTypeModel.Create(const ObjectValue: IValueType);
begin
  inherited Create;
  FObjectValue:= ObjectValue;
end;

{ TObjectListModel }

constructor TObjectListModel.Create(const ObjectValue: IValueType);
begin
  inherited Create(ObjectValue);
  CommandSet.Add(TObjectListAppendCommand);
  CommandSet.Add(TObjectListInsertCommand);
  CommandSet.Add(TObjectListDeleteCommand);
  CommandSet.Add(TObjectListCancelDeleteCommand);

  CommandSet.Add(TObjectListClearCommand);
  CommandSet.Add(TObjectListLoadCommand);
  CommandSet.Add(TObjectListPostCommand);
end;

{ TObjectModel }

constructor TObjectModel.Create(const ObjectValue: IValueType);
begin
  inherited;
  // add commands
end;

end.
