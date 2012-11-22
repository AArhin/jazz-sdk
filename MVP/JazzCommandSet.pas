unit JazzCommandSet;

interface

uses
  Classes,
  JazzClasses,
  JazzCommandSetIntf,
  JazzNotifyIntf;

type
  TCommandClass = class of TCommand;
  TCommand = class(TInterfacedObject, ICommand)
  private
    FEnabled: boolean;
  protected
    function GetEnabled: boolean;
    function GetName: string; virtual; abstract;
    procedure SetEnabled(const Value: boolean);

    procedure Execute(const Target: IInterface = nil); virtual; abstract;

    property Enabled: boolean read GetEnabled write SetEnabled;
    property Name: string read GetName;
  public
    constructor Create; virtual;
  end;

  TCommandSet = class(TInterfacedObject, ICommandSet)
  private
    FItems: INamedInterfaceList;
  protected
    function Add(const Command: ICommand): Integer; overload;
    function Add(const CommandClass: TClass): ICommand; overload;
    function GetCount: Integer;
    function GetItem(const Name: string): ICommand;
    function GetItems(const Index: Integer): ICommand;
    procedure Clear;
    procedure DisableAll;
    procedure EnableAll;
    procedure Remove(const Command: ICommand);
    property Count: Integer read GetCount;
    property Item[const Name: string]: ICommand read GetItem;
    property Items[const Index: Integer]: ICommand read GetItems; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TObjectListAppendCommand = class(TCommand)
  public
    function GetName: string; override;
    procedure Execute(const Target: IInterface = nil); override;
  end;

  TObjectListInsertCommand = class(TCommand)
  public
    function GetName: string; override;
    procedure Execute(const Target: IInterface = nil); override;
  end;

  TObjectListDeleteCommand = class(TCommand)
  public
    function GetName: string; override;
    procedure Execute(const Target: IInterface = nil); override;
  end;

  TObjectListPostCommand = class(TCommand)
  public
    function GetName: string; override;
    procedure Execute(const Target: IInterface = nil); override;
  end;

  TObjectListCancelDeleteCommand = class(TCommand)
  public
    function GetName: string; override;
    procedure Execute(const Target: IInterface = nil); override;
  end;

  TObjectListLoadCommand = class(TCommand)
  public
    function GetName: string; override;
    procedure Execute(const Target: IInterface = nil); override;
  end;

  TObjectListClearCommand = class(TCommand)
  public
    function GetName: string; override;
    procedure Execute(const Target: IInterface = nil); override;
  end;

implementation

uses
  SysUtils,
  JazzValueType,
  JazzValueTypeIntf,
  JazzConsts,
  JazzModelIntf,
  JazzSession;

{ TCommandSet }

function TCommandSet.Add(const Command: ICommand): Integer;
begin
  Result:= FItems.Add(Command.Name, Command);
end;

function TCommandSet.Add(const CommandClass: TClass): ICommand;
begin
  Result:= TCommandClass(CommandClass).Create; 
  Add(Result);
end;

procedure TCommandSet.Clear;
begin
  FItems.Clear;
end;

constructor TCommandSet.Create;
begin
  inherited Create;
  FItems:= TNamedInterfaceList.Create;
end;

destructor TCommandSet.Destroy;
begin
  Clear;
  inherited;
end;

procedure TCommandSet.DisableAll;
var
  I: Integer;
begin
  for I:= 0 to Count -1 do Items[I].Enabled:= False;
end;

procedure TCommandSet.EnableAll;
var
  I: Integer;
begin
  for I:= 0 to Count -1 do Items[I].Enabled:= True;
end;

function TCommandSet.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

function TCommandSet.GetItem(const Name: string): ICommand;
begin
  Result:= FItems.Item[Name] as ICommand;
end;

function TCommandSet.GetItems(const Index: Integer): ICommand;
begin
  Result:= FItems[Index] as ICommand;
end;

procedure TCommandSet.Remove(const Command: ICommand);
begin
  FItems.Remove(Command);
end;

{ TCommand }

constructor TCommand.Create;
begin
  inherited Create;
  FEnabled:= True;
end;

function TCommand.GetEnabled: boolean;
begin
  Result:= FEnabled;
end;

procedure TCommand.SetEnabled(const Value: boolean);
begin
  FEnabled:= Value;
end;

{ TInsertCommand }

procedure TObjectListInsertCommand.Execute(const Target: IInterface);
var
  LIndex: Integer;
  LModel: IModel;
  LObjectList: IObjectListType;
  LObject: IObjectType;
  LTarget: IObjectEvent;
begin
  if not Supports(Target, IObjectEvent, LTarget) then
  begin
    raise Exception.Create(SIntfNotSupported);
    Exit;
  end;

  if Enabled and Supports(LTarget.Param, IModel, LModel) then
  begin
    LObjectList:= LModel.ObjectValue as IObjectListType;
    LObject:= TObjectTypeClass(LObjectList.ItemClass).Create(LObjectList);

    if LModel.Selection.Count > 0 then
    begin
      LIndex:= LObjectList.IndexOf(LModel.Selection[LModel.Selection.Count -1] as IObjectType);
      if LIndex <> NotFound then
        LObjectList.Insert(Lindex, LObject)
    end
    else
      LIndex:= LObjectList.Add(LObject);

    LTarget.ClearParams;
    LTarget.AddParam(LObject);
    LTarget.AddParam(TypeFactory.NewInteger(LIndex));
  end;
end;

function TObjectListInsertCommand.GetName: string;
begin
  Result:= 'ObjectListInsert';
end;

{ TAppendCommand }

procedure TObjectListAppendCommand.Execute(const Target: IInterface);
var
  LIndex: Integer;
  LModel: IModel;
  LObjectList: IObjectListType;
  LObject: IObjectType;
  LTarget: IObjectEvent;
begin
  if not Supports(Target, IObjectEvent, LTarget) then
  begin
    raise Exception.Create(SIntfNotSupported);
    Exit;
  end;

  if Enabled and Supports(LTarget.Param, IModel, LModel) then
  begin
    LObjectList:= LModel.ObjectValue as IObjectListType;
    LObject:= TObjectTypeClass(LObjectList.ItemClass).Create(LObjectList);
    LIndex:= LObjectList.Add(LObject);
    LTarget.ClearParams;
    LTarget.AddParam(LObject);
    LTarget.AddParam(TypeFactory.NewInteger(LIndex));
  end;
end;

function TObjectListAppendCommand.GetName: string;
begin
  Result:= 'ObjectListAppend';
end;

{ TDeleteCommand }

procedure TObjectListDeleteCommand.Execute(const Target: IInterface);
var
  I: Integer;
  LModel: IModel;
  LObjectList: IObjectListType;
  LObject: IObjectType;
  LTarget: IObjectEvent;
begin
  if not Supports(Target, IObjectEvent, LTarget) then
  begin
    raise Exception.Create(SIntfNotSupported);
    Exit;
  end;

  if Enabled and Supports(LTarget.Param, IModel, LModel) then
  begin
    LObjectList:= LModel.ObjectValue as IObjectListType;

    if LModel.Selection.Count > 0 then
    begin
      for I:= LModel.Selection.Count -1 downto 0 do
      begin
        LObject:= LModel.Selection[I] as IObjectType;
        if (LObject <> nil) then
        begin
          if (LObject as IObjectState).State.Loaded or
             (LObject as IObjectState).State.Persisted then
            LObject.Delete
          else
            LObjectList.RemoveObject(LObject);
        end;
      end;
    end
  end;
end;

function TObjectListDeleteCommand.GetName: string;
begin
  Result:= 'ObjectListDelete';
end;

{ TObjectListLoadCommand }

procedure TObjectListLoadCommand.Execute(const Target: IInterface);
var
  LObjectList: IObjectListType;
begin
  if Supports(Target, IObjectListType, LObjectList) then
  begin
    LObjectList.BeginUpdate;
    try
      ActiveSession.Load(LObjectList);
    finally
      LObjectList.EndUpdate;
    end;
  end;
end;

function TObjectListLoadCommand.GetName: string;
begin
  Result:= 'ObjectListLoad';
end;

{ TObjectListClearCommand }

procedure TObjectListClearCommand.Execute(const Target: IInterface);
var
  LObjectList: IObjectListType;
begin
  if Supports(Target, IObjectListType, LObjectList) then
  begin
    LObjectList.BeginUpdate;
    try
      LObjectList.Clear;
    finally
      LObjectList.EndUpdate;
    end;
  end;
end;

function TObjectListClearCommand.GetName: string;
begin
  Result:= 'ObjectListClear';
end;

{ TObjectListCancelDeleteCommand }

procedure TObjectListCancelDeleteCommand.Execute(const Target: IInterface);
var
  LObjectList: IObjectListType;
begin
  if Supports(Target, IObjectListType, LObjectList) then
  begin
    LObjectList.BeginUpdate;
    try
      LObjectList.CancelDeleting;
    finally
      LObjectList.EndUpdate;
    end;
  end;
end;

function TObjectListCancelDeleteCommand.GetName: string;
begin
  Result:= 'ObjectListCancelDelete'; 
end;

{ TObjectListPostCommand }

procedure TObjectListPostCommand.Execute(const Target: IInterface);
var
  LModel: IModel;
  LObjectList: IObjectListType;
  LTarget: IObjectEvent;
begin
  if not Supports(Target, IObjectEvent, LTarget) then
  begin
    raise Exception.Create(SIntfNotSupported);
    Exit;
  end;

  if Enabled and Supports(LTarget.Param, IModel, LModel) then
  begin
    LObjectList:= LModel.ObjectValue as IObjectListType;
    ActiveSession.Save(LObjectList);
  end;
end;

function TObjectListPostCommand.GetName: string;
begin
  Result:= 'ObjectListPost';
end;

end.
