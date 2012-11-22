unit ObjectLinkManager;


interface

uses Classes, ComCtrls,
  JazzValueTypeIntf, JazzSessionIntf,
  ViewEditores;

type
  TPopulateMethod = procedure(const Owner: IObjectType; const Count: Integer) of object;
  
  IObjectManager = interface(IInterface)
    ['{C011EF90-872E-4E7A-B077-6F82D7C878A5}']
    function GetPopulateDetail: TPopulateMethod;
    function GetClassToManager: TClass;
    function GetInterfaceToManager: TGuid;
    function ListViewObserver: IListViewObserver;
    function Session: ISession;
    function GetObjectList: IObjectListType;
    procedure SetObjectList(const Value: IObjectListType);

    procedure CreateColumns; 
    procedure SetPopulateDetail(const Value: TPopulateMethod);

    procedure Populate(const Count: Integer);
    procedure LinkComponent(const Component: TComponent); 
    property ClassToManager: TClass read GetClassToManager;
    property ObjectList: IObjectListType read GetObjectList write SetObjectList;
    property PopulateDetail: TPopulateMethod read GetPopulateDetail write SetPopulateDetail;
  end;

  TObjectManager = class(TInterfacedObject, IObjectManager)
  private
    FClassToManager: TClass;
    FInterfaceToManager: TGuid;
    FSession: Pointer;
    FListViewObserver: IListViewObserver;      // unit: ViewEditors
    FStatusBarObserver: IStatusBarObserver;    // unit: ViewEditors

    FListView: TListView;
    FObjectList: IObjectListType;
    FStatusBar: TStatusBar;
  protected
    FPopulateDetail: TPopulateMethod;
    function GetPopulateDetail: TPopulateMethod;
    function GetClassToManager: TClass;
    function GetInterfaceToManager: TGuid;
    function ListViewObserver: IListViewObserver;
    function Session: ISession;
    function GetObjectList: IObjectListType;
    procedure SetObjectList(const Value: IObjectListType);

    procedure CreateColumns; virtual; abstract;
    procedure SetPopulateDetail(const Value: TPopulateMethod);

    procedure Populate(const Count: Integer); virtual; abstract;
    procedure LinkComponent(const Component: TComponent); virtual;
    property ClassToManager: TClass read GetClassToManager;
    property ObjectList: IObjectListType read GetObjectList write SetObjectList;
    property PopulateDetail: TPopulateMethod read GetPopulateDetail write SetPopulateDetail;
  public
    constructor Create(const Session: ISession; const ClassToManager: TClass); virtual;
  end;

const
  DebugColumnWidth = 75;

var
  CreateIDColumns: boolean = False;
  CreateDebugColumns: boolean = True;

implementation

uses SysUtils, JazzValueType, JazzTypeInfo, JazzConsts;

constructor TObjectManager.Create(const Session: ISession; const ClassToManager: TClass);
begin
  inherited Create;
  FSession:= Pointer(Session);
  FClassToManager:= ClassToManager;
end;

function TObjectManager.ListViewObserver: IListViewObserver;
begin
  Result:= FListViewObserver;
end;

function TObjectManager.Session: ISession;
begin
  Result:= ISession(FSession);
end;

function TObjectManager.GetObjectList: IObjectListType;
begin
  if FObjectList = nil then
    FObjectList:= TObjectListTypeClass(ClassToManager).NewList;
  Result:= FObjectList;
end;

procedure TObjectManager.LinkComponent(const Component: TComponent);
begin
  if Component is TListView then
  begin
    FListView:= Component as TListView;
    FListViewObserver := TListViewObserver.Create(ObjectList, FListView);
  end
  else
  if Component is TStatusBar then
  begin
    FStatusBar:= Component as TStatusBar;
    FStatusBarObserver:= TStatusBarObserver.Create(ObjectList, FStatusBar);
  end;
end;

function TObjectManager.GetClassToManager: TClass;
begin
  Result:= FClassToManager;
end;

function TObjectManager.GetInterfaceToManager: TGuid;
var
  LTypeInfo: ITypeInfo;
begin
  if GUIDToString(FInterfaceToManager) = GUID_NULL then
  begin
    LTypeInfo:= TypeRegister.GetTypeInfo(ClassToManager);
    if LTypeInfo <> nil then FInterfaceToManager:= LTypeInfo.IID; 
  end;

  Result:= FInterfaceToManager;
end;

function TObjectManager.GetPopulateDetail: TPopulateMethod;
begin
  Result:= FPopulateDetail;
end;

procedure TObjectManager.SetPopulateDetail(const Value: TPopulateMethod);
begin
  FPopulateDetail:= Value;
end;

procedure TObjectManager.SetObjectList(const Value: IObjectListType);
begin
  FObjectList:= Value;
end;

end.
